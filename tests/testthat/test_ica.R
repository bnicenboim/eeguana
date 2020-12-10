context("test eeg ica")
library(eeguana)
set.seed(123)

## simulate eye movements
## https://stackoverflow.com/questions/53071709/remove-eye-blinks-from-eeg-signal-with-ica

N <- 4000
fs <- 100
blink <- rbinom(N, 1, .003) %>%
  signal::filter(signal::butter(2, c(1 * 2 / fs, 10 * 2 / fs), "pass"), .) * 200
noise <- rpink(N, 100)
alpha <- (abs(sin(10 * seq_len(N) / fs)) - 0.5) * 3

s_tbl <- dplyr::tibble(sample = rep(seq_len(N), times = 3), A = c(blink, noise, alpha), component = rep(c("blink", "noise", "alpha"), each = N))
# ggplot(s_tbl,aes(x=sample,y=A)) + geom_line() + facet_grid(component~.)

# And they mix depending on the distance of S_pos:

signal_blinks <- dplyr::tibble(
  Fz = blink * 2 + alpha * 1 + noise,
  Cz = blink * 1 + alpha * .9 + noise,
  Pz = blink * .1 + alpha * 1 + noise
) %>%
  dplyr::mutate_all(channel_dbl)


signal <- dplyr::tibble(
  Fz = alpha * .1 + noise,
  Cz = alpha * .15 + noise,
  Pz = alpha * .1 + noise
) %>%
  dplyr::mutate_all(channel_dbl)

data <- eeg_lst(
  signal_tbl = signal %>%
    dplyr::mutate(
      .id = 1L,
      .sample = sample_int(seq_len(N), sampling_rate = 500)
    ),
  segments_tbl = dplyr::tibble(.id = 1L, .recording = "recording1", segment = 1L)
)

data_blinks <- eeg_lst(
  signal_tbl = signal_blinks %>%
    dplyr::mutate(
      .id = 1L,
      .sample = sample_int(seq_len(N), sampling_rate = 500)
    ),
  segments_tbl = dplyr::tibble(.id = 1L, .recording = "recording1", segment = 1L)
)


data_blinks_more <- eeg_lst(
  signal_tbl = signal_blinks %>%
    dplyr::mutate(
      .id = rep(1:4, each = N / 4),
      .sample = sample_int(rep(seq_len(N / 4), times = 4), sampling_rate = 500)
    ),
  segments_tbl = dplyr::tibble(.id = seq.int(4), .recording = paste0("recording", c(1, 1, 2, 2)), segment = seq.int(4))
)

data_more <- eeg_lst(
  signal_tbl = signal %>%
    dplyr::mutate(
      .id = rep(1:4, each = N / 4),
      .sample = sample_int(rep(seq_len(N / 4), times = 4), sampling_rate = 500)
    ),
  segments_tbl = dplyr::tibble(.id = seq.int(4), .recording = paste0("recording", c(1, 1, 2, 2)), segment = seq.int(4))
)

data_blinks_more_NA <- data_blinks_more
data_blinks_more_NA$.signal[1, ]$Fz <- NA_real_
data_blinks_more_NA$.signal[5, ]$Cz <- NA_real_

# library(ggplot2)
# plot(data_blinks)
# xx <- eeg_artif_peak(data_blinks,threshold = .3)
# plot(xx) +annotate_events()

m <- structure(c(
  -1.316038587729, -0.325231960551256, 3.97869823946626,
  -0.122103882193917, -0.0556199678365175, 1.02305178660224, -2.4335067665471,
  1.90720205416033, -0.116728090380369
), .Dim = c(3L, 3L))
data_fast_ICA <- eeg_ica(
  .data = data_blinks, .method = fast_ICA,
  .config = list(w.init = m)
)
data_fast_ICA2 <- eeg_ica(
  .data = data_blinks %>%
    dplyr::mutate(XEOG = channel_dbl(rnorm(nsamples(data_blinks)))),
  -XEOG, .method = fast_ICA,
  .config = list(w.init = m)
)
test_that("ica summaries", {

  out1 <- structure(list(.recording = c("recording1", "recording1", 
                                        "recording1"), 
                         EOG = c("Fz", "Fz", "Fz"), 
                         .ICA = c("ICA2", "ICA3", "ICA1"),
                         cor = c(-0.643107011726606, -0.765208111591799, 0.0294943625487529),
                         var = c(0.71789949085039, 0.280971141808226, 0.00112936734134494)), 
                    class = c("data.table", "data.frame"), row.names = c(
    NA,
    -3L
  ))

  out2 <- structure(list(.recording = c("recording1", "recording1", "recording1"
  ), EOG = c("XEOG", "XEOG", "XEOG"), .ICA = c("ICA2", "ICA3", 
                                               "ICA1"), cor = c(-0.0113051429911744, 0.0136562903764449, 0.00953493200108279
                                               ), var = c(0.71789949085039, 0.280971141808226, 0.00112936734134494
                                               )), class = c("data.table", "data.frame"), row.names = c(NA, 
                                                                                                        -3L))

  expect_equal(eeg_ica_summary_tbl(data_fast_ICA, "Fz"), out1, check.attributes = FALSE)
  expect_equal(eeg_ica_summary_tbl(data_fast_ICA2), out2, check.attributes = FALSE)
})



test_that("different implementations aren't too different", {
  ## initial matrix to have deterministic ICA mixing and unmixing matrices here:
  recover_fast_ICA <- data_fast_ICA %>%
    eeg_ica_show(ICA1, ICA2, ICA3) %>%
    dplyr::select(ICA1, ICA2, ICA3) %>%
    signal_tbl() %>%
    .[, .(alpha = scale(ICA1), noise = scale(ICA2), blink = scale(ICA3))] %>%
    as.matrix()
  data_adapt_fast_ica <- eeg_ica(data_blinks, .method = adapt_fast_ICA)
  recover_adapt_fast_ICA <- data_adapt_fast_ica %>%
    eeg_ica_show(ICA1, ICA2, ICA3) %>%
    dplyr::select(ICA1, ICA2, ICA3) %>%
    signal_tbl() %>%
    .[, .(alpha = -scale(ICA2), noise = scale(ICA3), blink = scale(ICA1))] %>%
    as.matrix()
  data_fast_ICA2 <- eeg_ica(data_blinks,
    .method = fast_ICA2,
    .config = list(init = m)
  )
  recover_fast_ICA2 <- data_fast_ICA2 %>%
    eeg_ica_show(ICA1, ICA2, ICA3) %>%
    dplyr::select(ICA1, ICA2, ICA3) %>%
    signal_tbl() %>%
    .[, .(alpha = scale(ICA2), noise = scale(ICA3), blink = scale(ICA1))] %>%
    as.matrix()
  true_comps <- data.frame(alpha = scale(alpha), noise = scale(noise), blink = scale(blink)) %>%
    as.matrix()

  expect_equivalent(true_comps, recover_fast_ICA, tolerance = 1)
  expect_equivalent(true_comps, recover_fast_ICA2, tolerance = .5)
  expect_equivalent(true_comps, recover_adapt_fast_ICA, tolerance = .3)
})

test_that("warns if problems", {
  expect_warning(eeg_ica(data_blinks, .method = fast_ICA, .config = list(maxit = 2)))
  ## TODO TRY TO produce non convergencies
  ## expect_warning(eeg_ica(data_blinks, .method = adapt_fast_ICA, .config=list(eps = 1e-12, maxiter =1)))
  ## expect_warning(eeg_ica(data_blinks, .method = fast_ICA2, .config=list(eps = 1e-12, maxiter =1)))
})


test_that("summaries work", {
  data_rec_default <- data_fast_ICA %>% eeg_ica_keep(ICA1, ICA2, ICA3)
  ica1 <- eeg_ica_show(data_fast_ICA, ICA1)
  ica2 <- eeg_ica_show(data_fast_ICA, ICA1, ICA2, ICA3)
  cors <- structure(list(.recording = c("recording1", "recording1", "recording1", "recording1", "recording1", "recording1"), EOG = c("Cz", "Fz", "Fz", "Cz", "Fz", "Cz"), .ICA = structure(c(2L, 3L, 2L, 3L, 1L, 1L), .Label = c("ICA1", "ICA2", "ICA3"), class = "factor"), cor = c(-0.854894402022447, -0.765208111591799, -0.643107011726606, -0.518101988618226, 0.0294943625487529, 0.0269423603363018)), row.names = c(NA, -6L), class = c("data.table", "data.frame"))
  expect_equal(eeg_ica_cor_tbl(data_fast_ICA, tidyselect::all_of(c("Fz", "Cz"))), cors)
  vars <- structure(list(
    .recording = c("recording1", "recording1", "recording1"),
    .ICA = c("ICA2", "ICA3", "ICA1"),
    var = c(0.71789949085039, 0.280971141808226, 0.00112936734134494)
  ),
  row.names = c(NA, -3L),
  class = c("data.table", "data.frame")
  )

  expect_equal(eeg_ica_var_tbl(data_fast_ICA), vars)
  expect_equal(ncomponents(data_fast_ICA), 3)
  expect_equal(component_names(data_fast_ICA), c("ICA1", "ICA2", "ICA3"))
  expect_equal(channel_ica_names(data_fast_ICA), c("Fz", "Cz", "Pz"))
})

test_that("ica is a reversible", {
  data_rec <- data_fast_ICA %>%
    eeg_ica_keep(ICA1, ICA2, ICA3)

  data_ica_m <- eeg_ica(data_blinks, .method = adapt_fast_ICA)
  data_rec_m <- data_ica_m %>%
    eeg_ica_keep(ICA1, ICA2, ICA3)

  data_ica_Fz <- eeg_ica(data_blinks, -Fz, .method = fast_ICA)
  data_rec_Fz <- data_ica_Fz %>%
    eeg_ica_keep(ICA1, ICA2)
  ica1 <- eeg_ica_show(data_fast_ICA, ICA1)
  data_rec_default <- data_fast_ICA %>% eeg_ica_keep(ICA1, ICA2, ICA3)
  expect_equal(data_blinks, as_eeg_lst(data_rec_default))
  expect_equal(data_blinks, as_eeg_lst(data_blinks))
  expect_equal(data_blinks$.signal, data_rec_default$.signal)
  expect_equal(data_blinks$.signal, data_rec$.signal)
  expect_equal(data_blinks$.signal, data_rec_m$.signal)
  expect_equal(data_blinks$.signal, data_rec_Fz$.signal)
  expect_true(is_component_dbl(ica1$.signal[[6]]))
})

if (0) {
  eeg_ica_show(data_fast_ICA, ICA1, ICA2, ICA3) %>% plot() # ICA3 is blinks
  data_fast_ICA %>% plot()
  data_no_blinks <- data_fast_ICA %>% eeg_ica_keep(-ICA3)
  data_no_blinks %>% plot()
  bind(
    data_no_blinks %>% dplyr::mutate(ICA = "yes") %>% as_eeg_lst(),
    data_blinks %>% dplyr::mutate(ICA = "no")
  ) %>%
    ggplot2::ggplot(ggplot2::aes(x = .time, y = .value)) +
    ggplot2::geom_line(ggplot2::aes(color = ICA, alpha = .5))
}

# data_fast_ICA %>% eeg_ica_keep(ICA2,ICA3) %>% plot()

data_no_blinks <- data_fast_ICA %>% eeg_ica_keep(-ICA3)
data_blinks_ref <- dplyr::mutate(data_blinks, R = channel_dbl(0))
data_ica_default_ref <- eeg_ica(data_blinks_ref, -R, .config = list(w.init = m))
## eeg_ica_show(data_ica_default_ref, ICA1, ICA2, ICA3) %>% plot()
data_no_blinks_ref <- data_ica_default_ref %>% eeg_ica_keep(-ICA3)
test_that("ica can remove blinks", {
  expect_equal(data$.signal, data_no_blinks$.signal, tolerance = .14)
  expect_equal(data$.signal, data_no_blinks_ref$.signal[, -6], tolerance = .14)
})

test_that("can use other (python) functions", {
  skip_on_ci()

  py_fica <- function(x) {
    x <- as.matrix(x)
    reticulate::use_condaenv("anaconda3")
    sk <- reticulate::import("sklearn.decomposition")
    ica <- sk$FastICA(whiten = TRUE, random_state = 23L)
    X <- scale(x, scale = FALSE) %>%
    as.matrix(x)
    S <- ica$fit_transform(X)
    W <- t(ica$components_)
    list(W = W)
  }
  data_ica_py <- eeg_ica(data_blinks, .method = py_fica)
  ## data_ica_py %>% eeg_ica_show(ICA1, ICA2, ICA3) %>%
  ##   dplyr::select(ICA1,ICA2, ICA3) %>%
  ##   plot()
  data_no_blinks_py <- data_ica_py %>% eeg_ica_keep(-ICA1)
  expect_equal(data$.signal, data_no_blinks_py$.signal, tolerance = .14)
})


data_ica_2participants <- data_blinks_more %>%
  eeg_ica(.method = fast_ICA, .config = list(w.init = m))

## data_ica_2participants %>%
##   eeg_ica_show(ICA1:ICA3) %>%
##   dplyr::select(ICA1:ICA3) %>%
##   plot() + ggplot2::facet_wrap(.recording~.key)

data_ica_2participants_keepall <- eeg_ica_keep(data_ica_2participants, ICA1, ICA2, ICA3)

# keeps everything also:
data_ica_2participants_exFz <- eeg_ica(data_blinks_more, -Fz, .method = fast_ICA) %>%
  eeg_ica_keep(ICA1, ICA2)

data_blinks_more_no_blinks <- data_ica_2participants %>%
  eeg_ica_keep(recording1 = -ICA3, recording2 = -ICA3)

data_ica_p2 <- data_ica_2participants %>%
  dplyr::filter(.recording != "recording1") %>%
  eeg_ica_keep(-ICA3)


data_ica_p2_filtered <- data_blinks_more %>%
  dplyr::filter(.recording != "recording1") %>%
  eeg_ica(.method = fast_ICA, .config = list(w.init = m)) %>%
  eeg_ica_keep(-ICA3)


test_that("ica grouped works", {
  expect_equal(data_blinks_more$.signal, data_ica_2participants_keepall$.signal)
  expect_equal(data_blinks_more$.signal, data_ica_2participants_exFz$.signal)
  expect_equal(data_more$.signal, data_blinks_more_no_blinks$.signal, tolerance = .15)
  expect_equal(dplyr::filter(data_more, .recording == "recording2")$.signal, data_ica_p2$.signal,
    tolerance = .16
  )
  expect_equal(signal_tbl(data_ica_p2), signal_tbl(data_ica_p2_filtered), tolerance = .15)
  expect_equal(segments_tbl(data_ica_p2), segments_tbl(data_ica_p2_filtered), tolerance = .15)
})

data_ica_b_m_NA <- data_blinks_more_NA %>% eeg_ica()
data_ica_b_m_NA %>% eeg_ica_show(ICA1, ICA2, ICA3)
data_b_m_rec_NA <- eeg_ica_keep(data_ica_b_m_NA, ICA1:ICA3)

test_that("ica with NAs is a reversible", {
  expect_equal(
    dplyr::filter(data_blinks_more_NA, !(.id == 1 & .sample == 1 | .sample == 5))$.signal,
    dplyr::filter(data_b_m_rec_NA, !(.id == 1 & .sample == 1 | .sample == 5))$.signal
  )
})

test_that("other functions work correctly in the eeg_ica_lst", {
  ## TODO take care of changes in channel names
  ## expect_error(channels_tbl(data_ica_default) <- channels_tbl(data_ica_default) %>% dplyr::mutate(.channel = paste(.channel, "1")))
  expect_error(select(data_fast_ICA, -Fz) %>% eeg_ica_keep(ICA1, ICA2, ICA3)) # TODO, better error
  expect_equal(class(as_eeg_lst(data_fast_ICA)), "eeg_lst")
})
