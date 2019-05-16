context("test eeg ica")
library(eeguana)
set.seed(123)

## simulate eye movements
## https://stackoverflow.com/questions/53071709/remove-eye-blinks-from-eeg-signal-with-ica

N <- 4000
fs <- 100
blink <- rbinom(N, 1, .003) %>%
  signal::filter(signal::butter(2, c(1 * 2 / fs, 10 * 2 / fs), "pass"), .)
noise <- rpink(N)
alpha <- (abs(sin(10 * seq_len(N) / fs)) - 0.5) / 2

s_tbl <- dplyr::tibble(sample = rep(seq_len(N), times = 3), A = c(blink, noise, alpha), component = rep(c("blink", "noise", "alpha"), each = N))
# ggplot(s_tbl,aes(x=sample,y=A)) + geom_line() + facet_grid(component~.)

# And they mix depending on the distance of S_pos:

signal_blinks <- dplyr::tibble(
  Fz = blink * 2 + alpha * .1 + noise,
  Cz = blink * 1 + alpha * .15 + noise,
  Pz = blink * .1 + alpha * .1 + noise
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
plot(data)

data_ica_default <- eeg_ica(data_blinks, method = fast_ICA)
data_rec_default <- data_ica_default %>% eeg_ica_keep(ICA1, ICA2, ICA3)

ica1 <- eeg_ica_show(data_ica_default, ICA1)
ica2 <- eeg_ica_show(data_ica_default, ICA1, ICA2,ICA3)
#plot(ica2)
data_rec <- data_ica_default %>% eeg_ica_keep(ICA1, ICA2, ICA3)

data_ica_m <- eeg_ica(data_blinks, method = adapt_fast_ICA)
data_rec_m <- data_ica_m %>% eeg_ica_keep(ICA1, ICA2, ICA3)

data_ica_Fz <- eeg_ica(data_blinks, -Fz, method = fast_ICA)
data_rec_Fz <- data_ica_Fz %>% eeg_ica_keep(ICA1, ICA2)

test_that("ica is a reversible", {
  expect_equal(data_blinks$.signal, data_rec_default$.signal)
  expect_equal(data_blinks$.signal, data_rec$.signal)
  expect_equal(data_blinks$.signal, data_rec_m$.signal)
  expect_equal(data_blinks$.signal, data_rec_Fz$.signal)
  expect_true(is_component_dbl(ica1$.signal[[6]]))
})

## plot(blink)
eeg_ica_show(data_ica_default, ICA1, ICA2, ICA3) %>% plot()
data_no_blinks <- data_ica_default %>% eeg_ica_keep(-ICA3)
## data_ica_default %>% plot()
## data_no_blinks %>% plot()
# data_ica_default %>% eeg_ica_keep(ICA2,ICA3) %>% plot()

data_blinks_ref <- dplyr::mutate(data_blinks, R = channel_dbl(0))
data_ica_default_ref <- eeg_ica(data_blinks_ref, -R)
data_no_blinks_ref <- data_ica_default_ref %>% eeg_ica_keep(-ICA3)

test_that("ica can remove blinks", {
  expect_equal(data$.signal, data_no_blinks$.signal, tolerance = .009)
  expect_equal(data$.signal, data_no_blinks_ref$.signal[, -6], tolerance = .009)
})

test_that("can use other (python) functions", {
  skip_on_appveyor()
  skip_on_travis()
  skip_on_cran()
  py_fica <- function(x) {
    x <- as.matrix(x)
    sk <- reticulate::import("sklearn.decomposition")
    ica <- sk$FastICA(whiten = TRUE, random_state = 23L)
    X <- scale(x, scale = FALSE)
    ## X  + matrix(colMeans(X), nrow=nrow(S), ncol=3,byrow = TRUE)
    as.matrix(x)
    S <- ica$fit_transform(X)
    A <- t(ica$mixing_)
    W <- ica$components_
    ## attributes(X) <- NULL
    ## X <- matrix(X, ncol =3)
    ## all.equal(tcrossprod(S, t(A)), X)
    list(A = A, W = W)
  }
  data_ica_py <- eeg_ica(data_blinks, method = py_fica)
  plot(data_ica_py %>% eeg_ica_show(ICA1, ICA2, ICA3))
  data_no_blinks_py <- data_ica_py %>% eeg_ica_keep(-ICA1)
  expect_equal(data$.signal, data_no_blinks_py$.signal, tolerance = .009)
})


data_ica_b_m <- data_blinks_more %>% eeg_ica(method= fast_ICA)
data_ica_b_m %>% eeg_ica_show(ICA1:ICA3) %>% plot() + ggplot2::facet_wrap(.recording~.key)
data_b_m_rec <- eeg_ica_keep(data_ica_b_m, ICA1, ICA2, ICA3)
data_b_m_rec_Fz <- eeg_ica(data_blinks_more, -Fz, method = fast_ICA) %>% eeg_ica_keep(ICA1, ICA2)

data_blinks_more_no_blinks <- data_ica_b_m %>%
  eeg_ica_keep(recording1 = -ICA3, recording2=-ICA1)

data_rec2 <- data_ica_b_m %>% dplyr::filter(.recording != "recording1") %>% eeg_ica_keep(-ICA1)



test_that("ica grouped works", {
  expect_equal(data_blinks_more$.signal, data_b_m_rec$.signal)
  expect_equal(data_blinks_more$.signal, data_b_m_rec_Fz$.signal)
  expect_equal(data_more$.signal, data_blinks_more_no_blinks$.signal, tolerance = .009)
  expect_equal(dplyr::filter(data_more, .recording == "recording2")$.signal, data_rec2$.signal,
    tolerance = .01
  )
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
  expect_error(select(data_ica_default, -Fz) %>% eeg_ica_keep(ICA1, ICA2, ICA3)) # TODO, better error
  expect_equal(class(as_eeg_lst(data_ica_default)), "eeg_lst")
})

