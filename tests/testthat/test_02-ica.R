library(eeguana)
set.seed(123)
options(eeguana.verbose=FALSE)

data <- eeguana:::data_no_blinks
data_blinks <- eeguana:::data_blinks
data_blinks_more <- eeguana:::data_blinks_2
data_more <- eeguana:::data_no_blinks_2

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

if (0) {
# check visually that it works fine
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

test_that("ica summaries", {
  expect_snapshot(eeg_ica_summary_tbl(data_fast_ICA, "Fz"))
  expect_snapshot(eeg_ica_summary_tbl(data_fast_ICA2))
})



test_that("different implementations aren't too different", {
  ## initial matrix to have deterministic ICA mixing and unmixing matrices here:
  recover_fast_ICA <- data_fast_ICA %>%
    eeg_ica_show(ICA1, ICA2, ICA3) %>%
    dplyr::select(ICA1, ICA2, ICA3) %>%
    signal_tbl() %>%
    .[, .(alpha = scale(ICA1), noise = scale(ICA2), blink = -scale(ICA3))] %>%
    as.matrix()


## mixed <- data_blinks$.signal[,-c(".id",".sample")] %>% as.matrix()
## fICA::adapt_fICA(mixed)
## out <- fICA::fICA(mixed)
## S <- out$S %>% data.table::as.data.table() %>%
##   data.table::melt(measure=1:3)
## S[,sample := 1:.N , by = "variable"][]

##   ggplot(S, aes(x = sample, y= value)) +
##     facet_wrap(.~variable, ncol =1) +
##     geom_line()

  data_adapt_fast_ICA <- eeg_ica(data_blinks, .method = adapt_fast_ICA)
  recover_adapt_fast_ICA <- data_adapt_fast_ICA %>%
    eeg_ica_show(ICA1, ICA2, ICA3) %>%
    dplyr::select(ICA3, ICA2, ICA1) %>%
    signal_tbl() %>%
    .[, .(alpha = -scale(ICA1), noise = scale(ICA3), blink = scale(ICA2))] %>%
    as.matrix()

  data_fast_ICA2 <- eeg_ica(data_blinks,
    .method = fast_ICA2,
    .config = list(init = m)
  )
  recover_fast_ICA2 <- data_fast_ICA2 %>%
    eeg_ica_show(ICA1, ICA2, ICA3) %>%
    dplyr::select(ICA1, ICA2, ICA3) %>%
    signal_tbl() %>%
    .[, .(alpha = -scale(ICA3), noise = scale(ICA2), blink = scale(ICA1))] %>%
    as.matrix()

  true_comps <- scale(eeguana:::true_comps)%>% c() %>% matrix(ncol=3)
if(0){
##  check how they look
  eeg_ica_show(data_adapt_fast_ICA, ICA1, ICA2, ICA3) %>% plot()
  S <- true_comps %>% data.table::as.data.table() %>%
  data.table::melt(measure=1:3)
  S[,sample := 1:.N , by = "variable"][]
  ggplot(S, aes(x = sample, y= value)) +
    facet_wrap(.~variable, ncol =1) +
    geom_line()
}
  expect_equal(true_comps, recover_fast_ICA, tolerance = .05, ignore_attr = TRUE)
  expect_equal(true_comps, recover_fast_ICA2, tolerance = .15, ignore_attr = TRUE)
  expect_equal(true_comps, recover_adapt_fast_ICA, tolerance = .1, ignore_attr = TRUE)
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

  expect_snapshot(eeg_ica_cor_tbl(data_fast_ICA, tidyselect::all_of(c("Fz", "Cz"))))
  expect_snapshot(eeg_ica_var_tbl(data_fast_ICA))
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


# data_fast_ICA %>% eeg_ica_keep(ICA2,ICA3) %>% plot()

data_no_blinks <- data_fast_ICA %>% eeg_ica_keep(-ICA3)
data_blinks_ref <- dplyr::mutate(data_blinks, R = channel_dbl(0))
data_ica_default_ref <- eeg_ica(data_blinks_ref, -R, .config = list(w.init = m))
## eeg_ica_show(data_ica_default_ref, ICA1, ICA2, ICA3) %>% plot()
data_no_blinks_ref <- data_ica_default_ref %>% eeg_ica_keep(-ICA3)
test_that("ica can remove blinks", {
  expect_equal(data$.signal, data_no_blinks$.signal, tolerance = .25)
  expect_equal(data$.signal, data_no_blinks_ref$.signal[, -6], tolerance = .25)
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
  expect_equal(data_more$.signal, data_blinks_more_no_blinks$.signal, tolerance = .25)
  expect_equal(dplyr::filter(data_more, .recording == "recording2")$.signal, data_ica_p2$.signal,
    tolerance = .35
  )
  expect_equal(signal_tbl(data_ica_p2), signal_tbl(data_ica_p2_filtered), tolerance = .15)
  expect_equal(segments_tbl(data_ica_p2), segments_tbl(data_ica_p2_filtered), tolerance = .15)
})

data_ica_b_m_NA <- data_blinks_more_NA %>% eeg_ica()
#data_ica_b_m_NA %>% eeg_ica_show(ICA1, ICA2, ICA3)
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
  expect_error(dplyr::select(data_fast_ICA, -Fz) %>% eeg_ica_keep(ICA1, ICA2, ICA3)) # TODO, better error
  expect_equal(class(as_eeg_lst(data_fast_ICA)), "eeg_lst")
})
