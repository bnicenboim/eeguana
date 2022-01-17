library(eeguana)
set.seed(123)
options(eeguana.verbose=FALSE)

N <- 1000
signal <- dplyr::tibble(
  X1 = sin(seq_len(N)), # f = 1/(2* pi)
  X2 = sin(seq_len(N) * 2), # f = 3/(2*pi)
  X3 = sin(seq_len(N) * 3), # f = 5/(2*pi)
  X4 = X1 + X2 + X3
) %>%
  dplyr::mutate_all(channel_dbl)

data_sin <- eeg_lst(
  signal_tbl = signal %>%
    dplyr::mutate(
      .id = 1L,
      .sample = sample_int(seq_len(N), sampling_rate = 500)
    ),
  segments_tbl = dplyr::tibble(.id = 1L, .recording = "recording1", segment = 1L)
)


data_sin_more <- eeg_lst(
  signal_tbl = signal %>%
    dplyr::mutate(
      .id = rep(1:4, each = N / 4),
      .sample = sample_int(rep(seq_len(N / 4), times = 4), sampling_rate = 500)
    ),
  segments_tbl = dplyr::tibble(.id = seq.int(4), .recording = paste0("recording", c(1, 1, 2, 2)), segment = seq.int(4))
)

data_sin_ref <- data.table::copy(data_sin)
data_sin_more_ref <- data.table::copy(data_sin_more)

data_sin_X1 <- eeg_filt_low_pass(data_sin, .freq = 500 * 1.5 / (2 * pi))

data_sin_X1_iir <- eeg_filt_low_pass(data_sin, .freq = 500 * 1.5 / (2 * pi), .config = list(method = "iir"))
 # data_sin_X1r <- data.table::copy(data_sin)
  # eeg_filt_low_pass(data_sin_X1r, .freq = 500 * 1.5 / (2 * pi), .by_reference = TRUE)
  # data_sin_X1r_iir <- data.table::copy(data_sin)
  # eeg_filt_low_pass(data_sin_X1r_iir, .freq = 500 * 1.5 / (2 * pi), .config = list(method = "iir"), .by_reference = TRUE)

## microbenchmark::microbenchmark(
## eeg_filt_low_pass(data_sin, .freq = 500 * 1 / (2 * pi)),
## eeg_filt_low_pass(data_sin, .freq = 500 * 1 / (2 * pi), .config = list(method = "iir")
## )
## Unit: milliseconds
##                                                                    expr      min       lq     mean
##                   eeg_filt_low_pass(data_sin, .freq = 500 * 1/(2 * pi)) 3.903569 4.193401 7.619947
##  eeg_filt_low_pass(data_sin, .freq = 500 * 1/(2 * pi), .config = list(method = "iir") 4.700039 4.876091 5.514172
##    median       uq       max neval
##  4.451486 5.260478 265.55056   100
##  5.148132 5.792554  12.22652   100


### plot(data_sin)
## plot(data_sin_X1)
## plot(data_sin_X1_iir)


test_that("low pass default pars",{
  expect_equal(data_sin_X1, eeg_filt_low_pass(data_sin, .freq = 500 * 1.5 / (2 * pi), .config = list(h_trans_bandwidth = 29.8415518297304 )))
  expect_equal( data_sin_X1_iir, eeg_filt_low_pass(data_sin, .freq = 500 * 1.5 / (2 * pi),  .config = list(method = "iir",type = "butter", order = 6)))
})

test_that("low pass signal fir", {
  data_sin_X1 <- data_sin_X1 %>% dplyr::filter(as_time(.sample) %>% between(.25, 1.75))
  data_sin <- data_sin %>% dplyr::filter(as_time(.sample) %>% between(.25, 1.75))
  expect_equal(data_sin_X1$.signal$X1, data_sin_X1$.signal$X4, tolerance = .002)
  expect_equal(data_sin_X1$.signal$X1, data_sin$.signal$X1, tolerance = .005)
  ## expect_lte(max(data_sin_X1$.signal$X2, data_sin_X1$.signal$X3), .001)
})

test_that("low pass iir and fir are not too different", {
  expect_equal(data_sin_X1 %>% dplyr::filter(as_time(.sample) %>% between(.25, 1.75)),
               data_sin_X1_iir %>% dplyr::filter(as_time(.sample) %>% between(.25, 1.75)), tolerance = .01)
})

test_that("low pass iir python implementation is not too different", {
   eeguana:::skip_on_actions()
   skip_on_ci()
   mne <- reticulate::import("mne")
  X3_iirmne <-  mne$filter$filter_data(signal$X3, sfreq =500, h_freq = 500 * 1.5 / (2 * pi), l_freq = NULL, method='iir', iir_params = list(ftype = "butter", order = 6, output ="ba"))

  expect_equal(data_sin_X1_iir$.signal$X3 %>% c(),
               X3_iirmne %>% c(), tolerance = .001)
#ggplot(data = data.frame(y= c(X3_iirmne), x=seq_along(c(X3_iirmne))), aes(x=x,y=y)) + geom_line()
})

data_sin_X3 <- eeg_filt_high_pass(data_sin, .freq = 500 * 3 / (2 * pi))
data_sin_X3_iir <- eeg_filt_high_pass(data_sin, .freq = 500 * 3 / (2 * pi), .config = list(method = "iir"))
# data_sin_X3r <- data.table::copy(data_sin)
# eeg_filt_high_pass(data_sin_X3r, .freq = 500 * 3 / (2 * pi), .by_reference = TRUE)
## plot(data_sin_X3)

test_that("high pass signal", {
  data_sin_X3 <- data_sin_X3 %>% dplyr::filter(as_time(.sample) %>% between(.25, 1.75))
  data_sin <- data_sin %>% dplyr::filter(as_time(.sample) %>% between(.25, 1.75))
  expect_equal(data_sin_X3$.signal$X3, data_sin_X3$.signal$X4, tolerance = .004)
  expect_equal(data_sin_X3$.signal$X3, data_sin$.signal$X3, tolerance = .002)
  expect_lte(max(data_sin_X3$.signal$X2, data_sin_X3$.signal$X1), .004)
})
test_that("high pass default pars",{
  expect_equal(data_sin_X3, eeg_filt_high_pass(data_sin, .freq = 500 * 3 / (2 * pi),  .config = list(l_trans_bandwidth = 59.6831036594608  )))
  expect_equal( data_sin_X3_iir, eeg_filt_high_pass(data_sin, .freq = 500 * 3 / (2 * pi),  .config = list(method = "iir",type = "butter", order = 6)))
})

#plot(data_sin_X3_iir)
#plot(data_sin_X3)

test_that("high pass iir python implementation is not too different", {
 eeguana:::skip_on_actions()
 skip_on_ci()
 mne <- reticulate::import("mne")
  X3_iirmne <-  mne$filter$filter_data(signal$X3, sfreq =500, l_freq = 500 * 3 / (2 * pi), h_freq = NULL, method='iir', iir_params = list(ftype = "butter", order = 6, output ="ba"))

  expect_equal(data_sin_X3_iir$.signal$X3 %>% c(),
               X3_iirmne %>% c(), tolerance = .01)
#ggplot(data = data.frame(y= c(X3_iirmne), x=seq_along(c(X3_iirmne))), aes(x=x,y=y)) + geom_line()
})

data_sin_X2 <- eeg_filt_band_pass(data_sin, .freq = c(1.5, 2.2) * 500 / (2 * pi))
data_sin_X2_iir <- eeg_filt_band_pass(data_sin, .freq = c(1.5, 2.2) * 500 / (2 * pi), .config = list(method ="iir"))
# data_sin_X2r <- data.table::copy(data_sin)
# eeg_filt_band_pass(data_sin_X2r, .freq = c(1.5, 2.2) * 500 / (2 * pi), .by_reference = TRUE)

## plot(data_sin_X2)
test_that("band pass signal", {
  data_sin_X2 <- data_sin_X2 %>% dplyr::filter(as_time(.sample) %>% between(.25, 1.75))
  data_sin <- data_sin %>% dplyr::filter(as_time(.sample) %>% between(.25, 1.75))
  expect_equal(data_sin_X2$.signal$X2, data_sin_X2$.signal$X4, tolerance = .004)
  expect_equal(data_sin_X2$.signal$X2, data_sin$.signal$X2, tolerance = .002)
  expect_lte(max(data_sin_X2$.signal$X3, data_sin_X2$.signal$X1), .004)
})

test_that("band pass default pars",{
  expect_equal(data_sin_X2, eeg_filt_band_pass(data_sin, .freq = c(1.5, 2.2)  * 500 / (2 * pi),  .config = list(l_trans_bandwidth = 29.8415518297304, h_trans_bandwidth = 43.7676093502712    )))
  expect_equal(data_sin_X2_iir, eeg_filt_band_pass(data_sin, .freq = c(1.5, 2.2)  * 500/ (2 * pi),  .config = list(method = "iir",type = "butter", order = 4)))
})

test_that("band pass iir python implementation is not too different", {
 eeguana:::skip_on_actions()
 skip_on_ci()
 mne <- reticulate::import("mne")
  X2_iirmne <-  mne$filter$filter_data(signal$X3, sfreq =500, l_freq = 500 * 1.5 / (2 * pi), h_freq = 500 * 2.2 / (2 * pi), method='iir', iir_params = list(ftype = "butter", order = 4, output ="ba"))

  expect_equal(data_sin_X2_iir$.signal$X3 %>% c(),
               X2_iirmne %>% c(), tolerance = .01)
})


data_sin_X1X3 <- eeg_filt_band_stop(data_sin, .freq = c(2.8, 1.5) * 500 / (2 * pi))
data_sin_X1X3_iir <- eeg_filt_band_stop(data_sin, .freq = c(2.8, 1.5) * 500 / (2 * pi),.config =list(method ="iir"))
# data_sin_X1X3r <- data.table::copy(data_sin)
# eeg_filt_band_stop(data_sin_X1X3r, .freq = c(2.8, 1.5) * 500 / (2 * pi), .by_reference = TRUE)
## plot(data_sin_X1X3)

## plot(data_sin_X2_onlyX1X2)



test_that("band stop signal", {
  data_sin_X1X3 <- data_sin_X1X3 %>% dplyr::filter(as_time(.sample) %>% between(.25, 1.75))
  data_sin <- data_sin %>% dplyr::filter(as_time(.sample) %>% between(.25, 1.75))
  expect_equal(data_sin_X1X3$.signal$X1 + data_sin_X1X3$.signal$X3, data_sin_X1X3$.signal$X4, tolerance = .004)
  expect_equal(data_sin_X1X3$.signal$X1, data_sin$.signal$X1, tolerance = .003)
  expect_lte(max(data_sin_X1X3$.signal$X2), .004)
})

test_that("band stop default pars",{
  expect_equal(data_sin_X1X3, eeg_filt_band_stop(data_sin, .freq = c(2.8, 1.5)  * 500 / (2 * pi), .config = list(l_trans_bandwidth = 29.8415518297304, h_trans_bandwidth = 27.1830796713465   )))
  expect_equal(data_sin_X1X3_iir, eeg_filt_band_stop(data_sin, .freq = c(2.8, 1.5)  * 500/ (2 * pi),  .config = list(method = "iir",type = "butter", order = 4)))
})


test_that("original doesn't change",{
  expect_equal(data_sin_ref, data_sin)
})

# test_that("by reference works",{
#   expect_equal(data_sin_X1r, data_sin_X1)
#   expect_equal(data_sin_X2r, data_sin_X2)
#   expect_equal(data_sin_X3r, data_sin_X3)
#   expect_equal(data_sin_X1X3r, data_sin_X1X3)
# })


## plot(data_sin_more) + facet_grid(.key~.id)
data_sin_more_X1 <- eeg_filt_low_pass(data_sin_more, .freq = 500 * 1 / (2 * pi))
## plot(data_sin_more_X1) + facet_grid(.key~.id)

data_sin_more_X3 <- eeg_filt_high_pass(data_sin_more, .freq = 500 * 3 / (2 * pi))
## plot(data_sin_more_X3)+ facet_grid(.key~.id)


data_sin_more_X2 <- eeg_filt_band_pass(data_sin_more, .freq = c(1.5, 2.2) * 500 / (2 * pi))
## plot(data_sin_more_X2)+ facet_grid(.key~.id)


data_sin_more_X1X3 <- eeg_filt_band_stop(data_sin_more, .freq = c(2.8, 1.5) * 500 / (2 * pi))
## plot(data_sin_more_X1X3)+ facet_grid(.key~.id)

data_sin_more_X2_onlyX1X2 <- eeg_filt_band_pass(data_sin_more, X1, X2, .freq = c(1.5, 2.2) * 500 / (2 * pi))

test_that("low pass signal", {
  data_sin_more_X1 <- data_sin_more_X1 %>% dplyr::filter(as_time(.sample) %>% between(.15, .4))
  data_sin_more <- data_sin_more %>% dplyr::filter(as_time(.sample) %>% between(.15, .4))
  expect_equal(data_sin_more_X1$.signal$X1, data_sin_more_X1$.signal$X4, tolerance = .001)
  expect_equal(data_sin_more_X1$.signal$X1, data_sin_more$.signal$X1, tolerance = .005)
  expect_lte(max(data_sin_more_X1$.signal$X2, data_sin_more_X1$.signal$X3), .0005)
})

test_that("high pass signal", {
  data_sin_more_X3 <- data_sin_more_X3 %>% dplyr::filter(as_time(.sample) %>% between(.15, .4))
  data_sin_more <- data_sin_more %>% dplyr::filter(as_time(.sample) %>% between(.15, .4))
  expect_equal(data_sin_more_X3$.signal$X3, data_sin_more_X3$.signal$X4, tolerance = .004)
  expect_equal(data_sin_more_X3$.signal$X3, data_sin_more$.signal$X3, tolerance = .002)
  expect_lte(max(data_sin_more_X3$.signal$X2, data_sin_more_X3$.signal$X1), .004)
})

test_that("band pass signal", {
  data_sin_more_X2 <- data_sin_more_X2 %>% dplyr::filter(as_time(.sample) %>% between(.15, .4))
  data_sin_more <- data_sin_more %>% dplyr::filter(as_time(.sample) %>% between(.15, .4))
  expect_equal(data_sin_more_X2$.signal$X2, data_sin_more_X2$.signal$X4, tolerance = .004)
  expect_equal(data_sin_more_X2$.signal$X2, data_sin_more$.signal$X2, tolerance = .002)
  expect_lte(max(data_sin_more_X2$.signal$X3, data_sin_more_X2$.signal$X1), .004)
})

test_that("band stop signal", {
  data_sin_more_X1X3 <- data_sin_more_X1X3 %>% dplyr::filter(as_time(.sample) %>% between(.15, .4))
  data_sin_more <- data_sin_more %>% dplyr::filter(as_time(.sample) %>% between(.15, .4))
  expect_equal(data_sin_more_X1X3$.signal$X1 + data_sin_more_X1X3$.signal$X3, data_sin_more_X1X3$.signal$X4, tolerance = .004)
  expect_equal(data_sin_more_X1X3$.signal$X1, data_sin_more$.signal$X1, tolerance = .003)
  expect_lte(max(data_sin_more_X1X3$.signal$X2), .004)
})

test_that("band pass signal, sel", {
  data_sin_more_X2_onlyX1X2 <- data_sin_more_X2_onlyX1X2 %>%
    dplyr::filter(as_time(.sample) %>% between(.15, .4))
  data_sin_more_X2 <- data_sin_more_X2 %>% dplyr::filter(as_time(.sample) %>% between(.15, .4))
  data_sin_more <- data_sin_more %>% dplyr::filter(as_time(.sample) %>% between(.15, .4))
  expect_equal(data_sin_more_X2_onlyX1X2$.signal$X1, data_sin_more_X2$.signal$X1, tolerance = .004)
  expect_equal(data_sin_more_X2_onlyX1X2$.signal$X2, data_sin_more$.signal$X2, tolerance = .002)
  expect_equal(data_sin_more_X2_onlyX1X2$.signal$X3, data_sin_more$.signal$X3, tolerance = .002)
  expect_equal(data_sin_more_X2_onlyX1X2$.signal$X4, data_sin_more$.signal$X4, tolerance = .002)
  ## expect_lte(max(data_sin_more_X2$.signal$X3, data_sin_more_X2$.signal$X1),.004)
})
