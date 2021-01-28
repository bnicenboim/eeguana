context("test dplyr::filters")
library(eeguana)
set.seed(123)


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

data_sin_X1 <- eeg_filt_low_pass(data_sin, .freq = 500 * 1 / (2 * pi))
data_sin_X1_fir <- eeg_filt_low_pass(data_sin, .freq = 500 * 1 / (2 * pi), .method = "fir", .config = list(h_trans_bandwidth = 19.89436788))

data_sin_X1_iir <- eeg_filt_low_pass(data_sin, .freq = 500 * 1 / (2 * pi), .method = "iir", .config = list(type = "butter", order = 6))

data_sin_X1r <- data.table::copy(data_sin)
eeg_filt_low_pass(data_sin_X1r, .freq = 500 * 1 / (2 * pi), .by_reference = TRUE)
## plot(data_sin_X1)

data_sin_X3 <- eeg_filt_high_pass(data_sin, .freq = 500 * 3 / (2 * pi))
data_sin_X3r <- data.table::copy(data_sin)
eeg_filt_high_pass(data_sin_X3r, .freq = 500 * 3 / (2 * pi), .by_reference = TRUE)
## plot(data_sin_X3)

data_sin_X2 <- eeg_filt_band_pass(data_sin, .freq = c(1.5, 2.2) * 500 / (2 * pi))
data_sin_X2r <- data.table::copy(data_sin)
eeg_filt_band_pass(data_sin_X2r, .freq = c(1.5, 2.2) * 500 / (2 * pi), .by_reference = TRUE)

## plot(data_sin_X2)

data_sin_X1X3 <- eeg_filt_band_stop(data_sin, .freq = c(2.8, 1.5) * 500 / (2 * pi))
data_sin_X1X3r <- data.table::copy(data_sin)
eeg_filt_band_stop(data_sin_X1X3r, .freq = c(2.8, 1.5) * 500 / (2 * pi), .by_reference = TRUE)
## plot(data_sin_X1X3)

## plot(data_sin_X2_onlyX1X2)
test_that("original doesn't change",{
expect_equal(data_sin_ref, data_sin)
})

test_that("by reference works",{
expect_equal(data_sin_X1r, data_sin_X1)
expect_equal(data_sin_X2r, data_sin_X2)
expect_equal(data_sin_X3r, data_sin_X3)
expect_equal(data_sin_X1X3r, data_sin_X1X3)
})


test_that("low pass signal", {
  data_sin_X1 <- data_sin_X1 %>% dplyr::filter(as_time(.sample) %>% between(.25, 1.75))
  data_sin <- data_sin %>% dplyr::filter(as_time(.sample) %>% between(.25, 1.75))
  expect_equal(data_sin_X1$.signal$X1, data_sin_X1$.signal$X4, tolerance = .001)
  expect_equal(data_sin_X1$.signal$X1, data_sin$.signal$X1, tolerance = .005)
  expect_lte(max(data_sin_X1$.signal$X2, data_sin_X1$.signal$X3), .0005)
})

test_that("high pass signal", {
  data_sin_X3 <- data_sin_X3 %>% dplyr::filter(as_time(.sample) %>% between(.25, 1.75))
  data_sin <- data_sin %>% dplyr::filter(as_time(.sample) %>% between(.25, 1.75))
  expect_equal(data_sin_X3$.signal$X3, data_sin_X3$.signal$X4, tolerance = .004)
  expect_equal(data_sin_X3$.signal$X3, data_sin$.signal$X3, tolerance = .002)
  expect_lte(max(data_sin_X3$.signal$X2, data_sin_X3$.signal$X1), .004)
})

test_that("band pass signal", {
  data_sin_X2 <- data_sin_X2 %>% dplyr::filter(as_time(.sample) %>% between(.25, 1.75))
  data_sin <- data_sin %>% dplyr::filter(as_time(.sample) %>% between(.25, 1.75))
  expect_equal(data_sin_X2$.signal$X2, data_sin_X2$.signal$X4, tolerance = .004)
  expect_equal(data_sin_X2$.signal$X2, data_sin$.signal$X2, tolerance = .002)
  expect_lte(max(data_sin_X2$.signal$X3, data_sin_X2$.signal$X1), .004)
})

test_that("band stop signal", {
  data_sin_X1X3 <- data_sin_X1X3 %>% dplyr::filter(as_time(.sample) %>% between(.25, 1.75))
  data_sin <- data_sin %>% dplyr::filter(as_time(.sample) %>% between(.25, 1.75))
  expect_equal(data_sin_X1X3$.signal$X1 + data_sin_X1X3$.signal$X3, data_sin_X1X3$.signal$X4, tolerance = .004)
  expect_equal(data_sin_X1X3$.signal$X1, data_sin$.signal$X1, tolerance = .003)
  expect_lte(max(data_sin_X1X3$.signal$X2), .004)
})


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
