context("test extended tidyverse: bind")
library(eeguana)


data_0 <- eeg_lst(
  signal = signal_tbl(
    signal_matrix = as.matrix(
      data.frame(X = sin(1:20), Y = cos(1:20))
    ),
    ids = rep(c(1L, 2L), each = 10),
    sample_ids = sample_int(rep(seq(-4L, 5L), times = 2), sampling_rate = 500),
    dplyr::tibble(
      channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = c(1, 1), .y = NA_real_, .z = NA_real_
    )
  ),
  events = dplyr::tribble(
    ~.id, ~type, ~description, ~.sample_0, ~.size, ~.channel,
    1L, "New Segment", NA_character_, -4L, 1L, NA,
    1L, "Bad", NA_character_, -2L, 3L, NA,
    1L, "Time 0", NA_character_, 1L, 1L, NA,
    1L, "Bad", NA_character_, 2L, 2L, "X",
    2L, "New Segment", NA_character_, -4L, 1L, NA,
    2L, "Time 0", NA_character_, 1L, 1L, NA,
    2L, "Bad", NA_character_, 2L, 1L, "Y"
  ),
  segments = dplyr::tibble(.id = c(1L, 2L), recording = "recording1", segment = c(1L, 2L))
)


data_1 <- eeg_lst(
  signal = signal_tbl(
    signal_matrix = as.matrix(
      data.frame(X = sin(1:30), Y = cos(1:30))
    ),
    ids = rep(c(1L, 2L, 3L), each = 10),
    sample_ids = sample_int(rep(seq(-4L, 5L), times = 3), sampling_rate = 500),
    dplyr::tibble(
      channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = c(1, 1), .y = NA_real_, .z = NA_real_
    )
  ),
  events = dplyr::tribble(
    ~.id, ~type, ~description, ~.sample_0, ~.size, ~.channel,
    1L, "New Segment", NA_character_, -4L, 1L, NA,
    1L, "Bad", NA_character_, -2L, 3L, NA,
    1L, "Time 0", NA_character_, 1L, 1L, NA,
    1L, "Bad", NA_character_, 2L, 2L, "X",
    2L, "New Segment", NA_character_, -4L, 1L, NA,
    2L, "Time 0", NA_character_, 1L, 1L, NA,
    2L, "Bad", NA_character_, 2L, 1L, "Y",
    3L, "New Segment", NA_character_, -4L, 1L, NA,
    3L, "Time 0", NA_character_, 1L, 1L, NA,
    3L, "Bad", NA_character_, 2L, 1L, "Y"
  ),
  segments = dplyr::tibble(.id = c(1L, 2L, 3L), recording = "recording1", segment = c(1L, 2L, 3L))
)

reference_data_0 <- data.table::copy(data_0)
reference_data_1 <- data.table::copy(data_1)

test_that("can bind unlisted files", {
  data_2 <- bind(data_0, data_1)
  expect_equal(nrow(data_2$signal), nrow(data_0$signal) + nrow(data_1$signal))
  expect_equal(max(data_2$signal$.id), max(data_0$signal$.id) + max(data_1$signal$.id))
})

test_that("can bind listed files", {
  expect_equal(bind(data_0, data_1), bind(list(data_0, data_1)))
})

data_1_extra_channel <- mutate(data_1, Z = X + 10)
channels_tbl(data_1_extra_channel)
test_that("can bind objects with different channels and throws a warning", {
  expect_warning(data_2_2 <- bind(data_0, data_1_extra_channel))
  expect_equal(max(data_2_2$signal$.id), max(data_0$signal$.id) + max(data_1_extra_channel$signal$.id))
})

