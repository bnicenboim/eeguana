context("test segmenation")
library(eeguana)

data <- eeg_lst(
  signal_tbl = tibble::tibble(
    X = sin(1:20),
    Y = cos(1:20),
    .id = rep(c(1L, 2L), each = 10),
    .sample = sample_int(rep(seq(-4L, 5L), times = 2), sampling_rate = 500)
  ),
  channels_tbl =
    dplyr::tibble(
      .channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_
    ),
  events_tbl = dplyr::tribble(
    ~.id, ~.type, ~.description, ~.initial, ~.final, ~.channel,
    1L, "New Segment", NA_character_, -4L, -4L, NA,
    1L, "Bad", NA_character_, -2L, 0L, NA,
    1L, "Time 0", NA_character_, 1L, 1L, NA,
    1L, "Bad", NA_character_, 2L, 3L, "X",
    2L, "New Segment", NA_character_, -4L, -4L, NA,
    2L, "Time 0", NA_character_, 1L, 1L, NA,
    2L, "Bad", NA_character_, 2L, 2L, "Y"
  ),
  segments_tbl = dplyr::tibble(.id = c(1L, 2L), .recording = "recording1", segment = c(1L, 2L))
)

data0 <- eeg_lst(
  signal_tbl =
    dplyr::tibble(
      X = sin(1:20),
      Y = cos(1:20),
      .id = rep(c(1L, 1L), each = 10),
      .sample = sample_int(seq(1L, 20L), sampling_rate = 500)
    ),
  channels_tbl = dplyr::tibble(
    .channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
    radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_
  ),
  events_tbl = dplyr::tribble(
    ~.id, ~.type, ~.description, ~.initial, ~.final, ~.channel,
    1L, "New Segment", NA, 1L, 1L, NA,
    1L, "Bad", NA, 3L, 5L, NA,
    1L, "Time 0", NA, 6L, 6L, NA,
    1L, "Bad", NA, 7L, 8L, "X",
    1L, "New Segment", NA, 11L, 11L, NA,
    1L, "Time 0", NA, 16L, 16L, NA,
    1L, "Bad", NA, 17L, 17L, "Y"
  ),
  segments_tbl = dplyr::tibble(.id = 1L, .recording = "recording1", segment = 1)
)

test_that("can segment using lim", {
  data_s <- eeg_segment(data, .type == "Time 0")
  expect_equal(data$.signal, data_s$.signal)
  expect_equal(data$.events, data_s$.events)
  expect_equal(data$.segments, dplyr::select(data_s$.segments, -type, -description))
  # expect_equal(data$.segments, data_s$.segments)
  d <- eeg_segment(data, .type == "Time 0")
  d_rec <- eeg_segment(d, .type == "Time 0")
  expect_equal(d$.signal, d_rec$.signal)
  expect_equal(d$.events, d_rec$.events)
  expect_equal(dplyr::select(d$.segments, -type, -description), dplyr::select(d_rec$.segments, -type.x, -description.x, -type.y, -description.y))
  # expect_equal(d$.segments,d_rec$.segments)
  d_0 <- eeg_segment(data, .type == "Time 0", lim = c(0, Inf))
  d_0_0 <- eeg_segment(d_0, .type == "Time 0", lim = c(0, Inf))
  expect_equal(nrow(d_0$.signal), 10)
  expect_equal(d_0$.signal, d_0_0$.signal)
  expect_equal(d_0$.events, d_0_0$.events)
  expect_equal(dplyr::select(d_0$.segments, -type, -description), dplyr::select(d_0_0$.segments, -type.x, -description.x, -type.y, -description.y))
  # expect_equal(d_0$.segments,d_0_0$.segments)
  s1 <- eeg_segment(data0, .type == "Time 0", lim = c(0, 1 / 500))
  expect_equal(s1$.signal$X[1], data0$.signal$X[6])
  expect_equal(nrow(s1$.signal), 4)
  expect_equal(nrow(s1$.segments), 2)
  expect_equal(s1$.segments$segment, 1:2)
  s1_u <- eeg_segment(data0, .type == "Time 0", lim = c(0, 1), unit = "sample")
  s1_u2 <- eeg_segment(data0, .type == "Time 0", lim = c(0, 2), unit = "ms")
  expect_equal(s1, s1_u2)
  double <- eeg_segment(data0, .type == "Time 0", lim = c(-20, 20), unit = "sample")
  expect_equal(nrow(double$.signal), 40)
  d00 <- eeg_segment(data0, .type == "Time 0", lim = c(0, 1), unit = "sample")
  expect_equal(all(d00$.events$.final <= max(d00$.signal$.sample)), TRUE)
  expect_equal(all(s1$.events$.final <= max(s1$.signal$.sample)), TRUE)
  expect_equal(all(s1_u$.events$.final <= max(s1_u$.signal$.sample)), TRUE)
  expect_equal(all(s1_u2$.events$.final <= max(s1_u2$.signal$.sample)), TRUE)
  expect_equal(all(s1_u2$.events$.final <= max(s1_u2$.signal$.sample)), TRUE)
})

test_that("can segment using end", {
  # TODO:  I should add an expect_equal
  data_s_e <- eeg_segment(data, .type == "New Segment", end = .type == "Time 0")
})




eeg_segment(data, .type == "Time 0", lim = c(-1 / 500, 0))
eeg_segment(data0, .type == "Time 0", lim = c(-1 / 500, 0))
data0_s <- eeg_segment(data0, .type == "Time 0", lim = c(-Inf, Inf))
