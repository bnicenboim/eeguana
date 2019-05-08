context("test tidyverse functions select")
library(eeguana)


data_1 <- eeg_lst(
  signal_tbl =
 dplyr::tibble(X = sin(1:30), Y = cos(1:30),
    .id = rep(c(1L, 2L, 3L), each = 10),
.sample = sample_int(rep(seq(-4L, 5L), times = 3), sampling_rate = 500)),
   channels_tbl = dplyr::tibble(
      .channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = c(1, 1), .y = NA_real_, .z = NA_real_
  ),
   events_tbl = dplyr::tribble(
    ~.id, ~.type, ~.description, ~.initial, ~.final, ~.channel,
    1L, "New Segment", NA_character_, -4L, -4L, NA,
    1L, "Bad", NA_character_, -2L, 0L, NA,
    1L, "Time 0", NA_character_, 1L, 1L, NA,
    1L, "Bad", NA_character_, 2L, 3L, "X",
    2L, "New Segment", NA_character_, -4L, -4L, NA,
    2L, "Time 0", NA_character_, 1L, 1L, NA,
    2L, "Bad", NA_character_, 2L, 2L, "Y",
    3L, "New Segment", NA_character_, -4L, -4L, NA,
    3L, "Time 0", NA_character_, 1L, 1L, NA,
    3L, "Bad", NA_character_, 2L, 2L, "Y"
    ),
  segments_tbl = dplyr::tibble(.id = c(1L, 2L, 3L),
                           .recording = "recording1",
                           segment = c(1L, 2L, 3L),
                           condition = c("a", "b", "a"))
)


cond1 <- eeg_lst(
  signal_tbl =
 dplyr::tibble(X = sin(1:10), Y = cos(1:10),
    .id = rep(1L, 10),
    .sample = sample_int(seq(-4L, 5L), sampling_rate = 500)),
   channels_tbl = dplyr::tibble(
      .channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_
),   events_tbl = dplyr::tribble(
    ~.id, ~.type, ~.description, ~.initial, ~.final, ~.channel,
    1L, "New Segment", NA, -4L, -4L, NA,
    1L, "Time 0", NA, 1L, 1L, NA
    ),
  segments = dplyr::tibble(.id = 1L, .recording = "recording1", segment = 1, .type = "initial")
)


cond2 <- eeg_lst(
  signal_tbl =
 dplyr::tibble(X = sin(1:10) + .1, Y = cos(1:10) + .1,
    .id = rep(1L, 10),
    .sample = sample_int(seq(-4L, 5L), sampling_rate = 500)),
   channels_tbl = dplyr::tibble(
      .channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_
),
   events_tbl = dplyr::tribble(
    ~.id, ~.type, ~.description, ~.initial, ~.final, ~.channel,
    1L, "New Segment", NA, -4L, -4L, NA,
    1L, "Time 0", NA, 1L, 1L, NA
  ),
  segments = dplyr::tibble(.id = 1L, .recording = "recording2", segment = 1, .type = "initial")
)


cond3 <- eeg_lst(
  signal_tbl =
 dplyr::tibble(X = sin(1:10) + .1, Y = cos(1:10) + .1,
    .id = rep(1L, 10),
    .sample = sample_int(seq(-4L, 5L), sampling_rate = 500)),
   channels_tbl = dplyr::tibble(
      .channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_)
 ,
   events_tbl = dplyr::tribble(
    ~.id, ~.type, ~.description, ~.initial, ~.final, ~.channel,
    1L, "New Segment", NA, -4L, -4L, "X",
    1L, "Time 0", NA, 1L, 1L, NA
  ),
  segments = dplyr::tibble(.id = 1L, .recording = "recording3", segment = 1, .type = "initial")
)


# eeg_lsts <- list(cond1, cond2)


test_that("can transform to tibble", {
  cond1_2 <- cond2
  cond1_2$.segments$.recording <- "recording2"
  conds <- bind(cond1, cond1_2)
  df <- as_tibble(conds)
  expect_equal(nrow(df), nrow(conds$.signal) * length(channel_names(conds)))
  expect_equal(max(df$.time), max((conds$.signal$.sample - 1)) / eeguana:::sampling_rate(conds))
})

