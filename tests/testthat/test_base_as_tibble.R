context("base functions: as_tibble")
library(eeguana)

cond1 <- eeg_lst(
  signal_tbl =
 dplyr::tibble(X = sin(1:10), Y = cos(1:10),
    .id = rep(1L, 10),
    .sample_id = sample_int(seq(-4L, 5L), sampling_rate = 500)),
   channels_tbl = dplyr::tibble(
      .channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_
),   events_tbl = dplyr::tribble(
    ~.id, ~type, ~description, ~.initial, ~.final, ~..channel,
    1L, "New Segment", NA, -4L, 1L, NA,
    1L, "Time 0", NA, 1L, 1L, NA
    ),
  segments = dplyr::tibble(.id = 1L, recording = "recording1", segment = 1, type = "initial")
)


cond2 <- eeg_lst(
  signal_tbl =
 dplyr::tibble(X = sin(1:10) + .1, Y = cos(1:10) + .1,
    .id = rep(1L, 10),
    .sample_id = sample_int(seq(-4L, 5L), sampling_rate = 500)),
   channels_tbl = dplyr::tibble(
      .channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_
),
   events_tbl = dplyr::tribble(
    ~.id, ~type, ~description, ~.initial, ~.final, ~..channel,
    1L, "New Segment", NA, -4L, 1L, NA,
    1L, "Time 0", NA, 1L, 1L, NA
  ),
  segments = dplyr::tibble(.id = 1L, recording = "recording2", segment = 1, type = "initial")
)


cond3 <- eeg_lst(
  signal_tbl =
 dplyr::tibble(X = sin(1:10) + .1, Y = cos(1:10) + .1,
    .id = rep(1L, 10),
    .sample_id = sample_int(seq(-4L, 5L), sampling_rate = 500)),
   channels_tbl = dplyr::tibble(
      .channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_)
 ,
   events_tbl = dplyr::tribble(
    ~.id, ~type, ~description, ~.initial, ~.final, ~..channel,
    1L, "New Segment", NA, -4L, 1L, "X",
    1L, "Time 0", NA, 1L, 1L, NA
  ),
  segments = dplyr::tibble(.id = 1L, recording = "recording3", segment = 1, type = "initial")
)


# eeg_lsts <- list(cond1, cond2)


test_that("can transform to tibble", {
  cond1_2 <- cond2
  cond1_2$segments$recording <- "recording2"
  conds <- bind(cond1, cond1_2)
  df <- as_tibble(conds)
  expect_equal(nrow(df), nrow(conds$signal) * length(channel_names(conds)))
  expect_equal(max(df$time), max((conds$signal$.sample_id - 1)) / eeguana:::sampling_rate(conds))
})


# summarize_by_id_tbl(cond1)
