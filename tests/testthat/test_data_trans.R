context("Binding and transforming")
library(eeguana)


  

cond1 <- eeg_lst(
signal = signal(signal_matrix = as.matrix(
                              data.frame(X = sin(1:10), Y = cos(1:10))),
  ids = rep(1L, 10), 
  sample_ids = sample_id(seq(-4L, 5L), sampling_rate = 500 ),
  dplyr::tibble(.name = c("X", "Y"), .reference = NA, theta = NA, phi = NA, 
    radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_) ),
  events = dplyr::tribble(
    ~.id, ~type, ~description, ~.sample_0, ~.size, ~.channel,
    1L, "New Segment", NA, -4L, 1L, NA,
    1L, "Time 0", NA, 1L, 1L, NA
  ),
  segments = dplyr::tibble(.id = 1L, recording = "recording1", segment = 1, type = "initial")
)


cond2 <- eeg_lst(
  signal = signal(signal_matrix = as.matrix(
                              data.frame(X = sin(1:10)+.1, Y = cos(1:10)+.1)),
  ids = rep(1L, 10), 
  sample_ids = sample_id(seq(-4L, 5L), sampling_rate = 500 ),
  dplyr::tibble(.name = c("X", "Y"), .reference = NA, theta = NA, phi = NA, 
    radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_) ),

  events = dplyr::tribble(
    ~.id, ~type, ~description, ~.sample_0, ~.size, ~.channel,
    1L, "New Segment", NA, -4L, 1L, NA,
    1L, "Time 0", NA, 1L, 1L, NA
  ),
  segments = dplyr::tibble(.id = 1L, recording = "recording1", segment = 1, type = "initial")
)


cond3 <- eeg_lst(
  signal = signal(signal_matrix = as.matrix(
                              data.frame(X = sin(1:10)+.1, Y = cos(1:10)+.1)),
  ids = rep(1L, 10), 
  sample_ids = sample_id(seq(-4L, 5L), sampling_rate = 500 ),
  dplyr::tibble(.name = c("X", "Y"), .reference = NA, theta = NA, phi = NA, 
    radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_) ),

  events = dplyr::tribble(
    ~.id, ~type, ~description, ~.sample_0, ~.size, ~.channel,
    1L, "New Segment", NA, -4L, 1L, "X",
    1L, "Time 0", NA, 1L, 1L, NA
  ),
  segments = dplyr::tibble(.id = 1L, recording = "recording1", segment = 1, type = "initial")
)



test_that("can bind unlisted files of same recording", {
  conds <- bind(cond1, cond2)
  expect_equal(nrow(conds$signal), nrow(cond1$signal) + nrow(cond2$signal))
  expect_equal(max(conds$signal$.id), max(cond1$signal$.id) + max(cond2$signal$.id))
  expect_equal(max(conds$segments$segment), max(cond1$segments$segment) + max(cond2$segments$segment))
})

test_that("can bind listed files of same recording", {
  conds <- bind(list(cond1, cond2))
  expect_equal(nrow(conds$signal), nrow(cond1$signal) + nrow(cond2$signal))
  expect_equal(max(conds$signal$.id), max(cond1$signal$.id) + max(cond2$signal$.id))
  expect_equal(max(conds$segments$segment), max(cond1$segments$segment) + max(cond2$segments$segment))
})


test_that("can bind listed files that show and do not show channels on the event table", {
  conds <- bind(list(cond1, cond3))
  expect_equal(nrow(conds$signal), nrow(cond1$signal) + nrow(cond3$signal))
  expect_equal(max(conds$signal$.id), max(cond1$signal$.id) + max(cond3$signal$.id))
  expect_equal(max(conds$segments$segment), max(cond1$segments$segment) + max(cond3$segments$segment))
})



test_that("can bind unlisted files of same recording", {
  cond1_2 <- cond2
  cond1_2$segments$recording <- "recording2"
  conds <- bind(cond1, cond1_2)
  expect_equal(nrow(conds$signal), nrow(cond1$signal) + nrow(cond2$signal))
  expect_equal(max(conds$signal$.id), max(cond1$signal$.id) + max(cond2$signal$.id))
  expect_equal(max(conds$segments$segment), max(cond1$segments$segment, cond2$segments$segment))
})


sloop::s3_methods_generic("as_tibble")
as_tibble(cond1)

test_that("can transform to tibble", {
  cond1_2 <- cond2
  cond1_2$segments$recording <- "recording2"
  conds <- bind(cond1, cond1_2)
  df <- as_tibble(conds)
  expect_equal(nrow(df), nrow(conds$signal) * length(channel_names(conds)))
  expect_equal(max(df$time), max((conds$signal$.sample_id - 1)) / eeguana:::sampling_rate(conds))
})


#summarize_id_as_tibble(cond1)

