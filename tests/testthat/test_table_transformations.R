context("table transformations")
library(eeguana)


data <- eeg_lst(
  signal = signal(signal_matrix = as.matrix(
                              data.frame(X = sin(1:20), Y = cos(1:20))),
  ids = rep(c(1L, 2L), each = 10), 
  sample_ids = sample_id(rep(seq(-4L, 5L), times = 2), sampling_rate = 500 ),
  dplyr::tibble(.name = c("X", "Y"), .reference = NA, theta = NA, phi = NA, 
    radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_) ),
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


data_NA <- eeg_lst(
  signal = signal(signal_matrix = as.matrix(
                              data.frame(X = sin(1:20), Y = cos(1:20))),
  ids = rep(c(1L, 2L), each = 10), 
  sample_ids = sample_id(rep(seq(-4L, 5L), times = 2), sampling_rate = 500 ),
  dplyr::tibble(.name = c("X", "Y"), .reference = NA, theta = NA, phi = NA, 
    radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_) ),
  events = dplyr::tribble(
    ~.id, ~type, ~description, ~.sample_0, ~.size, ~.channel,
    1L, "New Segment", NA_character_, -4L, 1L, NA_character_,
    1L, "Bad", NA_character_, -2L, 3L, NA,
    1L, "Time 0", NA_character_, 1L, 1L, NA,
    1L, "Bad", NA_character_, 2L, 2L, NA,
    2L, "New Segment", NA_character_, -4L, 1L, NA,
    2L, "Time 0", NA_character_, 1L, 1L, NA,
    2L, "Bad", NA_character_, 2L, 1L, NA
  ),
  segments = dplyr::tibble(.id = c(1L, 2L), recording = "recording1", segment = c(1L, 2L))
)

data_XY <- eeg_lst(
  signal = signal(signal_matrix = as.matrix(
                              data.frame(X = sin(1:20), Y = cos(1:20))),
  ids = rep(c(1L, 2L), each = 10), 
  sample_ids = sample_id(rep(seq(-4L, 5L), times = 2), sampling_rate = 500 ),
  dplyr::tibble(.name = c("X", "Y"), .reference = NA, theta = NA, phi = NA, 
    radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_) ),
  events = dplyr::tribble(
    ~.id, ~type, ~description, ~.sample_0, ~.size, ~.channel,
    1L, "New Segment", NA_character_, -4L, 1L, NA,
    1L, "Bad", NA_character_, -2L, 3L, "X",
    1L, "Bad", NA_character_, -2L, 3L, "Y",
    1L, "Time 0", NA_character_, 1L, 1L, NA,
    1L, "Bad", NA_character_, 2L, 2L, "X",
    2L, "New Segment", NA_character_, -4L, 1L, NA,
    2L, "Time 0", NA_character_, 1L, 1L, NA,
    2L, "Bad", NA_character_, 2L, 1L, "Y"
  ),
   segments = dplyr::tibble(.id = c(1L, 2L), recording = "recording1", segment = c(1L, 2L))
)

# TEST when the event exceeds the end of the segment


test_that("can clean files with entire_seg = FALSE", {
  clean_data <- event_to_ch_NA(data, type == "Bad", entire_seg = FALSE)
  clean_data_XY <- event_to_ch_NA(data_XY, type == "Bad", entire_seg = FALSE)
  expect_equal(clean_data, clean_data_XY)
  expect_equal(nrow(clean_data$events), 4)
  expect_equal(all(is.na(clean_data$signal[clean_data$signal$.sample_id %in% seq(-2, -3 + 3 - 1) &
    clean_data$signal$.id == 1, c("X", "Y")])), TRUE)
  expect_equal(all(is.na(clean_data$signal[clean_data$signal$.sample_id %in% seq(2, 2 + 2 - 1) &
    clean_data$signal$.id == 1, c("X")])), TRUE)
  expect_equal(all(is.na(clean_data$signal[clean_data$signal$.sample_id %in% seq(2, 2 + 2 - 1) &
    clean_data$signal$.id == 1, c("Y")])), FALSE)
  expect_equal(all(is.na(clean_data$signal[clean_data$signal$.sample_id %in% seq(2, 2 + 1 - 1) &
    clean_data$signal$.id == 2, c("Y")])), TRUE)
  expect_equal(all(is.na(clean_data$signal[clean_data$signal$.sample_id %in% seq(2, 2 + 1 - 1) &
    clean_data$signal$.id == 2, c("X")])), FALSE)
  expect_equal(all(is.na(clean_data$signal[clean_data$signal$.id == 1, c("X", "Y")])), FALSE)
  expect_equal(all(is.na(clean_data$signal[clean_data$signal$.id == 2, c("Y")])), FALSE)
  expect_equal(all(is.na(clean_data$signal[clean_data$signal$.id == 2, c("X")])), FALSE)
})

test_that("can clean whole channels in files", {
  clean_data_chan <- event_to_ch_NA(data, type == "Bad", all_chans = TRUE, entire_seg = FALSE)
  clean_data_chan2 <- event_to_ch_NA(data_NA, type == "Bad", entire_seg = FALSE)
  clean_data_chan3 <- event_to_ch_NA(data_NA, type == "Bad", all_chans = TRUE, entire_seg = FALSE)
  clean_data_XY2 <- event_to_ch_NA(data_XY, type == "Bad", all_chans = TRUE, entire_seg = FALSE)
  expect_equal(clean_data_chan, clean_data_chan2)
  expect_equal(clean_data_chan, clean_data_chan3)
  expect_equal(clean_data_chan, clean_data_XY2)
  expect_equal(nrow(clean_data_chan$events), 4)
  expect_equal(all(is.na(clean_data_chan$signal[clean_data_chan$signal$.sample_id %in% seq(-2, -3 + 3 - 1) &
    clean_data_chan$signal$.id == 1, c("X", "Y")])), TRUE)
  expect_equal(all(is.na(clean_data_chan$signal[clean_data_chan$signal$.sample_id %in% seq(2, 2 + 2 - 1) &
    clean_data_chan$signal$.id == 1, c("X")])), TRUE)
  expect_equal(all(is.na(clean_data_chan$signal[clean_data_chan$signal$.sample_id %in% seq(2, 2 + 2 - 1) &
    clean_data_chan$signal$.id == 1, c("Y")])), TRUE)
  expect_equal(all(is.na(clean_data_chan$signal[clean_data_chan$signal$.sample_id %in% seq(2, 2 + 1 - 1) &
    clean_data_chan$signal$.id == 2, c("Y")])), TRUE)
  expect_equal(all(is.na(clean_data_chan$signal[clean_data_chan$signal$.sample_id %in% seq(2, 2 + 1 - 1) &
    clean_data_chan$signal$.id == 2, c("X")])), TRUE)
  expect_equal(all(is.na(clean_data_chan$signal[clean_data_chan$signal$.id == 1, c("X", "Y")])), FALSE)
  expect_equal(all(is.na(clean_data_chan$signal[clean_data_chan$signal$.id == 2, c("Y")])), FALSE)
  expect_equal(all(is.na(clean_data_chan$signal[clean_data_chan$signal$.id == 2, c("X")])), FALSE)
})




test_that("can clean whole segments in files", {
  clean_data_seg <- event_to_ch_NA(data, type == "Bad", entire_seg = TRUE)
  expect_equal(nrow(clean_data_seg$events), 4)
  expect_equal(all(is.na(clean_data_seg$signal[clean_data_seg$signal$.id == 1, c("X", "Y")])), TRUE)
  expect_equal(all(is.na(clean_data_seg$signal[clean_data_seg$signal$.id == 2, c("Y")])), TRUE)
  expect_equal(all(is.na(clean_data_seg$signal[clean_data_seg$signal$.id == 2, c("X")])), FALSE)
})


data0 <- eeg_lst(
  signal = signal(signal_matrix = as.matrix(
                              data.frame(X = sin(1:20), Y = cos(1:20))),
  ids =  rep(c(1L, 1L), each = 10), 
  sample_ids = sample_id(seq(1L, 20L), sampling_rate = 500 ),
  dplyr::tibble(.name = c("X", "Y"), .reference = NA, theta = NA, phi = NA, 
    radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_) ),
  events = dplyr::tribble(
    ~.id, ~type, ~description, ~.sample_0, ~.size, ~.channel,
    1L, "New Segment", NA, 1L, 1L, NA,
    1L, "Bad", NA, 3L, 3L, NA,
    1L, "Time 0", NA, 6L, 1L, NA,
    1L, "Bad", NA, 7L, 2L, "X",
    1L, "New Segment", NA, 11L, 1L, NA,
    1L, "Time 0", NA, 16L, 1L, NA,
    1L, "Bad", NA, 17L, 1L, "Y"
  ),
  segments = dplyr::tibble(.id = 1L, recording = "recording1", segment = 1)
)


test_that("can segment", {
  data_s <- segment(data, type == "Time 0")
  expect_equal(data$signal, data_s$signal)
  expect_equal(data$events, data_s$events)
  expect_equal(data$segments, dplyr::select(data_s$segments, -type, -description))
  d <- segment(data, type == "Time 0")
  d_rec <- segment(d, type == "Time 0")
  expect_equal(d$signal, d_rec$signal)
  expect_equal(d$events, d_rec$events)
  expect_equal(dplyr::select(d$segments, -type, -description), dplyr::select(d_rec$segments, -type.x, -description.x, -type.y, -description.y))
  d_0 <- segment(data, type == "Time 0", lim = c(0, Inf))
  d_0_0 <- segment(d_0, type == "Time 0", lim = c(0, Inf))
  expect_equal(nrow(d_0$signal), 10)
  expect_equal(d_0$signal, d_0_0$signal)
  expect_equal(d_0$events, d_0_0$events)
  expect_equal(dplyr::select(d_0$segments, -type, -description), dplyr::select(d_0_0$segments, -type.x, -description.x, -type.y, -description.y))
  s1 <- segment(data0, type == "Time 0", lim = c(0, 1 / 500))
  expect_equal(s1$signal$X[1], data0$signal$X[6])
  expect_equal(nrow(s1$signal), 4)
  expect_equal(nrow(s1$segments), 2)
  s1_u <- segment(data0, type == "Time 0", lim = c(0, 1), unit = "sample")
  s1_u2 <- segment(data0, type == "Time 0", lim = c(0, 2), unit = "ms")
  expect_equal(s1, s1_u2)
  double <- segment(data0, type == "Time 0", lim = c(-20, 20), unit = "sample")
  expect_equal(nrow(double$signal), 40)
  d00 <- segment(data0, type == "Time 0", lim = c(0, 1), unit = "sample")
  expect_equal(all(d00$events$.sample_0 + d00$events$.size - 1 <= max(d00$signal$.sample_id)), TRUE)
  expect_equal(all(s1$events$.sample_0 + s1$events$.size - 1 <= max(s1$signal$.sample_id)), TRUE)
  expect_equal(all(s1_u$events$.sample_0 + s1_u$events$.size - 1 <= max(s1_u$signal$.sample_id)), TRUE)
  expect_equal(all(s1_u2$events$.sample_0 + s1_u2$events$.size - 1 <= max(s1_u2$signal$.sample_id)), TRUE)
  expect_equal(all(s1_u2$events$.sample_0 + s1_u2$events$.size - 1 <= max(s1_u2$signal$.sample_id)), TRUE)
})


baselines <- dplyr::summarize(dplyr::group_by(
                              dplyr::filter(eeguana:::declass(data$signal)$tbl, .sample_id <=0),
                              .id),  bX = mean(X), bY = mean(Y))
signal_with_baselines <- dplyr::left_join(eeguana:::declass(data$signal)$tbl, baselines) 
signal_with_baselines$new_X <- signal_with_baselines$X - signal_with_baselines$bX
signal_with_baselines$new_Y <- signal_with_baselines$Y - signal_with_baselines$bY
baselined <- ch_baseline(data)


test_that("baseline works", {
expect_equal(eeguana:::declass(baselined$signal)$tbl$X,  signal_with_baselines$new_X )
expect_equal(eeguana:::declass(baselined$signal)$tbl$Y,  signal_with_baselines$new_Y )
})



segment(data, type == "Time 0", lim = c(-1 / 500, 0))
segment(data0, type == "Time 0", lim = c(-1 / 500, 0))
data0_s <- segment(data0, type == "Time 0", lim = c(-Inf, Inf))
