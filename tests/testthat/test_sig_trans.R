context("data transformations")
library(eegble)


data <- eegble(
  signal = dplyr::tibble(.id = rep(c(1L, 2L), each = 10), sample = rep(seq(-4L, 5L), times = 2), X = sin(1:20), Y = cos(1:20)),
  events = dplyr::tribble(
    ~.id, ~type, ~description, ~sample, ~size, ~channel,
    1L, "New Segment", NA_character_, -4L, 1L, NA,
    1L, "Bad", NA_character_, -2L, 3L, NA,
    1L, "Time 0", NA_character_, 1L, 1L, NA,
    1L, "Bad", NA_character_, 2L, 2L, "X",
    2L, "New Segment", NA_character_, -4L, 1L, NA,
    2L, "Time 0", NA_character_, 1L, 1L, NA,
    2L, "Bad", NA_character_, 2L, 1L, "Y"
  ),
  channels = dplyr::tibble(labels = c("X", "Y"), theta = NA, phi = NA, radius = NA, x = NA, y = NA, z = NA),
  info = list(srate = 500, reference = NA),
  segments = dplyr::tibble(.id = c(1L, 2L), recording = "recording1", segment = c(1L, 2L))
)


data_NA <- eegble(
  signal = dplyr::tibble(.id = rep(c(1L, 2L), each = 10), sample = rep(seq(-4L, 5L), times = 2), X = sin(1:20), Y = cos(1:20)),
  events = dplyr::tribble(
    ~.id, ~type, ~description, ~sample, ~size, ~channel,
    1L, "New Segment", NA_character_, -4L, 1L, NA_character_,
    1L, "Bad", NA_character_, -2L, 3L, NA,
    1L, "Time 0", NA_character_, 1L, 1L, NA,
    1L, "Bad", NA_character_, 2L, 2L, NA,
    2L, "New Segment", NA_character_, -4L, 1L, NA,
    2L, "Time 0", NA_character_, 1L, 1L, NA,
    2L, "Bad", NA_character_, 2L, 1L, NA
  ),
  channels = dplyr::tibble(labels = c("X", "Y"), theta = NA, phi = NA, radius = NA, x = NA, y = NA, z = NA),
  info = list(srate = 500, reference = NA),
  segments = dplyr::tibble(.id = c(1L, 2L), recording = "recording1", segment = c(1L, 2L))
)

data_XY <- eegble(
  signal = dplyr::tibble(.id = rep(c(1L, 2L), each = 10), sample = rep(seq(-4L, 5L), times = 2), X = sin(1:20), Y = cos(1:20)),
  events = dplyr::tribble(
    ~.id, ~type, ~description, ~sample, ~size, ~channel,
    1L, "New Segment", NA_character_, -4L, 1L, NA,
    1L, "Bad", NA_character_, -2L, 3L, "X",
    1L, "Bad", NA_character_, -2L, 3L, "Y",
    1L, "Time 0", NA_character_, 1L, 1L, NA,
    1L, "Bad", NA_character_, 2L, 2L, "X",
    2L, "New Segment", NA_character_, -4L, 1L, NA,
    2L, "Time 0", NA_character_, 1L, 1L, NA,
    2L, "Bad", NA_character_, 2L, 1L, "Y"
  ),
  channels = dplyr::tibble(labels = c("X", "Y"), theta = NA, phi = NA, radius = NA, x = NA, y = NA, z = NA),
  info = list(srate = 500, reference = NA),
  segments = dplyr::tibble(.id = c(1L, 2L), recording = "recording1", segment = c(1L, 2L))
)

# TEST when the event exceeds the end of the segment


test_that("can clean files with entire_seg = FALSE", {
  clean_data <- event_to_NA(data, type == "Bad", entire_seg = FALSE)
  clean_data_XY <- event_to_NA(data_XY, type == "Bad", entire_seg = FALSE)
  expect_equal(clean_data, clean_data_XY)
  expect_equal(nrow(clean_data$events), 4)
  expect_equal(all(is.na(clean_data$signal[clean_data$signal$sample %in% seq(-2, -3 + 3 - 1) &
    clean_data$signal$.id == 1, c("X", "Y")])), TRUE)
  expect_equal(all(is.na(clean_data$signal[clean_data$signal$sample %in% seq(2, 2 + 2 - 1) &
    clean_data$signal$.id == 1, c("X")])), TRUE)
  expect_equal(all(is.na(clean_data$signal[clean_data$signal$sample %in% seq(2, 2 + 2 - 1) &
    clean_data$signal$.id == 1, c("Y")])), FALSE)
  expect_equal(all(is.na(clean_data$signal[clean_data$signal$sample %in% seq(2, 2 + 1 - 1) &
    clean_data$signal$.id == 2, c("Y")])), TRUE)
  expect_equal(all(is.na(clean_data$signal[clean_data$signal$sample %in% seq(2, 2 + 1 - 1) &
    clean_data$signal$.id == 2, c("X")])), FALSE)
  expect_equal(all(is.na(clean_data$signal[clean_data$signal$.id == 1, c("X", "Y")])), FALSE)
  expect_equal(all(is.na(clean_data$signal[clean_data$signal$.id == 2, c("Y")])), FALSE)
  expect_equal(all(is.na(clean_data$signal[clean_data$signal$.id == 2, c("X")])), FALSE)
})

test_that("can clean whole channels in files", {
  clean_data_chan <- event_to_NA(data, type == "Bad", all_chans = TRUE, entire_seg = FALSE)
  clean_data_chan2 <- event_to_NA(data_NA, type == "Bad", entire_seg = FALSE)
  clean_data_chan3 <- event_to_NA(data_NA, type == "Bad", all_chans = TRUE, entire_seg = FALSE)
  clean_data_XY2 <- event_to_NA(data_XY, type == "Bad", all_chans = TRUE, entire_seg = FALSE)
  expect_equal(clean_data_chan, clean_data_chan2)
  expect_equal(clean_data_chan, clean_data_chan3)
  expect_equal(clean_data_chan, clean_data_XY2)
  expect_equal(nrow(clean_data_chan$events), 4)
  expect_equal(all(is.na(clean_data_chan$signal[clean_data_chan$signal$sample %in% seq(-2, -3 + 3 - 1) &
    clean_data_chan$signal$.id == 1, c("X", "Y")])), TRUE)
  expect_equal(all(is.na(clean_data_chan$signal[clean_data_chan$signal$sample %in% seq(2, 2 + 2 - 1) &
    clean_data_chan$signal$.id == 1, c("X")])), TRUE)
  expect_equal(all(is.na(clean_data_chan$signal[clean_data_chan$signal$sample %in% seq(2, 2 + 2 - 1) &
    clean_data_chan$signal$.id == 1, c("Y")])), TRUE)
  expect_equal(all(is.na(clean_data_chan$signal[clean_data_chan$signal$sample %in% seq(2, 2 + 1 - 1) &
    clean_data_chan$signal$.id == 2, c("Y")])), TRUE)
  expect_equal(all(is.na(clean_data_chan$signal[clean_data_chan$signal$sample %in% seq(2, 2 + 1 - 1) &
    clean_data_chan$signal$.id == 2, c("X")])), TRUE)
  expect_equal(all(is.na(clean_data_chan$signal[clean_data_chan$signal$.id == 1, c("X", "Y")])), FALSE)
  expect_equal(all(is.na(clean_data_chan$signal[clean_data_chan$signal$.id == 2, c("Y")])), FALSE)
  expect_equal(all(is.na(clean_data_chan$signal[clean_data_chan$signal$.id == 2, c("X")])), FALSE)
})




test_that("can clean whole segments in files", {
  clean_data_seg <- event_to_NA(data, type == "Bad", entire_seg = TRUE)
  expect_equal(nrow(clean_data_seg$events), 4)
  expect_equal(all(is.na(clean_data_seg$signal[clean_data_seg$signal$.id == 1, c("X", "Y")])), TRUE)
  expect_equal(all(is.na(clean_data_seg$signal[clean_data_seg$signal$.id == 2, c("Y")])), TRUE)
  expect_equal(all(is.na(clean_data_seg$signal[clean_data_seg$signal$.id == 2, c("X")])), FALSE)
})

data0 <- eegble(
  signal = dplyr::tibble(.id = rep(c(1L, 1L), each = 10), sample = seq(1L, 20L), X = sin(1:20), Y = cos(1:20)),
  events = dplyr::tribble(
    ~.id, ~type, ~description, ~sample, ~size, ~channel,
    1L, "New Segment", NA, 1L, 1L, NA,
    1L, "Bad", NA, 3L, 3L, NA,
    1L, "Time 0", NA, 6L, 1L, NA,
    1L, "Bad", NA, 7L, 2L, "X",
    1L, "New Segment", NA, 11L, 1L, NA,
    1L, "Time 0", NA, 16L, 1L, NA,
    1L, "Bad", NA, 17L, 1L, "Y"
  ),
  channels = dplyr::tibble(labels = c("X", "Y"), theta = NA, phi = NA, radius = NA, x = NA, y = NA, z = NA),
  info = list(srate = 500, reference = NA),
  segments = dplyr::tibble(.id = 1L, recording = "recording1", segment = 1)
)


data1 <- eegble(
  signal = dplyr::tibble(.id = rep(c(1L, 2L), each = 10), sample = rep(seq(-4L, 5L), times = 2), X = sin(1:20), Y = cos(1:20)),
  events = dplyr::tribble(
    ~.id, ~type, ~description, ~sample, ~size, ~channel,
    1L, "New Segment", NA, -4L, 1L, NA,
    1L, "Bad", NA, -2L, 3L, NA,
    1L, "Time 0", NA, 1L, 1L, NA
  ),
  channels = dplyr::tibble(labels = c("X", "Y"), theta = NA, phi = NA, radius = NA, x = NA, y = NA, z = NA),
  info = list(srate = 500, reference = NA),
  segments = dplyr::tibble(.id = 1L, recording = "recording1", segment = 1)
)

# dots <- rlang::quos( type== "Time 0")

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
  expect_equal(all(d00$events$sample + d00$events$size - 1 <= max(d00$signal$sample)), TRUE)
  expect_equal(all(s1$events$sample + s1$events$size - 1 <= max(s1$signal$sample)), TRUE)
  expect_equal(all(s1_u$events$sample + s1_u$events$size - 1 <= max(s1_u$signal$sample)), TRUE)
  expect_equal(all(s1_u2$events$sample + s1_u2$events$size - 1 <= max(s1_u2$signal$sample)), TRUE)
  expect_equal(all(s1_u2$events$sample + s1_u2$events$size - 1 <= max(s1_u2$signal$sample)), TRUE)
})


# # time(data)
# dplyr::filter(data$signal, sample/srate(data) > .002)
# dplyr::filter(data$signal, !!time(data) > .002)
# dplyr::filter(data$signal, time(data) > .002)

# need to check what happens with different segments, in particular  segment by something else buttype




# segment(data0, type== "Time 0", lim = c(100,100))
# dots <- rlang::quos(type== "Time 0")

segment(data, type == "Time 0", lim = c(-1 / 500, 0))
segment(data0, type == "Time 0", lim = c(-1 / 500, 0))
data0_s <- segment(data0, type == "Time 0", lim = c(-Inf, Inf))
# check the recording
# check the number of segments
# check the new events
# check the length of teh segments under difference conditions

# lim could be 500s,500ms,5000 samples ,or add unit = sample, seconds/s, ms, milliseconds, ms, sec, msec, set the display and default uni with options

warning("segment tests needs to be done - check the ids everywhere when several recordings are there, check the lims working")
warning("baseline tests needs to be done")
warning("sig_wrangling tests needs to be done")
