context("test EEG-specialized functions")
library(eeguana)


data <- eeg_lst(
    signal_tbl = tibble::tibble( 
                            X = sin(1:20),
                            Y = cos(1:20),
                            .id = rep(c(1L, 2L), each = 10),
                            .sample_id = sample_int(rep(seq(-4L, 5L), times = 2), sampling_rate = 500)),
    channels_tbl = 
        dplyr::tibble(
                   .channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
                   radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_
               ),
    events_tbl = dplyr::tribble(
                            ~.id, ~type, ~description, ~.sample_0, ~.size, ~.channel,
                            1L, "New Segment", NA_character_, -4L, 1L, NA,
                            1L, "Bad", NA_character_, -2L, 3L, NA,
                            1L, "Time 0", NA_character_, 1L, 1L, NA,
                            1L, "Bad", NA_character_, 2L, 2L, "X",
                            2L, "New Segment", NA_character_, -4L, 1L, NA,
                            2L, "Time 0", NA_character_, 1L, 1L, NA,
                            2L, "Bad", NA_character_, 2L, 1L, "Y"
                        ),
    segments_tbl = dplyr::tibble(.id = c(1L, 2L), recording = "recording1", segment = c(1L, 2L))
)


data_NA <- eeg_lst(
  signal_tbl =dplyr::tibble(
                         X = sin(1:20),
                         Y = cos(1:20),
    .id = rep(c(1L, 2L), each = 10),
    .sample_id = sample_int(rep(seq(-4L, 5L), times = 2), sampling_rate = 500)),
  channels_tbl=  dplyr::tibble(
      .channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_
    ),
  events_tbl = dplyr::tribble(
    ~.id, ~type, ~description, ~.sample_0, ~.size, ~.channel,
    1L, "New Segment", NA_character_, -4L, 1L, NA_character_,
    1L, "Bad", NA_character_, -2L, 3L, NA,
    1L, "Time 0", NA_character_, 1L, 1L, NA,
    1L, "Bad", NA_character_, 2L, 2L, NA,
    2L, "New Segment", NA_character_, -4L, 1L, NA,
    2L, "Time 0", NA_character_, 1L, 1L, NA,
    2L, "Bad", NA_character_, 2L, 1L, NA
    ),
  segments_tbl = dplyr::tibble(.id = c(1L, 2L), recording = "recording1", segment = c(1L, 2L))
)

data_XY <- eeg_lst(
    signal_tbl =dplyr::tibble(
                           X = sin(1:20),
                           Y = cos(1:20),
                           .id = rep(c(1L, 2L), each = 10),
                           .sample_id = sample_int(rep(seq(-4L, 5L), times = 2),
                                                   sampling_rate = 500)),
channels_tbl=    dplyr::tibble(
      .channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_
    
  ),
  events_tbl = dplyr::tribble(
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
  segments_tbl = dplyr::tibble(.id = c(1L, 2L), recording = "recording1", segment = c(1L, 2L))
)

## TODO: TEST when the event exceeds the end of the segment


test_that("can clean files with entire_seg = FALSE", {
  clean_data <- eeg_events_to_NA(data, type == "Bad", entire_seg = FALSE)
  clean_data_XY <- eeg_events_to_NA(data_XY, type == "Bad", entire_seg = FALSE)
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
  clean_data_chan <- eeg_events_to_NA(data, type == "Bad", all_chans = TRUE, entire_seg = FALSE)
  clean_data_chan2 <- eeg_events_to_NA(data_NA, type == "Bad", entire_seg = FALSE)
  clean_data_chan3 <- eeg_events_to_NA(data_NA, type == "Bad", all_chans = TRUE, entire_seg = FALSE)
  clean_data_XY2 <- eeg_events_to_NA(data_XY, type == "Bad", all_chans = TRUE, entire_seg = FALSE)
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
  clean_data_seg <- eeg_events_to_NA(data, type == "Bad", entire_seg = TRUE)
  expect_equal(nrow(clean_data_seg$events), 4)
  expect_equal(all(is.na(clean_data_seg$signal[clean_data_seg$signal$.id == 1, c("X", "Y")])), TRUE)
  expect_equal(all(is.na(clean_data_seg$signal[clean_data_seg$signal$.id == 2, c("Y")])), TRUE)
  expect_equal(all(is.na(clean_data_seg$signal[clean_data_seg$signal$.id == 2, c("X")])), FALSE)
})


data0 <- eeg_lst(
    signal_tbl =
        dplyr::tibble(X = sin(1:20),
                      Y = cos(1:20),
    .id = rep(c(1L, 1L), each = 10),
    .sample_id = sample_int(seq(1L, 20L), sampling_rate = 500)),
channels_tbl =     dplyr::tibble(
      .channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_
    ),
  events_tbl = dplyr::tribble(
    ~.id, ~type, ~description, ~.sample_0, ~.size, ~.channel,
    1L, "New Segment", NA, 1L, 1L, NA,
    1L, "Bad", NA, 3L, 3L, NA,
    1L, "Time 0", NA, 6L, 1L, NA,
    1L, "Bad", NA, 7L, 2L, "X",
    1L, "New Segment", NA, 11L, 1L, NA,
    1L, "Time 0", NA, 16L, 1L, NA,
    1L, "Bad", NA, 17L, 1L, "Y"
    ),
  segments_tbl = dplyr::tibble(.id = 1L, recording = "recording1", segment = 1)
)

test_that("can segment using lim", {
  data_s <- eeg_segment(data, type == "Time 0")
  expect_equal(data$signal, data_s$signal)
  expect_equal(data$events, data_s$events)
  expect_equal(data$segments, dplyr::select(data_s$segments, -type, -description))
  # expect_equal(data$segments, data_s$segments)
  d <- eeg_segment(data, type == "Time 0")
  d_rec <- eeg_segment(d, type == "Time 0")
  expect_equal(d$signal, d_rec$signal)
  expect_equal(d$events, d_rec$events)
  expect_equal(dplyr::select(d$segments, -type, -description), dplyr::select(d_rec$segments, -type.x, -description.x, -type.y, -description.y))
  # expect_equal(d$segments,d_rec$segments)
  d_0 <- eeg_segment(data, type == "Time 0", lim = c(0, Inf))
  d_0_0 <- eeg_segment(d_0, type == "Time 0", lim = c(0, Inf))
  expect_equal(nrow(d_0$signal), 10)
  expect_equal(d_0$signal, d_0_0$signal)
  expect_equal(d_0$events, d_0_0$events)
  expect_equal(dplyr::select(d_0$segments, -type, -description), dplyr::select(d_0_0$segments, -type.x, -description.x, -type.y, -description.y))
  # expect_equal(d_0$segments,d_0_0$segments)
  s1 <- eeg_segment(data0, type == "Time 0", lim = c(0, 1 / 500))
  expect_equal(s1$signal$X[1], data0$signal$X[6])
  expect_equal(nrow(s1$signal), 4)
  expect_equal(nrow(s1$segments), 2)
  s1_u <- eeg_segment(data0, type == "Time 0", lim = c(0, 1), unit = "sample")
  s1_u2 <- eeg_segment(data0, type == "Time 0", lim = c(0, 2), unit = "ms")
  expect_equal(s1, s1_u2)
  double <- eeg_segment(data0, type == "Time 0", lim = c(-20, 20), unit = "sample")
  expect_equal(nrow(double$signal), 40)
  d00 <- eeg_segment(data0, type == "Time 0", lim = c(0, 1), unit = "sample")
  expect_equal(all(d00$events$.sample_0 + d00$events$.size - 1 <= max(d00$signal$.sample_id)), TRUE)
  expect_equal(all(s1$events$.sample_0 + s1$events$.size - 1 <= max(s1$signal$.sample_id)), TRUE)
  expect_equal(all(s1_u$events$.sample_0 + s1_u$events$.size - 1 <= max(s1_u$signal$.sample_id)), TRUE)
  expect_equal(all(s1_u2$events$.sample_0 + s1_u2$events$.size - 1 <= max(s1_u2$signal$.sample_id)), TRUE)
  expect_equal(all(s1_u2$events$.sample_0 + s1_u2$events$.size - 1 <= max(s1_u2$signal$.sample_id)), TRUE)
})

test_that("can segment using end", {
  #TODO:  I should add an expect_equal
  data_s_e <- eeg_segment(data,type == "New Segment" , end = type == "Time 0")
})

baselines <- dplyr::summarize(dplyr::group_by(
  dplyr::filter(as_tibble(data$signal), .sample_id <= 0),
  .id
), bX = mean(X), bY = mean(Y))

signal_with_baselines <- dplyr::left_join(as_tibble(data$signal), baselines)
signal_with_baselines$new_X <- signal_with_baselines$X - signal_with_baselines$bX
signal_with_baselines$new_Y <- signal_with_baselines$Y - signal_with_baselines$bY
baselined <- ch_baseline(data)


test_that("baseline works", {
  expect_equal(as_tibble(baselined$signal)$X, signal_with_baselines$new_X)
  expect_equal(as_tibble(baselined$signal)$Y, signal_with_baselines$new_Y)
})



eeg_segment(data, type == "Time 0", lim = c(-1 / 500, 0))
eeg_segment(data0, type == "Time 0", lim = c(-1 / 500, 0))
data0_s <- eeg_segment(data0, type == "Time 0", lim = c(-Inf, Inf))



N <- 1000
data_eeg <- eeg_lst(
  signal_tbl =  
      dplyr::tibble(X = sin(1:N/20),
                    Y = cos(1:N/20),
    .id = rep(c(1L, 2L), each = N/2),
    .sample_id = sample_int(rep(seq.int(-100, N/2 -101), times = 2), sampling_rate = 500)),
channels_tbl=    dplyr::tibble(
      .channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_
    ),
  events_tbl =  dplyr::tribble(
    ~.id, ~type, ~description, ~.sample_0, ~.size, ~.channel,
    1L, "New Segment", NA_character_, -100L, 1L, NA,
    1L, "Bad", NA_character_, -20L, 30L, NA,
    1L, "Time 0", NA_character_, 1L, 1L, NA,
    1L, "Bad", NA_character_, 20L, 2L, "X",
    2L, "New Segment", NA_character_, -100L, 1L, NA,
    2L, "Time 0", NA_character_, 1L, 1L, NA,
    2L, "Bad", NA_character_, 20L, 10L, "Y"
    ),
  segments_tbl = dplyr::tibble(.id = c(1L, 2L), recording = "recording1", segment = c(1L, 2L))
)


data_d <- eeg_downsample(data_eeg, q=2)

## dplyr::bind_rows(dplyr::tibble(x=seq_along(data_eeg$signal$X),y= data_eeg$signal$X %>% as.numeric, type= "original"),
##                  dplyr::tibble(x=seq(from = 1, to = N, by= 2),y= data_d$signal$X %>% as.numeric, type= "downsampled")) %>%
## ggplot(aes(x=x,y=y, color = type)) + geom_point()


test_that("the signal after downsampling remains similar; sample0 = -100 ", {
    expect_equal(as.numeric(data_d$signal$X), as.numeric(data_eeg$signal$X)[seq(1, N, by = 2)], tolerance = .011)
})


test_that("times remain similar; sample0 = -100", {
    expect_equal(as_time(data_d$signal$.sample_id),  as_time(data_eeg$signal$.sample_id)[seq(1, N, by = 2)], tolerance = 1/500 +.00001)
    expect_equal(as.numeric(events_tbl(data_d)$.sample_0/250 ), as.numeric(events_tbl(data_eeg)$.sample_0/500)  , tolerance = 1/500 + .00001)
    expect_equal(events_tbl(data_d)$.size/250, events_tbl(data_eeg)$.size/500, tolerance = 1/500 + .00001)
})


data_eegm1 <- data_eeg
data_eegm1$signal$.sample_id <- data_eegm1$signal$.sample_id +1
data_eegm1$events$.sample_0 <- data_eegm1$events$.sample_0 +1


data_dm1 <- eeg_downsample(data_eegm1, q=2)

test_that("the signal after downsampling remains similar; sample0 = 0 ", {
    expect_equal(as.numeric(data_dm1$signal$X), as.numeric(data_eegm1$signal$X)[seq(1, N, by = 2)], tolerance = .011)
})


test_that("times remain similar; sample0 = 0", {
    expect_equal(as_time(data_dm1$signal$.sample_id),  as_time(data_eegm1$signal$.sample_id)[seq(1, N, by = 2)], tolerance = 1/500 +.00001)
    expect_equal(as.numeric(events_tbl(data_dm1)$.sample_0/250 ), as.numeric(events_tbl(data_eegm1)$.sample_0/500 ), tolerance = 1/500 + .00001)
})



data_eeg0 <- data_eeg
data_eeg0$signal$.sample_id <- data_eeg0$signal$.sample_id +100
data_eeg0$events$.sample_0 <- data_eeg0$events$.sample_0 +100


data_d0 <- eeg_downsample(data_eeg0, q=2)

test_that("the signal after downsampling remains similar; sample0 = 0 ", {
    expect_equal(as.numeric(data_d0$signal$X), as.numeric(data_eeg0$signal$X)[seq(1, N, by = 2)], tolerance = .011)
})


test_that("times remain similar; sample0 = 0", {
    expect_equal(as_time(data_d0$signal$.sample_id),  as_time(data_eeg0$signal$.sample_id)[seq(1, N, by = 2)], tolerance = 1/500 +.00001)
    expect_equal(as.numeric(events_tbl(data_d0)$.sample_0/250 ), as.numeric(events_tbl(data_eeg0)$.sample_0/500 ), tolerance = 1/500 + .00001)
})


data_eeg1 <- data_eeg
data_eeg1$signal$.sample_id <- data_eeg1$signal$.sample_id +101
data_eeg1$events$.sample_0 <- data_eeg1$events$.sample_0 +101


data_d1 <- eeg_downsample(data_eeg1, q=2)

test_that("the signal after downsampling remains similar ; sample0 = 1", {
    expect_equal(as.numeric(data_d1$signal$X), as.numeric(data_eeg1$signal$X)[seq(1, N, by = 2)], tolerance = .011)
})


test_that("times remain similar; sample0 = 1", {
    expect_equal(as_time(data_d1$signal$.sample_id),  as_time(data_eeg1$signal$.sample_id)[seq(1, N, by = 2)], tolerance = 1/500 +.00001)
    expect_equal(as.numeric(events_tbl(data_d1)$.sample_0/250 ), as.numeric(events_tbl(data_eeg1)$.sample_0/500 ), tolerance = 1/500 + .00001)
})

data_eeg2 <- data_eeg
data_eeg2$signal$.sample_id <- data_eeg2$signal$.sample_id +102
data_eeg2$events$.sample_0 <- data_eeg2$events$.sample_0 +102


data_d2 <- eeg_downsample(data_eeg2, q=2)

test_that("the signal after downsampling remains similar ; sample0 = 2", {
    expect_equal(as.numeric(data_d2$signal$X), as.numeric(data_eeg2$signal$X)[seq(1, N, by = 2)], tolerance = .011)
})


test_that("times remain similar; sample0 = 2", {
    expect_equal(as_time(data_d2$signal$.sample_id),  as_time(data_eeg2$signal$.sample_id)[seq(1, N, by = 2)], tolerance = 1/500 +.00001)
    expect_equal(as.numeric(events_tbl(data_d2)$.sample_0/250), as.numeric(events_tbl(data_eeg2)$.sample_0/500), tolerance = 1/500 + .00001)
})



data_eeg100 <- data_eeg
data_eeg100$signal$.sample_id <- data_eeg100$signal$.sample_id +201
data_eeg100$events$.sample_0 <- data_eeg100$events$.sample_0 +201


data_d100 <- eeg_downsample(data_eeg100, q=2)

test_that("the signal after downsampling remains similar ; sample0 = 101", {
    expect_equal(as.numeric(data_d100$signal$X), as.numeric(data_eeg100$signal$X)[seq(1, N, by = 2)], tolerance = .011)
})


test_that("times remain similar; sample0 = 101", {
    expect_equal(as_time(data_d100$signal$.sample_id),  as_time(data_eeg100$signal$.sample_id)[seq(1, N, by = 2)], tolerance = 1/500 +.00001)
    expect_equal(as.numeric(events_tbl(data_d100)$.sample_0/250 ), as.numeric(events_tbl(data_eeg100)$.sample_0/500 ), tolerance = 1/500 + .00001)
})


#### OTHER Q:

data_d <- eeg_downsample(data_eeg, q=20,multiple_times = TRUE )

## dplyr::bind_rows(dplyr::tibble(x=seq_along(data_eeg$signal$X),y= data_eeg$signal$X %>% as.numeric, type= "original"),
##                  dplyr::tibble(x=seq(from = 1, to = N, by= 20),y= data_d$signal$X %>% as.numeric, type= "downsampled")) %>%
## ggplot(aes(x=x,y=y, color = type)) + geom_point()
 
 
test_that("the signal after downsampling remains similar;q=20 ", {
    expect_equal(as.numeric(data_d$signal$X), as.numeric(data_eeg$signal$X)[seq(1, N, by = 20)], tolerance = .04)
})


test_that("times remain similar; q=20", {
    expect_equal(as_time(data_d$signal$.sample_id),  as_time(data_eeg$signal$.sample_id)[seq(1, N, by = 20)], tolerance = 1/20)
    expect_equal(as.numeric(events_tbl(data_d)$.sample_0/25 ), as.numeric(events_tbl(data_eeg)$.sample_0/500 ), tolerance = 1/20)
    expect_equal(events_tbl(data_d)$.size/25, events_tbl(data_eeg)$.size/500, tolerance = 1/20)
})

#### OTHER Q:

data_dmax <- eeg_downsample(data_eeg,max_sample = 100 )

## dplyr::bind_rows(dplyr::tibble(x=seq_along(data_eeg$signal$X),y= data_eeg$signal$X %>% as.numeric, type= "original"),
##                  dplyr::tibble(x=seq(from = 1, to = N, by= 2),y= data_d$signal$X %>% as.numeric, type= "downsampled")) %>%
## ggplot(aes(x=x,y=y, color = type)) + geom_point()


test_that("the signal after downsampling remains similar; max_sample =100 ", {
    expect_equal(as.numeric(data_dmax$signal$X), as.numeric(data_eeg$signal$X)[seq(1, N, by = 5)], tolerance = .011)
})


test_that("times remain similar; max_sample=100", {
    expect_equal(as_time(data_dmax$signal$.sample_id),  as_time(data_eeg$signal$.sample_id)[seq(1, N, by = 5)], tolerance = 1/100)
    expect_equal(as.numeric(events_tbl(data_dmax)$.sample_0/100 ), as.numeric(events_tbl(data_eeg)$.sample_0/500 ), tolerance = 1/20)
    expect_equal(events_tbl(data_dmax)$.size/100, events_tbl(data_eeg)$.size/500, tolerance = 1/20)
})

