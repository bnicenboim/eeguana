context("Signal manipulation")
library(eegble)


data <- eegble(data = dplyr::tibble(.id= rep(c(1L,2L), each = 10), sample= rep(seq(-4L,5L), times = 2), X = sin(1:20), Y = cos(1:20)),
           events = dplyr::tribble(~.id, ~type,        ~description, ~sample, ~size, ~channel,
                                      1L, "New Segment",  NA,          -4L,       1L,   NA,
                                      1L, "Bad",          NA,          -2L,       3L,   NA,
                                      1L, "Time 0",       NA,           1L,       1L,   NA,
                                      1L, "Bad",          NA,           2L,       2L,   "X",
                                      2L, "New Segment",  NA,          -4L,       1L,   NA,
                                      2L, "Time 0",       NA,           2L,       1L,   NA,
                                      2L, "Bad",          NA,           2L,       1L,   "Y"),
           chan_info = dplyr::tibble(labels = c("X", "Y"), theta = NA, phi = NA, radius = NA, x = NA, y = NA, z = NA),
           eeg_info = list(srate = 500, reference = NA),
           seg_info =  dplyr::tibble(.id = 1L, recording = "recording1", segment = 1, type = "initial"))




test_that("can clean files", {
  clean_data <- event_to_NA(data, type == "Bad")
  expect_equal(nrow(clean_data$events), 4)
  expect_equal(all(is.na(clean_data$data[clean_data$data$sample %in% seq(-2,-3+3-1) &
           clean_data$data$.id == 1 ,c("X","Y")])), TRUE)
  expect_equal(all(is.na(clean_data$data[clean_data$data$sample %in% seq(2, 2+2-1) &
           clean_data$data$.id == 1 ,c("X")])), TRUE)
  expect_equal(all(is.na(clean_data$data[clean_data$data$sample %in% seq(2, 2+2-1) &
           clean_data$data$.id == 1 ,c("Y")])), FALSE)
  expect_equal(all(is.na(clean_data$data[clean_data$data$sample %in% seq(2, 2+1-1) &
           clean_data$data$.id == 2 ,c("Y")])), TRUE)
  expect_equal(all(is.na(clean_data$data[clean_data$data$sample %in% seq(2, 2+1-1) &
           clean_data$data$.id == 2 ,c("X")])), FALSE)
    expect_equal(all(is.na(clean_data$data[clean_data$data$.id == 1 ,c("X","Y")])), FALSE)
  expect_equal(all(is.na(clean_data$data[clean_data$data$.id == 2 ,c("Y")])), FALSE)
  expect_equal(all(is.na(clean_data$data[clean_data$data$.id == 2 ,c("X")])), FALSE)
})

test_that("can clean whole channels in files", {
  clean_data_chan <- event_to_NA(data, type == "Bad", all_chans = TRUE)
  expect_equal(nrow(clean_data_chan$events), 4)
  expect_equal(all(is.na(clean_data_chan$data[clean_data_chan$data$sample %in% seq(-2,-3+3-1) &
           clean_data_chan$data$.id == 1 ,c("X","Y")])), TRUE)
  expect_equal(all(is.na(clean_data_chan$data[clean_data_chan$data$sample %in% seq(2, 2+2-1) &
           clean_data_chan$data$.id == 1 ,c("X")])), TRUE)
  expect_equal(all(is.na(clean_data_chan$data[clean_data_chan$data$sample %in% seq(2, 2+2-1) &
           clean_data_chan$data$.id == 1 ,c("Y")])), TRUE)
  expect_equal(all(is.na(clean_data_chan$data[clean_data_chan$data$sample %in% seq(2, 2+1-1) &
           clean_data_chan$data$.id == 2 ,c("Y")])), TRUE)
  expect_equal(all(is.na(clean_data_chan$data[clean_data_chan$data$sample %in% seq(2, 2+1-1) &
           clean_data_chan$data$.id == 2 ,c("X")])), TRUE)
  expect_equal(all(is.na(clean_data_chan$data[clean_data_chan$data$.id == 1 ,c("X","Y")])), FALSE)
  expect_equal(all(is.na(clean_data_chan$data[clean_data_chan$data$.id == 2 ,c("Y")])), FALSE)
  expect_equal(all(is.na(clean_data_chan$data[clean_data_chan$data$.id == 2 ,c("X")])), FALSE)
})

test_that("can clean whole segments in files", {
  clean_data_seg <- event_to_NA(data, type == "Bad", entire_seg = TRUE)
  expect_equal(nrow(clean_data_seg$events), 4)
  expect_equal(all(is.na(clean_data_seg$data[clean_data_seg$data$.id == 1 ,c("X","Y")])), TRUE)
  expect_equal(all(is.na(clean_data_seg$data[clean_data_seg$data$.id == 2 ,c("Y")])), TRUE)
  expect_equal(all(is.na(clean_data_seg$data[clean_data_seg$data$.id == 2 ,c("X")])), FALSE)
})



warning("segment tests needs to be done - check the ids everywhere when several recordings are there, check the lims working")
warning("baseline tests needs to be done")
warning("sig_wrangling tests needs to be done")