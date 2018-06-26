context("Binding and transforming")
library(eegble)


cond1 <- eegble(data = dplyr::tibble(.id= rep(1L,10), sample= seq(-4L,5L), X = sin(1:10), Y = cos(1:10)),
           events = dplyr::tribble(~.id, ~type,        ~description, ~sample, ~size, ~channel,
                              1L, "New Segment",  NA,          -4L,       1L,   NA,
                              1L, "Time 0",       NA,           1L,       1L,   NA),
           chan_info = dplyr::tibble(labels = c("X", "Y"), theta = NA, phi = NA, radius = NA, x = NA, y = NA, z = NA),
           eeg_info = list(srate = 500, reference = NA),
           seg_info =  dplyr::tibble(.id = 1L, recording = "recording1", segment = 1, type = "initial"))


cond2 <- eegble(data = dplyr::tibble(.id= rep(1L,10), sample= seq(-4L,5L), X = sin(1:10) + .1, Y = cos(1:10) + .1),
           events = dplyr::tribble(~.id, ~type,        ~description, ~sample, ~size, ~channel,
                              1L, "New Segment",  NA,          -4L,       1L,   NA,
                              1L, "Time 0",       NA,           1L,       1L,   NA),
           chan_info = dplyr::tibble(labels = c("X", "Y"), theta = NA, phi = NA, radius = NA, x = NA, y = NA, z = NA),
           eeg_info = list(srate = 500, reference = NA),
           seg_info =  dplyr::tibble(.id = 1L, recording = "recording1", segment = 1, type = "initial"))


cond3 <- eegble(data = dplyr::tibble(.id= rep(1L,10), sample= seq(-4L,5L), X = sin(1:10) + .1, Y = cos(1:10) + .1),
           events = dplyr::tribble(~.id, ~type,        ~description, ~sample, ~size, ~channel,
                              1L, "New Segment",  NA,          -4L,       1L,   "X",
                              1L, "Time 0",       NA,           1L,       1L,   NA),
           chan_info = dplyr::tibble(labels = c("X", "Y"), theta = NA, phi = NA, radius = NA, x = NA, y = NA, z = NA),
           eeg_info = list(srate = 500, reference = NA),
           seg_info =  dplyr::tibble(.id = 1L, recording = "recording1", segment = 1, type = "initial"))



test_that("can bind unlisted files of same recording", {
 conds <- bind(cond1, cond2)
  expect_equal(nrow(conds$data), nrow(cond1$data) + nrow(cond2$data))
  expect_equal(max(conds$data$.id), max(cond1$data$.id) + max(cond2$data$.id))
  expect_equal(max(conds$seg_info$segment), max(cond1$seg_info$segment) + max(cond2$seg_info$segment))
})

test_that("can bind listed files of same recording", {
 conds <- bind(list(cond1, cond2))
  expect_equal(nrow(conds$data), nrow(cond1$data) + nrow(cond2$data))
  expect_equal(max(conds$data$.id), max(cond1$data$.id) + max(cond2$data$.id))
  expect_equal(max(conds$seg_info$segment), max(cond1$seg_info$segment) + max(cond2$seg_info$segment))
})


test_that("can bind listed files that show and do not show channels on the event table", {
 conds <- bind(list(cond1, cond3))
  expect_equal(nrow(conds$data), nrow(cond1$data) + nrow(cond3$data))
  expect_equal(max(conds$data$.id), max(cond1$data$.id) + max(cond3$data$.id))
  expect_equal(max(conds$seg_info$segment), max(cond1$seg_info$segment) + max(cond3$seg_info$segment))
})



test_that("can bind unlisted files of same recording", {
 cond1_2 <- cond2
 cond1_2$seg_info$recording <- "recording2"
 conds <- bind(cond1, cond1_2)
  expect_equal(nrow(conds$data), nrow(cond1$data) + nrow(cond2$data))
  expect_equal(max(conds$data$.id), max(cond1$data$.id) + max(cond2$data$.id))
  expect_equal(max(conds$seg_info$segment), max(cond1$seg_info$segment, cond2$seg_info$segment))
})



test_that("can transform to tibble", {
  cond1_2 <- cond2
  cond1_2$seg_info$recording <- "recording2"
  conds <- bind(cond1, cond1_2)
  df <- as_tibble(conds)
  expect_equal(nrow(df), nrow(conds$data) * length(chan_names(conds)) )
  expect_equal(max(df$time), max((conds$data$sample-1)) / srate(conds))
})


