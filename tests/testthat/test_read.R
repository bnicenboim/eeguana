context("Read dat files")
library(eegble)

eegbl_ascii <- read_vhdr("01_N400_CONG.vhdr")

test_that("can read ascii dat files", {
  expect_equal(max(eegbl_ascii$signal$sample), 1000)
  expect_equal(unique(eegbl_ascii$events$sample[eegbl_ascii$events$type == "New Segment"]), -499)
  expect_equal(unique(eegbl_ascii$events$sample[eegbl_ascii$events$type == "Time 0"]), 1)
  expect_equal(sum(eegbl_ascii$events$description == "s7", na.rm = TRUE), nrow(eegbl_ascii$segments))
})

eegbl_av <- read_vhdr("binary-avfaces.vhdr")

test_that("can read binary dat files", {
  expect_equal(max(eegbl_av$signal$sample), 500)
  expect_equal(eegbl_av$events$sample[eegbl_av$events$type == "New Segment"], -99)
  expect_equal(eegbl_av$events$sample[eegbl_av$events$type == "Time 0"], 1)
})
