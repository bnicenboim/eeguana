context("Read dat files")
library(eeguana)

# eeg_lst_ascii <- read_vhdr("01_N400_CONG.vhdr")

# test_that("can read ascii dat files", {
#   expect_equal(max(eeg_lst_ascii$signal$.sample_id), 1000)
#   expect_equal(max(eeg_lst_ascii$signal$.id), max(eeg_lst_ascii$segments$.id))
#   expect_equal(unique(eeg_lst_ascii$events$.sample_0[eeg_lst_ascii$events$type == "New Segment"]), -499)
#   expect_equal(unique(eeg_lst_ascii$events$.sample_0[eeg_lst_ascii$events$type == "Time 0"]), 1)
#   expect_equal(sum(eeg_lst_ascii$events$description == "s7", na.rm = TRUE), nrow(eeg_lst_ascii$segments))
# })



test_that("can read vectorized BV files", {
  noise_binary <- read_vhdr("../../inst/extdata/bvexport_asa_binary.vhdr")
  noise_ascii <- read_vhdr("../../inst/extdata/bvexport_asa_ascii.vhdr")
  expect_known_value(noise_ascii,"noise_ascii.Rds")
  expect_known_value(noise_binary,"noise_binary.Rds")
  # avoiding approximation differences, they should be the same
  noise_binary$signal[channel_names(noise_binary)] <- round(noise_binary$signal[channel_names(noise_binary)],-2) 
  noise_ascii$signal[channel_names(noise_ascii)] <- round(noise_ascii$signal[channel_names(noise_ascii)],-2) 
  noise_binary$segments$recording <- "X"
  noise_ascii$segments$recording <- "X"
  expect_equal(noise_binary, noise_ascii)
})




test_that("can read multiplexed BV files", {
  expect_known_value(read_vhdr("../../inst/extdata/01_N400_CONG.vhdr"),"n400.Rds")
})

test_that("can read fieldtrip files", {
  expect_known_value(read_ft("../../inst/extdata/data_h.mat", layout = "../../inst/extdata/easycapM23.mat"),
  				"fieldtrip.Rds")
})

