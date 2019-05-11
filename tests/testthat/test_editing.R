context("test editing functions")
library(eeguana)


eeg_file <- read_vhdr(file = system.file("testdata", "bv_export_bv_txt_bin_multi.vhdr", package = "eeguana"))


test_that("can read channels metadata", {
  channels_df <- channels_tbl(eeg_file)
  expect_equal(channels_df$.channel, channel_names(eeg_file))
  expect_equal(length(channels_df$.x), nchannels(eeg_file))
})

test_that("can change channels metadata", {
  channels_info <- channels_tbl(eeg_file)
  channels_info$.channel[1] <- "NEW_CHANNEL"
  channels_info$.x[1] <- 100
  channels_info$.x[2] <- 100
  channels_tbl(eeg_file) <- channels_info
  expect_equal(names(eeg_file$.signal[, 3]), "NEW_CHANNEL")
  expect_equal(attributes(eeg_file$.signal[[4]])$.x, 100)
})
