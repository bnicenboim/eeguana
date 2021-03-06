library(eeguana)
options(eeguana.verbose = FALSE)

eeg_file <- read_vhdr(system.file("testdata", "bv_export_bv_txt_bin_multi.vhdr", package = "eeguana"))

test_that("summarizing functions don't break", {
  expect_snapshot(channel_names(eeg_file))
  expect_snapshot(nchannels(eeg_file))
  expect_snapshot(nsamples(eeg_file))
  expect_snapshot(count_complete_cases_tbl(eeg_file))
})
