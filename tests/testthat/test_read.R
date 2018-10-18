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

eeg_lst_av <- read_vhdr("binary-avfaces.vhdr")

test_that("can read binary dat files", {
  expect_equal(max(eeg_lst_av$signal$.sample_id), 500)
  expect_equal(eeg_lst_av$events$.sample_0[eeg_lst_av$events$type == "New Segment"], -99)
  expect_equal(eeg_lst_av$events$.sample_0[eeg_lst_av$events$type == "Time 0"], 1)
})


ft_eeg <- read_ft("../../inst/extdata/data_h.mat", layout = "../../inst/extdata/easycapM23.mat")

