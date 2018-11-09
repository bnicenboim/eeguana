context("Read dat files")
library(eeguana)


multiplexed_bin_bv1 <- read_vhdr("../../inst/extdata/asalab_export_bv.vhdr", recording = "bv1")

multiplexed_bin_bv2 <- read_vhdr("../../inst/extdata/bv_export_asa_txt_bin_multi.vhdr", recording = "bv2")
vectorized_bin_bv2 <- read_vhdr("../../inst/extdata/bv_export_asa_txt_bin_vector.vhdr", recording = "bv2")
multiplexed_ascii_bv2 <- read_vhdr(file = "../../inst/extdata/bv_export_asa_txt_txt_multi.vhdr", recording = "bv2")
vectorized_ascii_bv2 <- read_vhdr("../../inst/extdata/bv_export_asa_txt_txt_vector.vhdr", recording = "bv2")


test_that("files match", {
  expect_equal(multiplexed_bin_bv2,vectorized_bin_bv2)
  expect_equal(vectorized_ascii_bv2,multiplexed_ascii_bv2)
})

#test that ascii and binary files are roughly the same

test_that("can read fieldtrip files", {
  expect_known_value(read_ft(file = "../../inst/extdata/data_h.mat", layout = "../../inst/extdata/easycapM23.mat"),
  				"fieldtrip.Rds")
})


