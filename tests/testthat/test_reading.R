context("Read dat files")
library(eeguana)



multiplexed_bin_bv1 <- read_vhdr(system.file("testdata","asalab_export_bv.vhdr",package="eeguana"), recording = "bv1")

multiplexed_bin_bv2 <- read_vhdr(system.file("testdata","bv_export_bv_txt_bin_multi.vhdr",package="eeguana"), recording = "bv2")
vectorized_bin_bv2 <- read_vhdr(system.file("testdata","bv_export_bv_txt_bin_vector.vhdr",package="eeguana"), recording = "bv2")
multiplexed_ascii_bv2 <- read_vhdr(system.file("testdata","bv_export_bv_txt_txt_multi.vhdr" ,package="eeguana"), recording = "bv2")
vectorized_ascii_bv2 <- read_vhdr(system.file("testdata","bv_export_bv_txt_txt_vector.vhdr",package="eeguana"), recording = "bv2")

#Integer encoding
multiplexed_bin_bv2_int16 <- read_vhdr(system.file("testdata","bv_export_bv_txt_bin_multi_16bit.vhdr" ,package="eeguana"), recording = "bv2")
vectorized_bin_bv2_int16 <- read_vhdr(system.file("testdata","bv_export_bv_txt_bin_vector_16bit.vhdr",package="eeguana"), recording = "bv2")

multiplexed_bin_bv2_int32 <- read_vhdr(system.file("testdata","bv_export_bv_txt_bin_multi_32bit.vhdr",package="eeguana"), recording = "bv2")
vectorized_bin_bv2_int32 <- read_vhdr(system.file("testdata","bv_export_bv_txt_bin_vector_32bit.vhdr",package="eeguana"), recording = "bv2")

test_that("files match", {
  expect_equal(multiplexed_bin_bv2,vectorized_bin_bv2)
  expect_equal(vectorized_ascii_bv2,multiplexed_ascii_bv2)
 #many rounding errors, but besides that the ascii and the binary should be the same:
 vectorized_ascii_bv2$signal <-  round(vectorized_ascii_bv2$signal,-2)
 vectorized_bin_bv2$signal <- round(vectorized_bin_bv2$signal,-2)
  expect_equal(vectorized_ascii_bv2,vectorized_bin_bv2)
})

test_that("int files match", {
  expect_equal(multiplexed_bin_bv2_int16,vectorized_bin_bv2_int16)
  expect_equal(multiplexed_bin_bv2_int32,vectorized_bin_bv2_int32)
})


ft <- read_ft(system.file("testdata","fieldtrip_matrix.mat",package="eeguana"), recording = "bv2")


channels_tbl(ft) <- channels_tbl(multiplexed_bin_bv2)

test_that("can read fieldtrip files", {
   # expect_equal(ft,multiplexed_bin_bv2)
})


