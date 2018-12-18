context("Read dat files")
library(eeguana)


multiplexed_bin_bv1 <- read_vhdr(file = system.file("testdata","asalab_export_bv.vhdr",package="eeguana"), recording = "bv1")
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
  expect_equal(vectorized_ascii_bv2,vectorized_bin_bv2, tolerance = .00001)
})

test_that("int files match", {
  expect_equal(multiplexed_bin_bv2_int16,vectorized_bin_bv2_int16)
  expect_equal(multiplexed_bin_bv2_int32,vectorized_bin_bv2_int32)
})


ft <- read_ft(file = system.file("testdata","fieldtrip_matrix.mat",package="eeguana"), recording = "bv2")
channels_tbl(ft) <- channels_tbl(multiplexed_bin_bv2)

test_that("can read fieldtrip files", {
   # expect_equal(ft,multiplexed_bin_bv2)
})



edf <- read_edf(file = system.file("testdata","asalab_export_edf_Segment_1.edf",package="eeguana"), recording = "edf")
edf_bv <- read_edf(file = system.file("testdata","bv_export_edf.edf",package="eeguana"), recording = "edf")
edf_plus_bv <- read_edf(file = system.file("testdata","bv_export_edf+.edf",package="eeguana"), recording = "edf")

ch_tbl <- channels_tbl(multiplexed_bin_bv2)
max_sample <- max(multiplexed_bin_bv2$signal$.sample_id)
edf_f <- dplyr::filter(edf, .sample_id <=  4722)
channels_tbl(edf_f) <- ch_tbl
channels_tbl(edf) <- channels_tbl(edf_bv)
channels_tbl(edf_plus_bv) <- channels_tbl(edf_bv)
events_bv <- events(multiplexed_bin_bv2) %>% dplyr::select(-type) %>% dplyr::rename(annotation=description) %>% 
  dplyr::mutate(annotation=ifelse(annotation=="","New Segment",annotation)) %>% dplyr::as_tibble()
events_edf <- events(edf_plus_bv) %>% as_tibble()      

test_that("edf and dat files match", {
  expect_equal(edf_f$signal, multiplexed_bin_bv2$signal, tolerance = 2)
  expect_equal(edf,edf_bv, tolerance=2)
  expect_equal(edf_plus_bv$signal,edf_bv$signal, tolerance=0.2)
  expect_equal(events_bv,  events_edf)
})


# From https://www.teuniz.net/edf_bdf_testfiles/
edf_test <- read_edf(file = system.file("testdata","test_generator_2.edf",package="eeguana"), recording = "test")
bdf_test <- read_edf(file = system.file("testdata","test_generator_2.bdf",package="eeguana"), recording = "test")


test_that("edf and bdf files match", {
  expect_equal(edf_test,bdf_test, tolerance = .1)
})




seged_ascii <- multiplexed_ascii_bv2 %>% segment(description %in% c("s10","s11","s12"),lim=c(0,.499))
seg_ascii_bv2 <- read_vhdr(system.file("testdata","bv_segexport_ascii.vhdr",package="eeguana"), recording = "bv2")

seged_bin <- multiplexed_bin_bv2 %>% segment(description %in% c("s10","s11","s12"),lim=c(0,.499))
seg_bin_bv2 <- read_vhdr(system.file("testdata","bv_segexport_bin.vhdr",package="eeguana"), recording = "bv2")

test_that("seg matches", {
  expect_equal(seg_ascii_bv2$signal,seged_ascii$signal)
  expect_equal(events(seg_ascii_bv2)[type=="Stimulus"],events(seged_ascii))
  expect_equal(seg_ascii_bv2$segments,dplyr::select(seged_ascii$segments,-type,-description))

  expect_equal(seg_bin_bv2$signal,seged_bin$signal)
  expect_equal(events(seg_bin_bv2)[type=="Stimulus"],events(seged_bin))
  expect_equal(seg_bin_bv2$segments,dplyr::select(seged_bin$segments,-type,-description))
})



#add sample as a possible unit


