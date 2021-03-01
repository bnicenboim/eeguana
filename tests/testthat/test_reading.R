context("Read  files")
library(eeguana)

multiplexed_bin_bv1 <- read_vhdr(file = system.file("testdata", "asalab_export_bv.vhdr",  package = "eeguana"), .recording = "bv2")
multiplexed_bin_bv2 <- read_vhdr(system.file("testdata", "bv_export_bv_txt_bin_multi.vhdr", package = "eeguana"), .recording = "bv2")

vectorized_bin_bv2 <- read_vhdr(system.file("testdata", "bv_export_bv_txt_bin_vector.vhdr", package = "eeguana"), .recording = "bv2")
multiplexed_ascii_bv2 <- read_vhdr(system.file("testdata", "bv_export_bv_txt_txt_multi.vhdr", package = "eeguana"), .recording = "bv2")
vectorized_ascii_bv2 <- read_vhdr(system.file("testdata", "bv_export_bv_txt_txt_vector.vhdr", package = "eeguana"), .recording = "bv2")

test_that("basic tests of the events", {
  expect_equal(events_tbl(multiplexed_bin_bv1)$.initial, events_tbl(multiplexed_bin_bv1)$.final)
  expect_equal(events_tbl(multiplexed_bin_bv1)$.initial[1], 1)
})

# Integer encoding
multiplexed_bin_bv2_int16 <- read_vhdr(system.file("testdata", "bv_export_bv_txt_bin_multi_16bit.vhdr", package = "eeguana"), .recording = "bv2")
vectorized_bin_bv2_int16 <- read_vhdr(system.file("testdata", "bv_export_bv_txt_bin_vector_16bit.vhdr", package = "eeguana"), .recording = "bv2")
multiplexed_bin_bv2_int32 <- read_vhdr(system.file("testdata", "bv_export_bv_txt_bin_multi_32bit.vhdr", package = "eeguana"), .recording = "bv2")
vectorized_bin_bv2_int32 <- read_vhdr(system.file("testdata", "bv_export_bv_txt_bin_vector_32bit.vhdr", package = "eeguana"), .recording = "bv2")

test_that("files match", {
  channels_tbl(multiplexed_bin_bv1) <- channels_tbl(vectorized_bin_bv2)
  expect_equal(multiplexed_bin_bv1, vectorized_bin_bv2, tolerance = .00001)
  expect_equal(vectorized_ascii_bv2, multiplexed_ascii_bv2)
  expect_equal(vectorized_ascii_bv2, vectorized_bin_bv2, tolerance = .00001)
})

test_that("int files match", {
  expect_equal(multiplexed_bin_bv2_int16, vectorized_bin_bv2_int16)
  expect_equal(multiplexed_bin_bv2_int32, vectorized_bin_bv2_int32)
})


## repeated channel
multiplexed_bin_repeat <- read_vhdr(file = system.file("testdata", "asalab_export_bv_2.vhdr", package = "eeguana"), .recording = "bv2")


test_that("repeated channels are not a problem", {
  expect_equal(multiplexed_bin_bv1, dplyr::rename(multiplexed_bin_repeat, VEOG = HEOG.1))
})



ft <- read_ft(file = system.file("testdata", "fieldtrip_matrix.mat", package = "eeguana"), .recording = "bv2")
channels_tbl(ft) <- channels_tbl(multiplexed_bin_bv2)

test_that("can read fieldtrip files", {
#  expect_equal(ft,)
})


test_that("can read unique eeglab files ",{
  eeguana:::skip_on_actions()
  skip_on_ci()
  #stefan frank data
  expect_s3_class(other1 <- read_set(file = system.file("testdata", "EEG01.mat", package = "eeguana"), .recording = "eeglab"), "eeg_lst")
  gc()
  expect_s3_class(other2 <- read_set(file = system.file("testdata", "eeglab_data.set", package = "eeguana"), .recording = "eeglab"), "eeg_lst")
})


test_that("can read eeglab from brainvision",{

expect_warning(set1 <- read_set(file = system.file("testdata", "bv_export_bv_txt_bin_multi.set", package = "eeguana"), .recording = "bv2"))

  expect_warning(set2 <- read_set(file = system.file("testdata", "bv_export_bv_txt_bin_multi2.set", package = "eeguana"), .recording = "bv2"))
  expect_warning(set3 <- read_set(file = system.file("testdata", "bv_export_bv_txt_bin_multi3.set", package = "eeguana"), .recording = "bv2")
)
  expect_equal(set1, set2)
  expect_equal(set1, set3)

  channels_tbl(set1) <- channels_tbl(set1) %>% dplyr::select(.channel, .x,.y,.z)
  channels_tbl(multiplexed_bin_bv2) <- channels_tbl(multiplexed_bin_bv2) %>% dplyr::select(.channel, .x,.y,.z)
expect_equal(channels_tbl(set1), channels_tbl(multiplexed_bin_bv2))
eeguana:::expect_equal_plain_df(multiplexed_bin_bv2$.signal, set1$.signal)
eeguana:::expect_equal_plain_df(multiplexed_bin_bv2$.segments, set1$.segments)

set1$.events[,`:=`(bvtime = NULL, bvmknum = NULL, urevent = NULL, event = NULL)]
set1$.events[.description=="boundary", .description:=""]
eeguana:::expect_equal_plain_df(multiplexed_bin_bv2$.events,set1$.events )

})


test_that("can read segmented data from eeglab",{

expect_warning(set_epoched <- read_set(file = system.file("testdata", "bv_export_bv_txt_bin_multi_epoched_1.set", package = "eeguana"), .recording = "bv2"))
expect_warning(set_epoched2 <- read_set(file = system.file("testdata", "bv_export_bv_txt_bin_multi_epoched.set", package = "eeguana"), .recording = "bv2"))
expect_equal(set_epoched, set_epoched2)


bv_seged <- read_vhdr(system.file("testdata", "bv_export_bv_txt_bin_multi.vhdr", package = "eeguana"), .recording = "bv2") %>% eeg_segment(startsWith(.description,"s"), .lim = c(-.1,.098))

channels_tbl(set_epoched) <- channels_tbl(set_epoched) %>% dplyr::select(.channel, .x,.y,.z)
channels_tbl(bv_seged) <- channels_tbl(bv_seged) %>% dplyr::select(.channel, .x,.y,.z)
message("check carefully the epoched datasets")
#expect_equal(set_epoched,bv_seged)

expect_warning(set_epoched3 <- read_set(file = system.file("testdata", "bv_export_bv_txt_bin_multi_epoched_one.set", package = "eeguana"), .recording = "bv2"))

bv_seged3 <- read_vhdr(system.file("testdata", "bv_export_bv_txt_bin_multi.vhdr", package = "eeguana"), .recording = "bv2") %>% eeg_segment(.description =="s11", .lim = c(-.1,.098))


})

## EDF tests

edf <- read_edf(file = system.file("testdata", "asalab_export_edf_Segment_1.edf", package = "eeguana"), .recording = "edf")
edf_bv <- read_edf(file = system.file("testdata", "bv_export_edf.edf", package = "eeguana"), .recording = "edf")
edf_plus_bv <- read_edf(file = system.file("testdata", "bv_export_edf+.edf", package = "eeguana"), .recording = "edf")

ch_tbl <- channels_tbl(multiplexed_bin_bv2)
max_sample <- max(multiplexed_bin_bv2$.signal$.sample)
edf_f <- dplyr::filter(edf, .sample <= 4722)
channels_tbl(edf_f) <- ch_tbl


channels_tbl(edf) <- channels_tbl(edf_bv)
channels_tbl(edf_plus_bv) <- channels_tbl(edf_bv)
events_bv <- events_tbl(multiplexed_bin_bv2) %>%
  dplyr::mutate(.type = NA_character_, .description = ifelse(.description == "", "New Segment", .description)) %>%
  dplyr::as_tibble()
events_edf <- events_tbl(edf_plus_bv) %>% dplyr::as_tibble()

test_that("edf and dat files match", {
  expect_equal(edf_f$.signal, multiplexed_bin_bv2$.signal, tolerance = 2)
  expect_equal(edf, edf_bv, tolerance = 2)
  expect_equal(edf_plus_bv$.signal, edf_bv$.signal, tolerance = 0.2)
  expect_equal(events_bv, events_edf)
})


# From https://www.teuniz.net/edf_bdf_testfiles/
edf_test <- read_edf(file = system.file("testdata", "test_generator_2.edf", package = "eeguana"), .recording = "test")
bdf_test <- read_edf(file = system.file("testdata", "test_generator_2.bdf", package = "eeguana"), .recording = "test")


test_that("edf and bdf files match", {
  expect_equal(edf_test, bdf_test, tolerance = .1)
})

test_that("bdf  match the bdf created by MNE", {
  bdf_file <- system.file("testdata", "Newtest17-256.bdf", package = "eeguana")
  data_mne_bdf <- eeguana:::data_mne_bdf
  bdf <- read_edf(file = bdf_file, .recording = "bdf")

  expect_equal(bdf, data_mne_bdf)
})

## segmented
seged_ascii <- multiplexed_ascii_bv2 %>%
  eeg_segment(.description %in% c("s10", "s11", "s12"), .lim = c(0, .499))
seg_ascii_bv2 <- read_vhdr(file = system.file("testdata", "bv_segexport_ascii.vhdr", package = "eeguana"), .recording = "bv2")

seged_bin <- multiplexed_bin_bv2 %>% eeg_segment(.description %in% c("s10", "s11", "s12"), .lim = c(0, .499))
seg_bin_bv2 <- read_vhdr(system.file("testdata", "bv_segexport_bin.vhdr", package = "eeguana"), .recording = "bv2")

test_that("seg matches", {
  expect_equal(seg_ascii_bv2$.signal, seged_ascii$.signal)
  expect_equal(seg_ascii_bv2$.segments$segment, 1:12)

  expect_equal(events_tbl(seg_ascii_bv2)[.type == "Stimulus"], events_tbl(seged_ascii))
  expect_equal(seg_ascii_bv2$.segments, seged_ascii$.segments[, -c("type", "description")]
)

  expect_equal(seg_bin_bv2$.signal, seged_bin$.signal)
  expect_equal(events_tbl(seg_bin_bv2)[.type == "Stimulus"], events_tbl(seged_bin))
  expect_equal(seg_bin_bv2$.segments, seged_bin$.segments[, -c("type", "description")])
})

