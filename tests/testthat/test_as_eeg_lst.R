context("convert formats")
library(eeguana)
test_that("raw brainvision read and converted from MNE match", {
  # it needs MNE installed
  ## skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  ## reference
  bvfile <- system.file("testdata", "bv_export_bv_txt_bin_vector.vhdr", package = "eeguana")
  eeg_read <- read_vhdr(file = bvfile, .recording = "r1")
  ## bvfile_pkl <- paste0(bvfile,".pkl")

  reticulate::use_condaenv("anaconda3")
  mne_io <- reticulate::import("mne.io")
  eeg_mne_obj <- mne_io$read_raw_brainvision(bvfile,
                                             preload = TRUE,
                                             eog = c("VEOG","HEOG"),
                                             misc = c("M1","M2"))

  ## eeg_pkl <- reticulate::py_load_object(bvfile_pkl, pickle = "pickle")

  eeg_mne <- as_eeg_lst(.data = eeg_mne_obj) %>%
    dplyr::mutate(.recording = "r1")

  # events_tbl(eeg_read) <- events_tbl(eeg_read) %>%
  # dplyr::mutate(.description=paste0(.type,"/",.description)) %>% dplyr::select(-.type)
  channels_tbl(eeg_read) <- channels_tbl(eeg_read) %>% 
    dplyr::select(.channel, .x, .y, .z, unit, .reference)
  channels_tbl(eeg_mne) 

   expect_equal(eeg_read, eeg_mne)
})
