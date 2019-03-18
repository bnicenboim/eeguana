context("convert formats")
library(eeguana)



#reference
bvfile <- system.file("testdata","bv_export_bv_txt_bin_vector.vhdr",package="eeguana")
eeg_read <- read_vhdr(bvfile, recording = "bv1")
bvfile_pkl <- paste0(bvfile,".pkl")

eeg_pkl <- py_load_object(bvfile_pkl, pickle = "pickle")

eeg_mne <- as_eeg_lst(eeg_pkl)

events(eeg_read) <- events(eeg_read) %>% mutate(description=paste0(type,"/",description)) %>% select(-type) 
channels_tbl(eeg_read) <- channels_tbl(eeg_read) %>% select(channel,.x,.y,.z,unit, .reference) %>% mutate(.reference=NA)
channels_tbl(eeg_mne)

test_that("objects match", {
    expect_equal(eeg_read,eeg_mne)
})

