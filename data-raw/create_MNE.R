library(eeguana)

bvfile <- system.file("testdata","bv_export_bv_txt_bin_vector.vhdr",package="eeguana")
eeg_read <- read_vhdr(bvfile, .recording = "bv1")
bvfile_pkl <- paste0(bvfile,".pkl")

library(reticulate)
use_condaenv("mne")
mne_io <- import("mne.io")
.data <- mne_io$read_raw_brainvision(bvfile, preload= TRUE, stim_channel= FALSE)

py_save_object(.data, bvfile_pkl, pickle = "pickle")
