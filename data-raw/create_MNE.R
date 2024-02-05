library(eeguana)

bvfile <- system.file("testdata", "bv_export_bv_txt_bin_vector.vhdr", package = "eeguana")
eeg_read <- read_vhdr(bvfile, .recording = "bv1")
bvfile_pkl <- paste0(bvfile, ".pkl")

library(reticulate)
use_condaenv("r-eeguana")
mne <- import("mne")
.data <- mne$io$read_raw_brainvision(bvfile, preload = TRUE, stim_channel = FALSE)

py_save_object(.data, bvfile_pkl, pickle = "pickle")


## Mne example
sample_data_folder <- mne$datasets$sample$data_path()
sample_data_raw_file <- file.path(sample_data_folder, "MEG", "sample", "sample_audvis_raw.fif")
raw = mne$io$read_raw_fif(sample_data_raw_file, verbose=FALSE)
raw$crop(0, 10)$load_data()
fiffile <- file.path(system.file("testdata", package = "eeguana"), "sample_audvis_raw_10s.fif")
raw$save(fiffile)
