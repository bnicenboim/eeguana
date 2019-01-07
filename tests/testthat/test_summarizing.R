context("test summarizing functions")
library(eeguana)


eeg_file <- read_vhdr(system.file("testdata","bv_export_bv_txt_bin_multi.vhdr",package="eeguana"))

# just to check if something break, but this should be done with test_that
channel_names(eeg_file)
nchannels(eeg_file)
nsamples(eeg_file)
count_complete_cases_tbl(eeg_file)


