context("test out functions")
library(eeguana)


eeg_lst_av <- read_vhdr("binary-avfaces.vhdr")

# just to check if something break, but this should be done with test_that
channel_names(eeg_lst_av)
nchannels(eeg_lst_av)
nsamples(eeg_lst_av)
count_complete_cases_tbl(eeg_lst_av)
#summary(eeg_lst_av)


