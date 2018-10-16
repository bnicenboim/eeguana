context("test out functions")
library(eeguana)


eegbl_av <- read_vhdr("binary-avfaces.vhdr")

# just to check if something break, but this should be done with test_that
channel_names(eegbl_av)
nchannels(eegbl_av)
nsamples(eegbl_av)
count_complete_cases_tbl(eegbl_av)
#summary(eegbl_av)


