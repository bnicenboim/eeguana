context("test dplyr functions")
library(eegble)


eegbl_av <- read_vhdr("binary-avfaces.vhdr")

# just to check if something break, but this should be done with test_that
group_by(eegbl_av, sample) %>% summarize_all(mean)
group_by(eegbl_av, segment) %>% summarize_all(mean)

# comment test
