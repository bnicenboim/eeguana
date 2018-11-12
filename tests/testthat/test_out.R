context("test out functions")
library(eeguana)

# xx <- 
#                 read_vhdr(paste0("preproc_n/",files)) %>%
#                 segment(description %in% c("s13","s23", "s33", "s43"), 
#                             lim=c(-.1,.9))

# x <- xx

eeg_lst_av <- read_vhdr("../../inst/extdata/binary-avfaces.vhdr")

# just to check if something break, but this should be done with test_that
channel_names(eeg_lst_av)
nchannels(eeg_lst_av)
nsamples(eeg_lst_av)
count_complete_cases_tbl(eeg_lst_av)
# summary(eeg_lst_av)


test_that("can read channels metadata", {
	  channels_df <-  channels_tbl(eeg_lst_av)
  expect_equal(channels_df$.name, channel_names(eeg_lst_av))
  expect_equal(length(channels_df$.x), nchannels(eeg_lst_av))
})

test_that("can change channels metadata", {
  channels_info <- channels_tbl(eeg_lst_av)
  channels_info$.name[1] <- "NEW_CHANNEL"
  channels_info$.x[1] <- 100
  channels_info$.x[2] <- 100
  channels_tbl(eeg_lst_av) <- channels_info
  expect_equal(names(eeg_lst_av$signal[, 3]), "NEW_CHANNEL")
  expect_equal(attributes(eeg_lst_av$signal[[4]])$.x, 100)
})

# eeguana:::update_channel_meta_data(select(eeg_lst_av$signal, channel_names(eeg_lst_av)), channels_info)
# eeg_lst_av$signal[[3]]

# select(eeg_lst_av$signal, -one_of(channel_names(eeg_lst_av)))
