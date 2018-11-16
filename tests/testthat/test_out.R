context("test out functions")
library(eeguana)

# xx <- 
#                 read_vhdr(paste0("preproc_n/",files)) %>%
#                 segment(description %in% c("s13","s23", "s33", "s43"), 
#                             lim=c(-.1,.9))

# x <- xx

eeg_file <- read_vhdr("../../inst/extdata/bv_export_bv_txt_bin_multi.vhdr")

# just to check if something break, but this should be done with test_that
channel_names(eeg_file)
nchannels(eeg_file)
nsamples(eeg_file)
count_complete_cases_tbl(eeg_file)
# summary(eeg_file)


test_that("can read channels metadata", {
	  channels_df <-  channels_tbl(eeg_file)
  expect_equal(channels_df$.name, channel_names(eeg_file))
  expect_equal(length(channels_df$.x), nchannels(eeg_file))
})

test_that("can change channels metadata", {
  channels_info <- channels_tbl(eeg_file)
  channels_info$.name[1] <- "NEW_CHANNEL"
  channels_info$.x[1] <- 100
  channels_info$.x[2] <- 100
  channels_tbl(eeg_file) <- channels_info
  expect_equal(names(eeg_file$signal[, 3]), "NEW_CHANNEL")
  expect_equal(attributes(eeg_file$signal[[4]])$.x, 100)
})

# eeguana:::update_channel_meta_data(select(eeg_file$signal, channel_names(eeg_file)), channels_info)
# eeg_file$signal[[3]]

# select(eeg_file$signal, -one_of(channel_names(eeg_file)))
