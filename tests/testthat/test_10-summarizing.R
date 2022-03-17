library(eeguana)
options(eeguana.verbose = FALSE)
expect_equal_plain_df <- eeguana:::expect_equal_plain_df

eeg_file <- read_vhdr(system.file("testdata", "bv_export_bv_txt_bin_multi.vhdr", package = "eeguana"))

data_sincos2id <- eeguana:::data_sincos2id_2

d0 <- data_sincos2id %>% eeg_events_to_NA(.type == "Bad")
d1 <- data_sincos2id
events_tbl(d1) <- events_tbl(d1) %>% tidytable::filter.(!.id==2 | !.type == "Bad")
d1 <- d1 %>% eeg_events_to_NA(.type == "Bad")

d2 <- data_sincos2id
events_tbl(d2) <- events_tbl(d2) %>% tidytable::filter.(!.id==1 | !.type == "Bad")
d2 <- d2 %>% eeg_events_to_NA(.type == "Bad")

test_that("count_complete_cases_tbl works", {
  
  expect_equal_plain_df(count_complete_cases_tbl(d0),data.table::data.table(N=0))
  expect_equal_plain_df(count_complete_cases_tbl(d0, .recording),
                        data.table::data.table(.recording = "recording1", N=0))
  expect_equal_plain_df(count_complete_cases_tbl(d0, .recording, segment,.id),
                        data.table::data.table(.recording = c("recording1","recording1"),
                                               segment = 1:2, .id =1:2,N=c(0,0)))

  expect_equal_plain_df(count_complete_cases_tbl(d1),data.table::data.table(N=1))
  expect_equal_plain_df(count_complete_cases_tbl(d1, .recording),
                        data.table::data.table(.recording = "recording1", N=1))
  expect_equal_plain_df(count_complete_cases_tbl(d1, segment),
                        data.table::data.table(segment = 1:2,N=c(0,1)))
  
  expect_equal_plain_df(count_complete_cases_tbl(d2),data.table::data.table(N=1))
  expect_equal_plain_df(count_complete_cases_tbl(d2, .recording),
                        data.table::data.table(.recording = "recording1", N=1))
  expect_equal_plain_df(count_complete_cases_tbl(d2, segment),
                        data.table::data.table(segment = 1:2,N=c(1,0)))
})
  
test_that("summarizing functions don't break", {
  expect_snapshot(channel_names(eeg_file))
  expect_snapshot(nchannels(eeg_file))
  expect_snapshot(nsamples(eeg_file))
})
