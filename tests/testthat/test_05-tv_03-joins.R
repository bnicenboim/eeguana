library(eeguana)
options(eeguana.verbose = FALSE)

data <- eeguana:::data_sincos2id

table0 <- dplyr::tibble(.id = 1L, condition = "BLUE")

data_l <- eeg_left_join(data, table0)
data_s <- eeg_semi_join(data, table0)
data_a <- eeg_anti_join(data, table0)

test_that("joins work", {
  expect_equal_plain_df(data_l$.segments, 
               dplyr::left_join(data$.segments, table0, by = ".id"))
  expect_equal_plain_df(data_l$.signal, data$.signal)
  expect_equal_plain_df(data_s$.segments, dplyr::semi_join(data$.segments, table0, by = ".id"))
  expect_equal_eeg_lst(data_s, eeg_filter(data, .id == 1))
  expect_equal_eeg_lst(data_a, eeg_filter(data, .id == 2))
})

message("\n***")
message(" test by reference")
message(" test errors")
message(" test data frames/tibbles/data.tables")
