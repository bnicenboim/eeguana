context("constructors")
library(eeguana)

test_that("can build eeg_lst", {
  expect_true(is_eeg_lst(eeg_lst()))
 new_obj <- eeg_lst(signal_tbl = data.table::data.table(.id=1, .sample=sample_int(1:10,246), channel= channel_dbl(1:10) ))
  expect_true(is_eeg_lst((new_obj)))
})
