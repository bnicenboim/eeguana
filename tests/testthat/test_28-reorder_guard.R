library(eeguana)

## validate_signal_tbl() and validate_psd_tbl() move the obligatory columns to
## the front, copying the table first when that is needed. Their check compared
## the column names against the named vectors in obligatory_cols, so
## identical() never matched and every call copied the whole table. On the
## 34-channel test recording that was an extra 117 Mb on each eeg_mutate() and
## 30 Mb on each eeg_select().

test_that("a table whose obligatory columns come first needs no reorder", {
  expect_false(eeguana:::needs_reorder(data_faces_ERPs$.signal, eeguana:::obligatory_cols[[".signal"]]))
  psd <- data.table::data.table(.id = 1L, .freq = 1, Fz = 1)
  expect_false(eeguana:::needs_reorder(psd, eeguana:::obligatory_cols[[".psd"]]))
})

test_that("a table whose obligatory columns do not come first needs one", {
  signal <- data.table::copy(data_faces_ERPs$.signal)
  data.table::setcolorder(signal, c(".sample", ".id"))
  expect_true(eeguana:::needs_reorder(signal, eeguana:::obligatory_cols[[".signal"]]))
})

test_that("validating a signal table that is already in order copies nothing", {
  signal <- data_faces_ERPs$.signal
  validated <- eeguana:::validate_signal_tbl(signal)
  expect_identical(data.table::address(validated), data.table::address(signal))
})
