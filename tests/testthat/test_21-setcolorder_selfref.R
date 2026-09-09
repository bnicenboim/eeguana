library(eeguana)
options(eeguana.verbose = FALSE)

## data.table::setcolorder() corrupts a table that has lost its
## over-allocation (truelength 0, which is what every tidytable verb hands
## back) once it has 64 or more columns: the column *names* are permuted but
## the data is not moved, so each name ends up pointing at a different vector.
##
## Confirmed present in data.table 1.18.4 and 1.18.6.1, so upgrading does not
## help. The package works around it by restoring the over-allocation before
## the setcolorder() calls that receive tidytable output.
##
## The 64-column threshold is easy to reach: any 64+ channel montage, or a
## 34-channel recording summarised with a two-function across_ch()
## (34 * 2 + .id + .sample + 2 grouping columns = 72).

wide_tt <- function(nch, second_col = ".sample") {
  # A table with .id/.sample last and no over-allocation, built the way the
  # package's own pipelines build one.
  cols <- stats::setNames(
    lapply(seq_len(nch), function(i) c(i + 0.1, i + 0.2)),
    paste0("C", seq_len(nch))
  )
  cols[[second_col]] <- c(1L, 2L)
  cols$.id <- c(1L, 2L)
  x <- do.call(tidytable::tidytable, cols)
  # a round trip through a tidytable verb is what drops the over-allocation
  # the package's own tidytable shims: the path that drops the over-allocation
  x <- eeguana:::select.(eeguana:::mutate.(x, .tmp = 1), -".tmp")
  x
}

test_that("the data.table bug this guards against is still present", {
  # If this test starts failing, data.table has fixed setcolorder() and the
  # alloc.col() calls in R/dplyr_verbs.R can be removed.
  narrow <- wide_tt(30)
  expect_equal(data.table::truelength(narrow), 0)
  data.table::setcolorder(narrow, c(".id", ".sample"))
  expect_equal(narrow$.id, c(1L, 2L)) # under 64 columns it is fine

  wide <- wide_tt(70)
  expect_gte(ncol(wide), 64L)
  expect_equal(data.table::truelength(wide), 0)
  data.table::setcolorder(wide, c(".id", ".sample"))
  # names moved, data did not: .id now holds what C1 held
  expect_false(identical(wide$.id, c(1L, 2L)))
  expect_equal(names(wide)[1:2], c(".id", ".sample"))
})

test_that("restoring the over-allocation is what makes setcolorder safe", {
  wide <- wide_tt(70)
  wide <- data.table::copy(wide) # copy() returns an over-allocated table
  expect_gt(data.table::truelength(wide), ncol(wide))
  data.table::setcolorder(wide, c(".id", ".sample"))
  expect_equal(wide$.id, c(1L, 2L))
  expect_equal(names(wide)[1:2], c(".id", ".sample"))
})

test_that("across_ch with two functions does not corrupt .segments", {
  # The end-to-end regression: 34 channels * 2 functions + 4 = 72 columns.
  # Before the fix .id held "s70"/"s71" and .recording held a channel variance.
  grouped <- data_faces_10_trials %>%
    eeg_segment(.description %in% c("s70", "s71"), .lim = c(-1, 1)) %>%
    eeg_events_to_NA(.description == "Bad Min-Max") %>%
    eeg_group_by(description, .recording)

  two <- grouped %>%
    eeg_summarize(across_ch(list(~ mean(., na.rm = TRUE), ~ var(., na.rm = TRUE))))

  expect_gte(ncol(two$.signal), 64L)
  expect_true(is.integer(two$.segments$.id))
  expect_equal(two$.segments$.id, 1:2)
  expect_true(is.character(two$.segments$.recording))
  expect_equal(unique(two$.segments$.recording), "faces.vhdr")
  expect_setequal(two$.segments$description, c("s70", "s71"))

  # one function stays under the threshold and must keep working
  one <- grouped %>% eeg_summarize(across_ch(list(~ mean(., na.rm = TRUE))))
  expect_equal(one$.segments$.id, 1:2)
})

test_that("new_signal_tbl over-allocates, so wide montages are safe", {
  m <- as.data.frame(stats::setNames(
    lapply(1:70, function(i) c(i + 0.1, i + 0.2)), paste0("C", 1:70)
  ))
  s <- eeguana:::new_signal_tbl(
    .id = c(1L, 2L), .sample = sample_int(c(1L, 2L), 500),
    signal_matrix = m, channels_tbl = NULL
  )
  expect_gte(ncol(s), 64L)
  expect_gt(data.table::truelength(s), ncol(s))
  expect_equal(s$.id, c(1L, 2L))
  expect_equal(names(s)[1:2], c(".id", ".sample"))
})

test_that("validate_signal_tbl is still vulnerable to a wide tidytable input", {
  skip("Known limitation, not yet fixed. validate_signal_tbl() reorders by
reference, and for a truelength-0 table with 64+ columns setcolorder() either
moves the names without the data (corrupt) or, if the over-allocation is
restored first, silently stops reordering the caller's table. Fixing it means
changing validate_signal_tbl() to return the table and updating its callers.
Remove this skip when that is done.")

  x <- wide_tt(70)
  data.table::setattr(x, "class", c("signal_tbl", class(x)))
  eeguana:::validate_signal_tbl(x)
  expect_equal(x$.id, c(1L, 2L))
  expect_equal(names(x)[1:2], c(".id", ".sample"))
})

test_that("validate_psd_tbl is still vulnerable to a wide tidytable input", {
  skip("Same known limitation as validate_signal_tbl(); see that test.")

  x <- wide_tt(70, second_col = ".freq")
  data.table::setattr(x, "class", c("psd_tbl", class(x)))
  eeguana:::validate_psd_tbl(x)
  expect_equal(x$.id, c(1L, 2L))
  expect_equal(names(x)[1:2], c(".id", ".freq"))
})
