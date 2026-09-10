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

# ---------------------------------------------------------------- 64 channels --

## A montage wide enough to cross the 64-column threshold, built so that any
## mislabelling is unmissable: channel k is the constant k, so the mean of
## channel k must come back as exactly k. If setcolorder() ever moves the
## names without the data again, every one of these assertions fails.
##
## The failure was only ever reproducible under devtools::load_all(), which is
## how the suite runs during development, so these tests are the tripwire for
## exactly that situation.

wide_eeg <- function(nch = 64L, nsamp = 2L, nseg = 4L, group_numeric = TRUE) {
  chn <- sprintf("E%02d", seq_len(nch))
  n <- nsamp * nseg
  sig <- data.table::data.table(
    .id = rep(seq_len(nseg), each = nsamp),
    .sample = rep(seq_len(nsamp), times = nseg)
  )
  for (j in seq_along(chn)) {
    data.table::set(sig, j = chn[j], value = channel_dbl(rep(as.numeric(j), n)))
  }
  seg <- data.table::data.table(
    .id = seq_len(nseg),
    .recording = rep(c("r1", "r2"), length.out = nseg),
    condition = if (group_numeric) {
      rep(c(1L, 2L), length.out = nseg)
    } else {
      rep(c("a", "b"), length.out = nseg)
    }
  )
  eeg_lst(signal_tbl = sig, segments_tbl = seg, .sampling_rate = 500)
}

# every channel must still report its own constant
expect_channels_intact <- function(x, nch) {
  chn <- sprintf("E%02d", seq_len(nch))
  got <- vapply(chn, function(c) {
    v <- x$.signal[[c]]
    if (is.null(v)) NA_real_ else as.numeric(v[1])
  }, numeric(1))
  testthat::expect_equal(unname(got), as.numeric(seq_len(nch)))
}

test_that("a 64-channel summarize keeps every channel on its own data", {
  d <- wide_eeg(64L)
  # 64 channels + .sample + .id is already over the threshold ungrouped
  ungrouped <- eeg_summarize(d, across_ch(mean, na.rm = TRUE))
  expect_gte(ncol(ungrouped$.signal), 64L)
  expect_channels_intact(ungrouped, 64L)

  grouped <- d %>%
    eeg_group_by(condition) %>%
    eeg_summarize(across_ch(mean, na.rm = TRUE))
  expect_channels_intact(grouped, 64L)
})

test_that("a 64-channel summarize keeps .segments intact", {
  # group by .recording too, so it is carried through and its contents can be
  # checked; grouping by condition alone sets .recording to NA by design.
  grouped <- wide_eeg(64L) %>%
    eeg_group_by(condition, .recording) %>%
    eeg_summarize(across_ch(mean, na.rm = TRUE))

  # .id used to come back holding a grouping label, and .recording a channel
  # variance, when the names moved but the data did not
  expect_true(is.integer(grouped$.segments$.id))
  expect_equal(sort(grouped$.segments$.id), seq_along(grouped$.segments$.id))
  expect_true(is.character(grouped$.segments$.recording))
  expect_setequal(unique(grouped$.segments$.recording), c("r1", "r2"))
  expect_true(is.numeric(grouped$.segments$condition))
  expect_setequal(unique(grouped$.segments$condition), c(1, 2))
  expect_channels_intact(grouped, 64L)
})

test_that("the grouping column type does not change the outcome", {
  # numeric grouping used to fail silently, character grouping used to raise
  # a confusing error from round(); both must simply work
  for (num in c(TRUE, FALSE)) {
    r <- wide_eeg(64L, group_numeric = num) %>%
      eeg_group_by(condition) %>%
      eeg_summarize(across_ch(mean, na.rm = TRUE))
    expect_channels_intact(r, 64L)
    expect_true(is.integer(r$.segments$.id))
  }
})

test_that("across_ch with several functions stays correct past the threshold", {
  # 64 channels * 2 functions doubles the width again
  r <- wide_eeg(64L) %>%
    eeg_group_by(condition) %>%
    eeg_summarize(across_ch(list(~ mean(.x, na.rm = TRUE), ~ min(.x, na.rm = TRUE))))

  expect_gte(ncol(r$.signal), 64L)
  expect_true(is.integer(r$.segments$.id))
  # both summaries of a constant channel are that constant
  for (k in c(1L, 2L, 32L, 63L, 64L)) {
    for (suffix in c("_1", "_2")) {
      col <- paste0(sprintf("E%02d", k), suffix)
      expect_equal(as.numeric(r$.signal[[col]][1]), as.numeric(k),
        info = paste("column", col)
      )
    }
  }
})

test_that("narrower montages, which were never at risk, still work", {
  for (nch in c(8L, 32L, 60L)) {
    r <- wide_eeg(nch) %>%
      eeg_group_by(condition) %>%
      eeg_summarize(across_ch(mean, na.rm = TRUE))
    expect_channels_intact(r, nch)
  }
})
