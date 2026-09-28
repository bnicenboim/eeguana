library(eeguana)
options(eeguana.verbose = FALSE)

test_that("prints correctly", {
  expect_snapshot(data_faces_10_trials)
  expect_snapshot(data_faces_10_trials)
})

test_that("summary is correct", {
  expect_snapshot(summary(data_faces_10_trials))
})

## drop_incomplete_segments() was broken from the day it was added in 2019:
## filter_at() with all_vars() on a GROUPED eeg_lst dies with
##   could not find function "na.omit"
## so every call errored. It has no examples in its .Rd and had no test, which
## is exactly why nobody saw it: R CMD check runs examples, and there were
## none to run.
##
## NOTE: the underlying bug is still there. filter_at() on a grouped eeg_lst
## still fails; this function simply no longer goes that way.

segments_with_na_in <- function(id) {
  segmented <- eeg_segment(data_faces_10_trials, .description == "s70", .lim = c(-.1, .3))
  segmented$.signal <- data.table::copy(segmented$.signal)
  first_channel <- channel_names(segmented)[1]
  segmented$.signal[.id == id, (first_channel) := NA_real_]
  segmented
}

ids_in <- function(x) sort(unique(as.integer(x$.signal$.id)))

test_that("drop_incomplete_segments() drops exactly the segment holding NAs", {
  intact <- eeg_segment(data_faces_10_trials, .description == "s70", .lim = c(-.1, .3))
  kept <- drop_incomplete_segments(segments_with_na_in(2L))

  expect_s3_class(kept, "eeg_lst")
  expect_equal(ids_in(kept), setdiff(ids_in(intact), 2L))
  # the segments table has to follow the signal, or the two disagree
  expect_equal(nrow(kept$.segments), nrow(intact$.segments) - 1L)
})

test_that("drop_incomplete_segments() drops a segment whatever its position", {
  intact <- eeg_segment(data_faces_10_trials, .description == "s70", .lim = c(-.1, .3))
  last <- max(ids_in(intact))
  kept <- drop_incomplete_segments(segments_with_na_in(last))
  expect_equal(ids_in(kept), setdiff(ids_in(intact), last))
})

test_that("drop_incomplete_segments() keeps everything when nothing is missing", {
  intact <- eeg_segment(data_faces_10_trials, .description == "s70", .lim = c(-.1, .3))
  kept <- drop_incomplete_segments(intact)
  expect_equal(nrow(kept$.signal), nrow(intact$.signal))
  expect_equal(nrow(kept$.segments), nrow(intact$.segments))
})

test_that("drop_incomplete_segments() ignores NAs outside the channels", {
  # only channel columns count as data; a missing value elsewhere is not
  # a reason to throw a segment away
  intact <- eeg_segment(data_faces_10_trials, .description == "s70", .lim = c(-.1, .3))
  with_na <- intact
  with_na$.segments <- data.table::copy(intact$.segments)
  with_na$.segments[1, condition := NA]
  expect_equal(ids_in(drop_incomplete_segments(with_na)), ids_in(intact))
})

test_that("print() shows only some channels when asked, or when the option says so", {
  chs <- channel_names(data_faces_ERPs)
  out <- capture.output(print(data_faces_ERPs, .max_channels = 3))
  header <- out[grep("^ +\\.id +\\.sample", out)[1]]
  expect_match(header, paste(chs[1:3], collapse = " +"))
  expect_no_match(header, chs[4])
  expect_true(any(out == paste0("# ... and ", length(chs) - 3, " more channels: ", paste(chs[-(1:3)], collapse = ", "))))
  ## and says how to see more
  expect_true(any(grepl("options(eeguana.print_max_channels = ...)", out, fixed = TRUE)))
  ## the option sets the default
  withr::local_options(eeguana.print_max_channels = 1)
  out <- capture.output(print(data_faces_ERPs))
  expect_true(any(grepl(paste0("# ... and ", length(chs) - 1, " more channels"), out)))
  ## by default, and when there are fewer channels than the maximum, all are shown
  withr::local_options(eeguana.print_max_channels = Inf)
  expect_false(any(grepl("more channel|max_channels", capture.output(print(data_faces_ERPs)))))
  expect_false(any(grepl("more channel", capture.output(print(data_faces_ERPs, .max_channels = 100)))))
  ## the object is returned invisibly, and not changed
  capture.output(shown <- withVisible(print(data_faces_ERPs, .max_channels = 2)))
  expect_false(shown$visible)
  expect_identical(shown$value, data_faces_ERPs)
  expect_identical(names(data_faces_ERPs$.signal), c(".id", ".sample", chs))
  expect_error(print(data_faces_ERPs, .max_channels = -1), "0 or more")
})

