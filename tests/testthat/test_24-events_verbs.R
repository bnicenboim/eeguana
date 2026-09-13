library(eeguana)
options(eeguana.verbose = FALSE)

## The events_tbl verb methods reach into tidytable and call
## tidytable:::<verb>.tidytable directly. tidytable has no transmute.tidytable,
## so every transmute() of an events table died with
##   object 'transmute.tidytable' not found
## Nothing called it and nothing tested it. R CMD check had been saying so all
## along, but the message reads like lint rather than like a dead function:
##   Missing object imported by a ':::' call: 'tidytable:::transmute.tidytable'

events <- function() data_faces_10_trials$.events

## the columns an events table must keep to stay a valid events table
obligatory <- c(".id", ".type", ".description", ".initial", ".final", ".channel")
keep_obligatory <- function(ev) {
  dplyr::transmute(ev, .id, .type, .description, .initial, .final, .channel)
}

test_that("transmute() on an events table works at all", {
  out <- keep_obligatory(events())
  expect_s3_class(out, "events_tbl")
  expect_equal(colnames(out), obligatory)
  expect_equal(nrow(out), nrow(events()))
})

test_that("transmute() dropping an obligatory column is refused", {
  # .final is required, so a transmute without it cannot yield an events table.
  # The message is pinned: a bare expect_error() passed even when transmute was
  # broken outright, because that failed too, just for the wrong reason.
  expect_error(dplyr::transmute(events(), .id, .initial), "\\.final")
})

test_that("no verb leaves tidytable's classes on an events table", {
  # tidytable verbs return "tidytable" "tbl" "data.table" "data.frame".
  # Those must not leak out, or every comparison against a freshly read
  # events table fails on class alone.
  ev <- events()
  expect_equal(class(ev), c("events_tbl", "data.table", "data.frame"))
  expect_equal(class(dplyr::mutate(ev, .id = .id)), class(ev))
  expect_equal(class(keep_obligatory(ev)), class(ev))
  expect_equal(class(dplyr::filter(ev, .initial > 0)), class(ev))
})

test_that("transmute() keeps .initial and .final as samples", {
  # a plain integer here would fail validation downstream
  out <- keep_obligatory(events())
  expect_true(is_sample_int(out$.initial))
  expect_true(is_sample_int(out$.final))
  expect_equal(sampling_rate(out$.initial), sampling_rate(events()$.initial))
})
