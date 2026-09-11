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

## dplyr's join generics take `copy` as their fourth argument:
##
##   left_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL)
##   semi_join(x, y, by = NULL, copy = FALSE, ...)
##   anti_join(x, y, by = NULL, copy = FALSE, ...)
##
## eeguana cannot copy `y` across data sources, but the argument still has to
## exist and sit in the same position, otherwise a positional call silently
## lands on the wrong parameter and a named one errors.

test_that("`copy` is accepted and warns that it is not supported", {
  for (join in list(eeg_left_join, eeg_semi_join, eeg_anti_join)) {
    expect_warning(join(data, table0, by = ".id", copy = TRUE), "copy")
  }
  for (join in list(dplyr::left_join, dplyr::semi_join, dplyr::anti_join)) {
    expect_warning(join(data, table0, by = ".id", copy = TRUE), "copy")
  }
})

test_that("`copy` at its default is silent", {
  for (join in list(eeg_left_join, eeg_semi_join, eeg_anti_join)) {
    expect_no_warning(join(data, table0, by = ".id"))
    expect_no_warning(join(data, table0, by = ".id", copy = FALSE))
  }
})

test_that("warning about `copy` does not change the result", {
  expect_warning(l <- eeg_left_join(data, table0, by = ".id", copy = TRUE))
  expect_warning(s <- eeg_semi_join(data, table0, by = ".id", copy = TRUE))
  expect_warning(a <- eeg_anti_join(data, table0, by = ".id", copy = TRUE))
  expect_equal_eeg_lst(l, data_l)
  expect_equal_eeg_lst(s, data_s)
  expect_equal_eeg_lst(a, data_a)
})

test_that("the fourth positional argument is `copy`, as in dplyr", {
  # a dplyr user writing left_join(x, y, by, TRUE) means copy = TRUE; before
  # `copy` existed that TRUE landed on `suffix`
  expect_warning(l <- eeg_left_join(data, table0, ".id", TRUE), "copy")
  expect_equal_eeg_lst(l, data_l)
  expect_warning(s <- eeg_semi_join(data, table0, ".id", TRUE), "copy")
  expect_equal_eeg_lst(s, data_s)
})

test_that("`suffix` still works, now in dplyr's position", {
  # colliding column, so suffix decides the resulting names
  table_dup <- dplyr::tibble(.id = 1L, condition = "BLUE")
  d <- eeg_mutate(data, condition = "RED")
  joined <- eeg_left_join(d, table_dup, by = ".id", suffix = c(".left", ".right"))
  expect_true(all(c("condition.left", "condition.right") %in%
    names(joined$.segments)))
})

## `keep` and `...` round out the same problem. dplyr's left_join is
##
##   left_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL)
##
## so `suffix` is the fifth positional argument once `copy` exists, `keep`
## must be named, and `...` sits between them.

test_that("`keep` accepts dplyr's default and warns on what is unsupported", {
  # dplyr's default is NULL, which for an equi join means the same as FALSE.
  # It currently errors with "invalid argument type".
  expect_no_warning(eeg_left_join(data, table0, by = ".id", keep = FALSE))
  expect_no_warning(l_null <- eeg_left_join(data, table0, by = ".id", keep = NULL))
  expect_equal_eeg_lst(l_null, data_l)

  # keeping both key columns is not implemented; it currently fails with
  # "non-numeric argument to mathematical function"
  expect_warning(l_keep <- eeg_left_join(data, table0, by = ".id", keep = TRUE), "keep")
  expect_equal_eeg_lst(l_keep, data_l)
})

test_that("extra arguments in ... warn instead of being dropped or rejected", {
  # left_join forwards ... and so accepts anything silently, including typos;
  # semi_join and anti_join have no ... at all and reject the argument
  expect_warning(eeg_left_join(data, table0, by = ".id", na_matches = "na"), "na_matches")
  expect_warning(eeg_semi_join(data, table0, by = ".id", na_matches = "na"), "na_matches")
  expect_warning(eeg_anti_join(data, table0, by = ".id", na_matches = "na"), "na_matches")
  expect_warning(eeg_left_join(data, table0, by = ".id", bogus = 1), "bogus")
})

test_that("`suffix` is the fifth positional argument, as in dplyr", {
  # left_join(x, y, by, copy, suffix): a positional call must reach suffix
  d <- eeg_mutate(data, condition = "RED")
  joined <- eeg_left_join(d, table0, ".id", FALSE, c(".left", ".right"))
  expect_true(all(c("condition.left", "condition.right") %in% names(joined$.segments)))
})
