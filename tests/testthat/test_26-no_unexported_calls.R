library(eeguana)

## eeguana used to reach into other packages with `:::`, calling functions they
## never exported. Those can disappear in any release, and one did: tidytable
## dropped transmute.tidytable, and transmute() on events tables stopped
## working. R CMD check only notes such calls, so this test fails outright as
## soon as one comes back.

test_that("eeguana calls no unexported function of another package", {
  ns <- asNamespace("eeguana")
  functions <- Filter(function(f) is.function(get(f, envir = ns)), ls(ns, all.names = TRUE))
  reaches_in <- Filter(function(f) {
    code <- gsub("eeguana:::", "", deparse(get(f, envir = ns)), fixed = TRUE)
    any(grepl("[A-Za-z][A-Za-z0-9.]*:::", code))
  }, functions)
  expect_equal(reaches_in, character(0))
})
