library(eeguana)

test_that("can check package", {
  expect_error(eeguana:::require_pkg("xzxzx"))
  expect_silent(eeguana:::require_pkg("data.table"))
})

test_that("operator", {
  expect_equal(eeguana:::`%||%`(3, 5), 3)
  expect_equal(eeguana:::`%||%`(NULL, 5), 5)
  expect_equal(eeguana:::`%||%`(integer(0), 5), 5)
})
