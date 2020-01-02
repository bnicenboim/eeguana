context("loads without errors")
library(eeguana)

test_that("loads without errors", {
expect_null(eeguana:::.onLoad())
})
