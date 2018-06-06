context("Read dat files")
library(eegble)

x <- read_vhdr("01_N400_CONG.vhdr")

test_that("can read ascii dat files", {
expect_equal(length(x$data[[1]]$signals), 56)
expect_equal(nrow(x$data[[1]]$events), 340)
})

b_av <- read_vhdr("binary-avfaces.vhdr")

test_that("can read binary dat files", {
expect_equal(length(b_av$data[[1]]$signals), 1)
expect_equal(nrow(b_av$data[[1]]$signals[[1]]), 600)
expect_equal(nrow(b_av$data[[1]]$events), 2)
})

