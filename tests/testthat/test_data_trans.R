context("Binding and transforming")
library(eegble)

x <- read_vhdr("01_N400_CONG.vhdr")
x2 <- read_vhdr("01_N400_INC.vhdr")
b <- bind(x,x2)
bl <- bind(list(x,x2))

N400_c <- c("Cz", "CP1", "CP2", "P3", "Pz", "P4", "POz")
N4_b <- as_tibble(b, chans = N400_c)
P3_b <- as_tibble(b, chans = "P3")


test_that("can bind files", {
  expect_equal(length(b$data[[1]]$signals), 56)
  expect_equal(length(b$data[[2]]$signals), 59)
  expect_equal(length(bl$data[[1]]$signals), 56)
  expect_equal(length(bl$data[[2]]$signals), 59)
  expect_equal(nrow(b$data[[1]]$events), 340)
  expect_equal(names(bl$data), c("01_N400_CONG", "01_N400_INC"))
  expect_equal(names(bl), c("data", "chan_info", "gral_info"))
})

test_that("can transform to dataframe", {
  expect_equal(nrow(N4_b), 1207500)
  expect_equal(ncol(N4_b), 6)
  expect_equal(colnames(N4_b), c("id","segment","sample","time", "channel","amplitude"))
})

