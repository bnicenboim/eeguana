context("Read dat files")
library(eeguana)

test_that("can read vectorized BV files", {
  noise_binary <- read_vhdr("../../inst/extdata/bvexport_asa_binary.vhdr")
  noise_ascii <- read_vhdr("../../inst/extdata/bvexport_asa_ascii.vhdr")
  expect_known_value(noise_ascii,"noise_ascii.Rds")
  expect_known_value(noise_binary,"noise_binary.Rds")
})

test_that("can read multiplexed BV files", {
  expect_known_value(read_vhdr("../../inst/extdata/01_N400_CONG.vhdr"),"n400.Rds")
})

test_that("can read fieldtrip files", {
  expect_known_value(read_ft(file = "../../inst/extdata/data_h.mat", layout = "../../inst/extdata/easycapM23.mat"),
  				"fieldtrip.Rds")
})


