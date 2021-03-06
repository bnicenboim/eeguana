library(eeguana)

data <- eeguana:::data_sincos2id

table0 <- dplyr::tibble(.id = 1L, condition = "BLUE")

data_l <- dplyr::left_join(data, table0)
data_s <- dplyr::semi_join(data, table0)
data_a <- dplyr::anti_join(data, table0)

test_that("joins work", {
  expect_equal(data_l$.segments, dplyr::left_join(data$.segments, table0, by = ".id"))
  expect_equal(data_l$.signal, data$.signal)
  expect_equal(data_s$.segments, dplyr::semi_join(data$.segments, table0, by = ".id"))
  expect_equal(data_s, dplyr::filter(data, .id == 1))
  expect_equal(data_a, dplyr::filter(data, .id == 2))
})

message("\n***")
message(" test by reference")
message(" test errors")
message(" test data frames/tibbles/data.tables")
