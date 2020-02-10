context("test conversion")
library(eeguana)


data1 <- eeguana:::data_sincos3id
data2 <- eeguana:::data_sincos3id %>% dplyr::mutate(.recording ="recording2")
data<- bind(data1,data2)
test_that("can transform to tibble", {
  df <- dplyr::as_tibble(data)
  expect_equal(nrow(df), nrow(data$.signal) * length(channel_names(data)))
  expect_equal(max(df$.time), max((data$.signal$.sample - 1)) / eeguana:::sampling_rate(data))
})
