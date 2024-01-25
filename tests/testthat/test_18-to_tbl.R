library(eeguana)


data1 <- eeguana:::data_sincos3id
data2 <- eeguana:::data_sincos3id %>% dplyr::mutate(.recording = "recording2")
data <- bind(data1, data2)
test_that("can transform to tibble", {
  df <- dplyr::as_tibble(data)
  expect_equal(nrow(df), nrow(data$.signal) * length(channel_names(data)))
  expect_equal(max(df$.time), max((data$.signal$.sample - 1)) / eeguana:::sampling_rate.eeg_lst(data))
  df2 <- dplyr::as_tibble(data, .unit = "s")
  df3 <- dplyr::as_tibble(data, .unit = "ms")
  df2$.time <- df2$.time * 1000
  expect_equal(df2,df3)
  df2 <- data.table::as.data.table(data, .unit = "s")
  df3 <- data.table::as.data.table(data, .unit = "ms")
  df2$.time <- df2$.time * 1000
  expect_equal(df2,df3)
  
  expect_equal_plain_df(as.data.frame(df), as.data.frame(data))
  expect_equal_plain_df(as.data.frame(data.table::as.data.table(data)), as.data.frame(data))
  expect_equal_plain_df(as.data.frame(tidytable::as_tidytable(data)), 
                        as.data.frame(data))
  expect_equal_plain_df(as.data.frame(df), as.data.frame(data, .unit="s"))
  expect_equal_plain_df(as.data.frame(data.table::as.data.table(data, .unit="s")), as.data.frame(data))
  expect_equal_plain_df(as.data.frame(tidytable::as_tidytable(data, .unit="s")), as.data.frame(data))
})
