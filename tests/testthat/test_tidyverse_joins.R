context("test tidyverse functions joins")
library(eeguana)


data <- eeg_lst(
  signal = signal_tbl(
    signal_matrix = as.matrix(
      data.frame(X = sin(1:20), Y = cos(1:20))
    ),
    ids = rep(c(1L, 2L), each = 10),
    sample_ids = sample_int(rep(seq(-4L, 5L), times = 2), sampling_rate = 500),
    dplyr::tibble(
      channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = c(1, 1), .y = NA_real_, .z = NA_real_
    )
  ),
  events = dplyr::tribble(
    ~.id, ~type, ~description, ~.sample_0, ~.size, ~.channel,
    1L, "New Segment", NA_character_, -4L, 1L, NA,
    1L, "Bad", NA_character_, -2L, 3L, NA,
    1L, "Time 0", NA_character_, 1L, 1L, NA,
    1L, "Bad", NA_character_, 2L, 2L, "X",
    2L, "New Segment", NA_character_, -4L, 1L, NA,
    2L, "Time 0", NA_character_, 1L, 1L, NA,
    2L, "Bad", NA_character_, 2L, 1L, "Y"
    )%>% as_events_tbl(),
  segments = dplyr::tibble(.id = c(1L, 2L), recording = "recording1", segment = c(1L, 2L))
)

table0 <- tibble(.id = 1L, condition = "BLUE") 

data_l <- left_join(data,table0)
data_s <- semi_join(data,table0)
data_a <- anti_join(data,table0)

test_that("joins work", {
  expect_equal(data_l$segments, dplyr::left_join(data$segments,table0, by = ".id"))
  expect_equal(data_l$signal, data$signal)
  expect_equal(data_s$segments, dplyr::semi_join(data$segments,table0, by = ".id"))
  expect_equal(data_s, filter(data, .id == 1))
  expect_equal(data_a, filter(data, .id == 2))
})

