context("test signal_tbl functions")
library(eeguana)


data <- eeg_lst(
  signal = signal_tbl(
    signal_matrix = as.matrix(
      data.frame(X = sin(1:20), Y = cos(1:20))
    ),
    ids = rep(c(1L, 2L), each = 10),
    sample_ids = sample_int(rep(seq(-4L, 5L), times = 2), sampling_rate = 500),
    dplyr::tibble(
      .name = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_
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
  ),
  segments = dplyr::tibble(.id = c(1L, 2L), recording = "recording1", segment = c(1L, 2L))
)



group_by_signal_tbl <- dplyr::group_by(data$signal, .sample_id)
mutate_signal_tbl <- dplyr::mutate(data$signal, X = X + 1)
mutate2_signal_tbl <- dplyr::mutate(data$signal, ZZ = X + 1)
transmute_signal_tbl <- dplyr::transmute(data$signal, X = X + 1)
mutate_all_signal_tbl <- dplyr::mutate_all(data$signal, mean)
mutate_at_signal_tbl <- dplyr::mutate_at(data$signal, channel_names(data), mean)
summarize_signal_tbl <- dplyr::summarize(data$signal)
summarize_at_signal_tbl <- dplyr::summarize_at(data$signal, channel_names(data), mean)

mutate_g_signal_tbl <- dplyr::mutate(group_by_signal_tbl, X = X + 1)
mutate2_g_signal_tbl <- dplyr::mutate(group_by_signal_tbl, ZZ = X + 1)
transmute_g_signal_tbl <- dplyr::transmute(group_by_signal_tbl, X = X + 1)
mutate_all_g_signal_tbl <- dplyr::mutate_all(group_by_signal_tbl, mean)
mutate_at_g_signal_tbl <- dplyr::mutate_at(group_by_signal_tbl, channel_names(data), mean)
summarize_g_signal_tbl <- dplyr::summarize(group_by_signal_tbl)
summarize_at_g_signal_tbl <- dplyr::summarize_at(group_by_signal_tbl, channel_names(data), mean)

group_by_declassed <- dplyr::group_by(eeguana:::declass(data$signal)$tbl, .sample_id)
mutate_declassed <- dplyr::mutate(eeguana:::declass(data$signal)$tbl, X = X + 1)
mutate2_declassed <- dplyr::mutate(eeguana:::declass(data$signal)$tbl, ZZ = X + 1)
transmute_declassed <- dplyr::transmute(eeguana:::declass(data$signal)$tbl, X = X + 1)
mutate_all_declassed <- dplyr::mutate_all(eeguana:::declass(data$signal)$tbl, mean)
mutate_at_declassed <- dplyr::mutate_at(eeguana:::declass(data$signal)$tbl, channel_names(data), mean)
summarize_declassed <- dplyr::summarize(eeguana:::declass(data$signal)$tbl)
summarize_at_declassed <- dplyr::summarize_at(eeguana:::declass(data$signal)$tbl, channel_names(data), mean)

mutate_g_declassed <- dplyr::mutate(group_by_declassed, X = X + 1)
mutate2_g_declassed <- dplyr::mutate(group_by_declassed, ZZ = X + 1)
transmute_g_declassed <- dplyr::transmute(group_by_declassed, X = X + 1)
mutate_all_g_declassed <- dplyr::mutate_all(group_by_declassed, mean)
mutate_at_g_declassed <- dplyr::mutate_at(group_by_declassed, channel_names(data), mean)
summarize_g_declassed <- dplyr::summarize(group_by_declassed)
summarize_at_g_declassed <- dplyr::summarize_at(group_by_declassed, channel_names(data), mean)



test_that("dplyr func work on signal_tbl", {
  expect_equal(eeguana:::declass(group_by_signal_tbl)$tbl, group_by_declassed)
  expect_equal(eeguana:::declass(mutate_signal_tbl)$tbl, mutate_declassed)
  expect_equal(eeguana:::declass(mutate2_signal_tbl)$tbl, mutate2_declassed)
  expect_equal(eeguana:::declass(transmute_signal_tbl)$tbl, transmute_declassed)
  expect_equal(eeguana:::declass(mutate_all_signal_tbl)$tbl, mutate_all_declassed)
  expect_equal(eeguana:::declass(mutate_at_signal_tbl)$tbl, mutate_at_declassed)
  expect_equal(eeguana:::declass(summarize_signal_tbl)$tbl, summarize_declassed)
  expect_equal(eeguana:::declass(summarize_at_signal_tbl)$tbl, summarize_at_declassed)
})

test_that("dplyr func work on groupped signal_tbl", {
  expect_equal(eeguana:::declass(mutate_g_signal_tbl)$tbl, mutate_g_declassed)
  expect_equal(eeguana:::declass(mutate2_g_signal_tbl)$tbl, mutate2_g_declassed)
  expect_equal(eeguana:::declass(transmute_g_signal_tbl)$tbl, transmute_g_declassed)
  expect_equal(eeguana:::declass(mutate_all_g_signal_tbl)$tbl, mutate_all_g_declassed)
  expect_equal(eeguana:::declass(mutate_at_g_signal_tbl)$tbl, mutate_at_g_declassed)
  expect_equal(eeguana:::declass(summarize_g_signal_tbl)$tbl, summarize_g_declassed)
  expect_equal(eeguana:::declass(summarize_at_g_signal_tbl)$tbl, summarize_at_g_declassed)
})



test_that("class signal_tbl is preserved", {
  expect_equal(class(group_by_signal_tbl)[1], "signal_tbl")
  expect_equal(class(mutate_signal_tbl)[1], "signal_tbl")
  expect_equal(class(mutate2_signal_tbl)[1], "signal_tbl")
  expect_equal(class(transmute_signal_tbl)[1], "signal_tbl")
  expect_equal(class(mutate_all_signal_tbl)[1], "signal_tbl")
  expect_equal(class(mutate_at_signal_tbl)[1], "signal_tbl")
  expect_equal(class(summarize_signal_tbl)[1], "signal_tbl")
  expect_equal(class(summarize_at_signal_tbl)[1], "signal_tbl")
})

test_that("class signal_tbl is preserved on grouped signals", {
  expect_equal(class(mutate_g_signal_tbl)[1], "signal_tbl")
  expect_equal(class(mutate2_g_signal_tbl)[1], "signal_tbl")
  expect_equal(class(transmute_g_signal_tbl)[1], "signal_tbl")
  expect_equal(class(mutate_all_g_signal_tbl)[1], "signal_tbl")
  expect_equal(class(mutate_at_g_signal_tbl)[1], "signal_tbl")
  expect_equal(class(summarize_g_signal_tbl)[1], "signal_tbl")
  expect_equal(class(summarize_at_g_signal_tbl)[1], "signal_tbl")
})

test_that("test that attributes of columns of signal_tbl are preserved", {
  expect_equal(length(attributes(group_by_signal_tbl$X)), 9)
  expect_equal(length(attributes(mutate_signal_tbl$X)), 9)
  expect_equal(length(attributes(mutate2_signal_tbl$X)), 9)
  expect_equal(length(attributes(transmute_signal_tbl$X)), 9)
  expect_equal(length(attributes(mutate_all_signal_tbl$X)), 9)
  expect_equal(length(attributes(mutate_at_signal_tbl$X)), 9)
  expect_equal(length(attributes(summarize_at_signal_tbl$X)), 9)
  expect_equal(length(attributes(mutate_g_signal_tbl$X)), 9)
  expect_equal(length(attributes(mutate2_g_signal_tbl$X)), 9)
  expect_equal(length(attributes(transmute_g_signal_tbl$X)), 9)
  expect_equal(length(attributes(mutate_all_g_signal_tbl$X)), 9)
  expect_equal(length(attributes(mutate_at_g_signal_tbl$X)), 9)
  expect_equal(length(attributes(summarize_at_g_signal_tbl$X)), 9)
})

test_that("New channel get the channel class", {
  expect_equal(class(mutate2_g_signal_tbl$X), "channel_dbl")
})
