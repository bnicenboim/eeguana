context("test dplyr functions")
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
  ),
  segments = dplyr::tibble(.id = c(1L, 2L), recording = "recording1", segment = c(1L, 2L))
)


reference_data <- data.table::copy(data)

test_that("dplyr functions work correctly on ungrouped data", {
  mutate_eeg_lst <- mutate(data, X = X + 10)
  mutate2_eeg_lst <- mutate(data, ZZ = X + 10)
  mutate3_eeg_lst <- mutate(data, mean(X))
  mutate4_eeg_lst <- mutate(data, subject = recording)
  transmute_eeg_lst <- transmute(data, X = X + 1)
  #TODO
  # mutate_all_eeg_lst <- mutate_all_ch(data, mean)
  mutate_at_eeg_lst <- mutate_at(data, channel_names(data), mean)
  summarizeX_eeg_lst <- summarize(data, mean(X))
  summarize_at_eeg_lst <- summarize_at_ch(data, channel_names(data), mean)
  summarize_all_eeg_lst <- summarize_all_ch(data, mean)
  summarize_all2_eeg_lst <- summarize_all_ch(data, "mean")
  summarize_all3_eeg_lst <- summarize_all_ch(data, funs(mean(.)))
  summarize_all4_eeg_lst <- summarize_all_ch(data, funs(m = mean(.)))
  group_by_eeg_lst <- group_by(data, .sample_id)
  group2_by_eeg_lst <- group_by(data, .id)
  group3_by_eeg_lst <- group_by(data, recording)
  group4_by_eeg_lst <- group_by(data, .sample_id, recording)
  group4_by_eeg_lst <- group_by(data, .id, recording)
  group5_by_eeg_lst <- group_by(data, .id, .sample_id, recording)
}
)

test_that("data didn't change", {
expect_equal(reference_data, data)
 }) 

test_that("the classes of channels of signal_tbl remain in non-grouped eeg_lst", {
  expect_equal(is_channel_dbl(mutate_eeg_lst$signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate2_eeg_lst$signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate2_eeg_lst$signal$ZZ), TRUE)
  expect_equal(is_channel_dbl(transmute_eeg_lst$signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate_all_eeg_lst$signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate_at_eeg_lst$signal$X), TRUE)
  expect_equal(is_channel_dbl(summarizeX_eeg_lst$signal$`mean(X)`), TRUE)
  expect_equal(is_channel_dbl(summarize_at_eeg_lst$signal$X), TRUE)
})

test_that("dplyr functions work correctly on  data grouped by .sample_id", {
mutate_g_signal_tbl <- mutate(group_by_eeg_lst, X = X + 1)
mutate2_g_signal_tbl <- mutate(group_by_eeg_lst, ZZ = X + 1)
transmute_g_signal_tbl <- transmute(group_by_eeg_lst, X = X + 1)
mutate_all_g_signal_tbl <- mutate_all(group_by_eeg_lst, mean)
mutate_at_g_signal_tbl <- mutate_at(group_by_eeg_lst, channel_names(data), mean)
summarize_g_signal_tbl <- summarize(group_by_eeg_lst, mean(X))
summarize_at_g_signal_tbl <- summarize_at_ch(group_by_eeg_lst, channel_names(data), mean)
})

test_that("data didn't change after grouping and dplyr functions", {
expect_equal(reference_data, data)
 }) 


test_that("dplyr functions work correctly on  data grouped by .sample_id", {
  summarize_g2_signal_tbl <- summarize(group2_by_eeg_lst, mean(X))
})

test_that("the classes of channels of signal_tbl remain in non-grouped eeg_lst", {
expect_equal(is_channel_dbl(group_by_eeg_lst$signal$X), TRUE)
expect_equal(is_channel_dbl(group2_by_eeg_lst$signal$X), TRUE)

expect_equal(is_channel_dbl(mutate_g_signal_tbl$signal$X), TRUE)
expect_equal(is_channel_dbl(mutate2_g_signal_tbl$signal$X), TRUE)
expect_equal(is_channel_dbl(mutate2_g_signal_tbl$signal$ZZ), TRUE)
expect_equal(is_channel_dbl(transmute_g_signal_tbl$signal$X), TRUE)
expect_equal(is_channel_dbl(mutate_all_g_signal_tbl$signal$X), TRUE)
expect_equal(is_channel_dbl(mutate_at_g_signal_tbl$signal$X), TRUE)
expect_equal(is_channel_dbl(summarize_g_signal_tbl$signal$`mean(X)`), TRUE)
expect_equal(is_channel_dbl(summarize_at_g_signal_tbl$signal$X), TRUE)
expect_equal(is_channel_dbl(  summarize_g2_signal_tbl$signal$`mean(X)`), TRUE)
})



# add tests 
# for filtering according to segments, grouped and ungrouped