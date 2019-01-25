context("test tidyverse functions filter")
library(eeguana)


# create fake dataset
data_1 <- eeg_lst(
  signal = signal_tbl(
    signal_matrix = as.matrix(
      data.frame(X = sin(1:30), Y = cos(1:30))
    ),
    ids = rep(c(1L, 2L, 3L), each = 10),
    sample_ids = sample_int(rep(seq(-4L, 5L), times = 3), sampling_rate = 500),
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
    2L, "Bad", NA_character_, 2L, 1L, "Y",
    3L, "New Segment", NA_character_, -4L, 1L, NA,
    3L, "Time 0", NA_character_, 1L, 1L, NA,
    3L, "Bad", NA_character_, 2L, 1L, "Y"
  ),
  segments = dplyr::tibble(.id = c(1L, 2L, 3L), recording = "recording1", segment = c(1L, 2L, 3L), condition = c("a", "b", "a"))
)

# just some different X and Y
data_2 <- mutate(data_1, recording = "recording2", X = sin(X + 10), Y = cos(Y - 10), condition = c("b", "a", "b"))

# bind it all together
data <- bind(data_1, data_2)


# for checks later
reference_data <- data.table::copy(data)




###  test filter within one table ###

filter1_sig_eeg <- filter(data, .id ==1)

filter1_sig_tbl <- data %>%
  as_tibble() %>%
  dplyr::filter(.id == 1)

filter2_sig_eeg <- filter(data, .sample_id >= 2)

filter2_sig_tbl <- data %>%
  as_tibble() %>%
  # this should be .004 except that as_time works differently within as_tibble than directly on signal$.sample_id
  dplyr::filter(time >= .002) 

filter3_sig_eeg <- filter(data, .id == 1 & .sample_id == 2)

filter3_sig_tbl <- data %>%
  as_tibble() %>%
  dplyr::filter(.id == 1 & time == .002)

filter4_seg_eeg <- filter2_eeg <- filter(data, segment != 2)

filter4_seg_tbl <- data %>%
  as_tibble() %>%
  dplyr::filter(segment != 2) 

filter5_seg_eeg <- filter(data, condition == "a" & segment == 3)

filter5_seg_tbl <- data %>%
  as_tibble() %>%
  dplyr::filter(condition == "a" & segment == 3)

# should filter work on the events table?
filter6_evt_eeg <- filter(data, type == "Bad") # I feel like this would be useful info

filter7_evt_eeg <- filter(data, description == "NA") # not so useful


test_that("filtering by variables within one eeg_lst table works as expected", {
  expect_equal(as.double(filter1_sig_eeg$signal[["X"]]), filter1_sig_tbl$amplitude[filter1_sig_tbl$channel == "X"])
  expect_equal(as.double(filter2_sig_eeg$signal[["X"]]), filter2_sig_tbl$amplitude[filter2_sig_tbl$channel == "X"])
  expect_equal(as.double(filter3_sig_eeg$signal[["X"]]), filter3_sig_tbl$amplitude[filter3_sig_tbl$channel == "X"])
  expect_equal(as.double(filter4_seg_eeg$signal[["X"]]), filter4_seg_tbl$amplitude[filter4_seg_tbl$channel == "X"])
  expect_equal(as.double(filter5_seg_eeg$signal[["X"]]), filter5_seg_tbl$amplitude[filter5_seg_tbl$channel == "X"])

})


test_that("the classes of channels of signal_tbl remain after within eeg_lst table", {
  expect_equal(is_channel_dbl(filter1_sig_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter2_sig_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter3_sig_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter4_seg_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter5_seg_eeg$signal$X), TRUE)
  # expect_equal(is_channel_dbl(filter6_evt_eeg$signal$X), TRUE)
  # expect_equal(is_channel_dbl(filter7_evt_eeg$signal$X), TRUE)

})


# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})



### test filter across tables ###

filter1_eeg <- filter(data, segment == 2 & .id == 2)

filter1_tbl <- data %>%
  as_tibble() %>%
  filter(segment == 2 & .id == 2)

filter2_eeg <- filter(data, segment == 1 & .sample_id == 2)

filter2_tbl <- data %>%
  as_tibble() %>%
  dplyr::filter(segment == 1 & time == .002)

filter3_eeg <- filter(data, .sample_id == 2 & !(recording == "recording2"))

filter3_tbl <- data %>%
  as_tibble() %>%
  dplyr::filter(time == .002 & !(recording == "recording2"))

filter4_eeg <- filter(data, .id == 1 | condition == "a")

filter4_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.id == 1 | condition == "a")

filter5_eeg <- filter(data, .id == 2 | condition == "b")

filter5_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.id == 2 | condition == "b")

# doesn't work on the events table
filter6_eeg <- filter(data, .sample_id == 1 & .channel == "X")

filter7_eeg <- filter(data, recording == "recording1" & description == "Bad")


test_that("filtering by variables on different eeg_lst tables works as expected", {
  expect_equal(as.double(filter1_eeg$signal[["X"]]), filter1_tbl$amplitude[filter1_tbl$channel == "X"])
  expect_equal(as.double(filter2_eeg$signal[["X"]]), filter2_tbl$amplitude[filter2_tbl$channel == "X"])
  expect_equal(as.double(filter3_eeg$signal[["X"]]), filter3_tbl$amplitude[filter3_tbl$channel == "X"])
  expect_equal(as.double(filter4_eeg$signal[["X"]]), filter4_tbl$amplitude[filter4_tbl$channel == "X"])
  expect_equal(as.double(filter5_eeg$signal[["X"]]), filter5_tbl$amplitude[filter5_tbl$channel == "X"])
})


test_that("the classes of channels of signal_tbl remain after filtering across eeg_lst tables", {
  expect_equal(is_channel_dbl(filter1_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter2_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter3_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter4_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter5_eeg$signal$X), TRUE)
  # expect_equal(is_channel_dbl(filter6_eeg$signal$X), TRUE)
  # expect_equal(is_channel_dbl(filter7_eeg$signal$X), TRUE)
  
})


# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})



### test whether filter works on new variables ###

mutate_filter1_eeg <- mutate(data, time = as_time(.sample_id, unit = "milliseconds")) %>%
  filter(time == 2)

mutate_filter2_eeg <- mutate(data, time = as_time(.sample_id, unit = "seconds")) %>%
  filter(time == 0.002)

mutate_filter3_eeg <- data %>% 
  mutate(group = ifelse(.sample_id > 0, "late", "early")) %>%
  filter(group == "late")

mutate_filter3_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(group = ifelse(time > 0, "late", "early")) %>%
  dplyr::filter(channel == "X" & group == "late")

mutate_filter4_eeg <- data %>% 
  mutate(group = ifelse(Y > 0, "pos", "neg")) %>%
  filter(group == "neg")

mutate_filter4_tbl <- data %>%
  as_tibble() %>%
  dplyr::select(.id, time, channel, amplitude) %>%
  tidyr::spread(key = channel, value = amplitude) %>%
  dplyr::mutate(group = ifelse(Y > 0, "pos", "neg")) %>%
  dplyr::filter(group == "neg")

transmute_filter_eeg <- transmute(data, X = X + 1) %>%
  filter(recording == "recording1")

transmute_filter_tbl <- data %>%
  as_tibble %>%
  dplyr::filter(channel == "X" & recording == "recording1") %>%
  dplyr::transmute(X = amplitude + 1) 
  

test_that("filtering works on newly created variables", {
  expect_equal(as.double(mutate_filter1_eeg$signal$X), as.double(mutate_filter2_eeg$signal$X))
  # won't work until as_time thing is sorted out
  expect_equal(as.double(mutate_filter3_eeg$signal$X), mutate_filter3_tbl$amplitude)
  expect_equal(as.matrix(mutate_filter4_eeg$signal[, c("X", "Y")]), as.matrix(select(mutate_filter4_tbl, X, Y)))
  expect_equal(as.double(summarize_filter_eeg$signal$mean), summarize_filter_tbl$mean)
  expect_equal(as.double(transmute_filter_eeg$signal$X), transmute_filter_tbl$X)
})


test_that("the classes of channels of signal_tbl remain after filtering by new variables", {
  expect_equal(is_channel_dbl(mutate_filter1_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate_filter2_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate_filter3_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate_filter4_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(transmute_filter_eeg$signal$X), TRUE)
})



### test whether filter works after grouping  ###

mutate_all_filter_eeg <- data %>%
  group_by(.sample_id) %>%
  mutate_all(mean) %>%
  filter(condition == "b")

mutate_at_filter_eeg <- data %>%
  group_by(.sample_id) %>%
  mutate_at(channel_names(data), mean)  %>%
  filter(condition == "b")

mutate_a_tbl <- data %>%
  as_tibble() %>%
  dplyr::group_by(time, channel) %>%
  dplyr::mutate(mean = mean(amplitude)) %>% 
  dplyr::select(.id, time, channel, mean, condition) %>%
  tidyr::spread(key = channel, value = mean) %>%
  ungroup() %>%
  dplyr::filter(condition == "b")

summarize_filter_eeg <- group_by(data, .sample_id) %>% 
  summarize(mean = mean(Y)) %>%
  filter(mean > -0.35)

summarize_filter_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(channel == "Y") %>%
  dplyr::group_by(time) %>%
  dplyr::summarize(mean = mean(amplitude)) %>%
  dplyr::filter(mean > -0.35)

summarize_at_filter_eeg <- data %>% 
  group_by(recording, condition) %>%
  summarize_at_ch(channel_names(data), mean) %>%
  filter(X > 0 | Y > 0)

summarize_at_filter_tbl <- data %>%
  as_tibble() %>%
  dplyr::group_by(channel, recording, condition) %>%
  dplyr::summarise(mean = mean(amplitude)) %>%
  tidyr::spread(key = channel, value = mean) %>%
  dplyr::ungroup() %>%
  dplyr::filter(X > 0 | Y > 0)

summarize_all_filter_eeg <- group_by(data, .sample_id) %>% 
  summarize_all_ch("mean") %>%
  filter(.sample_id < 0)

summarize_all_filter_tbl <- data %>%
  as_tibble() %>%
  dplyr::group_by(time, channel) %>%
  dplyr::summarize(mean = mean(amplitude)) %>%
  tidyr::spread(key = channel, value = mean) %>%
  dplyr::ungroup() %>%
  dplyr::filter(time < 0)

summarize_all1_filter_eeg <- group_by(data, condition) %>% 
  summarize_all_ch("mean") %>%
  filter(condition == "a")

summarize_all1_filter_tbl <- data %>%
  as_tibble() %>%
  dplyr::group_by(channel, condition) %>%
  dplyr::summarize(mean = mean(amplitude)) %>%
  tidyr::spread(key = channel, value = mean) %>%
  dplyr::ungroup() %>%
  dplyr::filter(condition == "a")


test_that("filtering works after grouping and summarizing", {
  expect_equal(as.matrix(mutate_all_filter_eeg$signal[, c("X", "Y")]), as.matrix(mutate_at_filter_eeg$signal[, c("X", "Y")]))
  expect_equal(as.matrix(mutate_at_filter_eeg$signal[, c("X", "Y")]), as.matrix(select(mutate_a_tbl, X, Y)))
  expect_equal(as.double(summarize_filter_eeg$signal$mean), summarize_filter_tbl$mean)
  expect_equal(as.matrix(summarize_at_filter_eeg$signal[, c("X", "Y")]), as.matrix(select(summarize_at_filter_tbl, X, Y)))
  # won't work until as_time thing sorted
  expect_equal(as.matrix(summarize_all_filter_eeg$signal[, c("X", "Y")]), as.matrix(select(summarize_all_filter_tbl, X, Y)))
  expect_equal(as.matrix(summarize_all1_filter_eeg$signal[, c("X", "Y")]), as.matrix(select(summarize_all1_filter_tbl, X, Y)))
})


test_that("the classes of channels of signal_tbl remain after filtering by new variables", {
  expect_equal(is_channel_dbl(mutate_all_filter_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate_at_filter_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(summarize_filter_eeg$signal$mean), TRUE)
  expect_equal(is_channel_dbl(summarize_at_filter_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(summarize_all_filter_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(summarize_all1_filter_eeg$signal$X), TRUE)
})


# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})
