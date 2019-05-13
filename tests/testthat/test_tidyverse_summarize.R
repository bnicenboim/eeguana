context("test tidyverse dplyr::summarize")
library(eeguana)

# tests when factors are used should be done.

# create fake dataset


data_1 <- eeg_lst(
  signal_tbl =
    dplyr::tibble(
      X = sin(1:30), Y = cos(1:30),
      .id = rep(c(1L, 2L, 3L), each = 10),
      .sample = sample_int(rep(seq(-4L, 5L), times = 3), sampling_rate = 500)
    ),
  channels_tbl = dplyr::tibble(
    .channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
    radius = NA, .x = c(1, 1), .y = NA_real_, .z = NA_real_
  ),
  events_tbl = dplyr::tribble(
    ~.id, ~.type, ~.description, ~.initial, ~.final, ~.channel,
    1L, "New Segment", NA_character_, -4L, -4L, NA,
    1L, "Bad", NA_character_, -2L, 0L, NA,
    1L, "Time 0", NA_character_, 1L, 1L, NA,
    1L, "Bad", NA_character_, 2L, 3L, "X",
    2L, "New Segment", NA_character_, -4L, -4L, NA,
    2L, "Time 0", NA_character_, 1L, 1L, NA,
    2L, "Bad", NA_character_, 2L, 2L, "Y",
    3L, "New Segment", NA_character_, -4L, -4L, NA,
    3L, "Time 0", NA_character_, 1L, 1L, NA,
    3L, "Bad", NA_character_, 2L, 2L, "Y"
  ),
  segments_tbl = dplyr::tibble(
    .id = c(1L, 2L, 3L),
    .recording = "recording1",
    segment = c(1L, 2L, 3L),
    condition = c("a", "b", "a")
  )
)



# just some different X and Y
data_2 <- dplyr::mutate(data_1, .recording = "recording2", X = sin(X + 10), Y = cos(Y - 10), condition = c("b", "a", "b"))

# bind it all together
data <- bind(data_1, data_2)


# for checks later
reference_data <- data.table::copy(data)


#################################################
### test dplyr dplyr::summarize on ungrouped eeg_lst ###
#################################################

summarize_eeg <- dplyr::summarize(data, mean = mean(X))

summarize_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.key == "X") %>%
  dplyr::summarize(mean = mean(.value))

summarize_at_eeg <- dplyr::summarize_at(data, channel_names(data), mean)
summarize_all_eeg <- dplyr::summarize_at(data, channel_names(data), mean)
summarize_all2_eeg <- dplyr::summarize_at(data, channel_names(data), "mean")
summarize_all3_eeg <- dplyr::summarize_at(data, channel_names(data), list(~ mean(.)))

summarize2_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::group_by(.key) %>%
  dplyr::summarize(mean = mean(.value)) %>%
  tidyr::spread(key = .key, value = mean)

summarize_all4_eeg <- dplyr::summarize_at(data, channel_names(data), list(mean = ~ mean(.)))

summarize4_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::group_by(.key) %>%
  dplyr::summarize(mean = mean(.value)) %>%
  tidyr::spread(key = .key, value = mean) %>%
  dplyr::rename(X_mean = X, Y_mean = Y)


test_that("dplyr::summarize works correctly on ungrouped data", {
  expect_equal(
    as.double(summarize_eeg$.signal[["mean"]]),
    summarize_tbl$mean
  )
  expect_equal(
    as.matrix(summarize_at_eeg$.signal[, c("X", "Y")]),
    as.matrix(summarize2_tbl)
  )
  expect_equal(
    as.matrix(summarize_all_eeg$.signal[, c("X", "Y")]),
    as.matrix(summarize2_tbl)
  )
  expect_equal(
    as.matrix(summarize_all2_eeg$.signal[, c("X", "Y")]),
    as.matrix(summarize2_tbl)
  )
  expect_equal(
    as.matrix(summarize_all3_eeg$.signal[, c("X", "Y")]),
    as.matrix(summarize2_tbl)
  )
  expect_equal(
    as.matrix(summarize_all4_eeg$.signal[, c("X_mean", "Y_mean")]),
    as.matrix(summarize4_tbl)
  )
})


test_that("dplyr::summarizes don't have any individual events", {
  expect_true(nrow(summarize_eeg$.events) == 0)
  expect_true(nrow(summarize_at_eeg$.events) == 0)
  expect_true(nrow(summarize_all_eeg$.events) == 0)
})


test_that("the classes of channels of signal_tbl remain in non-grouped eeg_lst", {
  expect_equal(is_channel_dbl(summarize_eeg$.signal[["mean"]]), TRUE)
  expect_equal(is_channel_dbl(summarize_at_eeg$.signal$"X"), TRUE)
})


# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})



############################################################
### test dplyr dplyr::summarize on grouped eeg_lst signal table ###
############################################################

# create groupings
group_by_eeg_lst <- dplyr::group_by(data, .sample)
group2_by_eeg_lst <- dplyr::group_by(data, .id)
group3_by_eeg_lst <- dplyr::group_by(data, .recording)
group4_by_eeg_lst <- dplyr::group_by(data, .sample, .recording)
group5_by_eeg_lst <- dplyr::group_by(data, .id, .recording)
group6_by_eeg_lst <- dplyr::group_by(data, .id, .sample, .recording)
group7_by_eeg_lst <- dplyr::group_by(data, .sample, condition)


summarize_g_signal_eeg <- dplyr::summarize(group_by_eeg_lst, mean = mean(X))

summarize_g_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.key == "X") %>%
  dplyr::group_by(.time) %>%
  dplyr::summarise(mean = mean(.value))

summarize_at_g_signal_eeg <- dplyr::summarize_at(group_by_eeg_lst, channel_names(data), mean)

summarize_at_g_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::group_by(.time, .key) %>%
  dplyr::summarise(mean = mean(.value)) %>%
  tidyr::spread(key = .key, value = mean) %>%
  dplyr::ungroup()

summarize_g2_signal_eeg <- dplyr::summarize(group2_by_eeg_lst, mean = mean(X))

summarize_g2_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.key == "X") %>%
  dplyr::group_by(.id) %>%
  dplyr::summarise(mean = mean(.value))

summarize_g3_signal_eeg <- dplyr::summarize(group3_by_eeg_lst, mean = mean(X))

summarize_g3_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.key == "X") %>%
  dplyr::group_by(.recording) %>%
  dplyr::summarise(mean = mean(.value))

summarize_g4_signal_eeg <- dplyr::summarize(group4_by_eeg_lst, mean = mean(X))

summarize_g4_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.key == "X") %>%
  dplyr::group_by(.recording, .time) %>%
  dplyr::summarise(mean = mean(.value))

summarize_g5_signal_eeg <- dplyr::summarize(group5_by_eeg_lst, mean = mean(X))

summarize_g5_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.key == "X") %>%
  dplyr::group_by(.id, .recording) %>%
  dplyr::summarise(mean = mean(.value))

summarize_g6_signal_eeg <- dplyr::summarize(group6_by_eeg_lst, mean = mean(X))

summarize_g6_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.key == "X") %>%
  dplyr::group_by(.id, .time, .recording) %>%
  dplyr::summarise(mean = mean(.value))

summarize_g7_signal_eeg <- dplyr::summarize(group7_by_eeg_lst, mean = mean(X))

summarize_g7_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.key == "X") %>%
  dplyr::group_by(condition, .time) %>% # have to reverse order
  dplyr::summarise(mean = mean(.value))


test_that("dplyr::summarize works correctly on  data grouped by .sample", {
  expect_equal(as.double(summarize_g_signal_eeg$.signal[["mean"]]), summarize_g_tbl$mean)
  expect_equal(as.matrix(summarize_at_g_signal_eeg$.signal[, c("X", "Y")]), as.matrix(dplyr::select(summarize_at_g_tbl, X, Y)))
  expect_equal(as.double(summarize_g2_signal_eeg$.signal[["mean"]]), summarize_g2_tbl$mean)
  expect_equal(as.double(summarize_g3_signal_eeg$.signal[["mean"]]), summarize_g3_tbl$mean)
  expect_equal(as.double(summarize_g4_signal_eeg$.signal[["mean"]]), summarize_g4_tbl$mean)
  expect_equal(as.double(summarize_g5_signal_eeg$.signal[["mean"]]), summarize_g5_tbl$mean)
  expect_equal(as.double(summarize_g6_signal_eeg$.signal[["mean"]]), summarize_g6_tbl$mean)
  expect_equal(as.double(summarize_g7_signal_eeg$.signal[["mean"]]), summarize_g7_tbl$mean)
})


test_that("dplyr::summarizes don't have any individual events", {
  expect_true(nrow(summarize_g_signal_eeg$.events) == 0)
  expect_true(nrow(summarize_at_g_signal_eeg$.events) == 0)
})


test_that("the classes of channels of signal_tbl remain in non-grouped eeg_lst", {
  expect_equal(is_channel_dbl(summarize_g_signal_eeg$.signal$mean), TRUE)
  expect_equal(is_channel_dbl(summarize_at_g_signal_eeg$.signal$X), TRUE)
})


# check against original data
test_that("data didn't change after grouping and dplyr::summarize functions", {
  expect_equal(reference_data, data)
})



########################################################################################
### test eeguana dplyr::summarize directly on eeg_lst, grouping by segments table variables ###
########################################################################################

data_s1 <- data %>%
  dplyr::group_by(condition, .sample, .recording) %>%
  dplyr::summarize(X = mean(X), Y = mean(Y))

data_s2 <- data_s1 %>%
  dplyr::group_by(condition, .sample) %>%
  dplyr::summarize(X = mean(X), Y = mean(Y))

data_s3 <- data_s2 %>%
  dplyr::group_by(condition) %>%
  dplyr::summarize(X = mean(X), Y = mean(Y))

data_s4 <- data_s3 %>%
  dplyr::group_by() %>%
  dplyr::summarize(X = mean(X), Y = mean(Y))





######################################
### test with pure dplyr functions ###
######################################
extended_signal <- dplyr::left_join(dplyr::as_tibble(data$.signal), data$.segments, by = ".id")

e_data_s1 <- data.table::data.table(extended_signal)[, .(X = mean(X), Y = mean(Y)),
  keyby = c("condition", ".sample", ".recording")
  ]
data.table::setkeyv(e_data_s1,cols =  c("condition",".recording"))
s_data_s1 <- e_data_s1[, unique(.SD), .SDcols = c("condition", ".recording")]

e_data_s2 <- data.table::data.table(e_data_s1)[, .(X = mean(X), Y = mean(Y)),
  keyby = c("condition", ".sample")
]
s_data_s2 <- e_data_s2[, unique(.SD), .SDcols = c("condition")]

e_data_s3 <- data.table::data.table(e_data_s2)[, .(X = mean(X), Y = mean(Y)),
  keyby = c("condition")
]
s_data_s3 <- e_data_s1[, unique(.SD), .SDcols = c("condition")]

e_data_s4 <- data.table::data.table(e_data_s3)[, .(X = mean(X), Y = mean(Y)),
  by = character(0)
]


test_that("summarizing by groups works as expected for the .key values", {
  expect_equal(data_s1$.signal$X, e_data_s1$X)
  expect_equal(data_s2$.signal$X, e_data_s2$X)
  expect_equal(data_s3$.signal$X, e_data_s3$X)
  expect_equal(data_s4$.signal$X, e_data_s4$X)
})


test_that("summarizing by groups works as expected for the segments", {
  expect_equal(data_s1$.segments, dplyr::as_tibble(s_data_s1) %>%
    dplyr::mutate(.id = 1:dplyr::n()))
  expect_equal(data_s2$.segments, dplyr::as_tibble(s_data_s2) %>%
                                  dplyr::mutate(.id = 1:dplyr::n(), .recording =NA))
  expect_equal(data_s3$.segments, dplyr::as_tibble(s_data_s3) %>%
                                  dplyr::mutate(.id = 1:dplyr::n(), .recording =NA))
  expect_equal(data_s4$.segments, dplyr::tibble(.id = 1L, .recording =NA))
})



###########################################
### test summarize_all_ch vs. dplyr::summarize ###
###########################################

data_all_s1 <- data %>%
  dplyr::group_by(.sample, condition, .recording) %>%
  dplyr::summarize_at(channel_names(.), mean)

data_all_s2 <- data_all_s1 %>%
  dplyr::group_by(.sample, condition) %>%
  dplyr::summarize_at(channel_names(.), mean)

data_all_s3 <- data_all_s2 %>%
  dplyr::group_by(.sample) %>%
  dplyr::summarize_at(channel_names(.), mean)

data_all_s4 <- data_all_s3 %>%
  dplyr::group_by() %>%
  dplyr::summarize_at(channel_names(.), mean)


test_that("dplyr::summarize all channels works as the regular dplyr::summarize", {
  expect_equal(data_all_s1, data_all_s1)
  expect_equal(data_all_s2, data_all_s2)
  expect_equal(data_all_s3, data_all_s3)
  expect_equal(data_all_s4, data_all_s4)
})


#######################################################################
### test dplyr::summarize on operations involving different eeg_lst tables ###
#######################################################################

eeg_diff_means_1 <- dplyr::group_by(data, .sample) %>%
  dplyr::summarize(mean = mean(X[condition == "a"] -
    X[condition == "b"]))

tbl_diff_means_1 <- data %>%
  dplyr::as_tibble() %>%
  dplyr::group_by(.time) %>%
  dplyr::filter(.key == "X") %>%
  dplyr::summarize(mean = mean(.value[condition == "a"] - .value[condition == "b"]))

eeg_diff_means_2 <- dplyr::group_by(data, .sample) %>%
  dplyr::summarize_at(channel_names(.), list(~ mean(.[condition == "a"] -
    .[condition == "b"])))

tbl_diff_means_2 <- data %>%
  dplyr::as_tibble() %>%
  dplyr::group_by(.time, .key) %>%
  dplyr::summarize(mean = mean(.value[condition == "a"] - .value[condition == "b"])) %>%
  tidyr::spread(key = .key, value = mean) %>%
  dplyr::ungroup()

eeg_diff_means_3 <- dplyr::group_by(data, .sample) %>%
  dplyr::summarize(mean = mean(X[condition == "a" & .recording == "recording1"] -
    X[condition == "b" & .recording == "recording2"]))

tbl_diff_means_3 <- data %>%
  dplyr::as_tibble() %>%
  dplyr::group_by(.time) %>%
  dplyr::filter(.key == "X") %>%
  dplyr::summarize(mean = mean(.value[condition == "a" & .recording == "recording1"] - .value[condition == "b" & .recording == "recording2"]))

eeg_diff_means_4 <- dplyr::group_by(data, .sample) %>%
  dplyr::summarize_at(channel_names(.), list(~ mean(.[condition == "a" & .recording == "recording1"] -
    .[condition == "b" & .recording == "recording2"])))

tbl_diff_means_4 <- data %>%
  dplyr::as_tibble() %>%
  dplyr::group_by(.time, .key) %>%
  dplyr::summarize(mean = mean(.value[condition == "a" & .recording == "recording1"] - .value[condition == "b" & .recording == "recording2"])) %>%
  tidyr::spread(key = .key, value = mean) %>%
  dplyr::ungroup()

eeg_means_5 <- dplyr::group_by(data, .sample) %>% dplyr::summarize_at(channel_names(.), "mean")

tbl_means_5 <- data %>%
  dplyr::as_tibble() %>%
  dplyr::group_by(.time, .key) %>%
  dplyr::summarize(mean = mean(.value)) %>%
  tidyr::spread(key = .key, value = mean) %>%
  dplyr::ungroup()


test_that("summarising functions work the same on eeg_lst as on tibble", {
  expect_equal(
    as.double(eeg_diff_means_1$.signal[["mean"]]),
    tbl_diff_means_1$mean
  )
  expect_equal(
    as.matrix(eeg_diff_means_2$.signal[, c("X", "Y")]),
    as.matrix(dplyr::select(tbl_diff_means_2, X, Y))
  )
  expect_equal(
    as.double(eeg_diff_means_3$.signal[["mean"]]),
    tbl_diff_means_3$mean
  )
  expect_equal(
    as.matrix(eeg_diff_means_4$.signal[, c("X", "Y")]),
    as.matrix(dplyr::select(tbl_diff_means_4, X, Y))
  )
  expect_equal(
    as.matrix(eeg_means_5$.signal[, c("X", "Y")]),
    as.matrix(dplyr::select(tbl_means_5, X, Y))
  )
})
