context("test tidyverse dplyr::mutate")
library(eeguana)

# tests when factors are used should be done.

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
data_2 <- dplyr::mutate(data_1,
  .recording = "recording2",
  X = sin(X + 10),
  Y = cos(Y - 10),
  condition = c("b", "a", "b")
)

# bind it all together
data <- bind(data_1, data_2)

# for checks later
reference_data <- data.table::copy(data)




##############################################
### test dplyr dplyr::mutate on ungrouped eeg_lst ###
##############################################

mutate_eeg_lst <- dplyr::mutate(data, X = X + 10)

mutate_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.key == "X") %>%
  dplyr::mutate(X = .value + 10)


mutate2_eeg_lst <- dplyr::mutate(data, ZZ = X + 10)

mutate2_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.key == "X") %>%
  dplyr::mutate(ZZ = .value + 10)

mutate3_eeg_lst <- dplyr::mutate(data, mean = mean(X))

mutate3_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.key == "X") %>%
  dplyr::mutate(mean = mean(.value))

mutate4_eeg_lst <- dplyr::mutate(data, subject = .recording)

mutate4_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.key == "X") %>%
  dplyr::distinct(segment, condition, .keep_all = TRUE) %>%
  dplyr::mutate(subject = .recording)

transmute_eeg_lst <- dplyr::transmute(data, X = X + 1)

transmute_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.key == "X") %>%
  dplyr::transmute(X = .value + 1)


test_that("dplyr::mutate functions work correctly on ungrouped data", {
  expect_equal(as.double(mutate_eeg_lst$.signal[["X"]]), mutate_tbl$X)
  expect_equal(as.double(mutate2_eeg_lst$.signal[["ZZ"]]), mutate2_tbl$ZZ)
  expect_equal(as.double(mutate3_eeg_lst$.signal[["mean"]]), mutate3_tbl$mean)
  expect_equal(mutate4_eeg_lst$.segments[["subject"]], mutate4_tbl$subject)
  expect_equal(as.double(transmute_eeg_lst$.signal[["X"]]), transmute_tbl$X)
})

# TODO - I don't think these functions exist yet
# mutate_all_eeg_lst <- mutate_all_ch(data, mean)
# mutate_at_eeg_lst <- dplyr::mutate_at(data, channel_names(data), mean)


# This shouldn't work, the transformed channel is a new channel, and it shouldn't be part of the events
test_that("new channels shouldn't appear in the events table", {
  expect_true(nrow(dplyr::filter(mutate2_eeg_lst$.events, .channel == "ZZ")) == 0)
})


test_that("new channels appear in the channels table", {
  expect_true(nrow(dplyr::filter(channels_tbl(mutate2_eeg_lst), .channel == "ZZ")) > 0)
})


test_that("the classes of channels of signal_tbl remain in non-grouped eeg_lst", {
  expect_equal(is_channel_dbl(mutate_eeg_lst$.signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate2_eeg_lst$.signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate2_eeg_lst$.signal$ZZ), TRUE)
  expect_equal(is_channel_dbl(transmute_eeg_lst$.signal$X), TRUE)
  # expect_equal(is_channel_dbl(mutate_all_eeg_lst$.signal$X), TRUE)
  # expect_equal(is_channel_dbl(mutate_at_eeg_lst$.signal$X), TRUE)
})


# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})

message("Check that the rest of the object didn't change")




############################################
### test dplyr dplyr::mutate on grouped eeg_lst ###
############################################

group_by_eeg_lst <- dplyr::group_by(data, .sample)
group2_by_eeg_lst <- dplyr::group_by(data, .id)
group3_by_eeg_lst <- dplyr::group_by(data, .recording)
group4_by_eeg_lst <- dplyr::group_by(data, .sample, .recording)
group5_by_eeg_lst <- dplyr::group_by(data, .id, .recording)
group6_by_eeg_lst <- dplyr::group_by(data, .id, .sample, .recording)
group7_by_eeg_lst <- dplyr::group_by(data, .sample, condition)


mutate_g_signal_eeg <- dplyr::mutate(group_by_eeg_lst, X = X + 1)

mutate_g_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::group_by(.recording) %>%
  dplyr::filter(.key == "X") %>%
  dplyr::mutate(X = .value + 1)

mutate2_g_signal_eeg <- dplyr::mutate(group3_by_eeg_lst, ZZ = X + 1)

mutate2_g_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::group_by(.id) %>%
  dplyr::filter(.key == "X") %>%
  dplyr::mutate(ZZ = .value + 1)

mutate3_g_signal_eeg <- dplyr::mutate(group3_by_eeg_lst, Y = Y + 1)

mutate3_g_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::group_by(.recording) %>%
  dplyr::filter(.key == "Y") %>%
  dplyr::mutate(Y = .value + 1)

mutate4_g_signal_eeg <- dplyr::mutate(group4_by_eeg_lst, X = X + 1)

mutate4_g_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::group_by(.time, .recording) %>%
  dplyr::filter(.key == "X") %>%
  dplyr::mutate(X = .value + 1)

mutate5_g_signal_eeg <- dplyr::mutate(group5_by_eeg_lst, ZZ = X + 1)

mutate5_g_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::group_by(.id, .recording) %>%
  dplyr::filter(.key == "X") %>%
  dplyr::mutate(ZZ = .value + 1)

mutate6_g_signal_eeg <- dplyr::mutate(group6_by_eeg_lst, Y = Y + 1)

mutate6_g_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::group_by(.id, .time, .recording) %>%
  dplyr::filter(.key == "Y") %>%
  dplyr::mutate(Y = .value + 1)

mutate7_g_signal_eeg <- dplyr::mutate(group7_by_eeg_lst, mean = mean(Y))

mutate7_g_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.key == "Y") %>%
  dplyr::group_by(condition, .time) %>% # have to reverse order
  dplyr::mutate(mean = mean(.value))

transmute_g_signal_eeg <- dplyr::transmute(group_by_eeg_lst, X = X + 1)

transmute_g_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::group_by(.time) %>%
  dplyr::filter(.key == "X") %>%
  dplyr::transmute(X = .value + 1)

# mean of everything except .sample
mutate_all_g_signal_eeg <- dplyr::mutate_all(group_by_eeg_lst, mean)

# mean of channels
mutate_at_g_signal_eeg <- dplyr::mutate_at(group_by_eeg_lst, channel_names(data), mean)

mutate_a_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::group_by(.time, .key) %>%
  dplyr::mutate(mean = mean(.value)) %>%
  dplyr::select(.id, .time, .key, mean) %>%
  tidyr::spread(key = .key, value = mean) %>%
  dplyr::ungroup()


test_that("dplyr::mutate works correctly on data grouped by .sample", {
  expect_equal(
    as.double(mutate_g_signal_eeg$.signal[["X"]]),
    mutate_g_tbl$X
  )
  expect_equal(
    as.double(mutate2_g_signal_eeg$.signal[["ZZ"]]),
    mutate2_g_tbl$ZZ
  )
  expect_equal(
    as.double(mutate3_g_signal_eeg$.signal[["Y"]]),
    mutate3_g_tbl$Y
  )
  expect_equal(
    as.double(mutate4_g_signal_eeg$.signal[["X"]]),
    mutate4_g_tbl$X
  )
  expect_equal(
    as.double(mutate5_g_signal_eeg$.signal[["ZZ"]]),
    mutate5_g_tbl$ZZ
  )
  expect_equal(
    as.double(mutate6_g_signal_eeg$.signal[["Y"]]),
    mutate6_g_tbl$Y
  )
  expect_equal(
    as.double(mutate7_g_signal_eeg$.signal[["mean"]]),
    mutate7_g_tbl$mean
  )
  expect_equal(
    as.double(transmute_g_signal_eeg$.signal[["X"]]),
    transmute_g_tbl$X
  )
  expect_equal(
    as.matrix(mutate_all_g_signal_eeg$.signal[, c("X", "Y")]),
    as.matrix(mutate_at_g_signal_eeg$.signal[, c("X", "Y")])
  )
  expect_equal(
    as.matrix(mutate_all_g_signal_eeg$.signal[, c("X", "Y")]),
    as.matrix(dplyr::select(mutate_a_tbl, X, Y))
  )
})


test_that("new channels created by dplyr::mutate shouldn't appear in the events table", {
  expect_true(nrow(dplyr::filter(mutate2_g_signal_eeg$.events, .channel == "ZZ")) == 0)
  expect_true(nrow(dplyr::filter(mutate5_g_signal_eeg$.events, .channel == "ZZ")) == 0)
})


test_that("new channels appear in the channels table", {
  expect_true(nrow(dplyr::filter(channels_tbl(mutate2_g_signal_eeg), .channel == "ZZ")) > 0)
  expect_true(nrow(dplyr::filter(channels_tbl(mutate5_g_signal_eeg), .channel == "ZZ")) > 0)
})


test_that("the classes of channels of signal_tbl remain in grouped eeg_lst", {
  expect_equal(is_channel_dbl(group_by_eeg_lst$.signal$X), TRUE)
  expect_equal(is_channel_dbl(group2_by_eeg_lst$.signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate_g_signal_eeg$.signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate2_g_signal_eeg$.signal$ZZ), TRUE)
  expect_equal(is_channel_dbl(mutate3_g_signal_eeg$.signal$Y), TRUE)
  expect_equal(is_channel_dbl(transmute_g_signal_eeg$.signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate_all_g_signal_eeg$.signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate_at_g_signal_eeg$.signal$X), TRUE)
})


# check against original data
test_that("data didn't change after grouping and dplyr::mutate functions", {
  expect_equal(reference_data, data)
})




### test as_time conversion  ###
eeg_time <- suppressWarnings(dplyr::mutate(data, .time = as_time(.sample, unit = "seconds")) %>%
  dplyr::summarize(mean = mean(.time)))

tbl_time <- data %>%
  dplyr::as_tibble() %>%
  dplyr::summarize(mean = mean(.time))

test_that("as_time works as expected", {
  expect_equal(as.double(eeg_time$.signal[["mean"]]), tbl_time$mean)
  expect_warning(dplyr::mutate(data, .time = as_time(.sample, unit = "seconds")) %>%
    dplyr::summarize(mean = mean(.time)))
})




###########################
### test serial dplyr::mutates ###
###########################

# Bruno's note: Maybe it's fine that the following fails:
# dplyr::mutate(data, .time = as_time(.sample, unit = "milliseconds")) %>%
#   dplyr::group_by(.time) %>%
#   dplyr::summarize(mean(X))

# create new variable with dplyr::mutate
eeg_mutate_1 <- data %>%
  dplyr::mutate(bin = dplyr::ntile(.sample, 5))

tbl_mutate_1 <- data %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(bin = dplyr::ntile(.time, 5))

# use new variable in second variable doesn't work in eeg_lst (#35)
## eeg_mutate_2 <- data %>% dplyr::mutate(.time = as_time(.sample, unit = "ms"), bin = dplyr::ntile(time, 5))
# work around:
eeg_mutate_2 <- data %>%
  dplyr::mutate(.time = as_time(.sample, unit = "ms")) %>%
  dplyr::mutate(bin = dplyr::ntile(.time, 5))

tbl_mutate_2 <- data %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(test = .time + 1, bin = dplyr::ntile(test, 5))

# can't dplyr::summarize by a dplyr::mutated variable within eeg_lst (#43)
eeg_mutate_3 <- data %>%
  dplyr::mutate(bin = dplyr::ntile(.sample, 5)) %>%
  dplyr::group_by(bin) %>%
  dplyr::summarize(mean = mean(X))

tbl_mutate_3 <- data %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(bin = dplyr::ntile(.time, 5)) %>%
  dplyr::group_by(bin) %>%
  dplyr::summarize(mean = mean(.value[.key == "X"]))


test_that("dplyr::mutate works the same on eeg_lst as on tibble", {
  expect_equal(eeg_mutate_1$.signal[["bin"]], tbl_mutate_1$bin[tbl_mutate_1$.key == "X"])
  expect_equal(eeg_mutate_2$.signal[["bin"]], tbl_mutate_2$bin[tbl_mutate_1$.key == "X"])
  expect_equal(eeg_mutate_3$.signal[["bin"]], tbl_mutate_3$bin)
})
