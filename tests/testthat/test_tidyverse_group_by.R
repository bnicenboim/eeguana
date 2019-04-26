context("test tidyverse group_by")
library(eeguana)

# tests when factors are used should be done.

# create fake dataset
data_1 <- eeg_lst(
  signal_tbl =
 dplyr::tibble(X = sin(1:30), Y = cos(1:30),
    .id = rep(c(1L, 2L, 3L), each = 10),
.sample_id = sample_int(rep(seq(-4L, 5L), times = 3), sampling_rate = 500)),
   channels_tbl = dplyr::tibble(
      .channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = c(1, 1), .y = NA_real_, .z = NA_real_
  ),
   events_tbl = dplyr::tribble(
    ~.id, ~type, ~description, ~.initial, ~.final, ~.channel,
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
  segments_tbl = dplyr::tibble(.id = c(1L, 2L, 3L),
                           recording = "recording1",
                           segment = c(1L, 2L, 3L),
                           condition = c("a", "b", "a"))
)



# just some different X and Y
data_2 <- mutate(data_1, recording = "recording2", X = sin(X + 10), Y = cos(Y - 10), condition = c("b", "a", "b"))

# bind it all together
data <- bind(data_1, data_2)


# for checks later
reference_data <- data.table::copy(data)

data_g_segment <- data %>% group_by(segment)
data_g_recording <- data %>% group_by(recording)
data_g_recording_segment <- data %>% group_by(recording,segment)
data_g_segment2 <- data %>% group_by(recording) %>% group_by(segment)
data_g_recording_segment2 <- data %>% group_by(recording) %>% group_by(segment, add =TRUE)

expect_equal(data_g_segment,data_g_segment2)
expect_equal(data_g_recording_segment,data_g_recording_segment2)
expect_equal(group_vars(data),character(0))
