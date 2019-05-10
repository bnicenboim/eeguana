context("non working features of tidyverse functions filter")
library(eeguana)
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
  segments = dplyr::tibble(
    .id = c(1L, 2L, 3L),
    recording = "recording1",
    segment = c(1L, 2L, 3L),
    condition = c("a", "b", "a")
  )
)

# just some different X and Y
data_2 <- mutate(data_1,
  recording = "recording2",
  X = sin(X + 10),
  Y = cos(Y - 10),
  condition = c("b", "a", "b")
)

# bind it all together
data <- bind(data_1, data_2)

# extract sample_id -1 via eeg_lst
filter_eeg <- data %>% filter(.sample_id == -1)

# extract events containing sample_id -1 via tibble
filter_tbl <- as_tibble(data$events) %>%
  group_by(.id, .sample_0) %>%
  filter(-1 %in% seq(.sample_0, by = 1, length.out = .size))

# should be the same
expect_setequal(as.matrix(filter_eeg$events), as.matrix(filter_tbl))
