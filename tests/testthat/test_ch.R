context("test ch functions")
library(eegble)


data <- eegble(
  signal = signal(signal_matrix = as.matrix(
                              data.frame(X = sin(1:20), Y = cos(1:20))),
  ids = rep(c(1L, 2L), each = 10), 
  sample_ids = sample_id(rep(seq(-4L, 5L), times = 2), sampling_rate = 500 ),
  dplyr::tibble(labels = c("X", "Y"), reference = NA, theta = NA, phi = NA, 
    radius = NA, x = NA_real_, y = NA_real_, z = NA_real_) ),
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

mutate(data, chs_mean(X,Y))
