context("test dplyr functions")
library(eeguana)


data <- eeg_lst(
  signal = signal(signal_matrix = as.matrix(
                              data.frame(X = sin(1:20), Y = cos(1:20))),
  ids = rep(c(1L, 2L), each = 10), 
  sample_ids = sample_id(rep(seq(-4L, 5L), times = 2), sampling_rate = 500 ),
  dplyr::tibble(labels = c("X", "Y"), reference = NA, theta = NA, phi = NA, 
    radius = NA, x = c(1,1), y = NA_real_, z = NA_real_) ),
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




group_by_eegbl <- dplyr::group_by(data, .sample_id)
mutate_eegbl <- dplyr::mutate(data, X = X + 1)
mutate2_eegbl <- dplyr::mutate(data, ZZ = X + 1)
transmute_eegbl <- dplyr::transmute(data, X = X + 1)
mutate_all_eegbl <- dplyr::mutate_all(data, mean)
mutate_at_eegbl <- dplyr::mutate_at(data,channel_names(data), mean)
summarize_eegbl <- dplyr::summarize(data)
summarizeX_eegbl <- dplyr::summarize(data, mean(X))
summarize_at_eegbl <- dplyr::summarize_at(data, channel_names(data), mean)
group2_by_eegbl <- dplyr::group_by(data, .id)

mutate_g_signal_tbl <- dplyr::mutate(group_by_eegbl, X = X + 1)
mutate2_g_signal_tbl <- dplyr::mutate(group_by_eegbl, ZZ = X + 1)
transmute_g_signal_tbl <- dplyr::transmute(group_by_eegbl, X = X + 1)
mutate_all_g_signal_tbl <- dplyr::mutate_all(group_by_eegbl, mean)
mutate_at_g_signal_tbl <- dplyr::mutate_at(group_by_eegbl,channel_names(data), mean)
summarize_g_signal_tbl <- dplyr::summarize(group_by_eegbl, mean(X))
summarize_at_g_signal_tbl <- dplyr::summarize_at(group_by_eegbl, channel_names(data), mean)

summarize2_g_signal_tbl <- dplyr::summarize(group2_by_eegbl, mean(X))

