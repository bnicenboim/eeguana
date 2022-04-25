library(eeguana)

data_sincos2id <- eeg_lst(
  signal_tbl = tibble::tibble(
    X = sin(1:20),
    Y = cos(1:20),
    .id = rep(c(1L, 2L), each = 10),
    .sample = sample_int(rep(seq(-4L, 5L), times = 2), .sampling_rate = 500)
  ),
  channels_tbl =
    dplyr::tibble(
      .channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_
    ),
  events_tbl = dplyr::tribble(
    ~.id, ~.type, ~.description, ~.initial, ~.final, ~.channel,
    1L, "New Segment", NA_character_, -4L, -4L, NA,
    1L, "Bad", NA_character_, -2L, 0L, NA,
    1L, "Time 0", NA_character_, 1L, 1L, NA,
    1L, "Bad", NA_character_, 2L, 3L, "X",
    2L, "New Segment", NA_character_, -4L, -4L, NA,
    2L, "Time 0", NA_character_, 1L, 1L, NA,
    2L, "Bad", NA_character_, 2L, 2L, "Y"
  ),
  segments_tbl = dplyr::tibble(.id = c(1L, 2L), .recording = "recording1", segment = c(1L, 2L))
)

data_sincos2id_2 <- eeg_lst(
  signal_tbl = dplyr::tibble(
    X = sin(1:20),
    Y = cos(1:20),
    .id = rep(c(1L, 2L), each = 10),
    .sample = sample_int(rep(seq(-4L, 5L), times = 2),
      .sampling_rate = 500
    )
  ),
  channels_tbl = dplyr::tibble(
    .channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
    radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_
  ),
  events_tbl = dplyr::tribble(
    ~.id, ~.type, ~.description, ~.initial, ~.final, ~.channel,
    1L, "New Segment", NA_character_, -4L, -4L, NA,
    1L, "Bad", NA_character_, -2L, 0L, "X",
    1L, "Bad", NA_character_, -2L, 0L, "Y",
    1L, "Time 0", NA_character_, 1L, 1L, NA,
    1L, "Bad", NA_character_, 2L, 3L, "X",
    2L, "New Segment", NA_character_, -4L, -4L, NA,
    2L, "Time 0", NA_character_, 1L, 1L, NA,
    2L, "Bad", NA_character_, 2L, 2L, "Y"
  ),
  segments_tbl = dplyr::tibble(.id = c(1L, 2L), .recording = "recording1", segment = c(1L, 2L))
)

N <- 1000
data_sincos2id_1000 <- eeg_lst(
  signal_tbl =
    dplyr::tibble(
      X = sin(1:N / 20),
      Y = cos(1:N / 20),
      .id = rep(c(1L, 2L), each = N / 2),
      .sample = sample_int(rep(seq.int(-100, N / 2 - 101), times = 2), .sampling_rate = 500)
    ),
  channels_tbl = dplyr::tibble(
    .channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
    radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_
  ),
  events_tbl = dplyr::tribble(
    ~.id, ~.type, ~.description, ~.initial, ~.final, ~.channel,
    1L, "New Segment", NA_character_, -100L, -100L, NA,
    1L, "Bad", NA_character_, -20L, 9L, NA,
    1L, "Time 0", NA_character_, 1L, 1L, NA,
    1L, "Bad", NA_character_, 20L, 21L, "X",
    2L, "New Segment", NA_character_, -100L, -100L, NA,
    2L, "Time 0", NA_character_, 1L, 1L, NA,
    2L, "Bad", NA_character_, 20L, 29L, "Y"
  ),
  segments_tbl = dplyr::tibble(.id = c(1L, 2L), .recording = "recording1", segment = c(1L, 2L))
)

data_sincos3id <- eeg_lst(
  signal_tbl =
    dplyr::tibble(
      X = sin(1:30), Y = cos(1:30),
      .id = rep(c(1L, 2L, 3L), each = 10),
      .sample = sample_int(rep(seq(-4L, 5L), times = 3), .sampling_rate = 500)
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

reticulate::use_condaenv("anaconda3")
mne <- reticulate::import("mne")
bdf_file <- system.file("testdata", "Newtest17-256.bdf", package = "eeguana")
eeg_mne_bdf <- mne$io$read_raw_bdf(bdf_file, preload = TRUE, stim_channel = "Status")
data_mne_bdf <- as_eeg_lst(.data = eeg_mne_bdf) %>%
  dplyr::mutate(.recording = "bdf")
events_found <- mne$find_events(eeg_mne_bdf)
events_tbl(data_mne_bdf) <- eeguana:::new_events_tbl(.id = 1, .initial = events_found[, 1] + 1, .final = events_found[, 1] + 1, .type = NA_character_, .description = events_found[, 3])
channels_tbl(data_mne_bdf) <- channels_tbl(data_mne_bdf)[, -5] # remove unit



#### Data for ICA
## simulate eye movements with
## https://stackoverflow.com/questions/53071709/remove-eye-blinks-from-eeg-signal-with-ica

N <- 4000
fs <- 100
blink <- rbinom(N, 1, .003) %>%
  signal::filter(signal::butter(2, c(1 * 2 / fs, 10 * 2 / fs), "pass"), .) * 200
noise <- rpink(N, 100)
alpha <- sin(2 * pi * 10 * as_time(sample_int(1:N, 500))) * 3
# (abs(sin(10 * seq_len(N) / fs)) - 0.5) * 10

s_tbl <- dplyr::tibble(sample = rep(seq_len(N), times = 3), A = c(blink, noise, alpha), component = rep(c("blink", "noise", "alpha"), each = N))

true_comps <- cbind(alpha, noise, blink)
# ggplot(s_tbl,aes(x=sample,y=A)) + geom_line() + facet_grid(component~.)



# And they mix depending on the distance of S_pos:

signal_blinks <- dplyr::tibble(
  Fz = blink * 2 + alpha * 1 + noise,
  Cz = blink * 1 + alpha * .9 + noise,
  Pz = blink * .1 + alpha * 1 + noise
) %>%
  dplyr::mutate_all(channel_dbl)


signal <- dplyr::tibble(
  Fz = alpha * .1 + noise,
  Cz = alpha * .15 + noise,
  Pz = alpha * .1 + noise
) %>%
  dplyr::mutate_all(channel_dbl)

data_no_blinks <- eeg_lst(
  signal_tbl = signal %>%
    dplyr::mutate(
      .id = 1L,
      .sample = sample_int(seq_len(N), .sampling_rate = 500)
    ),
  segments_tbl = dplyr::tibble(.id = 1L, .recording = "recording1", segment = 1L)
)

data_blinks <- eeg_lst(
  signal_tbl = signal_blinks %>%
    dplyr::mutate(
      .id = 1L,
      .sample = sample_int(seq_len(N), .sampling_rate = 500)
    ),
  segments_tbl = dplyr::tibble(.id = 1L, .recording = "recording1", segment = 1L)
)


data_blinks_2 <- eeg_lst(
  signal_tbl = signal_blinks %>%
    dplyr::mutate(
      .id = rep(1:4, each = N / 4),
      .sample = sample_int(rep(seq_len(N / 4), times = 4), .sampling_rate = 500)
    ),
  segments_tbl = data.table::data.table(.id = seq.int(4), .recording = paste0("recording", c(1, 1, 2, 2)), segment = seq.int(4))
)

data_no_blinks_2 <- eeg_lst(
  signal_tbl = signal %>%
    dplyr::mutate(
      .id = rep(1:4, each = N / 4),
      .sample = sample_int(rep(seq_len(N / 4), times = 4), .sampling_rate = 500)
    ),
  segments_tbl = data.table::data.table(.id = seq.int(4), .recording = paste0("recording", c(1, 1, 2, 2)), segment = seq.int(4))
)

#From https://raphaelvallat.com/bandpower.html
F3 <- tidytable::fread.("data.txt")[[1]]
data_sleep_F3 <- eeg_lst(signal_tbl = data.frame(F3 = channel_dbl(F3)),sampling_rate = 100)

reticulate::use_condaenv("anaconda3")
signal <- reticulate::import("scipy.signal")
win <- 400
srate <- 100

#with python
psd_scipy <- signal$welch(as.numeric(F3),fs = srate, nperseg=win)
data_psd_scipy <- psd_lst(psd_tbl = data.frame(.id =1, .freq = psd_scipy[[1]], F3 =channel_dbl(psd_scipy[[2]])))

usethis::use_data(data_sincos2id, data_sincos2id_2,
  data_sincos2id_1000,
  data_sincos3id, data_mne_bdf,
  data_blinks, data_blinks_2,
  data_no_blinks, data_no_blinks_2,
  true_comps,
  data_sleep_F3,
  data_psd_scipy,
  internal = TRUE, overwrite = TRUE,
  compress = "xz"
)
