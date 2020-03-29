library(eeguana)

data_sincos2id <- eeg_lst(
    signal_tbl = tibble::tibble(
                             X = sin(1:20),
                             Y = cos(1:20),
                             .id = rep(c(1L, 2L), each = 10),
                             .sample = sample_int(rep(seq(-4L, 5L), times = 2), sampling_rate = 500)
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
                                                 sampling_rate = 500
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
            .sample = sample_int(rep(seq.int(-100, N / 2 - 101), times = 2), sampling_rate = 500)
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

reticulate::use_condaenv("anaconda3")
mne <- reticulate::import("mne")
bdf_file <- system.file("testdata", "Newtest17-256.bdf", package = "eeguana")
eeg_mne_bdf <- mne$io$read_raw_bdf(bdf_file, preload = TRUE, stim_channel = "Status")
data_mne_bdf <- as_eeg_lst(.data = eeg_mne_bdf) %>%
    dplyr::mutate(.recording = "bdf")
events_found <-   mne$find_events(eeg_mne_bdf)
events_tbl(data_mne_bdf) <- eeguana:::new_events_tbl(.id=1, .initial=events_found[,1]+1, .final= events_found[,1]+1,descriptions_dt = data.table::data.table(.type=NA, .description = events_found[,3]))
channels_tbl(data_mne_bdf) <- channels_tbl(data_mne_bdf)[,-5] # remove unit


usethis::use_data(data_sincos2id, data_sincos2id_2,data_sincos2id_1000,
                  data_sincos3id, data_mne_bdf, internal = TRUE, overwrite = TRUE)
