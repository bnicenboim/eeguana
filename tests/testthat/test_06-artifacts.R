library(eeguana)
options(eeguana.verbose = FALSE)
set.seed(123)

N <- 1000
signal <- dplyr::tibble(
  .id = 1L,
  .sample = sample_int(seq_len(N), .sampling_rate = 500),
  Fz = channel_dbl(rep(0, N)),
  Cz = channel_dbl(rep(0, N)),
  Pz = channel_dbl(rep(0, N))
)
Fz_10 <- c(3, 4, 503, 504, 700, 701)
Cz_10 <- c(709, 710, 999, 1000)
signal$Fz[Fz_10] <- 10
signal$Cz[Cz_10] <- 10
data <- eeg_lst(
  signal_tbl = signal,
  segments = dplyr::tibble(.id = 1L, .recording = "recording1", segment = 1L)
)

data_1minmax <- eeg_lst(
  signal_tbl = dplyr::tibble(
    X = channel_dbl(c(0, 0, 0, -10, 10, 0, 0, 0, 0)),
    .id = 1L,
    .sample = sample_int(1:9, .sampling_rate = 500)
  ),
  segments = dplyr::tibble(.id = 1L, .recording = "recording1", segment = 1L)
)

data_1step <- eeg_lst(
  signal_tbl = dplyr::tibble(
    X = channel_dbl(c(0, 0, 0, 0, 10, 10, 10, 10, 10)),
    .id = 1L,
    .sample = sample_int(1:9,.sampling_rate = 500)
  ),
  segments = dplyr::tibble(.id = 1L, .recording = "recording1", segment = 1L)
)

data_more <- eeg_lst(
  signal_tbl =
    signal %>%
      dplyr::mutate(
        .id = rep(1:4, each = N / 4),
        .sample = sample_int(rep(seq_len(N / 4), times = 4), .sampling_rate = 500)
      ),
  segments = dplyr::tibble(.id = seq.int(4), .recording = paste0("recording", c(1, 1, 2, 2)), segment = seq.int(4))
)

###### voltage steps ######################3

test_that("window of 1 step", {
  art_eventsp1 <- suppressWarnings( # the .window is too small, but it's ok for the test
    data_1step %>%
      eeg_artif_step(.threshold = .01, .window = 2 / 500, .lim = c(0 / 500, 1 / 500), .unit = "second")
  ) %>%
    events_tbl()
  expect_equal(art_eventsp1[.channel == "X", ]$.initial %>% as.numeric(), 4)
  expect_equal(art_eventsp1[.channel == "X", ]$.final %>% as.numeric(), 5)
  art_events0 <- suppressWarnings( # the .window is too small, but it's ok for the test
    data_1step %>%
      eeg_artif_step(.threshold = .01, .window = 2 / 500, .lim = c(0 / 500, 0 / 500), .unit = "second")
  ) %>%
    events_tbl()
  expect_equal(art_events0[.channel == "X", ]$.initial %>% as.numeric(), 4)
  expect_equal(art_events0[.channel == "X", ]$.final %>% as.numeric(), 4)
  art_eventsm1 <- suppressWarnings( # the .window is too small, but it's ok for the test
    data_1step %>%
      eeg_artif_step(.threshold = .01, .window = 2 / 500, .lim = c(-1 / 500, 0 / 500), .unit = "second")
  ) %>%
    events_tbl()
  expect_equal(art_eventsm1[.channel == "X", ]$.initial %>% as.numeric(), 3)
  expect_equal(art_eventsm1[.channel == "X", ]$.final %>% as.numeric(), 4)

  art_eventsl <- data_1step %>%
    eeg_artif_step(.threshold = .01, .window = 2 / 500, .lim = c(-1000 / 500, 1000 / 500), .unit = "second") %>%
    events_tbl()
  expect_equal(art_eventsl[.channel == "X", ]$.initial %>% as.numeric(), 1)
  expect_equal(art_eventsl[.channel == "X", ]$.final %>% as.numeric(), 9)
})

Fzi <- Fz_10[seq(1, by = 2, to = length(Fz_10))]
Fzf <- Fz_10[seq(2, by = 2, to = length(Fz_10))]
Czi <- Cz_10[seq(1, by = 2, to = length(Cz_10))]
Czf <- Cz_10[seq(2, by = 2, to = length(Cz_10))]

test_that(".window of 1 element", {
  art_events <- suppressWarnings( # the .window is too small, but it's ok for the test
    data %>%
      eeg_artif_step(.threshold = .01, .window = 2 / 500, .lim = c(-1 / 500, 0 / 500), .unit = "second")
  ) %>%
    events_tbl()

  expect_equal(art_events[.channel == "Fz", ]$.initial %>% as.numeric(), c(pmax(Fzi - 2, 1)))
  expect_equal(art_events[.channel == "Fz", ]$.final %>% as.numeric(), c(pmin(Fzf, 1000)))
  expect_equal(art_events[.channel == "Cz", ]$.initial %>% as.numeric(), c(pmax(Czi - 2, 1)))
  expect_equal(art_events[.channel == "Cz", ]$.final %>% as.numeric(), c(pmin(Czf, 998)))
  expect_equal(nrow(art_events[.channel == "Pz", ]), 0)
})

test_that(".window of 22 element", {
  art_events <- data %>%
    eeg_artif_step(.threshold = .01, .window = 2 / 500, .lim = c(-10 / 500, 10 / 500), .unit = "second") %>%
    events_tbl()
  expect_equal(art_events[.channel == "Fz", ]$.initial %>% as.numeric(), c(pmax(Fzi - 11, 1)))
  expect_equal(art_events[.channel == "Fz", ]$.final %>% as.numeric(), c(pmin(Fzf + 10, 1000)))
  expect_equal(art_events[.channel == "Cz", ]$.initial %>% as.numeric(), c(pmax(Czi - 11, 1)))
  expect_equal(art_events[.channel == "Cz", ]$.final %>% as.numeric(), c(pmin(Czf + 10, 1000)))
  expect_equal(nrow(art_events[.channel == "Pz", ]), 0)
})

test_that("No artifacts", {
  art_events <- data %>%
    eeg_artif_step(.threshold = 100, .window = 2 / 500, .lim = c(-10 / 500, 10 / 500), .unit = "second") %>%
    events_tbl()
  empty_events <- eeguana:::new_events_tbl()
  empty_events[, .initial := sample_int(integer(0), 500)][, .final := sample_int(integer(0), 500)]
  expect_equal(art_events, empty_events)
})

Fzim <- Fzi - floor(Fzi / 250) * 250
Fzfm <- Fzf - floor(Fzf / 250) * 250
Czim <- Czi - floor(Czi / 250) * 250
Czfm <- (Czf - floor(Czf / 250 - .00001) * 250)

test_that(".window of 22 elements with different .id", {
  art_events <- data_more %>%
    eeg_artif_step(.threshold = .01, .window = 2 / 500, .lim = c(-10 / 500, 10 / 500), .unit = "second") %>%
    events_tbl()
  expect_equal(art_events[.channel == "Fz", ]$.initial %>% as.numeric(), c(pmax(Fzim - 11, 1)))
  expect_equal(art_events[.channel == "Fz", ]$.final %>% as.numeric(), c(pmin(Fzfm + 10, 250)))
  expect_equal(art_events[.channel == "Cz", ]$.initial %>% as.numeric(), c(pmax(Czim - 11, 1)))
  expect_equal(art_events[.channel == "Cz", ]$.final %>% as.numeric(), c(pmin(Czfm + 10, 250)))

  art_events <- data_more %>%
    eeg_artif_step(Fz, .threshold = .01, .window = 2 / 500, .lim = c(-10 / 500, 10 / 500), .unit = "second") %>%
    events_tbl()
  expect_equal(art_events[.channel == "Fz", ]$.initial %>% as.numeric(), c(pmax(Fzim - 11, 1)))
  expect_equal(art_events[.channel == "Fz", ]$.final %>% as.numeric(), c(pmin(Fzfm + 10, 250)))

  expect_equal(art_events[.channel == "Cz", ]$.initial %>% as.numeric(), integer(0))
  expect_equal(art_events[.channel == "Cz", ]$.final %>% as.numeric(), integer(0))
  expect_equal(nrow(art_events[.channel == "Pz", ]), 0)
})

test_that("missing samples", {
  data_1skip <- data_1step %>% dplyr::filter(!.sample %in% c(4, 5))
  art_events_skipped1 <- data_1skip %>%
    eeg_artif_step(.threshold = .01, .window = 2 / 500, .lim = c(-1000 / 500, 1000 / 500), .unit = "second") %>%
    events_tbl()
  expect_equal(nrow(art_events_skipped1), 0)
})

test_that(".window of 22 element, with NAs", {
  data$.signal$Fz[7] <- NA
  art_events <- data %>%
    eeg_artif_step(.threshold = .01, .window = 2 / 500, .lim = c(-10 / 500, 10 / 500), .unit = "second") %>%
    events_tbl()
  expect_equal(art_events[.channel == "Fz", ]$.initial %>% as.numeric(), c(pmax(Fzi - 11, 1)))
  expect_equal(art_events[.channel == "Fz", ]$.final %>% as.numeric(), c(pmin(Fzf + 10, 1000)))
  expect_equal(art_events[.channel == "Cz", ]$.initial %>% as.numeric(), c(pmax(Czi - 11, 1)))
  expect_equal(art_events[.channel == "Cz", ]$.final %>% as.numeric(), c(pmin(Czf + 10, 1000)))
  expect_equal(nrow(art_events[.channel == "Pz", ]), 0)
})


test_that("freq works", {
  events_h100 <- data %>%
    eeg_filt_high_pass(.freq = 10) %>%
    eeg_artif_minmax(.threshold = 10, .lim = c(0 / 500, 4 / 500), .window = 2 / 500, .unit = "second") %>%
    events_tbl()

  step_h10 <- data %>%
    eeg_artif_minmax(.threshold = 10, .lim = c(0 / 500, 4 / 500), .window = 2 / 500, .unit = "second", .freq = c(10, NA))
 
  events_h100s <- data %>%
    eeg_filt_high_pass(.freq = 10) %>%
    eeg_artif_step(.threshold = 10, .lim = c(0 / 500, 4 / 500), .window = 2 / 500, .unit = "second") %>%
    events_tbl()
  
  step_h10s <- data %>%
    eeg_artif_step(.threshold = 10, .lim = c(0 / 500, 4 / 500), .window = 2 / 500, .unit = "second", .freq = c(10, NA))

  events_h100p <- data %>%
    eeg_filt_high_pass(.freq = 10) %>%
    eeg_artif_peak(.threshold = 10, .lim = c(0 / 500, 4 / 500), .window = 2 / 500, .unit = "second") %>%
    events_tbl()
  
  step_h10p <- data %>%
    eeg_artif_peak(.threshold = 10, .lim = c(0 / 500, 4 / 500), .window = 2 / 500, .unit = "second", .freq = c(10, NA))

  events_h100a <- data %>%
    eeg_filt_high_pass(.freq = 10) %>%
    eeg_artif_amplitude(.threshold = c(-10,10), .lim = c(0 / 500, 4 / 500), .unit = "second") %>%
    events_tbl()
  
  step_h10a <- data %>%
    eeg_artif_amplitude(.threshold = c(-10,10), .lim = c(0 / 500, 4 / 500), .unit = "second", .freq = c(10, NA))
  
  expect_equal(events_h100, events_tbl(step_h10))
  expect_equal(signal_tbl(data), signal_tbl(step_h10))
  expect_equal(events_h100s, events_tbl(step_h10s))
  expect_equal(signal_tbl(data), signal_tbl(step_h10s))
  expect_equal(events_h100p, events_tbl(step_h10p))
  expect_equal(signal_tbl(data), signal_tbl(step_h10p))
  expect_equal(events_h100a, events_tbl(step_h10a))
  expect_equal(signal_tbl(data), signal_tbl(step_h10a))
})


###### voltage minmax ######################3
test_that("minmax: .window of 1 step", {
  art_eventsp1 <- suppressWarnings( # the .window is too small, but it's ok for the test
    data_1step %>%
      eeg_artif_minmax(.threshold = .01, .lim = c(0 / 500, 1 / 500), .window = 2 / 500, .unit = "second")
  ) %>%
    events_tbl()
  expect_equal(art_eventsp1[.channel == "X", ]$.initial %>% as.numeric(), 5)
  expect_equal(art_eventsp1[.channel == "X", ]$.final %>% as.numeric(), 6)

  art_events0 <- suppressWarnings(
    data_1step %>%
      eeg_artif_minmax(.threshold = .01, .lim = c(0 / 500, 0 / 500), .window = 2 / 500, .unit = "second")
  ) %>%
    events_tbl()
  expect_equal(art_events0[.channel == "X", ]$.initial %>% as.numeric(), 5)
  expect_equal(art_events0[.channel == "X", ]$.final %>% as.numeric(), 5)
  art_eventsm1 <- suppressWarnings(
    data_1step %>%
      eeg_artif_minmax(.threshold = .01, .lim = c(-1 / 500, 0 / 500), .window = 2 / 500, .unit = "second")
  ) %>%
    events_tbl()
  expect_equal(art_eventsm1[.channel == "X", ]$.initial %>% as.numeric(), 4)
  expect_equal(art_eventsm1[.channel == "X", ]$.final %>% as.numeric(), 5)

  art_eventsl <- suppressWarnings(
    data_1step %>%
      eeg_artif_minmax(.threshold = .01, .lim = c(-1000 / 500, 1000 / 500), .window = 2 / 500, .unit = "second")
  ) %>%
    events_tbl()
  expect_equal(art_eventsl[.channel == "X", ]$.initial %>% as.numeric(), 1)
  expect_equal(art_eventsl[.channel == "X", ]$.final %>% as.numeric(), 9)
})

test_that("NAs", {
  data_1step$.signal$X[1:2] <- NA
  data_1step$.signal$X[5] <- NA
  art_eventsl <- data_1step %>%
    eeg_artif_minmax(.threshold = .01, .lim = c(-1000 / 500, 1000 / 500), .window = 3 / 500, .unit = "second") %>%
    events_tbl()
  expect_equal(art_eventsl[.channel == "X", ]$.initial %>% as.numeric(), 1)
  expect_equal(art_eventsl[.channel == "X", ]$.final %>% as.numeric(), 9)
})

test_that("missing samples", {
  data_1skip <- data_1step %>% dplyr::filter(!.sample %in% c(4, 5))
  art_events_skipped1 <- data_1skip %>%
    eeg_artif_minmax(.threshold = .01, .lim = c(-1000 / 500, 1000 / 500), .window = 2 / 500, .unit = "second") %>%
    events_tbl()
  expect_equal(nrow(art_events_skipped1[.channel == "X", ]), 0)
  art_events_skipped1w <-
    data_1skip %>%
    eeg_artif_minmax(.threshold = .01, .lim = c(-1000 / 500, 1000 / 500), .window = 4 / 500, .unit = "second") %>%
    events_tbl()
  expect_equal(art_events_skipped1w[.channel == "X", ]$.initial %>% as.numeric(), 1)
  expect_equal(art_events_skipped1w[.channel == "X", ]$.final %>% as.numeric(), 9)
})

test_that("simple .window", {
  art_events <- data_1minmax %>%
    eeg_artif_minmax(.threshold = 19, .lim = c(0 / 500, 4 / 500), .window = 2 / 500, .unit = "second") %>%
    events_tbl()
  expect_equal(art_events[.channel == "X", ]$.initial %>% as.numeric(), c(5))
  expect_equal(art_events[.channel == "X", ]$.final %>% as.numeric(), 9)
})

test_that("low freq works", {
  events_l100 <- data_1minmax %>%
    eeg_filt_low_pass(.freq = 100) %>%
    eeg_artif_minmax(.threshold = 10, .lim = c(0 / 500, 4 / 500), .window = 2 / 500, .unit = "second") %>%
    events_tbl()

  minmax_l100 <- data_1minmax %>%
    eeg_artif_minmax(.threshold = 10, .lim = c(0 / 500, 4 / 500), .window = 2 / 500, .unit = "second", .freq = c(NA, 100))
  expect_equal(events_l100, events_tbl(minmax_l100))
  expect_equal(data_1minmax, minmax_l100)
})


message("test amplitude and peak, they are not too well tested")