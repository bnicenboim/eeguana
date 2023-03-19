library(eeguana)
options(eeguana.verbose = FALSE)

data <- eeguana:::data_sincos2id


data0 <- eeg_lst(
  signal_tbl =
    tidytable::tidytable(
      X = sin(1:20),
      Y = cos(1:20),
      .id = rep(c(1L, 1L), each = 10),
      .sample = sample_int(seq(1L, 20L), .sampling_rate = 500)
    ),
  channels_tbl = tidytable::tidytable(
    .channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
    radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_
  ),
  events_tbl = dplyr::tribble(
    ~.id, ~.type, ~.description, ~.initial, ~.final, ~.channel,
    1L, "New Segment", NA, 1L, 1L, NA,
    1L, "Bad", NA, 3L, 5L, NA,
    1L, "Time 0", NA, 6L, 6L, NA,
    1L, "Bad", NA, 7L, 8L, "X",
    1L, "New Segment", NA, 11L, 11L, NA,
    1L, "Time 0", NA, 16L, 16L, NA,
    1L, "Bad", NA, 17L, 17L, "Y"
  ),
  segments_tbl = tidytable::tidytable(.id = 1L, .recording = "recording1", segment = 1)
)

ref_data <- data.table::copy(data)
ref_data0 <- data.table::copy(data0)

test_that("can segment using .lim", {
  data_s <- eeg_segment(data, .type == "Time 0")
  expect_equal(data$.signal, data_s$.signal)
  expect_equal(data$.events, data_s$.events)
  expect_equal(data$.segments, data_s$.segments[, -c("type", "description")])
  # expect_equal(data$.segments, data_s$.segments)
  d <- eeg_segment(data, .type == "Time 0")
  d_rec <- eeg_segment(d, .type == "Time 0")
  expect_equal(d$.signal, d_rec$.signal)
  expect_equal(d$.events, d_rec$.events)
  expect_equal(dplyr::select(d$.segments, -type, -description), dplyr::select(d_rec$.segments, -i.type, -i.description, -type, -description))

  # old with data.table
  ## expect_equal(dplyr::select(d$.segments, -type, -description), dplyr::select(d_rec$.segments, -type.x, -description.x, -type.y, -description.y))

  d_0 <- eeg_segment(data, .type == "Time 0", .lim = c(0, Inf))
  d_0_0 <- eeg_segment(d_0, .type == "Time 0", .lim = c(0, Inf))
  expect_equal(nrow(d_0$.signal), 10)
  expect_equal(d_0$.signal, d_0_0$.signal)
  expect_equal(d_0$.events, d_0_0$.events)
  expect_equal(dplyr::select(d_0$.segments, -type, -description), dplyr::select(d_0_0$.segments, -type, -description, -i.type, -i.description))
  s1 <- eeg_segment(data0, .type == "Time 0", .lim = c(0, 1 / 500))
  expect_equal(s1$.signal$X[1], data0$.signal$X[6])
  expect_equal(nrow(s1$.signal), 4)
  expect_equal(nrow(s1$.segments), 2)
  expect_equal(s1$.segments$segment, 1:2)
  s1_u <- eeg_segment(data0, .type == "Time 0", .lim = c(0, 1), .unit = "sample")
  s1_u2 <- eeg_segment(data0, .type == "Time 0", .lim = c(0, 2), .unit = "ms")
  expect_equal(s1, s1_u2)
  double <- eeg_segment(data0, .type == "Time 0", .lim = c(-20, 20), .unit = "sample")
  expect_equal(nrow(double$.signal), 40)
  d00 <- eeg_segment(data0, .type == "Time 0", .lim = c(0, 1), .unit = "sample")
  expect_equal(all(d00$.events$.final <= max(d00$.signal$.sample)), TRUE)
  expect_equal(all(s1$.events$.final <= max(s1$.signal$.sample)), TRUE)
  expect_equal(all(s1_u$.events$.final <= max(s1_u$.signal$.sample)), TRUE)
  expect_equal(all(s1_u2$.events$.final <= max(s1_u2$.signal$.sample)), TRUE)
  expect_equal(all(s1_u2$.events$.final <= max(s1_u2$.signal$.sample)), TRUE)
})

test_that("can segment using .end", {
  data_s_e <- eeg_segment(data, .type == "New Segment", .end = .type == "Time 0")
  data_s_e_c <- eeg_segment(data, .type == "New Segment", .lim = c(0, 5), .unit = "sample")
  expect_equal(data_s_e, data_s_e_c)

  data_unm0 <- data
  events_tbl(data_unm0) <- events_tbl(data_unm0) %>% dplyr::slice(-1)
  expect_warning(eeg_segment(data_unm0, .type == "New Segment", .end = .type == "Time 0"))
  data_unm0 <- suppressWarnings(eeg_segment(data_unm0, .type == "New Segment", .end = .type == "Time 0"))
  expect_true(all(is.na(signal_tbl(data_unm0)[.id == 1, .(.sample, X, Y)])))
  expect_equal(signal_tbl(data_unm0)[.id != 1, ], signal_tbl(data_s_e)[.id != 1, ])
  expect_equal(segments_tbl(data_unm0) %>% dplyr::filter(.id == 1) %>% dplyr::pull(type), "incorrect segment")

  data_unm0_2 <- data
  events_tbl(data_unm0_2) <- events_tbl(data_unm0_2) %>% dplyr::slice(-5)
  expect_warning(eeg_segment(data_unm0_2, .type == "New Segment", .end = .type == "Time 0"))
  data_unm0_2 <- suppressWarnings(eeg_segment(data_unm0_2, .type == "New Segment", .end = .type == "Time 0"))
  expect_true(all(is.na(signal_tbl(data_unm0_2)[.id == 2, .(.sample, X, Y)])))
  expect_equal(signal_tbl(data_unm0_2)[.id != 2, ], signal_tbl(data_s_e)[.id != 2, ])
  expect_equal(segments_tbl(data_unm0_2) %>% dplyr::filter(.id == 2) %>% dplyr::pull(type), "incorrect segment")

  data_unme <- data
  events_tbl(data_unme) <- events_tbl(data_unme) %>% dplyr::slice(-3)
  expect_warning(eeg_segment(data_unme, .type == "New Segment", .end = .type == "Time 0"))
  data_unme <- suppressWarnings(eeg_segment(data_unme, .type == "New Segment", .end = .type == "Time 0"))
  expect_equal(signal_tbl(data_unme), signal_tbl(data_unm0))
  expect_equal(segments_tbl(data_unme), segments_tbl(data_unm0))
  data_unme_2 <- data
  events_tbl(data_unme_2) <- events_tbl(data_unme_2) %>% dplyr::slice(-6)
  expect_warning(eeg_segment(data_unme_2, .type == "New Segment", .end = .type == "Time 0"))
  data_unme_2 <- suppressWarnings(eeg_segment(data_unme_2, .type == "New Segment", .end = .type == "Time 0"))

  expect_equal(signal_tbl(data_unme_2), signal_tbl(data_unm0_2))
  expect_equal(segments_tbl(data_unme_2), segments_tbl(data_unm0_2))

  data_unm <- data
  events_tbl(data_unm) <- events_tbl(data_unm) %>% dplyr::slice(-1, -6)

  expect_warning(expect_warning(eeg_segment(data_unm, .type == "New Segment", .end = .type == "Time 0"), regexp = "Unmatched"),
    regexp = "Unmatched"
  )

  data_unm <- suppressWarnings(eeg_segment(data_unm, .type == "New Segment", .end = .type == "Time 0"))

  expect_true(all(is.na(signal_tbl(data_unm)[, .(.sample, X, Y)])))
  expect_equal(segments_tbl(data_unm) %>% dplyr::pull(type) %>% unique(), "incorrect segment")
})
# TODO
## warning("duplicated triggers should be tested")

test_that("duplicated triggers", {
  data_s_e <- eeg_segment(data, .type == "New Segment", .end = .type == "Time 0")
  data_s_e_c <- eeg_segment(data, .type == "New Segment", .lim = c(0, 5), .unit = "sample")

  data_dup <- data
  events_tbl(data_dup) <- suppressWarnings(dplyr::bind_rows(events_tbl(data_dup) %>% dplyr::slice(1), events_tbl(data_dup)))


  data_dups <- eeg_segment(data_dup, .type == "New Segment", .end = .type == "Time 0")
  expect_equal(signal_tbl(data_dups), signal_tbl(data_s_e))
  expect_equal(segments_tbl(data_dups), segments_tbl(data_s_e))
})


test_that("can add stuff from the events table", {
  events_tbl(data) <- events_tbl(data) %>% tidytable::mutate(cond = .type)
  data_s <- eeg_segment(data, .type == "Time 0")
  expect_equal(data_s$.segments$cond, data_s$.segments$type)
})

test_that("I didn't modify the objects", {
  expect_equal(data, ref_data)
  expect_equal(data0, ref_data0)
})
