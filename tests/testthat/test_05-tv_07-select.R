library(eeguana)
options(eeguana.verbose = FALSE)

# create fake dataset
data_1 <- eeguana:::data_sincos3id
# just some different X and Y
data_2 <- eeg_mutate(data_1,
  .recording = "recording2",
  X = sin(X + 10),
  Y = cos(Y - 10),
  condition = c("b", "a", "b")
)

# bind it all together
data <- bind(data_1, data_2)

# for checks later
reference_data <- data.table::copy(data)

test_that("internal (?) variables should show warnings", {
  # TODO multiple warnings, check this:
  expect_warning(eeg_select(data, ID = .id)) %>%
    expect_warning() %>%
    expect_warning()

  expect_warning(eeg_select(data, time = .sample)) %>%
    expect_warning()

  expect_warning(eeg_select(data, participant = .recording))
  expect_warning(events_tbl(data) <- events_tbl(data) %>% dplyr::select(elec = .channel)) %>%
    expect_warning()
})


test_that("bad things throw errors", {
  expect_error(eeg_select(data, X = Y, X))
})

test_that("selecting a regular eeg_lst", {
  # in signal table
  selectd_eeg1 <- eeg_select(data, ZZ = Y)
  selectd_eeg2 <- eeg_select(data, ZZ = Y, XX = X)
  selectd_eeg3 <- eeg_select(data, X = Y, Y = X)
  selectd_eeg4 <- eeg_select(data, X = Y)
  expect_equal(signal_tbl(selectd_eeg1), data$.signal[, .(.id, .sample, ZZ = Y)])
  expect_equal(signal_tbl(selectd_eeg2), data$.signal[, .(.id, .sample, ZZ = Y, XX = X)])
  expect_equal(signal_tbl(selectd_eeg3), data$.signal[, .(.id, .sample, X = Y, Y = X)])
  expect_equal(signal_tbl(selectd_eeg4), data$.signal[, .(.id, .sample, X = Y)])

  # in segments table
  expect_equal(segments_tbl(selectd_eeg1), segments_tbl(data))
  expect_equal(segments_tbl(selectd_eeg2), segments_tbl(data))
  expect_equal(segments_tbl(selectd_eeg3), segments_tbl(data))
  expect_equal(segments_tbl(selectd_eeg4), segments_tbl(data))

  expect_equal(events_tbl(selectd_eeg1), events_tbl(data) %>%
    dplyr::mutate(
      .channel =
        ifelse(.channel == "Y", "ZZ", .channel)
    ) %>%
    dplyr::filter(.channel != "X" | is.na(.channel)))
  expect_equal(events_tbl(selectd_eeg2), events_tbl(data) %>%
    dplyr::mutate(
      .channel =
        ifelse(.channel == "Y", "ZZ", "XX")
    ))
  expect_equal(events_tbl(selectd_eeg3), events_tbl(data) %>%
    dplyr::mutate(
      .channel =
        ifelse(.channel == "Y", "X", "Y")
    ))
  expect_equal(is_channel_dbl(selectd_eeg1$.signal$ZZ), TRUE)
  expect_equal(events_tbl(selectd_eeg4), events_tbl(data) %>%
    dplyr::mutate(
      .channel =
        ifelse(.channel == "Y", "X", "Y")
    ) %>%
    dplyr::filter(.channel == "X" | is.na(.channel)))
})

test_that("selecting in segments table doesn't change data", {
  selectd_eeg5 <- eeg_select(data, cond = condition)
  expect_equal(signal_tbl(selectd_eeg5), data$.signal)
  expect_equal(events_tbl(selectd_eeg5), data$.events)
  expect_equal(segments_tbl(selectd_eeg5), data$.segments[, c(".id", ".recording", "condition")] %>% data.table::copy() %>% data.table::setnames("condition", "cond"))
  expect_equal(reference_data, data)
})


test_that("selecting a grouped eeg_lst", {
  # in signal table
  datag <- data %>% dplyr::group_by(.id, .recording)
  # in signal table
  selectd_eeg1 <- dplyr::select(datag, ZZ = Y)
  selectd_eeg2 <- dplyr::select(datag, ZZ = Y, XX = X)
  selectd_eeg3 <- dplyr::select(datag, X = Y, Y = X)
  selectd_eeg4 <- dplyr::select(datag, X = Y)
  expect_equal(signal_tbl(selectd_eeg1), data$.signal[, .(.id, .sample, ZZ = Y)])
  expect_equal(signal_tbl(selectd_eeg2), data$.signal[, .(.id, .sample, ZZ = Y, XX = X)])
  expect_equal(signal_tbl(selectd_eeg3), data$.signal[, .(.id, .sample, X = Y, Y = X)])
  expect_equal(signal_tbl(selectd_eeg4), data$.signal[, .(.id, .sample, X = Y)])

  # in segments table
  expect_equal(segments_tbl(selectd_eeg1), segments_tbl(data))
  expect_equal(segments_tbl(selectd_eeg2), segments_tbl(data))
  expect_equal(segments_tbl(selectd_eeg3), segments_tbl(data))
  expect_equal(segments_tbl(selectd_eeg4), segments_tbl(data))

  expect_equal(events_tbl(selectd_eeg1), events_tbl(data) %>%
    dplyr::mutate(
      .channel =
        ifelse(.channel == "Y", "ZZ", .channel)
    ) %>%
    dplyr::filter(.channel != "X" | is.na(.channel)))
  expect_equal(events_tbl(selectd_eeg2), events_tbl(data) %>%
    dplyr::mutate(
      .channel =
        ifelse(.channel == "Y", "ZZ", "XX")
    ))
  expect_equal(events_tbl(selectd_eeg3), events_tbl(data) %>%
    dplyr::mutate(
      .channel =
        ifelse(.channel == "Y", "X", "Y")
    ))
  expect_equal(is_channel_dbl(selectd_eeg1$.signal$ZZ), TRUE)
  expect_equal(events_tbl(selectd_eeg4), events_tbl(data) %>%
    dplyr::mutate(
      .channel =
        ifelse(.channel == "Y", "X", "Y")
    ) %>%
    dplyr::filter(.channel == "X" | is.na(.channel)))
})

test_that("selecting in grouped segments table doesn't change data", {
  datag <- data %>% dplyr::group_by(.id, .recording)
  selectd_eeg5 <- dplyr::select(datag, cond = condition)
  expect_equal(signal_tbl(selectd_eeg5), data$.signal)
  expect_equal(events_tbl(selectd_eeg5), data$.events)
  expect_equal(segments_tbl(selectd_eeg5), data$.segments[, c(".id", ".recording", "condition")] %>% data.table::copy() %>% data.table::setnames("condition", "cond"))
  expect_equal(reference_data, data)
})
