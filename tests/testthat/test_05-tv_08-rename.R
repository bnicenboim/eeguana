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
  # TODO fix two times warning
  expect_warning(eeg_rename(data, ID = .id)) %>%
    expect_warning()
  expect_warning(eeg_rename(data, time = .sample))
  expect_warning(eeg_rename(data, participant = .recording))
  expect_warning(events_tbl(data) <- events_tbl(data) %>% dplyr::rename(elec = .channel)) %>%
    expect_warning()
})
message("change prev to error")



test_that("bad things throw errors", {
  expect_error(eeg_rename(data, X = Y))
})

test_that("renaming a regular eeg_lst", {
  # in signal table
  renamed_eeg1 <- eeg_rename(data, ZZ = Y)
  renamed_eeg2 <- eeg_rename(data, ZZ = Y, XX = X)
  renamed_eeg3 <- eeg_rename(data, X = Y, Y = X)

  expect_equal(signal_tbl(renamed_eeg1), setNames(data$.signal, c(".id", ".sample", "X", "ZZ")))
  expect_equal(
    signal_tbl(renamed_eeg2),
    setNames(data$.signal, c(".id", ".sample", "XX", "ZZ"))
  )

  expect_equal(signal_tbl(renamed_eeg3), setNames(data$.signal, c(".id", ".sample", "Y", "X")))

  # in segments table
  expect_equal(segments_tbl(renamed_eeg1), segments_tbl(data))
  expect_equal(segments_tbl(renamed_eeg2), segments_tbl(data))
  expect_equal(segments_tbl(renamed_eeg3), segments_tbl(data))

  expect_equal(events_tbl(renamed_eeg1), events_tbl(data) %>%
    dplyr::mutate(
      .channel =
        ifelse(.channel == "Y", "ZZ", .channel)
    ))
  expect_equal(events_tbl(renamed_eeg2), events_tbl(data) %>%
    dplyr::mutate(
      .channel =
        ifelse(.channel == "Y", "ZZ", "XX")
    ))
  expect_equal(events_tbl(renamed_eeg3), events_tbl(data) %>%
    dplyr::mutate(
      .channel =
        ifelse(.channel == "Y", "X", "Y")
    ))
  expect_equal(is_channel_dbl(renamed_eeg1$.signal$ZZ), TRUE)
})

test_that("renaming in segments table doesn't change data", {
  renamed_eeg4 <- eeg_rename(data, cond = condition)
  expect_equal(signal_tbl(renamed_eeg4), data$.signal)
  expect_equal(events_tbl(renamed_eeg4), data$.events)
  expect_equal(segments_tbl(renamed_eeg4), setNames(data$.segments, c(".id", ".recording", "segment", "cond")))
  expect_equal(reference_data, data)
})


test_that("renaming a grouped eeg_lst", {
  # in signal table
  datag <- data %>% dplyr::group_by(.id, .recording)
  renamed_eeg1 <- eeg_rename(datag, ZZ = Y)
  renamed_eeg2 <- eeg_rename(datag, ZZ = Y, XX = X)
  renamed_eeg3 <- eeg_rename(datag, X = Y, Y = X)

  expect_equal(signal_tbl(renamed_eeg1), setNames(data$.signal, c(".id", ".sample", "X", "ZZ")))
  expect_equal(
    signal_tbl(renamed_eeg2),
    setNames(data$.signal, c(".id", ".sample", "XX", "ZZ"))
  )

  expect_equal(signal_tbl(renamed_eeg3), setNames(data$.signal, c(".id", ".sample", "Y", "X")))

  # in segments table
  expect_equal(segments_tbl(renamed_eeg1), segments_tbl(data))
  expect_equal(segments_tbl(renamed_eeg2), segments_tbl(data))
  expect_equal(segments_tbl(renamed_eeg3), segments_tbl(data))

  expect_equal(events_tbl(renamed_eeg1), events_tbl(data) %>%
    dplyr::mutate(
      .channel =
        ifelse(.channel == "Y", "ZZ", .channel)
    ))
  expect_equal(events_tbl(renamed_eeg2), events_tbl(data) %>%
    dplyr::mutate(
      .channel =
        ifelse(.channel == "Y", "ZZ", "XX")
    ))
  expect_equal(events_tbl(renamed_eeg3), events_tbl(data) %>%
    dplyr::mutate(
      .channel =
        ifelse(.channel == "Y", "X", "Y")
    ))
  expect_equal(is_channel_dbl(renamed_eeg1$.signal$ZZ), TRUE)
})


#### rename with
test_that("rename_with works", {
  expect_equal(
    eeg_rename_with(data, .fn = tolower, .cols = tidyselect::all_of("X")),
    eeg_rename(data, x = X)
  )
  expect_equal(
    eeg_rename_with(data, .fn = tolower, .cols = where(is_channel_dbl)),
    eeg_rename(data, x = X, y = Y)
  )
  expect_equal(
    eeg_rename_with(data, .fn = tolower),
    eeg_rename(data, x = X, y = Y)
  )
  expect_equal(
    eeg_rename_with(data, .fn = toupper, .cols = "segment"),
    eeg_rename(data, SEGMENT = segment)
  )
  expect_equal(
    eeg_rename_with(data %>% eeg_group_by(segment), .fn = toupper, .cols = c("segment")),
    eeg_rename(data %>% eeg_group_by(segment), SEGMENT = segment)
  )

  expect_equal(
    eeg_rename_with(data %>% eeg_group_by(segment, X, .id), .fn = toupper, .cols = c("segment")),
    eeg_rename(data %>% eeg_group_by(segment, X, .id), SEGMENT = segment)
  )
})



test_that("internal variables  show errors if attempted to modify", {
  expect_error(eeg_rename_with(data, .fn = toupper, .cols = ".id"))
  expect_error(eeg_rename_with(data, time = .sample))
  expect_error(eeg_rename_with(data, participant = .recording))
})


test_that("renaming in grouped segments table doesn't change data", {
  datag <- data %>% dplyr::group_by(.id, .recording)
  renamed_eeg4 <- eeg_rename(datag, cond = condition)
  expect_equal(signal_tbl(renamed_eeg4), data$.signal)
  expect_equal(events_tbl(renamed_eeg4), data$.events)
  expect_equal(segments_tbl(renamed_eeg4), setNames(data$.segments, c(".id", ".recording", "segment", "cond")))
  expect_equal(reference_data, data)
})

