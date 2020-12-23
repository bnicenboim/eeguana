context("test tidyverse functions rename")
library(eeguana)


# create fake dataset
data_1 <- eeguana:::data_sincos3id 

#TODO: to remove later
data_1 <- as_eeg_lst(data_1)

# just some different X and Y
data_2 <- dplyr::mutate(data_1,
  .recording = "recording2",
  X = sin(X + 10),
  Y = cos(Y - 10),
  condition = c("b", "a", "b")
)

# bind it all together
data  <- bind(data_1, data_2)

# for checks later
reference_data <- data.table::copy(data)

test_that("internal (?) variables should show warnings", {
  expect_warning(dplyr::rename(data, ID = .id))
  expect_warning(dplyr::rename(data, time = .sample))
  expect_warning(dplyr::rename(data, participant = .recording))
  expect_warning(events_tbl(data) <- events_tbl(data) %>% dplyr::rename(elec = .channel))
})

test_that("bad things throw errors", {
  expect_error(dplyr::rename(data, X = Y))
})

test_that("renaming a regular eeg_lst", {
  # in signal table
  renamed_eeg1 <- dplyr::rename(data, ZZ = Y) 
  renamed_eeg2 <- dplyr::rename(data, ZZ = Y,XX =X) 
  renamed_eeg3 <- dplyr::rename(data, X = Y,Y =X) 
   
  expect_equal(signal_tbl(renamed_eeg1), setNames(data$.signal, c(".id",".sample","X","ZZ")))
  expect_equal(signal_tbl(renamed_eeg2),
                    setNames(data$.signal, c(".id",".sample","XX","ZZ")))

  expect_equal(signal_tbl(renamed_eeg3), setNames(data$.signal, c(".id",".sample","Y","X")))

  # in segments table
  expect_equal(segments_tbl(renamed_eeg1), segments_tbl(data))
  expect_equal(segments_tbl(renamed_eeg2), segments_tbl(data))
  expect_equal(segments_tbl(renamed_eeg3), segments_tbl(data))

  expect_equal(events_tbl(renamed_eeg1),events_tbl(data) %>%
                                        dplyr::mutate(.channel =
                                                        ifelse(.channel =="Y","ZZ",.channel)))
  expect_equal(events_tbl(renamed_eeg2),events_tbl(data) %>%
                                             dplyr::mutate(.channel =
                                                             ifelse(.channel =="Y","ZZ","XX")))
  expect_equal(events_tbl(renamed_eeg3),events_tbl(data) %>%
                                        dplyr::mutate(.channel =
                                                        ifelse(.channel =="Y","X","Y")))
  expect_equal(is_channel_dbl(renamed_eeg1$.signal$ZZ), TRUE)
})

test_that("renaming in segments table doesn't change data", {
  renamed_eeg4 <- dplyr::rename(data, cond =condition) 
  expect_equal(signal_tbl(renamed_eeg4), data$.signal)
  expect_equal(events_tbl(renamed_eeg4), data$.events)
  expect_equal(segments_tbl(renamed_eeg4), setNames(data$.segments, c(".id",".recording","segment","cond")))
  expect_equal(reference_data, data)
})


test_that("renaming a grouped eeg_lst", {
  # in signal table
  datag <- data %>% dplyr::group_by(.id, .recording)
  renamed_eeg1 <- dplyr::rename(datag, ZZ = Y)
  renamed_eeg2 <- dplyr::rename(datag, ZZ = Y,XX =X)
  renamed_eeg3 <- dplyr::rename(datag, X = Y,Y =X)
   
  expect_equal(signal_tbl(renamed_eeg1), setNames(data$.signal, c(".id",".sample","X","ZZ")))
  expect_equal(signal_tbl(renamed_eeg2),
                    setNames(data$.signal, c(".id",".sample","XX","ZZ")))

  expect_equal(signal_tbl(renamed_eeg3), setNames(data$.signal, c(".id",".sample","Y","X")))

  # in segments table
  expect_equal(segments_tbl(renamed_eeg1), segments_tbl(data))
  expect_equal(segments_tbl(renamed_eeg2), segments_tbl(data))
  expect_equal(segments_tbl(renamed_eeg3), segments_tbl(data))

  expect_equal(events_tbl(renamed_eeg1),events_tbl(data) %>%
                                        dplyr::mutate(.channel =
                                                        ifelse(.channel =="Y","ZZ",.channel)))
  expect_equal(events_tbl(renamed_eeg2),events_tbl(data) %>%
                                             dplyr::mutate(.channel =
                                                             ifelse(.channel =="Y","ZZ","XX")))
  expect_equal(events_tbl(renamed_eeg3),events_tbl(data) %>%
                                        dplyr::mutate(.channel =
                                                        ifelse(.channel =="Y","X","Y")))
  expect_equal(is_channel_dbl(renamed_eeg1$.signal$ZZ), TRUE)
})

test_that("renaming in grouped segments table doesn't change data", {
  datag <- data %>% dplyr::group_by(.id, .recording)
  renamed_eeg4 <- dplyr::rename(datag, cond =condition) 
  expect_equal(signal_tbl(renamed_eeg4), data$.signal)
  expect_equal(events_tbl(renamed_eeg4), data$.events)
  expect_equal(segments_tbl(renamed_eeg4), setNames(data$.segments, c(".id",".recording","segment","cond")))
  expect_equal(reference_data, data)
})
