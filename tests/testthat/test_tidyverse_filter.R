context("test tidyverse functions filter")
library(eeguana)


# create fake dataset
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
<<<<<<< Updated upstream
<<<<<<< Updated upstream
  segments = dplyr::tibble(.id = c(1L, 2L, 3L), recording = "recording1", segment = c(1L, 2L, 3L), condition = c("a", "b", "a"))
)

# just some different X and Y
data_2 <- mutate(data_1, recording = "recording2", X = sin(X + 10), Y = cos(Y - 10), condition = c("b", "a", "b"))
=======
<<<<<<< Updated upstream
  segments = dplyr::tibble(.id = c(1L, 2L), recording = "recording1", segment = c(1L, 2L))
)

filter(data, .id ==1)
=======
  segments = dplyr::tibble(.id = c(1L, 2L, 3L), 
                           recording = "recording1", 
                           segment = c(1L, 2L, 3L), 
                           condition = c("a", "b", "a"))
)

# just some different X and Y
data_2 <- mutate(data_1, 
                 recording = "recording2", 
                 X = sin(X + 10), 
                 Y = cos(Y - 10), 
                 condition = c("b", "a", "b"))
>>>>>>> Stashed changes
>>>>>>> Stashed changes
=======
  segments = dplyr::tibble(.id = c(1L, 2L, 3L),
                           recording = "recording1",
                           segment = c(1L, 2L, 3L),
                           condition = c("a", "b", "a"))
)

# just some different X and Y
data_2 <- mutate(data_1, recording = "recording2",
                 X = sin(X + 10),
                 Y = cos(Y - 10),
                 condition = c("b", "a", "b"))
>>>>>>> Stashed changes

# bind it all together
data <- bind(data_1, data_2)


<<<<<<< Updated upstream
# merge the eeg_lst tables into a tibble for testing against
extended_data <- left_join(as_tibble(data$signal), data$segments, by = ".id") %>%
  left_join(., data$events, by = ".id") 
=======
<<<<<<< Updated upstream
filter(data, segment == 2 & .id ==2)
=======
# create tibbles from eeg_lst to test against
sigseg_data <- left_join(data$signal, data$segments)

signal_data <- as_tibble(data$signal)
segments_data <- as_tibble(data$segments)
events_data <- data$events %>% left_join(data$segments)

>>>>>>> Stashed changes
>>>>>>> Stashed changes

=======
>>>>>>> Stashed changes
# for checks later
reference_data <- data.table::copy(data)


<<<<<<< Updated upstream

# create tibbles from eeg_lst to test against

# this is super weird - sometimes this works, sometimes i get the error (underneath)
# sigseg_data <- left_join(data$signal, data$segments, by = ".id")
# evtseg_data <- left_join(data$events, data$segments, by = ".id")

# Error in as_tibble.signal_tbl(data, .name_repair = "check_unique") : 
#   unused argument (.name_repair = "check_unique") 

# if i get the error, this works instead, 
# but then all of the tests fail for .id and heaps of other stuff no longer works either
sigseg_data <- left_join(as_tibble(data$signal), data$segments, by = ".id")
evtseg_data <- left_join(as_tibble(data$events), data$segments, by = ".id")

signal_data <- as_tibble(data$signal)
segments_data <- as_tibble(data$segments)
events_data <-  as_tibble(data$events)
  


<<<<<<< Updated upstream
filter1_sign_eeg <- filter(data, .id ==1)
filter1_sign_tbl <- extended_data %>%
=======
<<<<<<< Updated upstream
mutate(data, time = as_time(.sample_id, unit = "milliseconds")) %>%
        filter(time == 2)
=======
=======

>>>>>>> Stashed changes

###################################################
### 1. Filtering by .id (applies to all tables) ###
###################################################

# a) Create eeg_lsts and tibbles with same filters and test against each other

filter1_id_eeg <- filter(data, .id == 1)

filter1_id_sign_tbl <- signal_data %>%
  dplyr::filter(.id == 1)
filter1_id_segm_tbl <- segments_data %>%
  dplyr::filter(.id == 1)
filter1_id_evts_tbl <- events_data %>%
<<<<<<< Updated upstream
>>>>>>> Stashed changes
=======
>>>>>>> Stashed changes
  as_tibble() %>%
  dplyr::filter(.id == 1)


<<<<<<< Updated upstream
<<<<<<< Updated upstream
filter2_sign_eeg <- filter(data, .sample_id >= 2)
filter2_sign_tbl <- extended_data %>%
=======
filter2_id_eeg <- filter(data, .id != 2)

filter2_id_sign_tbl <- signal_data %>%
  dplyr::filter(.id != 2)
filter2_id_segm_tbl <- segments_data %>%
  dplyr::filter(.id != 2)
filter2_id_evts_tbl <- events_data %>%
>>>>>>> Stashed changes
  as_tibble() %>%
  dplyr::filter(.id != 2)


filter3_id_eeg <- filter(data, .id == 3)

filter3_id_sign_tbl <- signal_data %>%
  dplyr::filter(.id == 3)
filter3_id_segm_tbl <- segments_data %>%
  dplyr::filter(.id == 3)
filter3_id_evts_tbl <- events_data %>%
  as_tibble() %>%
  dplyr::filter(.id == 3)


test_that("filtering within signal table returns correct values in signal table", {
  expect_equal(as.matrix(filter1_id_eeg$signal), as.matrix(filter1_id_sign_tbl))
  expect_equal(as.matrix(filter2_id_eeg$signal), as.matrix(filter2_id_sign_tbl))
  expect_equal(as.matrix(filter3_id_eeg$signal), as.matrix(filter3_id_sign_tbl))
})


test_that("filtering within signal table returns correct values in segments table", {
  expect_equal(as.matrix(filter1_id_eeg$segments), as.matrix(filter1_id_segm_tbl))
  expect_equal(as.matrix(filter2_id_eeg$segments), as.matrix(filter2_id_segm_tbl))
  expect_equal(as.matrix(filter3_id_eeg$segments), as.matrix(filter3_id_segm_tbl))
})


test_that("filtering within signal table returns correct values in events table", {
  expect_equal(as.matrix(filter1_id_eeg$events), as.matrix(filter1_id_evts_tbl))
  expect_equal(as.matrix(filter2_id_eeg$events), as.matrix(filter2_id_evts_tbl))
  expect_equal(as.matrix(filter3_id_eeg$events), as.matrix(filter3_id_evts_tbl))
})


test_that("the classes of channels of signal_tbl remain after within eeg_lst table", {
  expect_equal(is_channel_dbl(filter1_id_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter2_id_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter3_id_eeg$signal$X), TRUE)
})


# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})




##############################################
### 2. Filtering by signal table variables ###
##############################################

# a) Test signal/segments table by comparing eeg_lst with tibble

filter1_sign_eeg <- filter(data, .sample_id >= 2) 
filter1_sign_tbl <- sigseg_data %>%
  dplyr::filter(.sample_id >= 2) 


filter2_sign_eeg <- filter(data, .id == 1 & .sample_id == 2)
filter2_sign_tbl <- sigseg_data %>%
  dplyr::filter(.id == 1 & .sample_id == 2)


filter3_sign_eeg <- filter(data, X < 0 & Y < 0)
filter3_sign_tbl <- sigseg_data %>%
  dplyr::filter(X < 0 & Y < 0)


test_that("filtering within signal table works in signal table", {
  expect_equal(as.matrix(filter1_sign_eeg$signal), 
               as.matrix(select(filter1_sign_tbl, .id, .sample_id, X, Y)))
  expect_equal(as.matrix(filter2_sign_eeg$signal), 
               as.matrix(select(filter2_sign_tbl, .id, .sample_id, X, Y)))
  expect_equal(as.matrix(filter3_sign_eeg$signal), 
               as.matrix(select(filter3_sign_tbl, .id, .sample_id, X, Y)))
})


test_that("filtering within signal table works in segments table", {
  expect_setequal(as.matrix(filter1_sign_eeg$segments), 
               as.matrix(select(filter1_sign_tbl, .id, recording, segment, condition)))
  expect_setequal(as.matrix(filter2_sign_eeg$segments), 
               as.matrix(select(filter2_sign_tbl, .id, recording, segment, condition)))
  expect_setequal(as.matrix(filter3_sign_eeg$segments),  
               as.matrix(select(filter3_sign_tbl, .id, recording, segment, condition)))
})


# b) To test the events table, create some opposing filters & test that they 
# don't return same events

# add events data to filtered eeg_lsts from above
filter1a_sign <- left_join(as_tibble(filter1_sign_eeg$signal), evtseg_data)
filter2a_sign <- left_join(as_tibble(filter2_sign_eeg$signal), evtseg_data)
filter3a_sign <- left_join(as_tibble(filter3_sign_eeg$signal), evtseg_data)


# apply opposite filter
filter1b_sign_eeg <- filter(data, .sample_id < 2) 
filter1b_sign <- left_join(as_tibble(filter1b_sign_eeg$signal), evtseg_data)

filter2b_sign_eeg <- filter(data, .id != 1 & .sample_id != 2)
filter2b_sign <- left_join(as_tibble(filter2b_sign_eeg$signal), evtseg_data)

filter3b_sign_eeg <- filter(data, X >= 0 & Y >= 0)
filter3b_sign <- left_join(as_tibble(filter3b_sign_eeg$signal), evtseg_data)


test_that("opposite filters return no matching events", {
  expect_true(nrow(semi_join(filter1a_sign, filter1b_sign)) == 0)
  expect_true(nrow(semi_join(filter2a_sign, filter2b_sign)) == 0)
  expect_true(nrow(semi_join(filter3a_sign, filter3b_sign)) == 0)
})


# and merging xa and xb should equal original data
filter1c_sign <- left_join(filter1_sign_eeg$events, filter1b_sign_eeg$events)
# this only keeps .id == 1, why aren't the others merged?
filter2c_sign <- left_join(filter2_sign_eeg$events, filter2b_sign_eeg$events)
filter3c_sign <- left_join(filter3_sign_eeg$events, filter3b_sign_eeg$events)


test_that("merging xa and xb recovers original events table", {
  expect_equal(as.matrix(filter1c_sign), as.matrix(data$events))
  expect_equal(as.matrix(filter2c_sign), as.matrix(data$events))
  expect_equal(as.matrix(filter3c_sign), as.matrix(data$events))
})


# filtered eeg_lst and tibble should match
filter1a_sign_tbl <- left_join(filter1_sign_tbl, evtseg_data)
filter2a_sign_tbl <- left_join(filter2_sign_tbl, evtseg_data)
filter3a_sign_tbl <- left_join(filter3_sign_tbl, evtseg_data)


test_that("the eeg_lst filter returns the same events as as_tibble", {
  expect_setequal(as.matrix(filter1a_sign_tbl), as.matrix(filter1a_sign))
  expect_setequal(as.matrix(filter2a_sign_tbl), as.matrix(filter2a_sign))
  expect_setequal(as.matrix(filter3a_sign_tbl), as.matrix(filter3a_sign))
})


test_that("the classes of channels of signal_tbl remain after within eeg_lst table", {
  expect_equal(is_channel_dbl(filter1_sign_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter2_sign_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter3_sign_eeg$signal$X), TRUE)
<<<<<<< Updated upstream
  expect_equal(is_channel_dbl(filter4_segm_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter5_segm_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter6_segm_eeg$signal$X), TRUE)
=======
filter2_id_eeg <- filter(data, .id != 2)

filter2_id_sign_tbl <- signal_data %>%
  dplyr::filter(.id != 2)
filter2_id_segm_tbl <- segments_data %>%
  dplyr::filter(.id != 2)
filter2_id_evts_tbl <- events_data %>%
  as_tibble() %>%
  dplyr::filter(.id != 2)


filter3_id_eeg <- filter(data, .id == 3)

filter3_id_sign_tbl <- signal_data %>%
  dplyr::filter(.id == 3)
filter3_id_segm_tbl <- segments_data %>%
  dplyr::filter(.id == 3)
filter3_id_evts_tbl <- events_data %>%
  as_tibble() %>%
  dplyr::filter(.id == 3)


test_that("filtering within signal table returns correct values in signal table", {
  expect_equal(as.matrix(filter1_id_eeg$signal), as.matrix(filter1_id_sign_tbl))
  expect_equal(as.matrix(filter2_id_eeg$signal), as.matrix(filter2_id_sign_tbl))
  expect_equal(as.matrix(filter3_id_eeg$signal), as.matrix(filter3_id_sign_tbl))
})


test_that("filtering within signal table returns correct values in segments table", {
  expect_equal(as.matrix(filter1_id_eeg$segments), as.matrix(filter1_id_segm_tbl))
  expect_equal(as.matrix(filter2_id_eeg$segments), as.matrix(filter2_id_segm_tbl))
  expect_equal(as.matrix(filter3_id_eeg$segments), as.matrix(filter3_id_segm_tbl))
})


# ooh this doesn't work after the ids update:
test_that("filtering within signal table returns correct values in events table", {
  expect_equal(as.matrix(filter1_id_eeg$events), as.matrix(filter1_id_evts_tbl))
  expect_equal(as.matrix(filter2_id_eeg$events), as.matrix(filter2_id_evts_tbl))
  expect_equal(as.matrix(filter3_id_eeg$events), as.matrix(filter3_id_evts_tbl))
})


test_that("the classes of channels of signal_tbl remain after within eeg_lst table", {
  expect_equal(is_channel_dbl(filter1_id_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter2_id_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter3_id_eeg$signal$X), TRUE)
})


# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})




##############################################
### 2. Filtering by signal table variables ###
##############################################

# a) Test signal/segments table by comparing eeg_lst with tibble

filter1_sign_eeg <- filter(data, .sample_id >= 2) 
filter1_sign_tbl <- sigseg_data %>%
  dplyr::filter(.sample_id >= 2) 


filter2_sign_eeg <- filter(data, .id == 1 & .sample_id == 2)
filter2_sign_tbl <- sigseg_data %>%
  dplyr::filter(.id == 1 & .sample_id == 2)


filter3_sign_eeg <- filter(data, X < 0 & Y < 0)
filter3_sign_tbl <- sigseg_data %>%
  dplyr::filter(X < 0 & Y < 0)


test_that("filtering within signal table works in signal table", {
  expect_equal(as.matrix(filter1_sign_eeg$signal), 
               as.matrix(select(filter1_sign_tbl, .id, .sample_id, X, Y)))
  expect_equal(as.matrix(filter2_sign_eeg$signal), 
               as.matrix(select(filter2_sign_tbl, .id, .sample_id, X, Y)))
  expect_equal(as.matrix(filter3_sign_eeg$signal), 
               as.matrix(select(filter3_sign_tbl, .id, .sample_id, X, Y)))
})


test_that("filtering within signal table works in segments table", {
  expect_equal(as.matrix(filter1_sign_eeg$segments), 
               as.matrix(distinct(filter1_sign_tbl, .id, recording, segment, condition)))
  expect_equal(as.matrix(filter2_sign_eeg$segments), 
               as.matrix(distinct(filter2_sign_tbl, .id, recording, segment, condition)))
  expect_equal(as.matrix(filter3_sign_eeg$segments),  
               as.matrix(distinct(filter3_sign_tbl, .id, recording, segment, condition)))
})


# b) To test the events table, create some opposing filters & test that they 
# don't return same events

# add events data to filtered eeg_lsts from above
filter1a <- left_join(filter1_sign_eeg$signal, events_data)
filter2a <- left_join(filter2_sign_eeg$signal, events_data)
filter3a <- left_join(filter3_sign_eeg$signal, events_data)


# apply opposite filter
filter1b_sign_eeg <- filter(data, .sample_id < 2) 
filter1b <- left_join(filter1b_sign_eeg$signal, events_data)

filter2b_sign_eeg <- filter(data, X <= 0)
filter2b <- left_join(filter2b_sign_eeg$signal, events_data)

filter3b_sign_eeg <- filter(data, .sample_id >= 0)
filter3b <- left_join(filter3b_sign_eeg$signal, events_data)


test_that("opposite filters return no matching events", {
  expect_true(nrow(semi_join(filter1a, filter1b)) == 0)
  expect_true(nrow(semi_join(filter2a, filter2b)) == 0)
  # why different?
  expect_true(nrow(semi_join(filter3a, filter3b)) == 0)
})


# and merging xa and xb should equal original data
filter1c <- left_join(filter1a_sign_eeg$events, filter1b_sign_eeg$events)
filter2c <- left_join(filter2a_sign_eeg$events, filter2b_sign_eeg$events)
filter3c <- left_join(filter3a_sign_eeg$events, filter3b_sign_eeg$events)


test_that("merging xa and xb recovers original events table", {
  expect_equal(as.matrix(filter1c), as.matrix(data$events))
  expect_equal(as.matrix(filter2c), as.matrix(data$events))
  expect_equal(as.matrix(filter3c), as.matrix(data$events))
})


# filtered eeg_lst and tibble should match
filter1a_tbl <- left_join(filter1_sign_tbl, events_data)
filter2a_tbl <- left_join(filter2_sign_tbl, events_data)
filter3a_tbl <- left_join(filter3_sign_tbl, events_data)


test_that("the eeg_lst filter returns the same events as as_tibble", {
  expect_equal(select(filter1a_tbl, sort(current_vars())),
               select(filter1a, sort(current_vars())))
  expect_equal(select(filter2a_tbl, sort(current_vars())),
               select(filter2a, sort(current_vars())))
  expect_equal(select(filter3a_tbl, sort(current_vars())),
               select(filter3a, sort(current_vars())))
})


test_that("the classes of channels of signal_tbl remain after within eeg_lst table", {
  expect_equal(is_channel_dbl(filter1a_sign_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter2a_sign_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter3a_sign_eeg$signal$X), TRUE)
})

# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})



################################################
### 3. Filtering by segments table variables ###
################################################

# a) Test all tables by comparing eeg_lst with tibble

# warnings about ids here - happens most often when filtering by segments (but not always)
filter1_segm_eeg <- filter(data, segment != 2)

filter1s_segm_tbl <- sigseg_data %>%
  as_tibble() %>%
  dplyr::filter(segment != 2)

filter1e_segm_tbl <- events_data %>%
  as_tibble() %>%
  dplyr::filter(segment != 2) %>%
  dplyr::distinct(.id, type, description, .sample_0, .size, .channel)


filter2_segm_eeg <- filter(data, condition == "a" & segment == 3)

filter2s_segm_tbl <- sigseg_data %>%
  as_tibble() %>%
  dplyr::filter(condition == "a" & segment == 3)

filter2e_segm_tbl <- events_data %>%
  as_tibble() %>%
  dplyr::filter(condition == "a" & segment == 3) %>%
  dplyr::distinct(.id, type, description, .sample_0, .size, .channel)


filter3_segm_eeg <- filter(data, recording == "recording2")

filter3s_segm_tbl <- sigseg_data %>%
  as_tibble() %>%
  filter(recording == "recording2") 

filter3e_segm_tbl <- events_data %>%
  as_tibble() %>%
  filter(recording == "recording2") %>%
  select(.id, type, description, .sample_0, .size, .channel)


test_that("filtering within segments table works in signal table", {
  expect_equal(as.matrix(filter1_segm_eeg$signal), 
               as.matrix(select(filter1s_segm_tbl, .id, .sample_id, X, Y)))
  expect_equal(as.matrix(filter2_segm_eeg$signal), 
               as.matrix(select(filter2s_segm_tbl, .id, .sample_id, X, Y)))
  expect_equal(as.matrix(filter3_segm_eeg$signal), 
               as.matrix(select(filter3s_segm_tbl, .id, .sample_id, X, Y)))
})


test_that("filtering within segments table works in segments table", {
  expect_equal(as.matrix(filter1_segm_eeg$segments), 
               as.matrix(distinct(filter1s_segm_tbl, .id, recording, segment, condition)))
  expect_equal(as.matrix(filter2_segm_eeg$segments), 
               as.matrix(distinct(filter2s_segm_tbl, .id, recording, segment, condition)))
  expect_equal(as.matrix(filter3_segm_eeg$segments), 
               as.matrix(distinct(filter3s_segm_tbl, .id, recording, segment, condition)))
})


test_that("filtering within segments table returns correct values in events table", {
  expect_equal(as.matrix(filter1_segm_eeg$events), 
               as.matrix(filter1e_segm_tbl))
  expect_equal(as.matrix(filter2_segm_eeg$events), 
               as.matrix(filter2e_segm_tbl))
  expect_equal(as.matrix(filter3_segm_eeg$events), 
               as.matrix(filter3e_segm_tbl))
  })


test_that("the classes of channels of signal_tbl remain after within eeg_lst table", {
  expect_equal(is_channel_dbl(filter1_segm_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter2_segm_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter3_segm_eeg$signal$X), TRUE)
>>>>>>> Stashed changes
=======
>>>>>>> Stashed changes
})

# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})



<<<<<<< Updated upstream
<<<<<<< Updated upstream
### test filter by variables across eeg_lst tables ###
=======
################################################
### 3. Filtering by segments table variables ###
################################################
>>>>>>> Stashed changes

# a) Test all tables by comparing eeg_lst with tibble

# warnings about ids here - happens most often when filtering by segments (but not always)
filter1_segm_eeg <- filter(data, segment != 2)

filter1s_segm_tbl <- sigseg_data %>%
  as_tibble() %>%
  dplyr::filter(segment != 2)

filter1e_segm_tbl <- evtseg_data %>%
  as_tibble() %>%
  dplyr::filter(segment != 2) %>%
  dplyr::distinct(.id, type, description, .sample_0, .size, .channel)


filter2_segm_eeg <- filter(data, condition == "a" & segment == 3)

filter2s_segm_tbl <- sigseg_data %>%
  as_tibble() %>%
  dplyr::filter(condition == "a" & segment == 3)

filter2e_segm_tbl <- evtseg_data %>%
  as_tibble() %>%
  dplyr::filter(condition == "a" & segment == 3) %>%
  dplyr::distinct(.id, type, description, .sample_0, .size, .channel)


filter3_segm_eeg <- filter(data, recording == "recording2")

filter3s_segm_tbl <- sigseg_data %>%
  as_tibble() %>%
  filter(recording == "recording2") 

filter3e_segm_tbl <- evtseg_data %>%
  as_tibble() %>%
  filter(recording == "recording2") %>%
  select(.id, type, description, .sample_0, .size, .channel)


test_that("filtering within segments table works in signal table", {
  expect_equal(as.matrix(filter1_segm_eeg$signal), 
               as.matrix(select(filter1s_segm_tbl, .id, .sample_id, X, Y)))
  expect_equal(as.matrix(filter2_segm_eeg$signal), 
               as.matrix(select(filter2s_segm_tbl, .id, .sample_id, X, Y)))
  expect_equal(as.matrix(filter3_segm_eeg$signal), 
               as.matrix(select(filter3s_segm_tbl, .id, .sample_id, X, Y)))
})


test_that("filtering within segments table works in segments table", {
  expect_setequal(as.matrix(filter1_segm_eeg$segments), 
               as.matrix(select(filter1s_segm_tbl, .id, recording, segment, condition)))
  expect_setequal(as.matrix(filter2_segm_eeg$segments), 
               as.matrix(select(filter2s_segm_tbl, .id, recording, segment, condition)))
  expect_setequal(as.matrix(filter3_segm_eeg$segments), 
               as.matrix(select(filter3s_segm_tbl, .id, recording, segment, condition)))
})


test_that("filtering within segments table returns correct values in events table", {
  expect_equal(as.matrix(filter1_segm_eeg$events), 
               as.matrix(filter1e_segm_tbl))
  expect_equal(as.matrix(filter2_segm_eeg$events), 
               as.matrix(filter2e_segm_tbl))
  expect_equal(as.matrix(filter3_segm_eeg$events), 
               as.matrix(filter3e_segm_tbl))
  })


test_that("the classes of channels of signal_tbl remain after within eeg_lst table", {
  expect_equal(is_channel_dbl(filter1_segm_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter2_segm_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter3_segm_eeg$signal$X), TRUE)
})


# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})




#########################################################
### 4. Test filter by variables across eeg_lst tables ###
#########################################################

# a) Test signal/segments tables by comparing eeg_lst with tibble

filter1_eeg <- filter(data, .sample_id == 2 & segment == 2)

filter1_tbl <- sigseg_data %>% 
  dplyr::filter(.sample_id == 2 & segment == 2)

# just checking this is the same as above
# filter1_tbl <- data %>%
#   as_tibble() %>%
#   filter(time == 0.002 & segment == 2) %>%
#   spread(key = channel, value = amplitude)


filter2_eeg <- filter(data, .sample_id == 2 & !(recording == "recording2"))
filter2_tbl <- sigseg_data %>%
  dplyr::filter(.sample_id == 2 & !(recording == "recording2"))

# neither filter has worked?
filter3_eeg <- filter(data, .sample_id == 1 | condition == "a")
filter3_tbl <- sigseg_data %>%
  dplyr::filter(.sample_id == 1 | condition == "a") 

# only condition filter has worked?
filter4_eeg <- filter(data, .id == 2 | condition == "b")
filter4_tbl <- sigseg_data %>%
  dplyr::filter(.id == 2 | condition == "b") 


filter5_eeg <- filter(data, between(X, 0, 0.5) & segment != 1)
filter5_tbl <- sigseg_data %>%
  dplyr::filter(between(X, 0, 0.5) & segment != 1)


filter6_eeg <- filter(data, Y > 0 & recording == "recording1")
filter6_tbl <- sigseg_data %>%
  dplyr::filter(Y > 0 & recording == "recording1")


test_that("filtering across tables returns the right signal table values", {
  expect_setequal(as.matrix(filter1_eeg$signal), 
               as.matrix(select(filter1_tbl, .id, .sample_id, X, Y)))
  expect_setequal(as.matrix(filter2_eeg$signal), 
               as.matrix(select(filter2_tbl, .id, .sample_id, X, Y)))
  expect_setequal(as.matrix(filter3_eeg$signal), 
               as.matrix(select(filter3_tbl, .id, .sample_id, X, Y)))
  expect_setequal(as.matrix(filter4_eeg$signal), 
               as.matrix(select(filter4_tbl, .id, .sample_id, X, Y)))
  expect_setequal(as.matrix(filter5_eeg$signal), 
               as.matrix(select(filter5_tbl, .id, .sample_id, X, Y)))
  expect_setequal(as.matrix(filter6_eeg$signal), 
               as.matrix(select(filter6_tbl, .id, .sample_id, X, Y)))
})


test_that("filtering across tables returns the right segments table values", {
  expect_setequal(as.matrix(filter1_eeg$segments), 
               as.matrix(select(filter1_tbl, .id, recording, segment, condition)))
  expect_setequal(as.matrix(filter2_eeg$segments), 
               as.matrix(select(filter2_tbl, .id, recording, segment, condition)))
  expect_setequal(as.matrix(filter3_eeg$segments), 
               as.matrix(select(filter3_tbl, .id, recording, segment, condition)))
  expect_setequal(as.matrix(filter4_eeg$segments), 
               as.matrix(select(filter4_tbl, .id, recording, segment, condition)))
  expect_setequal(as.matrix(filter5_eeg$segments), 
               as.matrix(select(filter5_tbl, .id, recording, segment, condition)))
  expect_setequal(as.matrix(filter6_eeg$segments), 
               as.matrix(select(filter6_tbl, .id, recording, segment, condition)))
})


# b) To test the events table, create some opposing filters & test that they 
# don't return same events

# add events data to above filters
filter1a_evts <- left_join(filter1_eeg$signal, evtseg_data, by = ".id")
filter2a_evts <- left_join(filter2_eeg$signal, evtseg_data, by = ".id")
filter3a_evts <- left_join(filter3_eeg$signal, evtseg_data, by = ".id")
filter4a_evts <- left_join(filter4_eeg$signal, evtseg_data, by = ".id")
filter5a_evts <- left_join(filter5_eeg$signal, evtseg_data, by = ".id")
filter6a_evts <- left_join(filter6_eeg$signal, evtseg_data, by = ".id")


# apply the opposite filter
filter1b_evts_eeg <- filter(data, .sample_id != 2 & segment != 2) 
filter1b_evts <- left_join(filter1b_evts_eeg$signal, evtseg_data, by = ".id")

filter2b_evts_eeg <- filter(data, .sample_id != 2 & recording == "recording2")
filter2b_evts <- left_join(filter2b_evts_eeg$signal, evtseg_data, by = ".id")

filter3b_evts_eeg <- filter(data, .sample_id != 1 | condition != "a")
filter3b_evts <- left_join(filter3b_evts_eeg$signal, evtseg_data, by = ".id")

filter4b_evts_eeg <- filter(data, .id != 2 | condition != "b")
filter4b_evts <- left_join(filter4b_evts_eeg$signal, evtseg_data, by = ".id")

filter5b_evts_eeg <- filter(data, X <= 0 | X >= 0.5 & segment == 1)
filter5b_evts <- left_join(filter5b_evts_eeg$signal, evtseg_data, by = ".id")

filter6b_evts_eeg <- filter(data, Y <= 0 & recording != "recording1")
filter6b_evts <- left_join(filter6b_evts_eeg$signal, evtseg_data, by = ".id")


test_that("what's not in xb matches xc", {
  expect_true(nrow(semi_join(filter1a_evts, filter1b_evts)) == 0)
  expect_true(nrow(semi_join(filter2a_evts, filter2b_evts)) == 0)
  # due to issue with 3a
  expect_true(nrow(semi_join(filter3a_evts, filter3b_evts)) == 0)
  # due to issue with 4a
  expect_true(nrow(semi_join(filter4a_evts, filter4b_evts)) == 0)
  expect_true(nrow(semi_join(filter5a_evts, filter5b_evts)) == 0)
  expect_true(nrow(semi_join(filter6a_evts, filter6b_evts)) == 0)
})


# and merging xa and xb should equal original data
# only the ids for x are kept, not for y (doesn't matter for 3 because first filter didn't work)
filter1c <- left_join(filter1_eeg$events, filter1b_evts_eeg$events)
filter2c <- left_join(filter2_eeg$events, filter2b_evts_eeg$events)
filter3c <- left_join(filter3_eeg$events, filter3b_evts_eeg$events)
filter4c <- left_join(filter4_eeg$events, filter4b_evts_eeg$events)
filter5c <- left_join(filter5_eeg$events, filter5b_evts_eeg$events)
filter6c <- left_join(filter6_eeg$events, filter6b_evts_eeg$events)


test_that("merging opposite filters recovers original events", {
  expect_equal(as.matrix(filter1c), as.matrix(data$events))
  expect_equal(as.matrix(filter2c), as.matrix(data$events))
  expect_equal(as.matrix(filter3c), as.matrix(data$events))
  expect_equal(as.matrix(filter4c), as.matrix(data$events))
  expect_equal(as.matrix(filter5c), as.matrix(data$events))
  expect_equal(as.matrix(filter6c), as.matrix(data$events))
})


<<<<<<< Updated upstream
# ids are different except for 3
test_that("filtering across eeg_lst tables returns the right events table values", {
  expect_equal(as.matrix(filter1_eeg$events), as.matrix(distinct(select(filter1_tbl, .id, type, description, .sample_0, .size, .channel))))
  expect_equal(as.matrix(filter2_eeg$events), as.matrix(distinct(select(filter2_tbl, .id, type, description, .sample_0, .size, .channel))))
  expect_equal(as.matrix(filter3_eeg$events), as.matrix(distinct(select(filter3_tbl, .id, type, description, .sample_0, .size, .channel))))
  expect_equal(as.matrix(filter4_eeg$events), as.matrix(distinct(select(filter4_tbl, .id, type, description, .sample_0, .size, .channel))))
  expect_equal(as.matrix(filter5_eeg$events), as.matrix(distinct(select(filter5_tbl, .id, type, description, .sample_0, .size, .channel))))
=======

#########################################################
### 4. Test filter by variables across eeg_lst tables ###
#########################################################

# a) Test signal/segments tables by comparing eeg_lst with tibble

filter1_eeg <- filter(data, .sample_id == 2 & segment == 2)

filter1_tbl <- sigseg_data %>% 
  dplyr::filter(.sample_id == 2 & segment == 2)

# just checking this is the same as above
# filter1_tbl <- data %>%
#   as_tibble() %>%
#   filter(time == 0.002 & segment == 2) %>%
#   spread(key = channel, value = amplitude)


filter2_eeg <- filter(data, .sample_id == 2 & !(recording == "recording2"))
filter2_tbl <- sigseg_data %>%
  dplyr::filter(.sample_id == 2 & !(recording == "recording2"))

# neither filter has worked?
filter3_eeg <- filter(data, .sample_id == 1 | condition == "a")
filter3_tbl <- sigseg_data %>%
  dplyr::filter(.sample_id == 1 | condition == "a") 

# only condition filter has worked?
filter4_eeg <- filter(data, .id == 2 | condition == "b")
filter4_tbl <- sigseg_data %>%
  dplyr::filter(.id == 2 | condition == "b") 


filter5_eeg <- filter(data, between(X, 0, 0.5) & segment != 1)
filter5_tbl <- sigseg_data %>%
  dplyr::filter(between(X, 0, 0.5) & segment != 1)


filter6_eeg <- filter(data, Y > 0 & recording == "recording1")
filter6_tbl <- sigseg_data %>%
  dplyr::filter(Y > 0 & recording == "recording1")


test_that("filtering across tables returns the right signal table values", {
  expect_equal(as.matrix(filter1_eeg$signal), 
               as.matrix(distinct(filter1_tbl, .id, .sample_id, X, Y)))
  expect_equal(as.matrix(filter2_eeg$signal), 
               as.matrix(distinct(filter2_tbl, .id, .sample_id, X, Y)))
  expect_equal(as.matrix(filter3_eeg$signal), 
               as.matrix(distinct(filter3_tbl, .id, .sample_id, X, Y)))
  expect_equal(as.matrix(filter4_eeg$signal), 
               as.matrix(distinct(filter4_tbl, .id, .sample_id, X, Y)))
  expect_equal(as.matrix(filter5_eeg$signal), 
               as.matrix(distinct(filter5_tbl, .id, .sample_id, X, Y)))
  expect_equal(as.matrix(filter6_eeg$signal), 
               as.matrix(distinct(filter6_tbl, .id, .sample_id, X, Y)))
})


test_that("filtering across tables returns the right segments table values", {
  expect_equal(as.matrix(filter1_eeg$segments), 
               as.matrix(distinct(filter1_tbl, .id, recording, segment, condition)))
  expect_equal(as.matrix(filter2_eeg$segments), 
               as.matrix(distinct(filter2_tbl, .id, recording, segment, condition)))
  expect_equal(as.matrix(filter3_eeg$segments), 
               as.matrix(distinct(filter3_tbl, .id, recording, segment, condition)))
  expect_equal(as.matrix(filter4_eeg$segments), 
               as.matrix(distinct(filter4_tbl, .id, recording, segment, condition)))
  expect_equal(as.matrix(filter5_eeg$segments), 
               as.matrix(distinct(filter5_tbl, .id, recording, segment, condition)))
  expect_equal(as.matrix(filter6_eeg$segments), 
               as.matrix(distinct(filter6_tbl, .id, recording, segment, condition)))
})


# b) To test the events table, create some opposing filters & test that they 
# don't return same events

# add events data to above filters
filter1a_evts <- left_join(filter1_eeg$signal, events_data, by = ".id")
filter2a_evts <- left_join(filter2_eeg$signal, events_data, by = ".id")
filter3a_evts <- left_join(filter3_eeg$signal, events_data, by = ".id")
filter4a_evts <- left_join(filter4_eeg$signal, events_data, by = ".id")
filter5a_evts <- left_join(filter5_eeg$signal, events_data, by = ".id")
filter6a_evts <- left_join(filter5_eeg$signal, events_data, by = ".id")


# apply the opposite filter
filter1b_evts <- filter(data, .sample_id != 2 & segment != 2) 
filter1b_evts <- left_join(filter1b_evts_eeg$signal, events_data, by = ".id")

filter2b_evts <- filter(data, .sample_id != 2 & recording == "recording2")
filter2b_evts <- left_join(filter2b_evts_eeg$signal, events_data, by = ".id")

filter3b_evts <- filter(data, .sample_id != 1 | condition != "a")
filter3b_evts <- left_join(filter3b_evts_eeg$signal, events_data, by = ".id")

filter4b_evts <- filter(data, .id != 2 | condition != "b")
filter4b_evts <- left_join(filter4b_evts_eeg$signal, events_data, by = ".id")

filter5b_evts <- filter(data, X <= 0 | X >= 0.5 & segment == 1)
filter5b_evts <- left_join(filter5b_evts_eeg$signal, events_data, by = ".id")

filter6b_evts <- filter(data, Y <= 0 & recording != "recording1")
filter6b_evts <- left_join(filter5b_evts_eeg$signal, events_data, by = ".id")


test_that("what's not in xb matches xc", {
  expect_true(nrow(semi_join(filter1a_evts, filter1b_evts)) == 0)
  expect_true(nrow(semi_join(filter2a_evts, filter2b_evts)) == 0)
  expect_true(nrow(semi_join(filter3a_evts, filter3b_evts)) == 0)
  expect_true(nrow(semi_join(filter4a_evts, filter4b_evts)) == 0)
  expect_true(nrow(semi_join(filter5a_evts, filter5b_evts)) == 0)
  expect_true(nrow(semi_join(filter6a_evts, filter6b_evts)) == 0)
})


# and merging xa and xb should equal original data
filter1c <- left_join(filter1a_eeg$events, filter1b_evts$events)
filter2c <- left_join(filter2a_eeg$events, filter2b_evts$events)
filter3c <- left_join(filter3a_eeg$events, filter3b_evts$events)
filter4c <- left_join(filter4a_eeg$events, filter4b_evts$events)
filter5c <- left_join(filter5a_eeg$events, filter5b_evts$events)
filter6c <- left_join(filter6a_eeg$events, filter6b_evts$events)


# hmm.
test_that("merging opposite filters recovers original events", {
  # all different lengths except 3
  expect_equal(as.matrix(filter1c), as.matrix(data$events))
  expect_equal(as.matrix(filter2c), as.matrix(data$events))
  expect_equal(as.matrix(filter3c), as.matrix(data$events))
  expect_equal(as.matrix(filter4c), as.matrix(data$events))
  expect_equal(as.matrix(filter5c), as.matrix(data$events))
  expect_equal(as.matrix(filter6c), as.matrix(data$events))
})


# check that events match the as_tibble version
filter1a_tbl <- left_join(filter1_tbl, events_data)
filter2a_tbl <- left_join(filter2_tbl, events_data)
filter3a_tbl <- left_join(filter3_tbl, events_data)
filter4a_tbl <- left_join(filter4_tbl, events_data)
filter5a_tbl <- left_join(filter5_tbl, events_data)
filter6a_tbl <- left_join(filter6_tbl, events_data)


test_that("the eeg_lst filter returns the same events as as_tibble", {
  expect_equal(select(filter1a_evts, sort(current_vars())),
               select(filter1a_tbl, sort(current_vars())))
  expect_equal(select(filter2a_evts, sort(current_vars())),
               select(filter2a_tbl, sort(current_vars())))
  expect_equal(select(filter3a_evts, sort(current_vars())),
               select(filter3a_tbl, sort(current_vars())))
  expect_equal(select(filter4a_evts, sort(current_vars())),
               select(filter4a_tbl, sort(current_vars())))
  expect_equal(select(filter5a_evts, sort(current_vars())),
               select(filter5a_tbl, sort(current_vars())))
  # hmm.
  expect_equal(select(filter6a_evts, sort(current_vars())),
               select(filter6a_tbl, sort(current_vars())))
>>>>>>> Stashed changes
=======
# check that events match the as_tibble version
filter1a_tbl <- left_join(filter1_tbl, evtseg_data)
filter2a_tbl <- left_join(filter2_tbl, evtseg_data)
filter3a_tbl <- left_join(filter3_tbl, evtseg_data)
filter4a_tbl <- left_join(filter4_tbl, evtseg_data)
filter5a_tbl <- left_join(filter5_tbl, evtseg_data)
filter6a_tbl <- left_join(filter6_tbl, evtseg_data)


test_that("the eeg_lst filter returns the same events as as_tibble", {
  expect_setequal(as.matrix(filter1a_evts), as.matrix(filter1a_tbl))
  expect_setequal(as.matrix(filter2a_evts), as.matrix(filter2a_tbl))
  expect_setequal(as.matrix(filter3a_evts), as.matrix(filter3a_tbl))
  expect_setequal(as.matrix(filter4a_evts), as.matrix(filter4a_tbl))
  expect_setequal(as.matrix(filter5a_evts), as.matrix(filter5a_tbl))
  expect_setequal(as.matrix(filter6a_evts), as.matrix(filter6a_tbl))
>>>>>>> Stashed changes
})


test_that("the classes of channels of signal_tbl remain after filtering across eeg_lst tables", {
  expect_equal(is_channel_dbl(filter1_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter2_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter3_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter4_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter5_eeg$signal$X), TRUE)
<<<<<<< Updated upstream
<<<<<<< Updated upstream
=======
  expect_equal(is_channel_dbl(filter6_eeg$signal$X), TRUE)
>>>>>>> Stashed changes
=======
  expect_equal(is_channel_dbl(filter6_eeg$signal$X), TRUE)
>>>>>>> Stashed changes
})


# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})




<<<<<<< Updated upstream
<<<<<<< Updated upstream
### what's going on with the ids? ###
tibble_data <- as_tibble(data)

# i suppose the lengths are different because of the as_time() thing? i.e. extra time points in var "time" vs. ".sample_id"
expect_equal(length(tibble_data), length(extended_data))

# check actual ids against each other
tbl <- tibble_data %>% filter(channel=="X") %>% select(.id, amplitude) %>% rename(X = amplitude)
ext <- distinct(select(extended_data, .id, X))

# the ids are the same, the attributes not
expect_equal(tbl, ext)
anti_join(tbl, ext)
=======
>>>>>>> Stashed changes

#####################################################
### 5. Test whether filter works on new variables ###
#####################################################

# a) Test signal/segments tables by comparing eeg_lst with tibble

mutate_filter1_eeg <- data %>%
  mutate(time = as_time(.sample_id, unit = "milliseconds")) %>%
  filter(time == 2)

mutate_filter1_tbl <- sigseg_data %>%
  mutate(time = as_time(.sample_id, unit = "milliseconds")) %>%
  dplyr::filter(time == 2)


mutate_filter2_eeg <- data %>%
  mutate(time = as_time(.sample_id, unit = "seconds")) %>%
  filter(time == 0.002)

<<<<<<< Updated upstream
=======

#####################################################
### 5. Test whether filter works on new variables ###
#####################################################

# a) Test signal/segments tables by comparing eeg_lst with tibble

mutate_filter1_eeg <- data %>%
  mutate(time = as_time(.sample_id, unit = "milliseconds")) %>%
  filter(time == 2)

mutate_filter1_tbl <- sigseg_data %>%
  mutate(time = as_time(.sample_id, unit = "milliseconds")) %>%
  dplyr::filter(time == 2)


mutate_filter2_eeg <- data %>%
  mutate(time = as_time(.sample_id, unit = "seconds")) %>%
  filter(time == 0.002)

=======
>>>>>>> Stashed changes
mutate_filter2_tbl <- sigseg_data %>%
  mutate(time = as_time(.sample_id, unit = "seconds")) %>%
  dplyr::filter(time == 0.002)

<<<<<<< Updated upstream
>>>>>>> Stashed changes
=======
>>>>>>> Stashed changes

mutate_filter3_eeg <- data %>% 
  mutate(group = ifelse(.sample_id > 0, "late", "early")) %>%
  filter(group == "late")

<<<<<<< Updated upstream
<<<<<<< Updated upstream
mutate_filter3_tbl <- extended_data %>%
=======
mutate_filter3_tbl <- sigseg_data %>%
>>>>>>> Stashed changes
=======
mutate_filter3_tbl <- sigseg_data %>%
>>>>>>> Stashed changes
  dplyr::mutate(group = ifelse(.sample_id > 0, "late", "early")) %>%
  dplyr::filter(group == "late")


mutate_filter4_eeg <- data %>% 
  mutate(group = ifelse(Y > 0, "pos", "neg")) %>%
  filter(group == "neg")

<<<<<<< Updated upstream
<<<<<<< Updated upstream
mutate_filter4_tbl <- extended_data %>%
=======
mutate_filter4_tbl <- sigseg_data %>%
>>>>>>> Stashed changes
=======
mutate_filter4_tbl <- sigseg_data %>%
>>>>>>> Stashed changes
  dplyr::mutate(group = ifelse(Y > 0, "pos", "neg")) %>%
  dplyr::filter(group == "neg")


<<<<<<< Updated upstream
<<<<<<< Updated upstream
# why does the eeg_lst have fewer rows?
transmute_filter_eeg <- transmute(data, X = X + 1) %>%
  filter(recording == "recording1")

transmute_filter_tbl <- extended_data %>%
=======
transmute_filter_eeg <- transmute(data, X = X + 1) %>%
  filter(recording == "recording1")

transmute_filter_tbl <- sigseg_data %>%
>>>>>>> Stashed changes
=======
transmute_filter_eeg <- transmute(data, X = X + 1) %>%
  filter(recording == "recording1")

transmute_filter_tbl <- sigseg_data %>%
>>>>>>> Stashed changes
  dplyr::filter(recording == "recording1") %>%
  dplyr::mutate(X = X + 1) 
  

<<<<<<< Updated upstream
<<<<<<< Updated upstream
test_that("filtering on newly created variables returns correct values in signal table", {
  expect_equal(as.matrix(mutate_filter1_eeg$signal[, c(".id", ".sample_id", "X", "Y")]), as.matrix(mutate_filter2_eeg$signal[, c(".id", ".sample_id", "X", "Y")]))
  expect_equal(as.matrix(mutate_filter3_eeg$signal), as.matrix(distinct(select(mutate_filter3_tbl, .id, .sample_id, X, Y, group))))
  expect_equal(as.matrix(mutate_filter4_eeg$signal), as.matrix(distinct(select(mutate_filter4_tbl, .id, .sample_id, X, Y, group))))
  expect_equal(as.double(transmute_filter_eeg$signal$X), as.double(unique(transmute_filter_tbl$X)))
=======
test_that("filtering on newly created variables works in signal table", {
  expect_equal(as.matrix(mutate_filter1_eeg$signal[, !c("time")]), 
               as.matrix(mutate_filter2_eeg$signal[, !c("time")]))
  expect_setequal(as.matrix(mutate_filter3_eeg$signal), 
               as.matrix(select(mutate_filter3_tbl, .id, .sample_id, X, Y, group)))
  expect_setequal(as.matrix(mutate_filter4_eeg$signal), 
               as.matrix(select(mutate_filter4_tbl, .id, .sample_id, X, Y, group)))
  expect_equal(as.double(transmute_filter_eeg$signal$X), 
               as.double(unique(transmute_filter_tbl$X)))
>>>>>>> Stashed changes
})


test_that("filtering on newly created variables works in segments table", {
  expect_equal(mutate_filter1_eeg$segments, 
               mutate_filter2_eeg$segments)
  expect_setequal(as.matrix(mutate_filter3_eeg$segments), 
               as.matrix(select(mutate_filter3_tbl, .id, recording, segment, condition)))
  expect_setequal(as.matrix(mutate_filter4_eeg$segments), 
               as.matrix(select(mutate_filter4_tbl, .id, recording, segment, condition)))
  expect_setequal(as.matrix(transmute_filter_eeg$segments), 
               as.matrix(select(transmute_filter_tbl, .id, recording, segment, condition)))
})



# b) To test the events table, create some opposing filters & test that they 
# don't return same events

# bind above mutate/filters to events table 
mutate_filter1a <- left_join(mutate_filter1_eeg$signal, evtseg_data, by = ".id")
mutate_filter2a <- left_join(mutate_filter2_eeg$signal, evtseg_data, by = ".id")
mutate_filter3a <- left_join(mutate_filter3_eeg$signal, evtseg_data, by = ".id")
mutate_filter4a <- left_join(mutate_filter4_eeg$signal, evtseg_data, by = ".id")
transmute_filtera <- left_join(transmute_filter_eeg$signal, evtseg_data, by = ".id")

# do the opposite filter
mutate_filter1b_eeg <- data %>%
  mutate(time = as_time(.sample_id, unit = "milliseconds")) %>%
  filter(time != 2)
mutate_filter1b <- left_join(mutate_filter1b_eeg$signal, evtseg_data, by = ".id")

mutate_filter2b_eeg <- data %>%
  mutate(time = as_time(.sample_id, unit = "milliseconds")) %>%
  filter(time != 0.02)
mutate_filter2b <- left_join(mutate_filter2b_eeg$signal, evtseg_data, by = ".id")

mutate_filter3b_eeg <- data %>%
  mutate(group = ifelse(.sample_id > 0, "late", "early")) %>%
  filter(group != "late")
mutate_filter3b <- left_join(mutate_filter3b_eeg$signal, evtseg_data, by = ".id")

# warnings about ids
mutate_filter4b_eeg <- data %>%
  mutate(group = ifelse(Y > 0, "pos", "neg")) %>%
  filter(group != "neg")
mutate_filter4b <- left_join(mutate_filter4b_eeg$signal, evtseg_data, by = ".id")

transmute_filterb_eeg <- transmute(data, X = X + 1) %>%
  filter(recording != "recording1")
transmute_filterb <- left_join(transmute_filterb_eeg$signal, evtseg_data, by = ".id")


test_that("doing opposite filters returns no matching values in events table", {
  expect_true(nrow(semi_join(mutate_filter1a, mutate_filter1b)) == 0)
  expect_true(nrow(semi_join(mutate_filter2a, mutate_filter2b)) == 0)
  expect_true(nrow(semi_join(mutate_filter3a, mutate_filter3b)) == 0)
  expect_true(nrow(semi_join(mutate_filter4a, mutate_filter4b)) == 0)
  expect_true(nrow(semi_join(transmute_filtera, transmute_filterb)) == 0)
})


# and merging xa and xb should equal original data
mutate_filter1c <- left_join(mutate_filter1_eeg$events, mutate_filter1b_eeg$events)
mutate_filter2c <- left_join(mutate_filter2_eeg$events, mutate_filter2b_eeg$events)
mutate_filter3c <- left_join(mutate_filter3_eeg$events, mutate_filter3b_eeg$events)
mutate_filter4c <- left_join(mutate_filter4_eeg$events, mutate_filter4b_eeg$events)
# doesn't apply to transmute


test_that("merging xa and xb recovers original events table", {
  expect_equal(as.matrix(mutate_filter1c), 
               as.matrix(data$events))
  expect_equal(as.matrix(mutate_filter2c), 
               as.matrix(data$events))  
  expect_equal(as.matrix(mutate_filter3c), 
               as.matrix(data$events))
  expect_equal(as.matrix(mutate_filter4c), 
               as.matrix(data$events))
})


# does it match as_tibble output
mutate_filter1a_tbl <- left_join(mutate_filter1_tbl, evtseg_data)
mutate_filter2a_tbl <- left_join(mutate_filter2_tbl, evtseg_data)
mutate_filter3a_tbl <- left_join(mutate_filter3_tbl, evtseg_data)
mutate_filter4a_tbl <- left_join(mutate_filter4_tbl, evtseg_data)
transmute_filtera_tbl <- transmute_filter_tbl %>% 
  select(-Y) %>% 
  left_join(., evtseg_data)


test_that("the eeg_lst filter returns the same events as as_tibble", {
  expect_setequal(as.matrix(mutate_filter1a), as.matrix(mutate_filter1a_tbl))
  expect_setequal(as.matrix(mutate_filter2a), as.matrix(mutate_filter2a_tbl))
  expect_setequal(as.matrix(mutate_filter3a), as.matrix(mutate_filter3a_tbl))
  expect_setequal(as.matrix(mutate_filter4a), as.matrix(mutate_filter4a_tbl))
  expect_setequal(as.matrix(transmute_filtera), as.matrix(transmute_filtera_tbl))
})


test_that("the classes of channels of signal_tbl remain after filtering by new variables", {
  expect_equal(is_channel_dbl(mutate_filter1_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate_filter2_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate_filter3_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate_filter4_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(transmute_filter_eeg$signal$X), TRUE)
})


# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})


################################################################################
### 6. Test whether filter works after grouping and adding/summarizing vars  ###
################################################################################

<<<<<<< Updated upstream
### test whether filter works after grouping and adding vars  ###
=======
test_that("filtering on newly created variables works in signal table", {
  expect_equal(as.matrix(mutate_filter1_eeg$signal[, !c("time")]), 
               as.matrix(mutate_filter2_eeg$signal[, !c("time")]))
  expect_equal(as.matrix(mutate_filter3_eeg$signal), 
               as.matrix(distinct(mutate_filter3_tbl, .id, .sample_id, X, Y, group)))
  expect_equal(as.matrix(mutate_filter4_eeg$signal), 
               as.matrix(distinct(mutate_filter4_tbl, .id, .sample_id, X, Y, group)))
  expect_equal(as.double(transmute_filter_eeg$signal$X), 
               as.double(unique(transmute_filter_tbl$X)))
})


test_that("filtering on newly created variables works in segments table", {
  expect_equal(mutate_filter1_eeg$segments, 
               mutate_filter2_eeg$segments)
  expect_equal(as.matrix(mutate_filter3_eeg$segments), 
               as.matrix(distinct(mutate_filter3_tbl, .id, recording, segment, condition)))
  expect_equal(as.matrix(mutate_filter4_eeg$segments), 
               as.matrix(distinct(mutate_filter4_tbl, .id, recording, segment, condition)))
  expect_equal(as.matrix(transmute_filter_eeg$segments), 
               as.matrix(distinct(transmute_filter_tbl, .id, recording, segment, condition)))
})



# b) To test the events table, create some opposing filters & test that they 
# don't return same events

# bind above mutate/filters to events table 
mutate_filter1a <- left_join(mutate_filter1_eeg$signal, events_data, by = ".id")
mutate_filter2a <- left_join(mutate_filter2_eeg$signal, events_data, by = ".id")
mutate_filter3a <- left_join(mutate_filter3_eeg$signal, events_data, by = ".id")
mutate_filter4a <- left_join(mutate_filter4_eeg$signal, events_data, by = ".id")
transmute_filtera <- left_join(transmute_filter_eeg$signal, events_data, by = ".id")

# do the opposite filter
mutate_filter1b_eeg <- data %>%
  mutate(time = as_time(.sample_id, unit = "milliseconds")) %>%
  filter(time != 2)
mutate_filter1b <- left_join(mutate_filter1b_eeg$signal, events_data, by = ".id")

mutate_filter2b_eeg <- data %>%
  mutate(time = as_time(.sample_id, unit = "milliseconds")) %>%
  filter(time != 0.02)
mutate_filter2b <- left_join(mutate_filter2b_eeg$signal, events_data, by = ".id")

mutate_filter3b_eeg <- data %>%
  mutate(group = ifelse(.sample_id > 0, "late", "early")) %>%
  filter(group != "late")
mutate_filter3b <- left_join(mutate_filter3b_eeg$signal, events_data, by = ".id")

# warnings about ids
mutate_filter4b_eeg <- data %>%
  mutate(group = ifelse(Y > 0, "pos", "neg")) %>%
  filter(group != "neg")
mutate_filter4b <- left_join(mutate_filter4b_eeg$signal, events_data, by = ".id")

transmute_filterb_eeg <- transmute(data, X = X + 1) %>%
  filter(recording != "recording1")
transmute_filterb <- left_join(transmute_filterb_eeg$signal, events_data, by = ".id")


test_that("doing opposite filters returns no matching values in events table", {
  expect_true(nrow(semi_join(mutate_filter1a, mutate_filter1b)) == 0)
  expect_true(nrow(semi_join(mutate_filter2a, mutate_filter2b)) == 0)
  expect_true(nrow(semi_join(mutate_filter3a, mutate_filter3b)) == 0)
  expect_true(nrow(semi_join(mutate_filter4a, mutate_filter4b)) == 0)
  expect_true(nrow(semi_join(transmute_filtera, transmute_filterb)) == 0)
})


# and merging xa and xb should equal original data
mutate_filter1c <- left_join(mutate_filter1a_eeg$events, mutate_filter1b_eeg$events)
mutate_filter2c <- left_join(mutate_filter2a_eeg$events, mutate_filter2b_eeg$events)
mutate_filter3c <- left_join(mutate_filter3a_eeg$events, mutate_filter3b_eeg$events)
mutate_filter4c <- left_join(mutate_filter4a_eeg$events, mutate_filter4b_eeg$events)
# doesn't apply to transmute


test_that("merging xa and xb recovers original events table", {
  expect_equal(as.matrix(mutate_filter1c), 
               as.matrix(data$events))
  expect_equal(as.matrix(mutate_filter2c), 
               as.matrix(data$events))  
  expect_equal(as.matrix(mutate_filter3c), 
               as.matrix(data$events))
  expect_equal(as.matrix(mutate_filter4c), 
               as.matrix(data$events))
})


# does it match as_tibble output
mutate_filter1a_tbl <- left_join(mutate_filter1_tbl, events_data)
mutate_filter2a_tbl <- left_join(mutate_filter2_tbl, events_data)
mutate_filter3a_tbl <- left_join(mutate_filter3_tbl, events_data)
mutate_filter4a_tbl <- left_join(mutate_filter4_tbl, events_data)
transmute_filtera_tbl <- transmute_filter_tbl %>% 
  select(-Y) %>% 
  left_join(., events_data)


test_that("the eeg_lst filter returns the same events as as_tibble", {
  expect_equal(select(mutate_filter1a, sort(current_vars())),
               select(mutate_filter1a_tbl, sort(current_vars())))
  expect_equal(select(mutate_filter2a, sort(current_vars())),
               select(mutate_filter2a_tbl, sort(current_vars())))
  expect_equal(select(mutate_filter3a, sort(current_vars())),
               select(mutate_filter3a_tbl, sort(current_vars())))
  expect_equal(select(mutate_filter4a, sort(current_vars())),
               select(mutate_filter4a_tbl, sort(current_vars())))
  expect_equal(select(transmute_filtera, sort(current_vars())),
               select(transmute_filtera_tbl, sort(current_vars())))
})



test_that("the classes of channels of signal_tbl remain after filtering by new variables", {
  expect_equal(is_channel_dbl(mutate_filter1a_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate_filter2a_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate_filter3a_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate_filter4a_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(transmute_filtera_eeg$signal$X), TRUE)
})


# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})


################################################################################
### 6. Test whether filter works after grouping and adding/summarizing vars  ###
################################################################################

# a) Test signal/segments tables by comparing eeg_lst with tibble
>>>>>>> Stashed changes
=======
# a) Test signal/segments tables by comparing eeg_lst with tibble
>>>>>>> Stashed changes

mutate_all_filter_eeg <- data %>%
  group_by(.sample_id) %>%
  mutate_all(mean) %>%
  filter(condition == "b")

mutate_at_filter_eeg <- data %>%
  group_by(.sample_id) %>%
  mutate_at(channel_names(data), mean)  %>%
  filter(condition == "b")

<<<<<<< Updated upstream
<<<<<<< Updated upstream
# this throws too many warnings about attributes - why doesn't it do this anywhere else?
# mutate_a_tbl <- extended_data %>%
#   dplyr::group_by(.sample_id) %>%
#   dplyr::mutate(mean = mean(Y)) %>%
#   dplyr::filter(condition == "b")

# a different way - ids are different again
=======
>>>>>>> Stashed changes
=======
>>>>>>> Stashed changes
mutate_a_tbl <- data %>%
  as_tibble() %>%
  dplyr::group_by(time, channel) %>%
  dplyr::mutate(mean = mean(amplitude)) %>% 
  dplyr::select(.id, time, channel, mean, condition, segment, recording) %>%
  tidyr::spread(key = channel, value = mean) %>%
<<<<<<< Updated upstream
<<<<<<< Updated upstream
  ungroup() %>% # have to add this or it does weird stuff
  dplyr::filter(condition == "b")


# different lengths & sample_ids... eh? 
=======
  dplyr::ungroup() %>% # have to add this or it does weird stuff
  dplyr::filter(condition == "b")


>>>>>>> Stashed changes
=======
  dplyr::ungroup() %>% # have to add this or it does weird stuff
  dplyr::filter(condition == "b")


>>>>>>> Stashed changes
summarize_filter_eeg <- group_by(data, .sample_id) %>% 
  summarize(mean = mean(Y)) %>%
  filter(mean > -0.35)

<<<<<<< Updated upstream
<<<<<<< Updated upstream
summarize_filter_tbl <- extended_data %>%
=======
summarize_filter_tbl <- sigseg_data %>%
>>>>>>> Stashed changes
=======
summarize_filter_tbl <- sigseg_data %>%
>>>>>>> Stashed changes
  dplyr::group_by(.sample_id) %>%
  dplyr::summarize(mean = mean(Y)) %>%
  dplyr::filter(mean > -0.35)


summarize_at_filter_eeg <- data %>% 
  group_by(.id, recording, condition) %>%
  summarize_at_ch(channel_names(data), mean) %>%
  filter(X > 0 & Y > 0)

<<<<<<< Updated upstream
<<<<<<< Updated upstream
summarize_at_filter_tbl <- extended_data %>%
  dplyr::group_by(.id, recording, condition) %>%
  dplyr::summarise(X = mean(X), Y = mean(Y)) %>%
  ungroup() %>% # have to add this or it does weird stuff
=======
summarize_at_filter_tbl <- sigseg_data %>%
  dplyr::group_by(.id, recording, condition) %>%
  dplyr::summarise(X = mean(X), Y = mean(Y)) %>%
  dplyr::ungroup() %>% # have to add this or it does weird stuff
>>>>>>> Stashed changes
  dplyr::filter(X > 0 | Y > 0)
=======
summarize_at_filter_tbl <- sigseg_data %>%
  dplyr::group_by(.id, recording, condition) %>%
  dplyr::summarise(X = mean(X), Y = mean(Y)) %>%
  dplyr::ungroup() %>% # have to add this or it does weird stuff
  dplyr::filter(X > 0 & Y > 0)
>>>>>>> Stashed changes


summarize_all_filter_eeg <- group_by(data, .id, .sample_id) %>% 
  summarize_all_ch("mean") %>%
  filter(.sample_id < 0)

<<<<<<< Updated upstream
<<<<<<< Updated upstream
summarize_all_filter_tbl <- extended_data %>%
=======
summarize_all_filter_tbl <- sigseg_data %>%
>>>>>>> Stashed changes
=======
summarize_all_filter_tbl <- sigseg_data %>%
>>>>>>> Stashed changes
  dplyr::group_by(.id, .sample_id) %>%
  dplyr::summarize(X = mean(X), Y = mean(Y)) %>%
  dplyr::filter(.sample_id < 0)


<<<<<<< Updated upstream
<<<<<<< Updated upstream
# ids are different
=======
# warnings about ids
>>>>>>> Stashed changes
=======
# warnings about ids
>>>>>>> Stashed changes
summarize_all1_filter_eeg <- group_by(data, .id, condition) %>% 
  summarize_all_ch("mean") %>%
  filter(condition == "a")

<<<<<<< Updated upstream
<<<<<<< Updated upstream
summarize_all1_filter_tbl <- extended_data %>%
=======
summarize_all1_filter_tbl <- sigseg_data %>%
>>>>>>> Stashed changes
=======
summarize_all1_filter_tbl <- sigseg_data %>%
>>>>>>> Stashed changes
  dplyr::group_by(.id, condition) %>%
  dplyr::summarize(X = mean(X), Y = mean(Y)) %>%
  dplyr::filter(condition == "a")


<<<<<<< Updated upstream
<<<<<<< Updated upstream
# no grouping by id: means are different  
=======
>>>>>>> Stashed changes
=======
>>>>>>> Stashed changes
summarize_all2_filter_eeg <- group_by(data, condition) %>% 
  summarize_all_ch("mean") %>%
  filter(condition == "a")

<<<<<<< Updated upstream
<<<<<<< Updated upstream
summarize_all2_filter_tbl <- extended_data %>%
=======
summarize_all2_filter_tbl <- sigseg_data %>%
>>>>>>> Stashed changes
=======
summarize_all2_filter_tbl <- sigseg_data %>%
>>>>>>> Stashed changes
  dplyr::group_by(condition) %>%
  dplyr::summarize(X = mean(X), Y = mean(Y)) %>%
  dplyr::filter(condition == "a")


<<<<<<< Updated upstream
<<<<<<< Updated upstream
test_that("filtering after grouping and summarizing returns correct values in signal table", {
  expect_equal(mutate_all_filter_eeg$signal, mutate_at_filter_eeg$signal)
  expect_equal(as.matrix(mutate_at_filter_eeg$signal[, c(".id", "X", "Y")]), as.matrix(select(mutate_a_tbl, .id, X, Y)))
  expect_equal(as.double(summarize_filter_eeg$signal$mean), as.double(summarize_filter_tbl$mean))
  expect_equal(as.matrix(summarize_at_filter_eeg$signal[, c(".id", "X", "Y")]), as.matrix(select(summarize_at_filter_tbl, .id, X, Y)))
  expect_equal(as.matrix(summarize_all_filter_eeg$signal), as.matrix(select(summarize_all_filter_tbl, .id, .sample_id, X, Y)))
  expect_equal(as.matrix(summarize_all1_filter_eeg$signal[, c(".id", "X", "Y")]), as.matrix(select(summarize_all1_filter_tbl, .id, X, Y)))
  expect_equal(as.matrix(summarize_all2_filter_eeg$signal[, c("X", "Y")]), as.matrix(select(summarize_all2_filter_tbl, X, Y)))
=======

test_that("filtering after grouping and summarizing works in signal table", {
  expect_equal(mutate_all_filter_eeg$signal, 
               mutate_at_filter_eeg$signal)
  expect_equal(as.matrix(mutate_at_filter_eeg$signal[, !c(".sample_id")]), 
               as.matrix(select(mutate_a_tbl, .id, X, Y)))
  expect_equal(as.double(summarize_filter_eeg$signal$mean), 
               as.double(summarize_filter_tbl$mean))
  expect_equal(as.matrix(summarize_at_filter_eeg$signal[, !c(".sample_id")]), 
               as.matrix(select(summarize_at_filter_tbl, .id, X, Y)))
  expect_equal(as.matrix(summarize_all_filter_eeg$signal), 
               as.matrix(select(summarize_all_filter_tbl, .id, .sample_id, X, Y)))
  expect_equal(as.matrix(summarize_all1_filter_eeg$signal[, !c(".sample_id")]), 
               as.matrix(select(summarize_all1_filter_tbl, .id, X, Y)))
  expect_equal(as.matrix(summarize_all2_filter_eeg$signal[, c("X", "Y")]), 
               as.matrix(select(summarize_all2_filter_tbl, X, Y)))
>>>>>>> Stashed changes
})


test_that("filtering after grouping and summarizing works in segments table", {
  expect_equal(mutate_all_filter_eeg$segments, 
               mutate_at_filter_eeg$segments)
  expect_setequal(as.matrix(mutate_at_filter_eeg$segments), 
               as.matrix(select(mutate_a_tbl, .id, recording, segment, condition)))
  expect_setequal(as.matrix(summarize_at_filter_eeg$segments[, c(".id", "recording", "condition")]), 
               as.matrix(select(summarize_at_filter_tbl, .id, recording, condition)))
  expect_setequal(as.double(summarize_all_filter_eeg$segments$.id), 
               as.double(summarize_all_filter_tbl$.id))
  expect_equal(as.matrix(summarize_all1_filter_eeg$segments[, c(".id", "condition")]), 
               as.matrix(select(summarize_all1_filter_tbl, .id, condition)))
  expect_equal(as.matrix(summarize_all2_filter_eeg$segments[, c("condition")]), 
               as.matrix(select(summarize_all2_filter_tbl, condition)))
})


<<<<<<< Updated upstream
# not relevant
# test_that("filtering after grouping and summarizing returns correct values in events table", {
# 
# })
=======

test_that("filtering after grouping and summarizing works in signal table", {
  expect_equal(mutate_all_filter_eeg$signal, 
               mutate_at_filter_eeg$signal)
  expect_equal(as.matrix(mutate_at_filter_eeg$signal[, !c(".sample_id")]), 
               as.matrix(select(mutate_a_tbl, .id, X, Y)))
  expect_equal(as.double(summarize_filter_eeg$signal$mean), 
               as.double(summarize_filter_tbl$mean))
  expect_equal(as.matrix(summarize_at_filter_eeg$signal[, !c(".sample_id")]), 
               as.matrix(select(summarize_at_filter_tbl, .id, X, Y)))
  expect_equal(as.matrix(summarize_all_filter_eeg$signal), 
               as.matrix(select(summarize_all_filter_tbl, .id, .sample_id, X, Y)))
  expect_equal(as.matrix(summarize_all1_filter_eeg$signal[, !c(".sample_id")]), 
               as.matrix(select(summarize_all1_filter_tbl, .id, X, Y)))
  expect_equal(as.matrix(summarize_all2_filter_eeg$signal[, c("X", "Y")]), 
               as.matrix(select(summarize_all2_filter_tbl, X, Y)))
})


test_that("filtering after grouping and summarizing works in segments table", {
  expect_equal(mutate_all_filter_eeg$segments, 
               mutate_at_filter_eeg$segments)
  expect_equal(as.matrix(mutate_at_filter_eeg$segments), 
               as.matrix(distinct(select(mutate_a_tbl, .id, recording, segment, condition))))
  expect_equal(as.matrix(summarize_at_filter_eeg$segments[, c(".id", "recording", "condition")]), 
               as.matrix(distinct(summarize_at_filter_tbl, .id, recording, condition)))
  expect_equal(as.double(summarize_all_filter_eeg$segments$.id), 
               as.double(unique(summarize_all_filter_tbl$.id)))
  expect_equal(as.matrix(summarize_all1_filter_eeg$segments[, c(".id", "condition")]), 
               as.matrix(select(summarize_all1_filter_tbl, .id, condition)))
  expect_equal(as.matrix(summarize_all2_filter_eeg$segments[, c("condition")]), 
               as.matrix(select(summarize_all2_filter_tbl, condition)))
})


=======
>>>>>>> Stashed changes


# b) To test the events table, create some opposing filters & test that they 
# don't return same events

# join events info to above filtered data (lots of id warnings)
<<<<<<< Updated upstream
mutate_all_filtera <- left_join(mutate_all_filter_eeg$signal, events_data, by = ".id")
mutate_at_filtera <- left_join(mutate_at_filter_eeg$signal, events_data, by = ".id")
summarize_filtera<- left_join(summarize_filter_eeg$signal, events_data, by = ".id")
summarize_at_filtera <- left_join(summarize_at_filter_eeg$signal, events_data, by = ".id")
# joining these returns sample_ids >= = (should be <)!
summarize_all_filtera <- left_join(summarize_all_filter_eeg$signal, events_data, by = ".id")
# .ids in x are not being matched to y?
summarize_all1_filtera <- left_join(summarize_all1_filter_eeg$signal, events_data, by = ".id")
# ditto (i.e. only .id 1 remains in x but is being matched with 2 in y)
summarize_all2_filtera <- left_join(summarize_all2_filter_eeg$signal, events_data, by = ".id")
=======
mutate_all_filtera <- left_join(mutate_all_filter_eeg$signal, 
                                evtseg_data, by = ".id")
mutate_at_filtera <- left_join(mutate_at_filter_eeg$signal, 
                               evtseg_data, by = ".id")
summarize_filtera<- left_join(summarize_filter_eeg$signal, 
                              evtseg_data, by = ".id")
summarize_at_filtera <- left_join(summarize_at_filter_eeg$signal, 
                                  evtseg_data, by = ".id")
summarize_all_filtera <- left_join(summarize_all_filter_eeg$signal, 
                                   evtseg_data, by = ".id")
summarize_all1_filtera <- left_join(summarize_all1_filter_eeg$signal, 
                                    evtseg_data, by = ".id")
summarize_all2_filtera <- left_join(summarize_all2_filter_eeg$signal, 
                                    evtseg_data, by = ".id")
>>>>>>> Stashed changes


# apply opposite filter

# ids warnings
mutate_all_filterb_eeg <- data %>%
  group_by(.sample_id) %>%
  mutate_all(mean) %>%
  filter(condition != "b")

<<<<<<< Updated upstream
mutate_all_filterb <- left_join(mutate_all_filterb_eeg$signal, events_data, by = ".id")
=======
mutate_all_filterb <- left_join(mutate_all_filterb_eeg$signal, 
                                evtseg_data, by = ".id")
>>>>>>> Stashed changes


mutate_at_filterb_eeg <- data %>%
  group_by(.sample_id) %>%
  mutate_all(mean) %>%
  filter(condition != "b")

<<<<<<< Updated upstream
mutate_at_filterb <- left_join(mutate_at_filterb_eeg$signal, events_data, by = ".id")
=======
mutate_at_filterb <- left_join(mutate_at_filterb_eeg$signal, 
                               evtseg_data, by = ".id")
>>>>>>> Stashed changes


summarize_filterb_eeg <- group_by(data, .sample_id) %>% 
  summarize(mean = mean(Y)) %>%
  filter(mean <= -0.35)

<<<<<<< Updated upstream
summarize_filterb <- left_join(summarize_filterb_eeg$signal, events_data, by = ".id")
=======
summarize_filterb <- left_join(summarize_filterb_eeg$signal, 
                               evtseg_data, by = ".id")
>>>>>>> Stashed changes


summarize_at_filterb_eeg <- data %>% 
  group_by(.id, recording, condition) %>%
  summarize_at_ch(channel_names(data), mean) %>%
<<<<<<< Updated upstream
  filter(X <= 0 | Y <= 0)

summarize_at_filterb <- left_join(summarize_at_filterb_eeg$signal, events_data, by = ".id")

# this seems correct (vs. a - weird when above doesn't work?)
summarize_all_filterb_eeg <- group_by(data, .id, .sample_id) %>% 
  summarize_all_ch("mean") %>%
  filter(.sample_id >= 0)
summarize_all_filterb <- left_join(summarize_all_filterb_eeg$signal, events_data, by = ".id")

# this too
=======
  filter(X <= 0 & Y <= 0)

summarize_at_filterb <- left_join(summarize_at_filterb_eeg$signal, 
                                  evtseg_data, by = ".id")

summarize_all_filterb_eeg <- group_by(data, .id, .sample_id) %>% 
  summarize_all_ch("mean") %>%
  filter(.sample_id >= 0)
summarize_all_filterb <- left_join(summarize_all_filterb_eeg$signal, 
                                   evtseg_data, by = ".id")


>>>>>>> Stashed changes
summarize_all1_filterb_eeg <- group_by(data, .id, condition) %>% 
  summarize_all_ch("mean") %>%
  filter(condition != "a")

<<<<<<< Updated upstream
summarize_all1_filterb <- left_join(summarize_all1_filterb_eeg$signal, events_data, by = ".id")

# this too
=======
summarize_all1_filterb <- left_join(summarize_all1_filterb_eeg$signal, 
                                    evtseg_data, by = ".id")


>>>>>>> Stashed changes
summarize_all2_filterb_eeg <- group_by(data, condition) %>% 
  summarize_all_ch("mean") %>%
  filter(condition != "a")

<<<<<<< Updated upstream
summarize_all2_filterb <- left_join(summarize_all2_filterb_eeg$signal, events_data, by = ".id")
=======
summarize_all2_filterb <- left_join(summarize_all2_filterb_eeg$signal, 
                                    evtseg_data, by = ".id")
>>>>>>> Stashed changes



test_that("doing opposite filters returns no matching values in events table", {
  expect_true(nrow(semi_join(mutate_all_filtera, mutate_all_filterb)) == 0)
  expect_true(nrow(semi_join(mutate_at_filtera, mutate_at_filterb)) == 0)
  expect_true(nrow(semi_join(summarize_filtera, summarize_filterb)) == 0)
  expect_true(nrow(semi_join(summarize_at_filtera, summarize_at_filterb)) == 0)
  expect_true(nrow(semi_join(summarize_all_filtera, summarize_all_filterb)) == 0)
  expect_true(nrow(semi_join(summarize_all1_filtera, summarize_all1_filterb)) == 0)
  expect_true(nrow(semi_join(summarize_all2_filtera, summarize_all2_filterb)) == 0)
})


test_that("summarizes don't have any individual events", {
  expect_true(nrow(summarize_filter_eeg$events) == 0)
  expect_true(nrow(summarize_at_filter_eeg$events) == 0)
  expect_true(nrow(summarize_all_filter_eeg$events) == 0)
  expect_true(nrow(summarize_all1_filter_eeg$events) == 0)
  expect_true(nrow(summarize_all2_filter_eeg$events) == 0)
})
  
# and merging xa and xb should equal original data

# not recognising the .ids as separate? takes .ids only from x
mutate_all_filterc <- left_join(mutate_all_filter_eeg$events, 
                                mutate_all_filterb_eeg$events)
mutate_at_filterc <- left_join(mutate_at_filter_eeg$events, 
                                mutate_at_filterb_eeg$events)


test_that("merging xa and xb recovers original events table", {
  expect_equal(as.matrix(mutate_all_filterc), as.matrix(data$events))
  expect_equal(as.matrix(mutate_at_filterc), as.matrix(data$events))
})



# and the original filter output should be the same as when converted to tibble
# (not relevant for summarize)
<<<<<<< Updated upstream
mutate_a_filtera_tbl <- left_join(mutate_a_tbl, events_data)


test_that("eeg_lst filter output matches tibble", {
  expect_equal(select(mutate_at_filtera, sort(current_vars())),
               select(mutate_a_filtera_tbl, sort(current_vars())))
  expect_equal(select(mutate_all_filtera, sort(current_vars())),
               select(mutate_a_filtera_tbl, sort(current_vars())))
})

>>>>>>> Stashed changes
=======
mutate_a_filtera_tbl <- left_join(mutate_a_tbl, evtseg_data)

# idk why only these tibbles need to be converted to a matrix to work, but ok:
test_that("eeg_lst filter output matches tibble", {
  expect_setequal(as.matrix(select(mutate_at_filtera, -.sample_id)),
               as.matrix(select(mutate_a_filtera_tbl, -time)))
  expect_setequal(as.matrix(select(mutate_all_filtera, -.sample_id)),
               as.matrix(select(mutate_a_filtera_tbl, -time)))
})
>>>>>>> Stashed changes


test_that("the classes of channels of signal_tbl remain after filtering by new variables", {
  expect_equal(is_channel_dbl(mutate_all_filter_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate_at_filter_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(summarize_filter_eeg$signal$mean), TRUE)
  expect_equal(is_channel_dbl(summarize_at_filter_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(summarize_all_filter_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(summarize_all1_filter_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(summarize_all2_filter_eeg$signal$X), TRUE)
})


# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})
<<<<<<< Updated upstream
<<<<<<< Updated upstream
=======
>>>>>>> Stashed changes
>>>>>>> Stashed changes
=======

>>>>>>> Stashed changes
