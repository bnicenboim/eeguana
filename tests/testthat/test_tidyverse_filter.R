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

# bind it all together
data <- bind(data_1, data_2)

# for checks later
reference_data <- data.table::copy(data)



# create tibbles from eeg_lst to test against

# this is super weird - sometimes this works, sometimes i get the error (underneath)
sigseg_data <- eeguana:::left_join_dt(data$signal, data.table::as.data.table(data$segments), by = ".id")

evtseg_data <- left_join(data$events, data$segments, by = ".id")

## # Error in as_tibble.signal_tbl(data, .name_repair = "check_unique") : 
## #   unused argument (.name_repair = "check_unique") 

## # if i get the error, this works instead, 
## # but then all of the tests fail for .id and heaps of other stuff no longer works either
## sigseg_data <- left_join(as_tibble(data$signal), data$segments, by = ".id")
## evtseg_data <- left_join(as_tibble(data$events), data$segments, by = ".id")

signal_data <- as_tibble(data$signal)
segments_data <- as_tibble(data$segments)
events_data <-  as_tibble(data$events)
  




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
  as_tibble() %>%
  dplyr::filter(.id == 1)


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

## # add events data to filtered eeg_lsts from above
## filter1a_sign <- left_join(as_tibble(filter1_sign_eeg$signal), evtseg_data)
## filter2a_sign <- left_join(as_tibble(filter2_sign_eeg$signal), evtseg_data)
## filter3a_sign <- left_join(as_tibble(filter3_sign_eeg$signal), evtseg_data)


## # apply opposite filter
## filter1b_sign_eeg <- filter(data, .sample_id < 2) 
## filter1b_sign <- left_join(as_tibble(filter1b_sign_eeg$signal), evtseg_data)

## filter2b_sign_eeg <- filter(data, .id != 1 & .sample_id != 2)
## filter2b_sign <- left_join(as_tibble(filter2b_sign_eeg$signal), evtseg_data)

## filter3b_sign_eeg <- filter(data, X >= 0 & Y >= 0)
## filter3b_sign <- left_join(as_tibble(filter3b_sign_eeg$signal), evtseg_data)


## test_that("opposite filters return no matching events", {
##   expect_true(nrow(semi_join(filter1a_sign, filter1b_sign)) == 0)
##   expect_true(nrow(semi_join(filter2a_sign, filter2b_sign)) == 0)
##   expect_true(nrow(semi_join(filter3a_sign, filter3b_sign)) == 0)
## })


## # and merging xa and xb should equal original data
## filter1c_sign <- left_join(filter1_sign_eeg$events, filter1b_sign_eeg$events)
## # this only keeps .id == 1, why aren't the others merged?
## filter2c_sign <- left_join(filter2_sign_eeg$events, filter2b_sign_eeg$events)
## filter3c_sign <- left_join(filter3_sign_eeg$events, filter3b_sign_eeg$events)


## test_that("merging xa and xb recovers original events table", {
##   expect_equal(as.matrix(filter1c_sign), as.matrix(data$events))
##   expect_equal(as.matrix(filter2c_sign), as.matrix(data$events))
##   expect_equal(as.matrix(filter3c_sign), as.matrix(data$events))
## })


## # filtered eeg_lst and tibble should match
## filter1a_sign_tbl <- left_join(filter1_sign_tbl, evtseg_data)
## filter2a_sign_tbl <- left_join(filter2_sign_tbl, evtseg_data)
## filter3a_sign_tbl <- left_join(filter3_sign_tbl, evtseg_data)


## test_that("the eeg_lst filter returns the same events as as_tibble", {
##   expect_setequal(as.matrix(filter1a_sign_tbl), as.matrix(filter1a_sign))
##   expect_setequal(as.matrix(filter2a_sign_tbl), as.matrix(filter2a_sign))
##   expect_setequal(as.matrix(filter3a_sign_tbl), as.matrix(filter3a_sign))
## })


## test_that("the classes of channels of signal_tbl remain after within eeg_lst table", {
##   expect_equal(is_channel_dbl(filter1_sign_eeg$signal$X), TRUE)
##   expect_equal(is_channel_dbl(filter2_sign_eeg$signal$X), TRUE)
##   expect_equal(is_channel_dbl(filter3_sign_eeg$signal$X), TRUE)
## })

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

filter3_eeg <- filter(data, .sample_id == 1 | condition == "a")
filter3_tbl <- sigseg_data %>%
  dplyr::filter(.sample_id == 1 | condition == "a") 

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

## # add events data to above filters
## filter1a_evts <- left_join(filter1_eeg$signal, evtseg_data, by = ".id")
## filter2a_evts <- left_join(filter2_eeg$signal, evtseg_data, by = ".id")
## filter3a_evts <- left_join(filter3_eeg$signal, evtseg_data, by = ".id")
## filter4a_evts <- left_join(filter4_eeg$signal, evtseg_data, by = ".id")
## filter5a_evts <- left_join(filter5_eeg$signal, evtseg_data, by = ".id")
## filter6a_evts <- left_join(filter6_eeg$signal, evtseg_data, by = ".id")


## # apply the opposite filter
## filter1b_evts_eeg <- filter(data, .sample_id != 2 & segment != 2) 
## filter1b_evts <- left_join(filter1b_evts_eeg$signal, evtseg_data, by = ".id")

## filter2b_evts_eeg <- filter(data, .sample_id != 2 & recording == "recording2")
## filter2b_evts <- left_join(filter2b_evts_eeg$signal, evtseg_data, by = ".id")

## filter3b_evts_eeg <- filter(data, .sample_id != 1 | condition != "a")
## filter3b_evts <- left_join(filter3b_evts_eeg$signal, evtseg_data, by = ".id")

## filter4b_evts_eeg <- filter(data, .id != 2 | condition != "b")
## filter4b_evts <- left_join(filter4b_evts_eeg$signal, evtseg_data, by = ".id")

## filter5b_evts_eeg <- filter(data, X <= 0 | X >= 0.5 & segment == 1)
## filter5b_evts <- left_join(filter5b_evts_eeg$signal, evtseg_data, by = ".id")

## filter6b_evts_eeg <- filter(data, Y <= 0 & recording != "recording1")
## filter6b_evts <- left_join(filter6b_evts_eeg$signal, evtseg_data, by = ".id")


## test_that("what's not in xb matches xc", {
##   expect_true(nrow(semi_join(filter1a_evts, filter1b_evts)) == 0)
##   expect_true(nrow(semi_join(filter2a_evts, filter2b_evts)) == 0)
##   # due to issue with 3a
##   expect_true(nrow(semi_join(filter3a_evts, filter3b_evts)) == 0)
##   # due to issue with 4a
##   expect_true(nrow(semi_join(filter4a_evts, filter4b_evts)) == 0)
##   expect_true(nrow(semi_join(filter5a_evts, filter5b_evts)) == 0)
##   expect_true(nrow(semi_join(filter6a_evts, filter6b_evts)) == 0)
## })


## # and merging xa and xb should equal original data
## # only the ids for x are kept, not for y (doesn't matter for 3 because first filter didn't work)
## filter1c <- left_join(filter1_eeg$events, filter1b_evts_eeg$events)
## filter2c <- left_join(filter2_eeg$events, filter2b_evts_eeg$events)
## filter3c <- left_join(filter3_eeg$events, filter3b_evts_eeg$events)
## filter4c <- left_join(filter4_eeg$events, filter4b_evts_eeg$events)
## filter5c <- left_join(filter5_eeg$events, filter5b_evts_eeg$events)
## filter6c <- left_join(filter6_eeg$events, filter6b_evts_eeg$events)


## test_that("merging opposite filters recovers original events", {
##   expect_equal(as.matrix(filter1c), as.matrix(data$events))
##   expect_equal(as.matrix(filter2c), as.matrix(data$events))
##   expect_equal(as.matrix(filter3c), as.matrix(data$events))
##   expect_equal(as.matrix(filter4c), as.matrix(data$events))
##   expect_equal(as.matrix(filter5c), as.matrix(data$events))
##   expect_equal(as.matrix(filter6c), as.matrix(data$events))
## })


## # check that events match the as_tibble version
## filter1a_tbl <- left_join(filter1_tbl, evtseg_data)
## filter2a_tbl <- left_join(filter2_tbl, evtseg_data)
## filter3a_tbl <- left_join(filter3_tbl, evtseg_data)
## filter4a_tbl <- left_join(filter4_tbl, evtseg_data)
## filter5a_tbl <- left_join(filter5_tbl, evtseg_data)
## filter6a_tbl <- left_join(filter6_tbl, evtseg_data)


## test_that("the eeg_lst filter returns the same events as as_tibble", {
##   expect_setequal(as.matrix(filter1a_evts), as.matrix(filter1a_tbl))
##   expect_setequal(as.matrix(filter2a_evts), as.matrix(filter2a_tbl))
##   expect_setequal(as.matrix(filter3a_evts), as.matrix(filter3a_tbl))
##   expect_setequal(as.matrix(filter4a_evts), as.matrix(filter4a_tbl))
##   expect_setequal(as.matrix(filter5a_evts), as.matrix(filter5a_tbl))
##   expect_setequal(as.matrix(filter6a_evts), as.matrix(filter6a_tbl))
## })


## test_that("the classes of channels of signal_tbl remain after filtering across eeg_lst tables", {
##   expect_equal(is_channel_dbl(filter1_eeg$signal$X), TRUE)
##   expect_equal(is_channel_dbl(filter2_eeg$signal$X), TRUE)
##   expect_equal(is_channel_dbl(filter3_eeg$signal$X), TRUE)
##   expect_equal(is_channel_dbl(filter4_eeg$signal$X), TRUE)
##   expect_equal(is_channel_dbl(filter5_eeg$signal$X), TRUE)
##   expect_equal(is_channel_dbl(filter6_eeg$signal$X), TRUE)
## })


# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})





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

mutate_filter2_tbl <- sigseg_data %>%
  mutate(time = as_time(.sample_id, unit = "seconds")) %>%
  dplyr::filter(time == 0.002)


mutate_filter3_eeg <- data %>% 
  mutate(group = ifelse(.sample_id > 0, "late", "early")) %>%
  filter(group == "late")

mutate_filter3_tbl <- sigseg_data %>%
  dplyr::mutate(group = ifelse(.sample_id > 0, "late", "early")) %>%
  dplyr::filter(group == "late")


mutate_filter4_eeg <- data %>% 
  mutate(group = ifelse(Y > 0, "pos", "neg")) %>%
  filter(group == "neg")

mutate_filter4_tbl <- sigseg_data %>%
  dplyr::mutate(group = ifelse(Y > 0, "pos", "neg")) %>%
  dplyr::filter(group == "neg")


transmute_filter_eeg <- transmute(data, X = X + 1) %>%
  filter(recording == "recording1")

transmute_filter_tbl <- sigseg_data %>%
  dplyr::filter(recording == "recording1") %>%
  dplyr::mutate(X = X + 1) 
  

test_that("filtering on newly created variables works in signal table", {
  expect_equal(as.matrix(mutate_filter1_eeg$signal[, !c("time")]), 
               as.matrix(mutate_filter2_eeg$signal[, !c("time")]))
  expect_setequal(as.matrix(mutate_filter3_eeg$signal), 
               as.matrix(select(mutate_filter3_tbl, .id, .sample_id, X, Y, group)))
  expect_setequal(as.matrix(mutate_filter4_eeg$signal), 
               as.matrix(select(mutate_filter4_tbl, .id, .sample_id, X, Y, group)))
  expect_equal(as.double(transmute_filter_eeg$signal$X), 
               as.double(unique(transmute_filter_tbl$X)))
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



## # b) To test the events table, create some opposing filters & test that they 
## # don't return same events

## # bind above mutate/filters to events table 
## mutate_filter1a <- left_join(mutate_filter1_eeg$signal, evtseg_data, by = ".id")
## mutate_filter2a <- left_join(mutate_filter2_eeg$signal, evtseg_data, by = ".id")
## mutate_filter3a <- left_join(mutate_filter3_eeg$signal, evtseg_data, by = ".id")
## mutate_filter4a <- left_join(mutate_filter4_eeg$signal, evtseg_data, by = ".id")
## transmute_filtera <- left_join(transmute_filter_eeg$signal, evtseg_data, by = ".id")

## # do the opposite filter
## mutate_filter1b_eeg <- data %>%
##   mutate(time = as_time(.sample_id, unit = "milliseconds")) %>%
##   filter(time != 2)
## mutate_filter1b <- left_join(mutate_filter1b_eeg$signal, evtseg_data, by = ".id")

## mutate_filter2b_eeg <- data %>%
##   mutate(time = as_time(.sample_id, unit = "milliseconds")) %>%
##   filter(time != 0.02)
## mutate_filter2b <- left_join(mutate_filter2b_eeg$signal, evtseg_data, by = ".id")

## mutate_filter3b_eeg <- data %>%
##   mutate(group = ifelse(.sample_id > 0, "late", "early")) %>%
##   filter(group != "late")
## mutate_filter3b <- left_join(mutate_filter3b_eeg$signal, evtseg_data, by = ".id")

## # warnings about ids
## mutate_filter4b_eeg <- data %>%
##   mutate(group = ifelse(Y > 0, "pos", "neg")) %>%
##   filter(group != "neg")
## mutate_filter4b <- left_join(mutate_filter4b_eeg$signal, evtseg_data, by = ".id")

## transmute_filterb_eeg <- transmute(data, X = X + 1) %>%
##   filter(recording != "recording1")
## transmute_filterb <- left_join(transmute_filterb_eeg$signal, evtseg_data, by = ".id")


## test_that("doing opposite filters returns no matching values in events table", {
##   expect_true(nrow(semi_join(mutate_filter1a, mutate_filter1b)) == 0)
##   expect_true(nrow(semi_join(mutate_filter2a, mutate_filter2b)) == 0)
##   expect_true(nrow(semi_join(mutate_filter3a, mutate_filter3b)) == 0)
##   expect_true(nrow(semi_join(mutate_filter4a, mutate_filter4b)) == 0)
##   expect_true(nrow(semi_join(transmute_filtera, transmute_filterb)) == 0)
## })


## # and merging xa and xb should equal original data
## mutate_filter1c <- left_join(mutate_filter1_eeg$events, mutate_filter1b_eeg$events)
## mutate_filter2c <- left_join(mutate_filter2_eeg$events, mutate_filter2b_eeg$events)
## mutate_filter3c <- left_join(mutate_filter3_eeg$events, mutate_filter3b_eeg$events)
## mutate_filter4c <- left_join(mutate_filter4_eeg$events, mutate_filter4b_eeg$events)
## # doesn't apply to transmute


## test_that("merging xa and xb recovers original events table", {
##   expect_equal(as.matrix(mutate_filter1c), 
##                as.matrix(data$events))
##   expect_equal(as.matrix(mutate_filter2c), 
##                as.matrix(data$events))  
##   expect_equal(as.matrix(mutate_filter3c), 
##                as.matrix(data$events))
##   expect_equal(as.matrix(mutate_filter4c), 
##                as.matrix(data$events))
## })


## # does it match as_tibble output
## mutate_filter1a_tbl <- left_join(mutate_filter1_tbl, evtseg_data)
## mutate_filter2a_tbl <- left_join(mutate_filter2_tbl, evtseg_data)
## mutate_filter3a_tbl <- left_join(mutate_filter3_tbl, evtseg_data)
## mutate_filter4a_tbl <- left_join(mutate_filter4_tbl, evtseg_data)
## transmute_filtera_tbl <- transmute_filter_tbl %>% 
##   select(-Y) %>% 
##   left_join(., evtseg_data)


## test_that("the eeg_lst filter returns the same events as as_tibble", {
##   expect_setequal(as.matrix(mutate_filter1a), as.matrix(mutate_filter1a_tbl))
##   expect_setequal(as.matrix(mutate_filter2a), as.matrix(mutate_filter2a_tbl))
##   expect_setequal(as.matrix(mutate_filter3a), as.matrix(mutate_filter3a_tbl))
##   expect_setequal(as.matrix(mutate_filter4a), as.matrix(mutate_filter4a_tbl))
##   expect_setequal(as.matrix(transmute_filtera), as.matrix(transmute_filtera_tbl))
## })


## test_that("the classes of channels of signal_tbl remain after filtering by new variables", {
##   expect_equal(is_channel_dbl(mutate_filter1_eeg$signal$X), TRUE)
##   expect_equal(is_channel_dbl(mutate_filter2_eeg$signal$X), TRUE)
##   expect_equal(is_channel_dbl(mutate_filter3_eeg$signal$X), TRUE)
##   expect_equal(is_channel_dbl(mutate_filter4_eeg$signal$X), TRUE)
##   expect_equal(is_channel_dbl(transmute_filter_eeg$signal$X), TRUE)
## })


# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})


################################################################################
### 6. Test whether filter works after grouping and adding/summarizing vars  ###
################################################################################

# a) Test signal/segments tables by comparing eeg_lst with tibble

mutate_all_filter_eeg <- data %>%
  group_by(.sample_id) %>%
  mutate_all(mean) %>%
  filter(condition == "b")

mutate_at_filter_eeg <- data %>%
  group_by(.sample_id) %>%
  mutate_at(channel_names(data), mean)  %>%
  filter(condition == "b")

mutate_a_tbl <- data %>%
  as_tibble() %>%
  dplyr::group_by(time, channel) %>%
  dplyr::mutate(mean = mean(amplitude)) %>% 
  dplyr::select(.id, time, channel, mean, condition, segment, recording) %>%
  tidyr::spread(key = channel, value = mean) %>%
  dplyr::ungroup() %>% # have to add this or it does weird stuff
  dplyr::filter(condition == "b")


summarize_filter_eeg <- group_by(data, .sample_id) %>% 
  summarize(mean = mean(Y)) %>%
  filter(mean > -0.35)

summarize_filter_tbl <- sigseg_data %>%
  dplyr::group_by(.sample_id) %>%
  dplyr::summarize(mean = mean(Y)) %>%
  dplyr::filter(mean > -0.35)


summarize_at_filter_eeg <- data %>% 
  group_by(.id, recording, condition) %>%
  summarize_at_ch(channel_names(data), mean) %>%
  filter(X > 0 & Y > 0)

summarize_at_filter_tbl <- sigseg_data %>%
  dplyr::group_by(.id, recording, condition) %>%
  dplyr::summarise(X = mean(X), Y = mean(Y)) %>%
  dplyr::ungroup() %>% # have to add this or it does weird stuff
  dplyr::filter(X > 0 & Y > 0)


summarize_all_filter_eeg <- group_by(data, .id, .sample_id) %>% 
  summarize_all_ch("mean") %>%
  filter(.sample_id < 0)

summarize_all_filter_tbl <- sigseg_data %>%
  dplyr::group_by(.id, .sample_id) %>%
  dplyr::summarize(X = mean(X), Y = mean(Y)) %>%
  dplyr::filter(.sample_id < 0)


# warnings about ids
summarize_all1_filter_eeg <- group_by(data, .id, condition) %>% 
  summarize_all_ch("mean") %>%
  filter(condition == "a")

summarize_all1_filter_tbl <- sigseg_data %>%
  dplyr::group_by(.id, condition) %>%
  dplyr::summarize(X = mean(X), Y = mean(Y)) %>%
  dplyr::filter(condition == "a")


summarize_all2_filter_eeg <- group_by(data, condition) %>% 
  summarize_all_ch("mean") %>%
  filter(condition == "a")

summarize_all2_filter_tbl <- sigseg_data %>%
  dplyr::group_by(condition) %>%
  dplyr::summarize(X = mean(X), Y = mean(Y)) %>%
  dplyr::filter(condition == "a")



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




# b) To test the events table, create some opposing filters & test that they 
# don't return same events

# join events info to above filtered data (lots of id warnings)
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


# apply opposite filter

# ids warnings
mutate_all_filterb_eeg <- data %>%
  group_by(.sample_id) %>%
  mutate_all(mean) %>%
  filter(condition != "b")

mutate_all_filterb <- left_join(mutate_all_filterb_eeg$signal, 
                                evtseg_data, by = ".id")


mutate_at_filterb_eeg <- data %>%
  group_by(.sample_id) %>%
  mutate_all(mean) %>%
  filter(condition != "b")

mutate_at_filterb <- left_join(mutate_at_filterb_eeg$signal, 
                               evtseg_data, by = ".id")


summarize_filterb_eeg <- group_by(data, .sample_id) %>% 
  summarize(mean = mean(Y)) %>%
  filter(mean <= -0.35)

summarize_filterb <- left_join(summarize_filterb_eeg$signal, 
                               evtseg_data, by = ".id")


summarize_at_filterb_eeg <- data %>% 
  group_by(.id, recording, condition) %>%
  summarize_at_ch(channel_names(data), mean) %>%
  filter(X <= 0 & Y <= 0)

summarize_at_filterb <- left_join(summarize_at_filterb_eeg$signal, 
                                  evtseg_data, by = ".id")

summarize_all_filterb_eeg <- group_by(data, .id, .sample_id) %>% 
  summarize_all_ch("mean") %>%
  filter(.sample_id >= 0)
summarize_all_filterb <- left_join(summarize_all_filterb_eeg$signal, 
                                   evtseg_data, by = ".id")


summarize_all1_filterb_eeg <- group_by(data, .id, condition) %>% 
  summarize_all_ch("mean") %>%
  filter(condition != "a")

summarize_all1_filterb <- left_join(summarize_all1_filterb_eeg$signal, 
                                    evtseg_data, by = ".id")


summarize_all2_filterb_eeg <- group_by(data, condition) %>% 
  summarize_all_ch("mean") %>%
  filter(condition != "a")

summarize_all2_filterb <- left_join(summarize_all2_filterb_eeg$signal, 
                                    evtseg_data, by = ".id")



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


## test_that("merging xa and xb recovers original events table", {
##   expect_equal(as.matrix(mutate_all_filterc), as.matrix(data$events))
##   expect_equal(as.matrix(mutate_at_filterc), as.matrix(data$events))
## })



# and the original filter output should be the same as when converted to tibble
# (not relevant for summarize)
mutate_a_filtera_tbl <- left_join(mutate_a_tbl, evtseg_data)

# idk why only these tibbles need to be converted to a matrix to work, but ok:
test_that("eeg_lst filter output matches tibble", {
  expect_setequal(as.matrix(select(mutate_at_filtera, -.sample_id)),
               as.matrix(select(mutate_a_filtera_tbl, -time)))
  expect_setequal(as.matrix(select(mutate_all_filtera, -.sample_id)),
               as.matrix(select(mutate_a_filtera_tbl, -time)))
})


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

