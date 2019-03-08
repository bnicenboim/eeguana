context("test tidyverse functions select")
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



#####################################################
############### select and operators ################
#####################################################


test_that("selecting non-existent variables returns error", {
  expect_error(select(data, Z))
  expect_error(select(data, subject))
})


### signal table

select1_eeg <- select(data, X)
select1_tbl <- data %>%
  as_tibble() %>%
  tidyr::spread(key = channel, value = amplitude) %>%
  dplyr::select(X)


select2_eeg <- select(data, -Y)
select2_tbl <- data %>%
  as_tibble() %>%
  tidyr::spread(key = channel, value = amplitude) %>%
  dplyr::select(-Y)


select3_eeg <- select(data, starts_with("Y"))
select3_tbl <- data %>%
  as_tibble() %>%
  tidyr::spread(key = channel, value = amplitude) %>%
  dplyr::select(starts_with("Y"))

select4_eeg <- select(data, ends_with("X"))
select4_tbl <- data %>%
    as_tibble(add_channels_info=FALSE) %>%
    tidyr::spread(key = channel, value = amplitude) %>%
    dplyr::select(ends_with("X"))

select4.1_eeg <- select(data, one_of("X"))
select4.1_tbl <- data %>%
    as_tibble(add_channels_info=FALSE) %>%
  tidyr::spread(key = channel, value = amplitude) %>%
  dplyr::select(one_of("X"))

select5_eeg <- select(data, contains("Y"))
select5_tbl <- data %>%
    as_tibble(add_channels_info=FALSE) %>%
  tidyr::spread(key = channel, value = amplitude) %>%
  dplyr::select(contains("Y"))

select5.1_eeg <- select(data, one_of("Y"))
select5.1_tbl <- data %>%
    as_tibble(add_channels_info=FALSE) %>%
  tidyr::spread(key = channel, value = amplitude) %>%
  dplyr::select(one_of("Y"))


select6_eeg <- select(data, tidyselect::matches("X"))
select6_tbl <- data %>%
    as_tibble(add_channels_info=FALSE) %>%
  tidyr::spread(key = channel, value = amplitude) %>%
  dplyr::select(tidyselect::matches("X"))



test_that("different select operators produce the same eeg_lst", {
  expect_equal(select1_eeg, select2_eeg)
  expect_equal(select1_eeg, select4_eeg)
  expect_equal(select3_eeg, select5_eeg)
   expect_equal(select1_eeg, select6_eeg)
})


# since we've established that all the above are the same so can just test 1
test_that("select in signal table doesn't change the data", {
  # in signal table
  expect_equal(select1_eeg$signal[, c(".id", ".sample_id", "X")],
               data$signal[, c(".id", ".sample_id", "X")])
  # in segments table
  expect_equal(select1_eeg$segments, data$segments)
})


test_that("select in signal table removes the right data from the events table", {
  # test a select that removed Y events
  expect_true(nrow(filter(select1_eeg$events, .channel == "Y")) == 0)
  # and one that removed X events
  expect_true(nrow(filter(select3_eeg$events, .channel == "X")) == 0)
})


test_that("select works the same on eeg_lst as on tibble", {
  expect_setequal(as.matrix(select1_eeg$signal$X), as.matrix(select1_tbl))
  expect_setequal(as.matrix(select2_eeg$signal[, !c(".sample_id")]), 
                  as.matrix(select(select2_tbl, .id, X)))
  expect_setequal(as.matrix(select3_eeg$signal$Y), 
                  as.matrix(select3_tbl))
  expect_setequal(as.matrix(select4_eeg$signal$X), 
                  as.matrix(select4_tbl$X))
  expect_equal(select4_eeg, select4.1_eeg)
  expect_equal(select4_tbl$X, select4.1_tbl$X)
  expect_setequal(as.matrix(select5_eeg$signal$Y), 
                  as.matrix(select5_tbl)) 
  expect_equal(select5_eeg, select5.1_eeg)
  expect_setequal(as.matrix(select6_eeg$signal$X), 
                  as.matrix(select6_tbl))  
  
})


test_that("the classes of channels of signal_tbl haven't changed", {
  expect_equal(is_channel_dbl(select1_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(select2_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(select3_eeg$signal$Y), TRUE)
  expect_equal(is_channel_dbl(select4_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(select5_eeg$signal$Y), TRUE)
})


# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})




### segments table

select9_eeg <- select(data, recording)
select9_tbl <- data %>%
  as_tibble() %>%
  tidyr::spread(key = channel, value = amplitude) %>%
  dplyr::select(recording)


select10_eeg <- select(data, segment)
select10_tbl <- data %>%
  as_tibble() %>%
  tidyr::spread(key = channel, value = amplitude) %>%
  dplyr::select(segment)


select11_eeg <- select(data, condition)
select11_tbl <- data %>%
  as_tibble() %>%
  tidyr::spread(key = channel, value = amplitude) %>%
  dplyr::select(condition)



test_that("selecting in segments table doesn't change data", {
  # in signal table
  expect_equal(select9_eeg$signal, data$signal)
  expect_equal(select10_eeg$signal, data$signal)
  expect_equal(select11_eeg$signal, data$signal)
  # in segments table
  expect_equal(select9_eeg$segments, data$segments[c(".id", "recording")])
  expect_equal(select10_eeg$segments, data$segments[c(".id", "segment")])
  expect_equal(select11_eeg$segments, data$segments[c(".id", "condition")])
  # in events table
  expect_equal(select9_eeg$events, data$events)
  expect_equal(select10_eeg$events, data$events)
  expect_equal(select11_eeg$events, data$events)
})


test_that("select works the same on eeg_lst as on tibble", {
  expect_setequal(as.matrix(select9_eeg$segments$recording), 
                  as.matrix(select9_tbl$recording))
  expect_setequal(as.matrix(select10_eeg$segments$segment), 
                  as.matrix(select10_tbl$segment))
  expect_setequal(as.matrix(select11_eeg$segments$condition), 
                  as.matrix(select11_tbl$condition))
})


test_that("the classes of channels of signal_tbl haven't changed", {
  expect_equal(is_channel_dbl(select9_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(select10_eeg$signal$Y), TRUE)
  expect_equal(is_channel_dbl(select11_eeg$signal$X), TRUE)
})


# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})



### events table


#TODO TEST


## ### scoped selects



## # should change all vars to upper/lower case
## # works only for signal table
## select_all1_eeg <- select_all(data, toupper) 
## select_all1_tbl <- data %>%
##   as_tibble() %>%
##   tidyr::spread(key = channel, value = amplitude) %>%
##   dplyr::select_all(toupper)


## select_all2_eeg <- select_all(data, tolower) 
## select_all2_tbl <- data %>%
##   as_tibble() %>%
##   tidyr::spread(key = channel, value = amplitude) %>%
##   dplyr::select_all(tolower)



## test_that("scoped selects don't change data", {
##   # in signal table
##   expect_equivalent(select_all1_eeg$signal, data$signal)
##   expect_equivalent(select_all2_eeg$signal, data$signal)  
##   expect_equal(select_all1_eeg$signal[, c(".id", ".sample_id")],
##                data$signal[, c(".id", ".sample_id")])
##   expect_equal(select_all2_eeg$signal[, c(".id", ".sample_id")],
##                data$signal[, c(".id", ".sample_id")])
##   # in segments table
##   expect_equal(select_all1_eeg$segments, data$segments) 
##   expect_equal(select_all2_eeg$segments, data$segments) 
##   # in events table
##   expect_equal(select_all1_eeg$events, data$events)
##   # uh oh
##   expect_equal(select_all2_eeg$events, data$events)
## })


## test_that("scoped selects work the same on eeg_lst as tibble", {
##   expect_setequal(as.matrix(select_all1_eeg$signal[, !c(".sample_id")]),
##                as.matrix(select(select_all1_tbl, .ID, X, Y)))
##   expect_setequal(as.matrix(select_all2_eeg$signal[, !c(".sample_id")]),
##                as.matrix(select(select_all2_tbl, .id, x, y)))
##   # windows doesn't notice the case difference (although it does notice if you try to select lowercase in line 2)
##   # can use skip_on_os if it will cause problems
##   expect_setequal(as.matrix(select_all1_eeg$segments),
##                as.matrix(select(select_all1_tbl, .ID, RECORDING, SEGMENT, CONDITION)))
##   expect_setequal(as.matrix(select_all2_eeg$segments),
##                   as.matrix(select(select_all2_tbl, .id, recording, segment, condition)))
## })


## test_that("the classes of channels of signal_tbl haven't changed", {
##   expect_equal(is_channel_dbl(select_all1_eeg$signal$X), TRUE)
##   expect_equal(is_channel_dbl(select_all2_eeg$signal$x), TRUE)
## })


## # check against original data
## test_that("data didn't change", {
##   expect_equal(reference_data, data)
## })



###################################################################
################# select on new variables #########################
###################################################################


mutate_select_eeg <- mutate(data, Z = Y + 1) %>%
  select(Z)
mutate_select_tbl <- data %>%
  as_tibble() %>%
  tidyr::spread(key = channel, value = amplitude) %>%
  dplyr::mutate(Z = Y + 1) %>%
  dplyr::select(Z)


summarize_all_select_eeg <- summarize_all_ch(data, mean) %>%
  select(Y)
summarize_all_select_tbl <- data %>%
  as_tibble() %>%
  tidyr::spread(key = channel, value = amplitude) %>%
  dplyr::summarise(X = mean(X), Y = mean(Y)) %>%
  dplyr::select(Y)


test_that("select on new variables doesn't change data", {
  # in signal table
  expect_equal(mutate_select_eeg$signal[, c(".id", ".sample_id")],
               data$signal[, c(".id", ".sample_id")])
  expect_equivalent(mutate_select_eeg$signal$Z, data$signal$Y+1)
  # in segments table
  expect_equal(mutate_select_eeg$segments, data$segments)
})


test_that("select on new variables removes the right data from events", {
  expect_true(nrow(filter(mutate_select_eeg$events, .channel == "X")) == 0)
  # should the events table get larger with the new "channel"?
})


test_that("select works the same on eeg_lst as on tibble", {
  ## expect_setequal(as.matrix(rename_select_eeg$signal$ZZ),
                  ## as.matrix(rename_select_tbl))
  expect_setequal(as.matrix(mutate_select_eeg$signal$Z),
                  as.matrix(mutate_select_tbl)) 
})



test_that("the classes of channels of signal_tbl haven't changed", {
  expect_equal(is_channel_dbl(mutate_select_eeg$signal$Z), TRUE)
})


# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})



### with grouping

group_select_eeg <- data %>%
  group_by(condition) %>%
  select(Y)

group_select_tbl <- data %>%
  as_tibble() %>%
  tidyr::spread(key = channel, value = amplitude) %>%
  dplyr::group_by(condition) %>%
  dplyr::select(Y)


group_select_summarize_eeg <- data %>%
  group_by(.sample_id) %>%
  summarize_all_ch(mean) %>%
  select(X)

group_select_summarize_tbl <- data %>%
  as_tibble() %>%
  tidyr::spread(key = channel, value = amplitude) %>%
  dplyr::group_by(time) %>%
  dplyr::summarise(X = mean(X)) %>%
  dplyr::select(X)



test_that("select on grouped variables doesn't change data", {
  # in the signal table
  expect_equal(group_select_eeg$signal[, c(".id", ".sample_id", "Y")],
               data$signal[, c(".id", ".sample_id", "Y")])
  # in the segments table
  expect_equal(group_select_eeg$segments, data$segments)
})


test_that("select on grouped variable removes the right events data", {
  expect_true(nrow(filter(group_select_eeg$events, .channel == "X")) == 0)
})


test_that("select on group vars works same in eeg_lst and tibble", {
  expect_setequal(as.matrix(group_select_eeg$signal$Y),
                  as.matrix(group_select_tbl$Y))
  expect_setequal(as.matrix(group_select_eeg$segments$condition),
                  as.matrix(select(group_select_tbl, condition)))
  expect_equal(as.matrix(group_select_summarize_eeg$signal$X),
               as.matrix(group_select_summarize_tbl$X))
})


test_that("the classes of channels of signal_tbl haven't changed", {
  expect_equal(is_channel_dbl(group_select_eeg$signal$Y), TRUE)
  expect_equal(is_channel_dbl(group_select_summarize_eeg$signal$X), TRUE)
})


# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})
