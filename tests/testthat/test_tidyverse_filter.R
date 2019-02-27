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



###################################################
### 1. Filtering by .id (applies to all tables) ###
###################################################

# a) Create eeg_lsts and tibbles with same filters and test against each other

filter1_id_eeg <- filter(data, .id == 1)

filter1_id_sign_tbl <- as_tibble(data$signal) %>%
  dplyr::filter(.id == 1)
filter1_id_segm_tbl <- as_tibble(data$segments) %>%
  dplyr::filter(.id == 1)
filter1_id_evts_tbl <- as_tibble(data$events) %>%
  as_tibble() %>%
  dplyr::filter(.id == 1)


filter2_id_eeg <- filter(data, .id != 2)

filter2_id_sign_tbl <- as_tibble(data$signal) %>%
  dplyr::filter(.id != 2)
filter2_id_segm_tbl <- as_tibble(data$segments) %>%
  dplyr::filter(.id != 2)
filter2_id_evts_tbl <- as_tibble(data$events) %>%
  as_tibble() %>%
  dplyr::filter(.id != 2)


filter3_id_eeg <- filter(data, .id == 3)

filter3_id_sign_tbl <- as_tibble(data$signal) %>%
  dplyr::filter(.id == 3)
filter3_id_segm_tbl <- as_tibble(data$segments) %>%
  dplyr::filter(.id == 3)
filter3_id_evts_tbl <- as_tibble(data$events) %>%
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
filter1_sign_tbl <- left_join(as_tibble(data$signal), as_tibble(data$segments)) %>%
  dplyr::filter(.sample_id >= 2) 


filter2_sign_eeg <- filter(data, .id == 1 & .sample_id == 2)
filter2_sign_tbl <- left_join(as_tibble(data$signal), as_tibble(data$segments)) %>%
  dplyr::filter(.id == 1 & .sample_id == 2)


filter3_sign_eeg <- filter(data, X < 0 & Y < 0)
filter3_sign_tbl <- left_join(as_tibble(data$signal), as_tibble(data$segments)) %>%
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

# b. Test the events table which will fail for now

filter4_sign_eeg <- data %>% filter(.sample_id == -1)
filter4_sign_tbl <- as_tibble(data$events) %>%
  group_by(.id, .sample_0) %>%
  filter(-1 %in% seq(.sample_0, by = 1, length.out = .size))


# really want *only* the events < 0 (filter won't take a vector), but probs ok
filter5_sign_eeg <- data %>% filter(.id == 1 & .sample_id < 0)
filter5_sign_tbl <- as_tibble(data$events) %>%
  group_by(.id, .sample_0) %>%
  filter(.id == 1 & any(seq(.sample_0, by = 1, length.out = .size) < 0))

# this might be impossible to test?
# filter6_sign_eeg <- data %>% filter(X < 0)
# filter6_sign_tbl <- as_tibble(data$events) %>%
#   group_by(.id, .sample_0) %>%
#   filter(.id == 1 & any(seq(.sample_0, by = 1, length.out = .size) < 0))

test_that("filtering in signal table returns the right events", {
  expect_setequal(as.matrix(filter4_sign_eeg$events), as.matrix(filter4_sign_tbl))
  expect_setequal(as.matrix(filter5_sign_eeg$events), as.matrix(filter5_sign_tbl))
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

filter1s_segm_tbl <- left_join(as_tibble(data$signal), as_tibble(data$segments)) %>%
  as_tibble() %>%
  dplyr::filter(segment != 2)

filter1e_segm_tbl <- left_join(as_tibble(data$segments), as_tibble(data$events)) %>%
  as_tibble() %>%
  dplyr::filter(segment != 2) %>%
  dplyr::distinct(.id, type, description, .sample_0, .size, .channel)


filter2_segm_eeg <- filter(data, condition == "a" & segment == 3)

filter2s_segm_tbl <- left_join(as_tibble(data$signal), as_tibble(data$segments)) %>%
  as_tibble() %>%
  dplyr::filter(condition == "a" & segment == 3)

filter2e_segm_tbl <- left_join(as_tibble(data$segments), as_tibble(data$events)) %>%
  as_tibble() %>%
  dplyr::filter(condition == "a" & segment == 3) %>%
  dplyr::distinct(.id, type, description, .sample_0, .size, .channel)


filter3_segm_eeg <- filter(data, recording == "recording2")

filter3s_segm_tbl <- left_join(as_tibble(data$signal), as_tibble(data$segments)) %>%
  as_tibble() %>%
  filter(recording == "recording2") 

filter3e_segm_tbl <- left_join(as_tibble(data$segments), as_tibble(data$events)) %>%
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

filter1_tbl <- left_join(as_tibble(data$signal), as_tibble(data$segments)) %>% 
  dplyr::filter(.sample_id == 2 & segment == 2)

# just checking this is the same as above
# filter1_tbl <- data %>%
#   as_tibble() %>%
#   filter(time == 0.002 & segment == 2) %>%
#   spread(key = channel, value = amplitude)


filter2_eeg <- filter(data, .sample_id < 2 & !(recording == "recording2"))
filter2_tbl <- left_join(as_tibble(data$signal), as_tibble(data$segments)) %>%
  dplyr::filter(.sample_id < 2 & !(recording == "recording2"))

filter3_eeg <- filter(data, .sample_id == 1 | condition == "a")
filter3_tbl <- left_join(as_tibble(data$signal), as_tibble(data$segments)) %>%
  dplyr::filter(.sample_id == 1 | condition == "a") 

filter4_eeg <- filter(data, .id == 2 | condition == "b")
filter4_tbl <- left_join(as_tibble(data$signal), as_tibble(data$segments)) %>%
  dplyr::filter(.id == 2 | condition == "b") 


filter5_eeg <- filter(data, between(X, 0, 0.5) & segment != 1)
filter5_tbl <- left_join(as_tibble(data$signal), as_tibble(data$segments)) %>%
  dplyr::filter(between(X, 0, 0.5) & segment != 1)


filter6_eeg <- filter(data, Y > 0 & recording == "recording1")
filter6_tbl <- left_join(as_tibble(data$signal), as_tibble(data$segments)) %>%
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


# b) A couple of tests of the events table from the above filters

filter1_evts_tbl <- left_join(as_tibble(data$segments), as_tibble(data$events)) %>%
  group_by(.id, .sample_0) %>%
  filter(segment == 2 & 2 %in% seq(.sample_0, by = 1, length.out = .size))

filter2_evts_tbl <- left_join(as_tibble(data$segments), as_tibble(data$events)) %>%
  group_by(.id, .sample_0) %>%
  filter(!(recording == "recording2") & any(seq(.sample_0, by = 1, length.out = .size) < 2))


# won't work for now
test_that("filtering in signal table returns the right events", {
  expect_setequal(as.matrix(filter1_eeg$events), as.matrix(filter1_evts_tbl))
  expect_setequal(as.matrix(filter2_eeg$events), as.matrix(filter2_evts_tbl))
})


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

mutate_filter1_tbl <- left_join(as_tibble(data$signal), as_tibble(data$segments)) %>%
  mutate(time = as_time(.sample_id, unit = "milliseconds")) %>%
  dplyr::filter(time == 2)


mutate_filter2_eeg <- data %>%
  mutate(time = as_time(.sample_id, unit = "seconds")) %>%
  filter(time == 0.002)

mutate_filter2_tbl <- left_join(as_tibble(data$signal), as_tibble(data$segments)) %>%
  mutate(time = as_time(.sample_id, unit = "seconds")) %>%
  dplyr::filter(time == 0.002)


mutate_filter3_eeg <- data %>% 
  mutate(group = ifelse(.sample_id > 0, "late", "early")) %>%
  filter(group == "late")

mutate_filter3_tbl <- left_join(as_tibble(data$signal), as_tibble(data$segments)) %>%
  dplyr::mutate(group = ifelse(.sample_id > 0, "late", "early")) %>%
  dplyr::filter(group == "late")


mutate_filter4_eeg <- data %>% 
  mutate(group = ifelse(Y > 0, "pos", "neg")) %>%
  filter(group == "neg")

mutate_filter4_tbl <- left_join(as_tibble(data$signal), as_tibble(data$segments)) %>%
  dplyr::mutate(group = ifelse(Y > 0, "pos", "neg")) %>%
  dplyr::filter(group == "neg")


transmute_filter_eeg <- transmute(data, X = X + 1) %>%
  filter(recording == "recording1")

transmute_filter_tbl <- left_join(as_tibble(data$signal), as_tibble(data$segments)) %>%
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



# b) A couple of tests of the events table

mutate_filter1_evts_tbl <- as_tibble(data$events) %>%
  group_by(.id, .sample_0) %>%
  filter(2 %in% seq(.sample_0, by = 1, length.out = .size))


mutate_filter3_evts_tbl <- as_tibble(data$events) %>%
  group_by(.id, .sample_0) %>%
  filter(any(seq(.sample_0, by = 1, length.out = .size) > 0))


# won't work for now
test_that("filtering with new variables returns the right events", {
  expect_setequal(as.matrix(mutate_filter1_eeg$events), 
                  as.matrix(mutate_filter1_evts_tbl))
  expect_setequal(as.matrix(mutate_filter3_eeg$events), 
               as.matrix(mutate_filter3_evts_tbl))
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

# a) Test signal/segments tables by comparing eeg_lst with tibble

# doesn't work but not really relevant to eeguana
# mutate_all_filter_eeg <- data %>%
#   group_by(.sample_id) %>%
#   mutate_all(mean) %>%
#   filter(condition == "b")

# # shouldn't this group by .sample_id?
# mutate_at_filter_eeg <- data %>%
#   group_by(.sample_id) %>%
#   mutate_at(channel_names(data), funs(mean)) %>%
#   filter(condition == "b")
# 
# # this doesn't group by time either
# mutate_at_tbl <- data %>%
#   as_tibble() %>%
#   dplyr::select(.id, time, channel, amplitude, condition, segment, recording) %>%
#   tidyr::spread(key = channel, value = amplitude) %>%
#   dplyr::group_by(time) %>%
#   dplyr::mutate_at(channel_names(data), funs(mean)) %>%
#   dplyr::filter(condition == "b")


summarize_filter_eeg <- group_by(data, .sample_id) %>% 
  summarize(mean = mean(Y)) %>%
  filter(mean > -0.35)

summarize_filter_tbl <- left_join(as_tibble(data$signal), as_tibble(data$segments)) %>%
  dplyr::group_by(.sample_id) %>%
  dplyr::summarize(mean = mean(Y)) %>%
  dplyr::filter(mean > -0.35)


summarize_at_filter_eeg <- data %>% 
  group_by(.id, recording, condition) %>%
  summarize_at_ch(channel_names(data), mean) %>%
  filter(X > 0 & Y > 0)

summarize_at_filter_tbl <- left_join(as_tibble(data$signal), as_tibble(data$segments)) %>%
  dplyr::group_by(.id, recording, condition) %>%
  dplyr::summarise(X = mean(X), Y = mean(Y)) %>%
  dplyr::ungroup() %>% # have to add this or it does weird stuff
  dplyr::filter(X > 0 & Y > 0)


summarize_all_filter_eeg <- group_by(data, .id, .sample_id) %>% 
  summarize_all_ch("mean") %>%
  filter(.sample_id < 0)

summarize_all_filter_tbl <- left_join(as_tibble(data$signal), as_tibble(data$segments)) %>%
  dplyr::group_by(.id, .sample_id) %>%
  dplyr::summarize(X = mean(X), Y = mean(Y)) %>%
  dplyr::filter(.sample_id < 0)


# warnings about ids
summarize_all1_filter_eeg <- group_by(data, .id, condition) %>% 
  summarize_all_ch("mean") %>%
  filter(condition == "a")

summarize_all1_filter_tbl <- left_join(as_tibble(data$signal), as_tibble(data$segments)) %>%
  dplyr::group_by(.id, condition) %>%
  dplyr::summarize(X = mean(X), Y = mean(Y)) %>%
  dplyr::filter(condition == "a")


summarize_all2_filter_eeg <- group_by(data, condition) %>% 
  summarize_all_ch("mean") %>%
  filter(condition == "a")

summarize_all2_filter_tbl <- left_join(as_tibble(data$signal), as_tibble(data$segments)) %>%
  dplyr::group_by(condition) %>%
  dplyr::summarize(X = mean(X), Y = mean(Y)) %>%
  dplyr::filter(condition == "a")



test_that("filtering after grouping and summarizing works in signal table", {
  # expect_equal(as.matrix(mutate_at_filter_eeg$signal[, !c(".sample_id")]), 
  #              as.matrix(select(mutate_a_tbl, .id, X, Y)))
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
  # expect_setequal(as.matrix(mutate_at_filter_eeg$segments), 
  #              as.matrix(select(mutate_a_tbl, .id, recording, segment, condition)))
  expect_setequal(as.matrix(summarize_at_filter_eeg$segments[, c(".id", "recording", "condition")]), 
               as.matrix(select(summarize_at_filter_tbl, .id, recording, condition)))
  expect_setequal(as.double(summarize_all_filter_eeg$segments$.id), 
               as.double(summarize_all_filter_tbl$.id))
  expect_equal(as.matrix(summarize_all1_filter_eeg$segments[, c(".id", "condition")]), 
               as.matrix(select(summarize_all1_filter_tbl, .id, condition)))
  expect_equal(as.matrix(summarize_all2_filter_eeg$segments[, c("condition")]), 
               as.matrix(select(summarize_all2_filter_tbl, condition)))
})




# b) A couple of events table tests


test_that("summarizes don't have any individual events", {
  expect_true(nrow(summarize_filter_eeg$events) == 0)
  expect_true(nrow(summarize_at_filter_eeg$events) == 0)
  expect_true(nrow(summarize_all_filter_eeg$events) == 0)
  expect_true(nrow(summarize_all1_filter_eeg$events) == 0)
  expect_true(nrow(summarize_all2_filter_eeg$events) == 0)
})
  



test_that("the classes of channels of signal_tbl remain after filtering by new variables", {
  # expect_equal(is_channel_dbl(mutate_at_filter_eeg$signal$X), TRUE)
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

