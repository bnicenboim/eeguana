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
  segments = dplyr::tibble(.id = c(1L, 2L, 3L), recording = "recording1", segment = c(1L, 2L, 3L), condition = c("a", "b", "a"))
)

# just some different X and Y
data_2 <- mutate(data_1, recording = "recording2", X = sin(X + 10), Y = cos(Y - 10), condition = c("b", "a", "b"))

# bind it all together
data <- bind(data_1, data_2)


# merge the eeg_lst tables into a tibble for testing against
extended_data <- left_join(as_tibble(data$signal), data$segments, by = ".id") %>%
  left_join(., data$events, by = ".id") # joining events gives duplicate cases - why?

# for checks later
reference_data <- data.table::copy(data)




###  filtering by signal table variables ###

filter1_sign_eeg <- filter(data, .id ==1)
filter1_sign_tbl <- extended_data %>%
  as_tibble() %>%
  dplyr::filter(.id == 1)


filter2_sign_eeg <- filter(data, .sample_id >= 2)
filter2_sign_tbl <- extended_data %>%
  as_tibble() %>%
  dplyr::filter(.sample_id >= 2) 


filter3_sign_eeg <- filter(data, .id == 1 & .sample_id == 2)
filter3_sign_tbl <- extended_data %>%
  as_tibble() %>%
  dplyr::filter(.id == 1 & .sample_id == 2)


test_that("filtering within eeg_lst signal table returns correct values in signal table", {
  expect_equal(as.matrix(filter1_sign_eeg$signal), as.matrix(distinct(select(filter1_sign_tbl, .id, .sample_id, X, Y))))
  expect_equal(as.matrix(filter2_sign_eeg$signal), as.matrix(distinct(select(filter2_sign_tbl, .id, .sample_id, X, Y))))
  expect_equal(as.matrix(filter3_sign_eeg$signal), as.matrix(distinct(select(filter3_sign_tbl, .id, .sample_id, X, Y))))
})


test_that("filtering within eeg_lst signal table returns correct values in segments table", {
  expect_equal(as.matrix(filter1_sign_eeg$segments), as.matrix(distinct(filter1_sign_tbl, .id, recording, segment, condition)))
  expect_equal(as.matrix(filter2_sign_eeg$segments), as.matrix(distinct(filter2_sign_tbl, .id, recording, segment, condition)))
  expect_equal(as.matrix(filter3_sign_eeg$segments), as.matrix(distinct(filter3_sign_tbl, .id, recording, segment, condition)))
})


test_that("filtering within eeg_lst signal table returns correct values in events table", {
  expect_equal(as.matrix(filter1_sign_eeg$events), as.matrix(distinct(select(filter1_sign_tbl, .id, type, description, .sample_0, .size, .channel))))
  expect_equal(as.matrix(filter2_sign_eeg$events), as.matrix(distinct(select(filter2_sign_tbl, .id, type, description, .sample_0, .size, .channel))))
  expect_equal(as.matrix(filter3_sign_eeg$events), as.matrix(distinct(select(filter3_sign_tbl, .id, type, description, .sample_0, .size, .channel))))
})



### filtering by eeg_list segments table variables ###

filter4_segm_eeg <- filter2_eeg <- filter(data, segment != 2)
filter4_segm_tbl <- extended_data %>%
  as_tibble() %>%
  dplyr::filter(segment != 2)


filter5_segm_eeg <- filter(data, condition == "a" & segment == 3)
filter5_segm_tbl <- extended_data %>%
  as_tibble() %>%
  dplyr::filter(condition == "a" & segment == 3)


filter6_segm_eeg <- filter(data, recording == "recording2")
filter6_segm_tbl <- extended_data %>%
  as_tibble() %>%
  dplyr::filter(recording == "recording2")


# the .id numbers are different, all else ok
test_that("filtering within eeg_lst segments table returns correct values in signal table", {
  expect_equal(as.matrix(filter4_segm_eeg$signal[, c(".id", "X", "Y")]), as.matrix(distinct(select(filter4_segm_tbl, .id, X, Y))))
  expect_equal(as.matrix(filter5_segm_eeg$signal[, c(".id", "X", "Y")]), as.matrix(distinct(select(filter5_segm_tbl, .id, X, Y))))
  expect_equal(as.matrix(filter6_segm_eeg$signal[, c(".id", "X", "Y")]), as.matrix(distinct(select(filter6_segm_tbl, .id, X, Y))))
})


# .id numbers are different, all else ok
test_that("filtering within eeg_lst segments table returns correct values in segments table", {
  expect_equal(as.matrix(filter4_segm_eeg$segments), as.matrix(distinct(filter4_segm_tbl, .id, recording, segment, condition)))
  expect_equal(as.matrix(filter5_segm_eeg$segments), as.matrix(distinct(filter5_segm_tbl, .id, recording, segment, condition)))
  expect_equal(as.matrix(filter6_segm_eeg$segments), as.matrix(distinct(filter6_segm_tbl, .id, recording, segment, condition)))
})


# .id numbers are different, all else ok
test_that("filtering within eeg_lst segments table returns correct values in segments table", {
  expect_equal(as.matrix(filter4_segm_eeg$events), as.matrix(distinct(select(filter4_segm_tbl, .id, type, description, .sample_0, .size, .channel))))
  expect_equal(as.matrix(filter5_segm_eeg$events), as.matrix(distinct(select(filter5_segm_tbl, .id, type, description, .sample_0, .size, .channel))))
  expect_equal(as.matrix(filter6_segm_eeg$events), as.matrix(distinct(select(filter6_segm_tbl, .id, type, description, .sample_0, .size, .channel))))
})


test_that("the classes of channels of signal_tbl remain after within eeg_lst table", {
  expect_equal(is_channel_dbl(filter1_sign_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter2_sign_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter3_sign_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter4_segm_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter5_segm_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter6_segm_eeg$signal$X), TRUE)
})


# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})



### test filter by variables across eeg_lst tables ###

filter1_eeg <- filter(data, segment == 2 & .id == 2)

# .ids are different
filter1_tbl <- extended_data %>%
  as_tibble() %>%
  filter(segment == 2 & .id == 2)

# it's not to do with the extended_data tibble because this gives the same (wrong) ids:
filter1_tbl_test <- data %>%
  as_tibble() %>%
  filter(segment == 2 & .id == 2) %>%
  dplyr::select(.id, time, recording, segment, condition, channel, amplitude) %>%
  tidyr::spread(key = channel, value = amplitude)


filter2_eeg <- filter(data, segment == 1 & .sample_id == 2)
filter2_tbl <- extended_data %>%
  as_tibble() %>%
  dplyr::filter(segment == 1 & .sample_id == 2)


# why does only this one work? 
filter3_eeg <- filter(data, .sample_id == 2 & !(recording == "recording2"))
filter3_tbl <- extended_data %>%
  as_tibble() %>%
  dplyr::filter(.sample_id == 2 & !(recording == "recording2"))


filter4_eeg <- filter(data, .id == 1 | condition == "a")
filter4_tbl <- extended_data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.id == 1 | condition == "a") 


filter5_eeg <- filter(data, .id == 2 | condition == "b")
filter5_tbl <- extended_data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.id == 2 | condition == "b") 


filter6_eeg <- filter(data, between(X, 0, 0.5)  & condition != "a")
filter6_tbl <- extended_data %>%
  as_tibble() %>%
  dplyr::filter(between(X, 0, 0.5)  & condition != "a")


# ids are different except for 3
test_that("filtering across eeg_lst tables returns the right signal table values", {
  expect_equal(as.matrix(filter1_eeg$signal[, c(".id", "X", "Y")]), as.matrix(distinct(select(filter1_tbl, .id, X, Y))))
  expect_equal(as.matrix(filter2_eeg$signal[, c(".id", "X", "Y")]), as.matrix(distinct(select(filter2_tbl, .id, X, Y))))
  expect_equal(as.matrix(filter3_eeg$signal[, c(".id", "X", "Y")]), as.matrix(distinct(select(filter3_tbl, .id, X, Y))))
  expect_equal(as.matrix(filter4_eeg$signal[, c(".id", "X", "Y")]), as.matrix(distinct(select(filter4_tbl, .id, X, Y))))
  expect_equal(as.matrix(filter5_eeg$signal[, c(".id", "X", "Y")]), as.matrix(distinct(select(filter5_tbl, .id, X, Y))))
  expect_equal(as.matrix(filter6_eeg$signal[, c(".id", "X", "Y")]), as.matrix(distinct(select(filter6_tbl, .id, X, Y))))
})


# ids are different except for 3
test_that("filtering across eeg_lst tables returns the right segment table values", {
  expect_equal(as.matrix(filter1_eeg$segments), as.matrix(distinct(filter1_tbl, .id, recording, segment, condition)))
  expect_equal(as.matrix(filter2_eeg$segments), as.matrix(distinct(filter2_tbl, .id, recording, segment, condition)))
  expect_equal(as.matrix(filter3_eeg$segments), as.matrix(distinct(filter3_tbl, .id, recording, segment, condition)))
  expect_equal(as.matrix(filter4_eeg$segments), as.matrix(distinct(filter4_tbl, .id, recording, segment, condition)))
  expect_equal(as.matrix(filter5_eeg$segments), as.matrix(distinct(filter5_tbl, .id, recording, segment, condition)))
})


# ids are different except for 3
test_that("filtering across eeg_lst tables returns the right events table values", {
  expect_equal(as.matrix(filter1_eeg$events), as.matrix(distinct(select(filter1_tbl, .id, type, description, .sample_0, .size, .channel))))
  expect_equal(as.matrix(filter2_eeg$events), as.matrix(distinct(select(filter2_tbl, .id, type, description, .sample_0, .size, .channel))))
  expect_equal(as.matrix(filter3_eeg$events), as.matrix(distinct(select(filter3_tbl, .id, type, description, .sample_0, .size, .channel))))
  expect_equal(as.matrix(filter4_eeg$events), as.matrix(distinct(select(filter4_tbl, .id, type, description, .sample_0, .size, .channel))))
  expect_equal(as.matrix(filter5_eeg$events), as.matrix(distinct(select(filter5_tbl, .id, type, description, .sample_0, .size, .channel))))
})


test_that("the classes of channels of signal_tbl remain after filtering across eeg_lst tables", {
  expect_equal(is_channel_dbl(filter1_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter2_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter3_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter4_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(filter5_eeg$signal$X), TRUE)
})


# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})




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




### test whether filter works on new variables ###

mutate_filter1_eeg <- mutate(data, time = as_time(.sample_id, unit = "milliseconds")) %>%
  filter(time == 2)


mutate_filter2_eeg <- mutate(data, time = as_time(.sample_id, unit = "seconds")) %>%
  filter(time == 0.002)


mutate_filter3_eeg <- data %>% 
  mutate(group = ifelse(.sample_id > 0, "late", "early")) %>%
  filter(group == "late")

mutate_filter3_tbl <- extended_data %>%
  dplyr::mutate(group = ifelse(.sample_id > 0, "late", "early")) %>%
  dplyr::filter(group == "late")


mutate_filter4_eeg <- data %>% 
  mutate(group = ifelse(Y > 0, "pos", "neg")) %>%
  filter(group == "neg")

mutate_filter4_tbl <- extended_data %>%
  dplyr::mutate(group = ifelse(Y > 0, "pos", "neg")) %>%
  dplyr::filter(group == "neg")


# why does the eeg_lst have fewer rows?
transmute_filter_eeg <- transmute(data, X = X + 1) %>%
  filter(recording == "recording1")

transmute_filter_tbl <- extended_data %>%
  dplyr::filter(recording == "recording1") %>%
  dplyr::mutate(X = X + 1) 
  

test_that("filtering on newly created variables returns correct values in signal table", {
  expect_equal(as.matrix(mutate_filter1_eeg$signal[, c(".id", ".sample_id", "X", "Y")]), as.matrix(mutate_filter2_eeg$signal[, c(".id", ".sample_id", "X", "Y")]))
  expect_equal(as.matrix(mutate_filter3_eeg$signal), as.matrix(distinct(select(mutate_filter3_tbl, .id, .sample_id, X, Y, group))))
  expect_equal(as.matrix(mutate_filter4_eeg$signal), as.matrix(distinct(select(mutate_filter4_tbl, .id, .sample_id, X, Y, group))))
  expect_equal(as.double(transmute_filter_eeg$signal$X), as.double(unique(transmute_filter_tbl$X)))
})


test_that("filtering on newly created variables returns correct values in segments table", {
  expect_equal(mutate_filter1_eeg$segments, mutate_filter2_eeg$segments)
  expect_equal(as.matrix(mutate_filter3_eeg$segments), as.matrix(distinct(select(mutate_filter3_tbl, .id, recording, segment, condition))))
  expect_equal(as.matrix(mutate_filter4_eeg$segments), as.matrix(distinct(select(mutate_filter4_tbl, .id, recording, segment, condition))))
  expect_equal(as.matrix(transmute_filter_eeg$segments), as.matrix(distinct(select(transmute_filter_tbl, .id, recording, segment, condition))))
})


test_that("filtering on newly created variables returns correct values in events table", {
  expect_equal(mutate_filter1_eeg$events, mutate_filter2_eeg$events)
  expect_equal(as.matrix(mutate_filter3_eeg$events), as.matrix(distinct(select(mutate_filter3_tbl, .id, type, description, .sample_0, .size, .channel))))
  expect_equal(as.matrix(mutate_filter4_eeg$events), as.matrix(distinct(select(mutate_filter4_tbl, .id, type, description, .sample_0, .size, .channel))))
  # why does the eeg_lst have fewer rows?
  expect_equal(as.matrix(transmute_filter_eeg$events), as.matrix(distinct(select(transmute_filter_tbl, .id, type, description, .sample_0, .size, .channel))))
})


test_that("the classes of channels of signal_tbl remain after filtering by new variables", {
  expect_equal(is_channel_dbl(mutate_filter1_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate_filter2_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate_filter3_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate_filter4_eeg$signal$X), TRUE)
  expect_equal(is_channel_dbl(transmute_filter_eeg$signal$X), TRUE)
})



### test whether filter works after grouping and adding vars  ###

mutate_all_filter_eeg <- data %>%
  group_by(.sample_id) %>%
  mutate_all(mean) %>%
  filter(condition == "b")

mutate_at_filter_eeg <- data %>%
  group_by(.sample_id) %>%
  mutate_at(channel_names(data), mean)  %>%
  filter(condition == "b")

# this throws too many warnings about attributes - why doesn't it do this anywhere else?
# mutate_a_tbl <- extended_data %>%
#   dplyr::group_by(.sample_id) %>%
#   dplyr::mutate(mean = mean(Y)) %>%
#   dplyr::filter(condition == "b")

# a different way - ids are different again
mutate_a_tbl <- data %>%
  as_tibble() %>%
  dplyr::group_by(time, channel) %>%
  dplyr::mutate(mean = mean(amplitude)) %>% 
  dplyr::select(.id, time, channel, mean, condition, segment, recording) %>%
  tidyr::spread(key = channel, value = mean) %>%
  ungroup() %>% # have to add this or it does weird stuff
  dplyr::filter(condition == "b")


# different lengths & sample_ids... eh? 
summarize_filter_eeg <- group_by(data, .sample_id) %>% 
  summarize(mean = mean(Y)) %>%
  filter(mean > -0.35)

summarize_filter_tbl <- extended_data %>%
  dplyr::group_by(.sample_id) %>%
  dplyr::summarize(mean = mean(Y)) %>%
  dplyr::filter(mean > -0.35)


summarize_at_filter_eeg <- data %>% 
  group_by(.id, recording, condition) %>%
  summarize_at_ch(channel_names(data), mean) %>%
  filter(X > 0 | Y > 0)

summarize_at_filter_tbl <- extended_data %>%
  dplyr::group_by(.id, recording, condition) %>%
  dplyr::summarise(X = mean(X), Y = mean(Y)) %>%
  ungroup() %>% # have to add this or it does weird stuff
  dplyr::filter(X > 0 | Y > 0)


summarize_all_filter_eeg <- group_by(data, .id, .sample_id) %>% 
  summarize_all_ch("mean") %>%
  filter(.sample_id < 0)

summarize_all_filter_tbl <- extended_data %>%
  dplyr::group_by(.id, .sample_id) %>%
  dplyr::summarize(X = mean(X), Y = mean(Y)) %>%
  dplyr::filter(.sample_id < 0)


# ids are different
summarize_all1_filter_eeg <- group_by(data, .id, condition) %>% 
  summarize_all_ch("mean") %>%
  filter(condition == "a")

summarize_all1_filter_tbl <- extended_data %>%
  dplyr::group_by(.id, condition) %>%
  dplyr::summarize(X = mean(X), Y = mean(Y)) %>%
  dplyr::filter(condition == "a")


# no grouping by id: means are different  
summarize_all2_filter_eeg <- group_by(data, condition) %>% 
  summarize_all_ch("mean") %>%
  filter(condition == "a")

summarize_all2_filter_tbl <- extended_data %>%
  dplyr::group_by(condition) %>%
  dplyr::summarize(X = mean(X), Y = mean(Y)) %>%
  dplyr::filter(condition == "a")


test_that("filtering after grouping and summarizing returns correct values in signal table", {
  expect_equal(mutate_all_filter_eeg$signal, mutate_at_filter_eeg$signal)
  expect_equal(as.matrix(mutate_at_filter_eeg$signal[, c(".id", "X", "Y")]), as.matrix(select(mutate_a_tbl, .id, X, Y)))
  expect_equal(as.double(summarize_filter_eeg$signal$mean), as.double(summarize_filter_tbl$mean))
  expect_equal(as.matrix(summarize_at_filter_eeg$signal[, c(".id", "X", "Y")]), as.matrix(select(summarize_at_filter_tbl, .id, X, Y)))
  expect_equal(as.matrix(summarize_all_filter_eeg$signal[, c(".id", "X", "Y")]), as.matrix(select(summarize_all_filter_tbl, .id, X, Y)))
  expect_equal(as.matrix(summarize_all1_filter_eeg$signal[, c(".id", "X", "Y")]), as.matrix(select(summarize_all1_filter_tbl, .id, X, Y)))
  expect_equal(as.matrix(summarize_all2_filter_eeg$signal[, c("X", "Y")]), as.matrix(select(summarize_all2_filter_tbl, X, Y)))
})


test_that("filtering after grouping and summarizing returns correct values in segments table if, applicable", {
  expect_equal(mutate_all_filter_eeg$segments, mutate_at_filter_eeg$segments)
  expect_equal(as.matrix(mutate_at_filter_eeg$segments), as.matrix(distinct(select(mutate_a_tbl, .id, recording, segment, condition))))
  expect_equal(as.matrix(summarize_at_filter_eeg$segments)[, c(".id", "recording", "condition")], as.matrix(distinct(select(summarize_at_filter_tbl, .id, recording, condition))))
  expect_equal(as.double(summarize_all_filter_eeg$segments$.id), as.double(unique(summarize_all_filter_tbl$.id)))
  expect_equal(as.matrix(summarize_all1_filter_eeg$segments[, c(".id", "condition")]), as.matrix(select(summarize_all1_filter_tbl, .id, condition)))
  expect_equal(as.matrix(summarize_all2_filter_eeg$segments[, c("condition")]), as.matrix(select(summarize_all2_filter_tbl, condition)))
})


# not relevant
# test_that("filtering after grouping and summarizing returns correct values in events table", {
# 
# })


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
