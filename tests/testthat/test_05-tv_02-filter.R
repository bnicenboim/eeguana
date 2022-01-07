library(eeguana)
options(dplyr.summarise.inform = FALSE)
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

###########3

test_that("filter and eeg_filter are the same", {
  expect_equal_eeg_lst(eeg_filter(data, .id == 1), dplyr::filter(data, .id == 1))
})


###################################################
### 1. Dplyr::Filtering by .id (applies to all tables) ###
###################################################

# a) Create eeg_lsts and tibbles with same dplyr::filters and test against each other

filter1_id_eeg <- eeg_filter(data, .id == 1)

filter1_id_sign_tbl <- dplyr::as_tibble(data$.signal) %>%
  dplyr::filter(.id == 1)
filter1_id_segm_tbl <- dplyr::as_tibble(data$.segments) %>%
  dplyr::filter(.id == 1)
filter1_id_evts_tbl <- dplyr::as_tibble(data$.events) %>%
  dplyr::filter(.id == 1)


filter2_id_eeg <- eeg_filter(data, .id != 2)

filter2_id_sign_tbl <- dplyr::as_tibble(data$.signal) %>%
  dplyr::filter(.id != 2)
filter2_id_segm_tbl <- dplyr::as_tibble(data$.segments) %>%
  dplyr::filter(.id != 2)
filter2_id_evts_tbl <- dplyr::as_tibble(data$.events) %>%
  dplyr::filter(.id != 2)


filter3_id_eeg <- dplyr::filter(data, .id == 3)

filter3_id_sign_tbl <- dplyr::as_tibble(data$.signal) %>%
  dplyr::filter(.id == 3)
filter3_id_segm_tbl <- dplyr::as_tibble(data$.segments) %>%
  dplyr::filter(.id == 3)
filter3_id_evts_tbl <- dplyr::as_tibble(data$.events) %>%
  dplyr::filter(.id == 3)


test_that("dplyr::filtering within signal table returns correct values in signal table", {
  expect_equal(as.matrix(filter1_id_eeg$.signal), as.matrix(filter1_id_sign_tbl))
  expect_equal(as.matrix(filter2_id_eeg$.signal), as.matrix(filter2_id_sign_tbl))
  expect_equal(as.matrix(filter3_id_eeg$.signal), as.matrix(filter3_id_sign_tbl))
})


test_that("dplyr::filtering within signal table returns correct values in segments table", {
  expect_equal(as.matrix(filter1_id_eeg$.segments), as.matrix(filter1_id_segm_tbl))
  expect_equal(as.matrix(filter2_id_eeg$.segments), as.matrix(filter2_id_segm_tbl))
  expect_equal(as.matrix(filter3_id_eeg$.segments), as.matrix(filter3_id_segm_tbl))
})


test_that("dplyr::filtering within signal table returns correct values in events table", {
  expect_equal(as.matrix(filter1_id_eeg$.events), as.matrix(filter1_id_evts_tbl))
  expect_equal(as.matrix(filter2_id_eeg$.events), as.matrix(filter2_id_evts_tbl))
  expect_equal(as.matrix(filter3_id_eeg$.events), as.matrix(filter3_id_evts_tbl))
})


test_that("the classes of channels of signal_tbl remain after within eeg_lst table", {
  expect_equal(is_channel_dbl(filter1_id_eeg$.signal$X), TRUE)
  expect_equal(is_channel_dbl(filter2_id_eeg$.signal$X), TRUE)
  expect_equal(is_channel_dbl(filter3_id_eeg$.signal$X), TRUE)
})


# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})




##############################################
### 2. Dplyr::Filtering by signal table variables ###
##############################################

# a) Test signal/segments table by comparing eeg_lst with tibble

filter1_sign_eeg <- eeg_filter(data, .sample >= 0)
filter1_sign_tbl <- dplyr::left_join(dplyr::as_tibble(data$.signal), dplyr::as_tibble(data$.segments), by = ".id") %>%
  dplyr::filter(.sample >= 0)
filter1_events <- events_tbl(data) %>%
  dplyr::filter(.initial >= 0 | .final >= 0) %>%
  dplyr::mutate(.final = ifelse(.initial < 0, 0, .final), .initial = ifelse(.initial < 0, 0, .initial))

filter2_sign_eeg <- eeg_filter(data, .id == 1 & .sample == 2)
filter2_sign_tbl <- dplyr::left_join(dplyr::as_tibble(data$.signal), dplyr::as_tibble(data$.segments), by = ".id") %>%
  dplyr::filter(.id == 1 & .sample == 2)


filter3_sign_eeg <- eeg_filter(data, X < 0 & Y < 0)
filter3_sign_tbl <- dplyr::left_join(dplyr::as_tibble(data$.signal), dplyr::as_tibble(data$.segments), by = ".id") %>%
  dplyr::filter(X < 0 & Y < 0)


test_that("dplyr::filtering within signal table works in signal table", {
  expect_equal(
    as.matrix(filter1_sign_eeg$.signal),
    as.matrix(dplyr::select(filter1_sign_tbl, .id, .sample, X, Y))
  )
  expect_equal(
    as.matrix(filter2_sign_eeg$.signal),
    as.matrix(dplyr::select(filter2_sign_tbl, .id, .sample, X, Y))
  )
  expect_equal(
    as.matrix(filter3_sign_eeg$.signal),
    as.matrix(dplyr::select(filter3_sign_tbl, .id, .sample, X, Y))
  )
})


test_that("dplyr::filtering within signal table works in segments table", {
  expect_setequal(
    as.matrix(filter1_sign_eeg$.segments),
    as.matrix(dplyr::select(filter1_sign_tbl, .id, .recording, segment, condition))
  )
  expect_setequal(
    as.matrix(filter2_sign_eeg$.segments),
    as.matrix(dplyr::select(filter2_sign_tbl, .id, .recording, segment, condition))
  )
  expect_setequal(
    as.matrix(filter3_sign_eeg$.segments),
    as.matrix(dplyr::select(filter3_sign_tbl, .id, .recording, segment, condition))
  )
})

# b. Test the events table which will fail for now

filter4_sign_eeg <- data %>% eeg_filter(.sample == -1)
filter4_evn_tbl <- dplyr::as_tibble(data$.events) %>%
  dplyr::filter(-1 %>% between(.initial, .final)) %>%
  dplyr::mutate(.initial = -1, .final = -1)


# really want *only* the events < 0 (dplyr::filter won't take a vector), but probs ok
filter5_sign_eeg <- data %>% eeg_filter(.id == 1 & .sample < 0)
filter5_evn_tbl <- dplyr::as_tibble(data$.events) %>%
  dplyr::group_by(.id, .initial) %>%
  dplyr::filter(.id == 1, any(seq(.initial, by = 1, .final) < 0)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(.final = ifelse(.final >= 0, -1, .final))

test_that("dplyr::filtering in signal table returns the right events", {
  expect_setequal(as.matrix(filter4_sign_eeg$.events), as.matrix(filter4_evn_tbl))
  expect_setequal(as.matrix(filter5_sign_eeg$.events), as.matrix(filter5_evn_tbl))
  expect_equal(
    as.matrix(filter1_sign_eeg$.events),
    as.matrix(filter1_events)
  )
})



# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})



################################################
### 3. Dplyr::Filtering by segments table variables ###
################################################

# a) Test all tables by comparing eeg_lst with tibble

filter1_segm_eeg <- eeg_filter(data, segment != 2)

filter1s_segm_tbl <- dplyr::left_join(dplyr::as_tibble(data$.signal), dplyr::as_tibble(data$.segments), by = ".id") %>%
  dplyr::filter(segment != 2)

filter1e_segm_tbl <- dplyr::left_join(dplyr::as_tibble(data$.segments), dplyr::as_tibble(data$.events), by = ".id") %>%
  dplyr::filter(segment != 2) %>%
  dplyr::distinct(.id, .type, .description, .initial, .final, .channel)


filter2_segm_eeg <- dplyr::filter(data, condition == "a" & segment == 3)

filter2s_segm_tbl <- dplyr::left_join(dplyr::as_tibble(data$.signal), dplyr::as_tibble(data$.segments), by = ".id") %>%
  dplyr::filter(condition == "a" & segment == 3)

filter2e_segm_tbl <- dplyr::left_join(dplyr::as_tibble(data$.segments), dplyr::as_tibble(data$.events), by = ".id") %>%
  dplyr::filter(condition == "a" & segment == 3) %>%
  dplyr::distinct(.id, .type, .description, .initial, .final, .channel)


filter3_segm_eeg <- eeg_filter(data, .recording == "recording2")

filter3s_segm_tbl <- dplyr::left_join(dplyr::as_tibble(data$.signal), dplyr::as_tibble(data$.segments), by = ".id") %>%
  dplyr::filter(.recording == "recording2")

filter3e_segm_tbl <- dplyr::left_join(dplyr::as_tibble(data$.segments), dplyr::as_tibble(data$.events), by = ".id") %>%
  dplyr::filter(.recording == "recording2") %>%
  dplyr::select(.id, .type, .description, .initial, .final, .channel)


test_that("dplyr::filtering within segments table works in signal table", {
  expect_equal(
    as.matrix(filter1_segm_eeg$.signal),
    as.matrix(dplyr::select(filter1s_segm_tbl, .id, .sample, X, Y))
  )
  expect_equal(
    as.matrix(filter2_segm_eeg$.signal),
    as.matrix(dplyr::select(filter2s_segm_tbl, .id, .sample, X, Y))
  )
  expect_equal(
    as.matrix(filter3_segm_eeg$.signal),
    as.matrix(dplyr::select(filter3s_segm_tbl, .id, .sample, X, Y))
  )
})


test_that("dplyr::filtering within segments table works in segments table", {
  expect_setequal(
    as.matrix(filter1_segm_eeg$.segments),
    as.matrix(dplyr::select(filter1s_segm_tbl, .id, .recording, segment, condition))
  )
  expect_setequal(
    as.matrix(filter2_segm_eeg$.segments),
    as.matrix(dplyr::select(filter2s_segm_tbl, .id, .recording, segment, condition))
  )
  expect_setequal(
    as.matrix(filter3_segm_eeg$.segments),
    as.matrix(dplyr::select(filter3s_segm_tbl, .id, .recording, segment, condition))
  )
})


test_that("dplyr::filtering within segments table returns correct values in events table", {
  expect_equal(
    as.matrix(filter1_segm_eeg$.events),
    as.matrix(filter1e_segm_tbl)
  )
  expect_equal(
    as.matrix(filter2_segm_eeg$.events),
    as.matrix(filter2e_segm_tbl)
  )
  expect_equal(
    as.matrix(filter3_segm_eeg$.events),
    as.matrix(filter3e_segm_tbl)
  )
})


test_that("the classes of channels of signal_tbl remain after within eeg_lst table", {
  expect_equal(is_channel_dbl(filter1_segm_eeg$.signal$X), TRUE)
  expect_equal(is_channel_dbl(filter2_segm_eeg$.signal$X), TRUE)
  expect_equal(is_channel_dbl(filter3_segm_eeg$.signal$X), TRUE)
})


# check against original data
test_that("data didn't change", {
  expect_equal_eeg_lst(reference_data, data)
})




#########################################################
### 4. Test dplyr::filter by variables across eeg_lst tables ###
#########################################################

# a) Test signal/segments tables by comparing eeg_lst with tibble

filter1_eeg <- eeg_filter(data, .sample == 2 & segment == 2)

filter1_tbl <- dplyr::left_join(dplyr::as_tibble(data$.signal), dplyr::as_tibble(data$.segments), by = ".id") %>%
  dplyr::filter(.sample == 2 & segment == 2)

# just checking this is the same as above
# filter1_tbl <- data %>%
#   dplyr::as_tibble() %>%
#   dplyr::filter(time == 0.002 & segment == 2) %>%
#   spread(key = channel, value = amplitude)


filter2_eeg <- eeg_filter(data, .sample < 2 & !(.recording == "recording2"))
filter2_tbl <- dplyr::left_join(dplyr::as_tibble(data$.signal), dplyr::as_tibble(data$.segments), by = ".id") %>%
  dplyr::filter(.sample < 2 & !(.recording == "recording2"))

filter3_eeg <- eeg_filter(data, .sample == 1 | condition == "a")
filter3_tbl <- dplyr::left_join(dplyr::as_tibble(data$.signal), dplyr::as_tibble(data$.segments), by = ".id") %>%
  dplyr::filter(.sample == 1 | condition == "a")

filter4_eeg <- eeg_filter(data, .id == 2 | condition == "b")
filter4_tbl <- dplyr::left_join(dplyr::as_tibble(data$.signal), dplyr::as_tibble(data$.segments), by = ".id") %>%
  dplyr::filter(.id == 2 | condition == "b")


filter5_eeg <- eeg_filter(data, between(X, 0, 0.5) & segment != 1)
filter5_tbl <- dplyr::left_join(dplyr::as_tibble(data$.signal), dplyr::as_tibble(data$.segments), by = ".id") %>%
  dplyr::filter(between(X, 0, 0.5) & segment != 1)


filter6_eeg <- eeg_filter(data, Y > 0 & .recording == "recording1")
filter6_tbl <- dplyr::left_join(dplyr::as_tibble(data$.signal), dplyr::as_tibble(data$.segments), by = ".id") %>%
  dplyr::filter(Y > 0 & .recording == "recording1")


test_that("dplyr::filtering across tables returns the right signal table values", {
  expect_setequal(
    as.matrix(filter1_eeg$.signal),
    as.matrix(dplyr::select(filter1_tbl, .id, .sample, X, Y))
  )
  expect_setequal(
    as.matrix(filter2_eeg$.signal),
    as.matrix(dplyr::select(filter2_tbl, .id, .sample, X, Y))
  )
  expect_setequal(
    as.matrix(filter3_eeg$.signal),
    as.matrix(dplyr::select(filter3_tbl, .id, .sample, X, Y))
  )
  expect_setequal(
    as.matrix(filter4_eeg$.signal),
    as.matrix(dplyr::select(filter4_tbl, .id, .sample, X, Y))
  )
  expect_setequal(
    as.matrix(filter5_eeg$.signal),
    as.matrix(dplyr::select(filter5_tbl, .id, .sample, X, Y))
  )
  expect_setequal(
    as.matrix(filter6_eeg$.signal),
    as.matrix(dplyr::select(filter6_tbl, .id, .sample, X, Y))
  )
})


test_that("dplyr::filtering across tables returns the right segments table values", {
  expect_setequal(
    as.matrix(filter1_eeg$.segments),
    as.matrix(dplyr::select(filter1_tbl, .id, .recording, segment, condition))
  )
  expect_setequal(
    as.matrix(filter2_eeg$.segments),
    as.matrix(dplyr::select(filter2_tbl, .id, .recording, segment, condition))
  )
  expect_setequal(
    as.matrix(filter3_eeg$.segments),
    as.matrix(dplyr::select(filter3_tbl, .id, .recording, segment, condition))
  )
  expect_setequal(
    as.matrix(filter4_eeg$.segments),
    as.matrix(dplyr::select(filter4_tbl, .id, .recording, segment, condition))
  )
  expect_setequal(
    as.matrix(filter5_eeg$.segments),
    as.matrix(dplyr::select(filter5_tbl, .id, .recording, segment, condition))
  )
  expect_setequal(
    as.matrix(filter6_eeg$.segments),
    as.matrix(dplyr::select(filter6_tbl, .id, .recording, segment, condition))
  )
})



## # b) A couple of tests of the events table from the above dplyr::filters

## filter1_evts_tbl <- dplyr::left_join(dplyr::as_tibble(data$.segments), dplyr::as_tibble(data$.events)) %>%
##   dplyr::group_by(.id, .initial) %>%
##   dplyr::filter(segment == 2 & 2 %in% seq(.initial, by = 1, length.out = .final))

## filter2_evts_tbl <- dplyr::left_join(dplyr::as_tibble(data$.segments), dplyr::as_tibble(data$.events)) %>%
##   dplyr::group_by(.id, .initial) %>%
##   dplyr::filter(!(.recording == "recording2") & any(seq(.initial, by = 1, length.out = .final) < 2))


## # won't work for now
## test_that("dplyr::filtering in signal table returns the right events", {
##   expect_setequal(as.matrix(filter1_eeg$.events), as.matrix(filter1_evts_tbl))
##   expect_setequal(as.matrix(filter2_eeg$.events), as.matrix(filter2_evts_tbl))
## })


# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})





#####################################################
### 5. Test whether dplyr::filter works on new variables ###
#####################################################

# a) Test signal/segments tables by comparing eeg_lst with tibble

mutate_filter1_eeg <- data %>%
  eeg_mutate(time = as_time(.sample, .unit = "milliseconds")) %>%
  eeg_filter(time == 2)


suppressMessages(mutate_filter1_tbl <- data$.signal %>%
  dplyr::mutate(time = as_time(.sample, .unit = "milliseconds")) %>%
  dplyr::filter(time == 2) %>%
  dplyr::left_join(dplyr::as_tibble(data$.segments), by = ".id"))


mutate_filter2_eeg <- data %>%
  eeg_mutate(time = as_time(.sample, .unit = "seconds")) %>%
  eeg_filter(time == 0.002)


suppressMessages(mutate_filter2_tbl <- data$.signal %>%
  dplyr::mutate(time = as_time(.sample, .unit = "seconds")) %>%
  dplyr::filter(time == 0.002) %>%
  dplyr::left_join(dplyr::as_tibble(data$.segments), by = ".id"))

mutate_filter3_eeg <- data %>%
  eeg_mutate(group = ifelse(.sample > 0, "late", "early")) %>%
  eeg_filter(group == "late")

mutate_filter3_tbl <- dplyr::left_join(dplyr::as_tibble(data$.signal), dplyr::as_tibble(data$.segments), by = ".id") %>%
  dplyr::mutate(group = ifelse(.sample > 0, "late", "early")) %>%
  dplyr::filter(group == "late")

mutate_filter4_eeg <- data %>%
  eeg_mutate(group = ifelse(Y > 0, "pos", "neg")) %>%
  eeg_filter(group == "neg")

mutate_filter4_tbl <- dplyr::left_join(dplyr::as_tibble(data$.signal), dplyr::as_tibble(data$.segments), by = ".id") %>%
  dplyr::mutate(group = ifelse(Y > 0, "pos", "neg")) %>%
  dplyr::filter(group == "neg")


transmute_filter_eeg <- eeg_transmute(data, X = X + 1) %>%
  eeg_filter(.recording == "recording1")

transmute_filter_tbl <- dplyr::left_join(dplyr::as_tibble(data$.signal), dplyr::as_tibble(data$.segments), by = ".id") %>%
  dplyr::filter(.recording == "recording1") %>%
  dplyr::mutate(X = X + 1)


test_that("dplyr::filtering on newly created variables works in signal table", {
  expect_equal(
    as.matrix(mutate_filter1_eeg$.signal[, !c("time")]),
    as.matrix(mutate_filter2_eeg$.signal[, !c("time")])
  )
  expect_setequal(
    as.matrix(mutate_filter3_eeg$.signal),
    as.matrix(dplyr::select(mutate_filter3_tbl, .id, .sample, X, Y, group))
  )
  expect_setequal(
    as.matrix(mutate_filter4_eeg$.signal),
    as.matrix(dplyr::select(mutate_filter4_tbl, .id, .sample, X, Y, group))
  )
  expect_equal(
    as.double(transmute_filter_eeg$.signal$X),
    as.double(unique(transmute_filter_tbl$X))
  )
})


test_that("dplyr::filtering on newly created variables works in segments table", {
  expect_equal(
    mutate_filter1_eeg$.segments,
    mutate_filter2_eeg$.segments
  )
  expect_setequal(
    as.matrix(mutate_filter3_eeg$.segments),
    as.matrix(dplyr::select(mutate_filter3_tbl, .id, .recording, segment, condition))
  )
  expect_setequal(
    as.matrix(mutate_filter4_eeg$.segments),
    as.matrix(dplyr::select(mutate_filter4_tbl, .id, .recording, segment, condition))
  )
  expect_setequal(
    as.matrix(transmute_filter_eeg$.segments),
    as.matrix(dplyr::select(transmute_filter_tbl, .id, .recording, segment, condition))
  )
})




## # b) A couple of tests of the events table

## mutate_filter1_evts_tbl <- dplyr::as_tibble(data$.events) %>%
##   dplyr::group_by(.id, .initial) %>%
##   dplyr::filter(2 %in% seq(.initial, by = 1, .final))


## mutate_filter3_evts_tbl <- dplyr::as_tibble(data$.events) %>%
##   dplyr::group_by(.id, .initial) %>%
##   dplyr::filter(any(seq(.initial, by = 1, length.out = .final) > 0))


## # won't work for now
## test_that("dplyr::filtering with new variables returns the right events", {
##   expect_setequal(as.matrix(mutate_filter1_eeg$.events),
##                   as.matrix(mutate_filter1_evts_tbl))
##   expect_setequal(as.matrix(mutate_filter3_eeg$.events),
##                as.matrix(mutate_filter3_evts_tbl))
## })


test_that("the classes of channels of signal_tbl remain after dplyr::filtering by new variables", {
  expect_equal(is_channel_dbl(mutate_filter1_eeg$.signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate_filter2_eeg$.signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate_filter3_eeg$.signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate_filter4_eeg$.signal$X), TRUE)
  expect_equal(is_channel_dbl(transmute_filter_eeg$.signal$X), TRUE)
})


# check against original data
test_that("data didn't change", {
  expect_equal_eeg_lst(reference_data, data)
})


################################################################################
### 6. Test whether dplyr::filter works after grouping and adding/summarizing vars  ###
################################################################################

# a) Test signal/segments tables by comparing eeg_lst with tibble

# doesn't work but not really relevant to eeguana
# dplyr::mutate_all_filter_eeg <- data %>%
#   dplyr::group_by(.sample) %>%
#   dplyr::mutate_all(mean) %>%
#   dplyr::filter(condition == "b")

# # shouldn't this group by .sample?
# dplyr::mutate_at_filter_eeg <- data %>%
#   dplyr::group_by(.sample) %>%
#   dplyr::mutate_at(channel_names(data), funs(mean)) %>%
#   dplyr::filter(condition == "b")
#
# # this doesn't group by time either
# dplyr::mutate_at_tbl <- data %>%
#   dplyr::as_tibble() %>%
#    dplyr::select(.id, time, channel, amplitude, condition, segment, recording) %>%
#   tidyr::spread(key = channel, value = amplitude) %>%
#    dplyr::group_by(time) %>%
#    dplyr::mutate_at(channel_names(data), funs(mean)) %>%
#    dplyr::filter(condition == "b")



summarize_filter_eeg <- eeg_group_by(data, .sample) %>%
  eeg_summarize(mean = mean(Y)) %>%
  eeg_filter(mean > -0.35)

summarize_filter_tbl <- dplyr::left_join(dplyr::as_tibble(data$.signal), dplyr::as_tibble(data$.segments), by = ".id") %>%
  dplyr::group_by(.sample) %>%
  dplyr::summarize(mean = mean(Y)) %>%
  dplyr::filter(mean > -0.35)


summarize_at_filter_eeg <- data %>%
  dplyr::group_by(.id, .recording, condition) %>%
  eeg_summarize(across(channel_names(data), mean)) %>%
  dplyr::filter(X > 0 & Y > 0)


summarize_at_filter_tbl <- dplyr::left_join(dplyr::as_tibble(data$.signal), dplyr::as_tibble(data$.segments), by = ".id") %>%
  dplyr::group_by(.id, .recording, condition) %>%
  dplyr::summarise(X = mean(X), Y = mean(Y)) %>%
  dplyr::ungroup() %>% # have to add this or it does weird stuff
  dplyr::filter(X > 0 & Y > 0)


summarize_all_filter_eeg <- eeg_group_by(data, .id, .sample) %>%
  eeg_summarize(across(where(is_channel_dbl), "mean")) %>%
  eeg_filter(.sample < 0)

summarize_all_filter_tbl <- dplyr::left_join(dplyr::as_tibble(data$.signal), dplyr::as_tibble(data$.segments), by = ".id") %>%
  dplyr::group_by(.id, .sample) %>%
  dplyr::summarize(X = mean(X), Y = mean(Y)) %>%
  dplyr::filter(.sample < 0)


# warnings about .id
summarize_all1_filter_eeg <- eeg_group_by(data, .id, condition) %>%
  eeg_summarize(across(channel_names(data), "mean")) %>%
  eeg_filter(condition == "a")

summarize_all1_filter_tbl <- dplyr::left_join(dplyr::as_tibble(data$.signal), dplyr::as_tibble(data$.segments), by = ".id") %>%
  dplyr::group_by(.id, condition) %>%
  dplyr::summarize(X = mean(X), Y = mean(Y)) %>%
  dplyr::filter(condition == "a")


summarize_all2_filter_eeg <- eeg_group_by(data, condition) %>%
  eeg_summarize(across(channel_names(data), "mean")) %>%
  eeg_filter(condition == "a")


summarize_all2_filter_tbl <- dplyr::left_join(dplyr::as_tibble(data$.signal), dplyr::as_tibble(data$.segments), by = ".id") %>%
  dplyr::group_by(condition) %>%
  dplyr::summarize(X = mean(X), Y = mean(Y)) %>%
  dplyr::filter(condition == "a")



test_that("dplyr::filtering after grouping and summarizing works in signal table", {
  # expect_equal(as.matrix(dplyr::mutate_at_filter_eeg$.signal[, !c(".sample")]),
  #              as.matrix(dplyr::select(dplyr::mutate_a_tbl, .id, X, Y)))
  expect_equal(
    as.double(summarize_filter_eeg$.signal$mean),
    as.double(summarize_filter_tbl$mean)
  )
  expect_equal(
    as.matrix(summarize_all_filter_eeg$.signal),
    as.matrix(dplyr::select(summarize_all_filter_tbl, .id, .sample, X, Y))
  )
  expect_equal(
    as.matrix(summarize_all1_filter_eeg$.signal[, !c(".sample")]),
    as.matrix(dplyr::select(summarize_all1_filter_tbl, .id, X, Y))
  )
  expect_equal(
    as.matrix(summarize_all2_filter_eeg$.signal[, c("X", "Y")]),
    as.matrix(dplyr::select(summarize_all2_filter_tbl, X, Y))
  )
})


test_that("dplyr::filtering after grouping and summarizing works in segments table", {
  # expect_setequal(as.matrix(dplyr::mutate_at_filter_eeg$.segments),
  #              as.matrix(dplyr::select(dplyr::mutate_a_tbl, .id, .recording, segment, condition)))
  expect_setequal(
    as.matrix(summarize_at_filter_eeg$.segments[, c(".id", ".recording", "condition")]),
    as.matrix(dplyr::select(summarize_at_filter_tbl, .id, .recording, condition))
  )
  expect_setequal(
    as.double(summarize_all_filter_eeg$.segments$.id),
    as.double(summarize_all_filter_tbl$.id)
  )
  expect_equal(
    as.matrix(summarize_all1_filter_eeg$.segments[, c(".id", "condition")]),
    as.matrix(dplyr::select(summarize_all1_filter_tbl, .id, condition))
  )
  expect_equal(
    as.matrix(summarize_all2_filter_eeg$.segments[, c("condition")]),
    as.matrix(dplyr::select(summarize_all2_filter_tbl, condition))
  )
})




# b) A couple of events table tests

test_that("dplyr::summarizes don't have any individual events", {
  expect_true(nrow(summarize_filter_eeg$.events) == 0)
  expect_true(nrow(summarize_at_filter_eeg$.events) == 0)
  expect_true(nrow(summarize_all_filter_eeg$.events) == 0)
  expect_true(nrow(summarize_all1_filter_eeg$.events) == 0)
  expect_true(nrow(summarize_all2_filter_eeg$.events) == 0)
})


test_that("the classes of channels of signal_tbl remain after dplyr::filtering by new variables", {
  # expect_equal(is_channel_dbl(dplyr::mutate_at_filter_eeg$.signal$X), TRUE)
  expect_equal(is_channel_dbl(summarize_filter_eeg$.signal$mean), TRUE)
  expect_equal(is_channel_dbl(summarize_at_filter_eeg$.signal$X), TRUE)
  expect_equal(is_channel_dbl(summarize_all_filter_eeg$.signal$X), TRUE)
  expect_equal(is_channel_dbl(summarize_all1_filter_eeg$.signal$X), TRUE)
  expect_equal(is_channel_dbl(summarize_all2_filter_eeg$.signal$X), TRUE)
})



###############
## Group by dplyr::filter
##################

data_NA <- data %>% eeg_mutate(X = ifelse(.id == 1 & .sample == 1, NA, X))
data_NAm1 <- data_NA %>% eeg_filter(.id != 1 | .sample != 1)
data_NAm1id <- data_NA %>% eeg_filter(.id != 1)

test_that("grouped filter works", {
  expect_equal(data_NA %>%
    dplyr::group_by(.id) %>%
    dplyr::filter(!anyNA(X)) %>%
    dplyr::ungroup(), data_NAm1id)
})

test_that("dplyr::filter_at and grouped dplyr::filtered at", {
  ## everything except the NA:
  expect_equal(data_NA %>% dplyr::filter_at(channel_names(.), ~ !is.na(.)), data_NAm1)
  expect_equal(data_NA %>%
    dplyr::group_by(.id) %>%
    dplyr::filter_at(channel_names(.), ~ !is.na(.)) %>%
    dplyr::ungroup(), data_NAm1)
  ## removes .id ==1
  expect_equal(data_NA %>%
    dplyr::group_by(.id) %>%
    dplyr::filter_at(channel_names(.), ~ !anyNA(.)) %>%
    dplyr::ungroup(), data_NAm1id)
  expect_equal(data_NA %>%
    dplyr::group_by(.id) %>%
    dplyr::filter_at(channel_names(.), dplyr::all_vars(!anyNA(.))) %>%
    dplyr::ungroup(), data_NAm1id)
})

test_that("slice_signal works", {
  expect_equal(slice_signal(data, 11:60), dplyr::filter(data, .id != 1))
  expect_equal(slice_signal(data, 1:5), dplyr::filter(data, .id == 1, .sample <= 0))
})

####

## check against original data
test_that("data didn't change", {
  expect_equal_eeg_lst(reference_data, data)
})

message("\n*****")
message("check when I filter all the samples from .signal of some .id, it needs to remove it from segments, and viceversa")
message("*****\n")
