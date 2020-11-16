context("test tidyverse dplyr::mutate")
library(eeguana)
library(dplyr)
expect_equal_plain_df <- eeguana:::expect_equal_plain_df
expect_equal_but_sgl <- eeguana:::expect_equal_but_sgl
expect_equal_but_cnt_sgl <- eeguana:::expect_equal_but_cnt_sgl
expect_equal_but_sgm <- eeguana:::expect_equal_but_sgm
expect_equal_but_cnt_sgm <- eeguana:::expect_equal_but_cnt_sgm

# tests when factors are used should be done.
data_1 <- eeguana:::data_sincos3id

# just some different X and Y
data_2 <- dplyr::mutate(data_1,
  .recording = "recording2",
  X = sin(X + 10),
  Y = cos(Y - 10),
  condition = c("b", "a", "b")
)

# bind it all together
data <- bind(data_1, data_2)

# for checks later
reference_data <- data.table::copy(data)



##############################################
### test dplyr dplyr::mutate on ungrouped eeg_lst ###
##############################################
test_mutates <- function(data) {
  ref_data <- data.table::copy(data)
  signal_tbl <- as.data.frame(data$.signal)%>%
      left_join(data$.segments) %>%
      group_by_at(vars(groups))
  grouped <- length(group_vars(data))>0
    groups <- group_vars(data)
    to_remove <- colnames(data$.segments)[-1]

  mutate_c <- function(tbl,...) {
    mutate(tbl, ...) %>%
      ungroup() %>%
      select(-one_of(to_remove))
  }

  data_X10 <- mutate(data, X = X + 10)
  expect_equal_plain_df(data_X10$.signal,
                        mutate_c(signal_tbl, X= X +10))
  expect_equal_but_cnt_sgl(data_X10,
                           data)

  data_Xs10 <- mutate(data, X = X * segment)
  expect_equal_plain_df(data_Xs10$.signal,
                        mutate_c(signal_tbl, X = X * segment)
                        )
  expect_equal_but_cnt_sgl(data_Xs10,
                           data)


  data_ZZX10 <- mutate(data, ZZ = X + 10)
  expect_equal_plain_df(data_ZZX10$.signal,
                        mutate_c(signal_tbl, ZZ = X + 10))
  expect_equal_but_sgl(data_ZZX10,
                       data)
  expect_true(nrow(filter(data_ZZX10$.events, .channel == "ZZ")) == 0)
  expect_true(nrow(filter(channels_tbl(data_ZZX10), .channel == "ZZ")) > 0)

if(!grouped){
  data_ZZX10XX <- mutate(data, ZZ = X + 10, XX = 1+ ZZ )
  expect_equal_plain_df(data_ZZX10$.signal,
                        mutate_c(signal_tbl, ZZ = X + 10, XX = 1 +ZZ))
  expect_equal_but_sgl(data_ZZX10XX,
                       data)
  expect_true(nrow(filter(data_ZZX10XX$.events, .channel == "ZZ")) == 0)
  expect_true(nrow(filter(channels_tbl(data_ZZX10XX), .channel == "ZZ")) > 0)
} else {
message(" mutate(data, ZZ = X + 10, XX = 1+ ZZ ) won't work for groups")
}

message("fix this so that mutate(data, ZZ = X + 10, XX = 1+ ZZ ) won't work for groups or ungroups and speedsup")
  message("test grouped transmute")
  data_mean <- mutate(data, mean = mean(X))
  expect_equal_plain_df(data_mean$.signal,
                        mutate_c(signal_tbl, mean = mean(X)))
  expect_equal_but_sgl(data_mean,
                       data)

  data_mean <- mutate(data, mean = mean(X), m = X +2)
  expect_equal_plain_df(data_mean$.signal,
                        mutate_c(signal_tbl, mean = mean(X), m = X +2))
  expect_equal_but_sgl(data_mean,
                       data)

  if(!grouped){
  data_NULL <- mutate(data, Y = NULL)
  expect_equal_plain_df(data_NULL$.signal,
                        mutate(signal_tbl, Y = NULL))
  expect_equal(data_NULL,
               select(data, -Y))
 }else {
   expect_error(mutate(data, Y = NULL))
 }
  if(!grouped){
  expect_message(data_cst <- mutate(data, Y =10) )
  } else {
 # when groupped it keeps being a channel
    data_cst <- mutate(data, Y =10)
  }
  expect_equal_plain_df(data_cst$.signal,
                        mutate(signal_tbl, Y = 10))

  if(!grouped){
  expect_equal_but_sgl(data_cst,
                       select(data, - Y))
 }else{
  expect_equal_but_sgl(data_cst,
                       data)
 }
  expect_message(data_cst2 <- mutate(data, Y = 1:60))

  expect_equal_plain_df(data_cst2$.signal,
                        mutate(signal_tbl, Y = 1:length(Y)))
  expect_equal_but_sgl(data_cst2,
                       select(data, - Y))

  expect_message(data_ch <- mutate(data, Y = channel_dbl(10)), regexp = NA)
  expect_equal_plain_df(data_ch$.signal,
                        mutate(signal_tbl, Y = 10))
  expect_equal_but_sgl(data_ch,
                       select(data))

  expect_message(data_ch2 <- mutate(data, Y = channel_dbl(1:length(Y))),regexp = NA)
  expect_equal_plain_df(data_ch2$.signal,
                        mutate(signal_tbl, Y = 1:length(Y)))
  expect_equal_but_sgl(data_ch2,
                       data)

  expect_equal(data, ref_data)
}

test_that("dplyr::mutate functions work correctly on ungrouped signal_tbl", {
test_mutates(data)
})


test_that("dplyr::mutate functions work correctly on ungrouped segments_tbl", {

  segments_tbl <- data$.segments
  data_seg10 <- mutate(data, segment = segment + 10)
  expect_equal_plain_df(data_seg10$.segments,
                      mutate(segments_tbl, segment = segment + 10))
  expect_equal_but_cnt_sgm(data_seg10,
                      data)

data_ZZseg10 <- mutate(data, ZZ = segment + 10)
expect_equal_plain_df(data_ZZseg10$.segments,
                      mutate(segments_tbl, ZZ = segment + 10))
expect_equal_but_sgm(data_ZZseg10,
                      data)


data_NULL <- mutate(data, segment = NULL)
expect_equal_plain_df(data_NULL$.segments,
                      mutate(segments_tbl, segment = NULL))
expect_equal(data_NULL,
                      select(data, -segment))

data_cst <- mutate(data, segment =10)
expect_equal_plain_df(data_cst$.segments,
                      mutate(segments_tbl, segment = 10))
expect_equal_but_sgm(data_cst,
                      data)

data_cst <- mutate(data, segment =1:6)
expect_equal_plain_df(data_cst$.segments,
                      mutate(segments_tbl, segment = 1:6))
expect_equal_but_sgm(data_cst,
                      data)
expect_equal(data, reference_data)
}

message("mutation of .id and .sample should be tested")

mutate4_eeg_lst <- mutate(data, subject = .recording)


nsamples <- 100
nsamples_ <- 100
data_nsamples_ <- data %>% mutate(Z = X + nsamples_)
data_100 <- data %>% mutate(Z = X + 100)
data_nsamples <- data %>% mutate(Z = X + nsamples)

test_that("dplyr:mutate functions understand the right scope",{
  expect_equal(data_100, data_nsamples)
  expect_equal(data_nsamples_, data_nsamples)
})



# TODO  I don't think these functions exist yet
message("mutate_at and mutate_all, etc haven't been tested")



test_that("dplyr::transmute functions work correctly on ungrouped signal_tbl", {

  signal_tbl <- as.data.frame(data$.signal)
  data_X10 <- transmute(data, X = X + 10)
expect_equal_plain_df(data_X10$.signal,
                      transmute(signal_tbl,.id=.id,.sample =.sample, X = X + 10))
expect_equal_but_cnt_sgl(data_X10,
                      select(data, - Y))

data_ZZX10 <- transmute(data, ZZ = X + 10)
expect_equal_plain_df(data_ZZX10$.signal,
                      transmute(signal_tbl,.id=.id,.sample =.sample, ZZ = X + 10))
expect_equal_but_sgl(data_ZZX10,
                      suppressWarnings(select(data, -X, -Y)))

expect_true(nrow(filter(data_ZZX10$.events, .channel == "ZZ")) == 0)
expect_true(nrow(filter(channels_tbl(data_ZZX10), .channel == "ZZ")) > 0)

data_mean <- transmute(data, mean = mean(X))
expect_equal_plain_df(data_mean$.signal,
                      transmute(signal_tbl,.id=.id,.sample =.sample, mean = mean(X)))
expect_equal_but_sgl(data_mean,
                      suppressWarnings(select(data, -X, -Y)))

expect_warning(data_NULL <- transmute(data, Y = NULL))
expect_equal_plain_df(data_NULL$.signal,
                      transmute(signal_tbl,.id=.id,.sample =.sample, Y = NULL))
expect_equal(data_NULL,
                      suppressWarnings(select(data, -X, -Y)))

expect_message(expect_warning(data_cst <- transmute(data, Y =10)))
expect_equal_plain_df(data_cst$.signal,
                      transmute(signal_tbl,.id=.id,.sample =.sample, Y = 10))
expect_equal_but_sgl(data_cst,
                      suppressWarnings(select(data, -X, -Y)))

expect_message(expect_warning(data_cst2 <- transmute(data, Y = 1:60)))
expect_equal_plain_df(data_cst2$.signal,
                      transmute(signal_tbl,.id=.id,.sample =.sample, Y = 1:60))
expect_equal_but_sgl(data_cst2,
                      suppressWarnings(select(data, -X, -Y)))

expect_message(data_ch <- transmute(data, Y = channel_dbl(10)), regexp = NA)
expect_equal_plain_df(data_ch$.signal,
                      transmute(signal_tbl,.id=.id,.sample =.sample, Y = 10))
expect_equal_but_sgl(data_ch,
                      select(data, -X))

expect_message(data_ch2 <- transmute(data, Y = channel_dbl(1:60)),regexp = NA)
expect_equal_plain_df(data_ch2$.signal,
                      transmute(signal_tbl,.id=.id,.sample =.sample, Y = 1:60))
expect_equal_but_sgl(data_ch2,
                      select(data, - X))

expect_equal(data, reference_data)
})


############################################
### test dplyr mutate on grouped eeg_lst ###
############################################
grouped_data <- list()
grouped_data[[1]] <- group_by(data, .sample)
grouped_data[[2]] <- group_by(data, .id)
grouped_data[[3]] <- group_by(data, .recording)
grouped_data[[4]] <- group_by(data, .sample, .recording)
grouped_data[[5]] <- group_by(data, .id, .recording)
grouped_data[[6]] <- group_by(data, .id, .sample, .recording)
grouped_data[[7]] <- group_by(data, .sample, condition)

for(data in grouped_data){
  test_mutates(data)
}
## mutate9_g_eeg_lst <- mutate(group3_by_eeg_lst, X = NULL)
## mutate10_g_eeg_lst <- mutate(group3_by_eeg_lst, X = 10)
## mutate11_g_eeg_lst <- mutate(group3_by_eeg_lst , X = 1:30)
## stop("compare with data table, and check reference")


## mutate_g_signal_eeg <- mutate(group_by_eeg_lst, X = X + 1)

## mutate_g_tbl <- data %>%
##   as_tibble() %>%
##   group_by(.recording) %>%
##   filter(.key == "X") %>%
##   mutate(X = .value + 1)

## mutate2_g_signal_eeg <- mutate(group3_by_eeg_lst, ZZ = X + 1)

## mutate2_g_tbl <- data %>%
##   as_tibble() %>%
##   group_by(.id) %>%
##   filter(.key == "X") %>%
##   mutate(ZZ = .value + 1)

## mutate3_g_signal_eeg <- mutate(group3_by_eeg_lst, Y = Y + 1)

## mutate3_g_tbl <- data %>%
##   as_tibble() %>%
##   group_by(.recording) %>%
##   filter(.key == "Y") %>%
##   mutate(Y = .value + 1)

## mutate4_g_signal_eeg <- mutate(group4_by_eeg_lst, X = X + 1)

## mutate4_g_tbl <- data %>%
##   as_tibble() %>%
##   group_by(.time, .recording) %>%
##   filter(.key == "X") %>%
##   mutate(X = .value + 1)

## mutate5_g_signal_eeg <- mutate(group5_by_eeg_lst, ZZ = X + 1)

## mutate5_g_tbl <- data %>%
##   as_tibble() %>%
##   group_by(.id, .recording) %>%
##   filter(.key == "X") %>%
##   mutate(ZZ = .value + 1)

## mutate6_g_signal_eeg <- mutate(group6_by_eeg_lst, Y = Y + 1)

## mutate6_g_tbl <- data %>%
##   as_tibble() %>%
##   group_by(.id, .time, .recording) %>%
##   filter(.key == "Y") %>%
##   mutate(Y = .value + 1)

## mutate7_g_signal_eeg <- mutate(group7_by_eeg_lst, mean = mean(Y))

## mutate7_g_tbl <- data %>%
##   as_tibble() %>%
##   filter(.key == "Y") %>%
##   group_by(condition, .time) %>% # have to reverse order
##   mutate(mean = mean(.value))

## transmute_g_signal_eeg <- transmute(group_by_eeg_lst, X = X + 1)

## transmute_g_tbl <- data %>%
##   as_tibble() %>%
##   group_by(.time) %>%
##   filter(.key == "X") %>%
##   transmute(X = .value + 1)

## # mean of everything except .sample
## mutate_all_g_signal_eeg <- mutate_all(group_by_eeg_lst, mean)

## # mean of channels
## mutate_at_g_signal_eeg <- mutate_at(group_by_eeg_lst, channel_names(data), mean)

## mutate_a_tbl <- data %>%
##   as_tibble() %>%
##   group_by(.time, .key) %>%
##   mutate(mean = mean(.value)) %>%
##   select(.id, .time, .key, mean) %>%
##   tidyr::spread(key = .key, value = mean) %>%
##   ungroup()


## test_that("mutate works correctly on data grouped by .sample", {
##   expect_equal(
##     as.double(mutate_g_signal_eeg$.signal[["X"]]),
##     mutate_g_tbl$X
##   )
##   expect_equal(
##     as.double(mutate2_g_signal_eeg$.signal[["ZZ"]]),
##     mutate2_g_tbl$ZZ
##   )
##   expect_equal(
##     as.double(mutate3_g_signal_eeg$.signal[["Y"]]),
##     mutate3_g_tbl$Y
##   )
##   expect_equal(
##     as.double(mutate4_g_signal_eeg$.signal[["X"]]),
##     mutate4_g_tbl$X
##   )
##   expect_equal(
##     as.double(mutate5_g_signal_eeg$.signal[["ZZ"]]),
##     mutate5_g_tbl$ZZ
##   )
##   expect_equal(
##     as.double(mutate6_g_signal_eeg$.signal[["Y"]]),
##     mutate6_g_tbl$Y
##   )
##   expect_equal(
##     as.double(mutate7_g_signal_eeg$.signal[["mean"]]),
##     mutate7_g_tbl$mean
##   )
##   expect_equal(
##     as.double(transmute_g_signal_eeg$.signal[["X"]]),
##     transmute_g_tbl$X
##   )
##   expect_equal(
##     as.matrix(mutate_all_g_signal_eeg$.signal[, c("X", "Y")]),
##     as.matrix(mutate_at_g_signal_eeg$.signal[, c("X", "Y")])
##   )
##   expect_equal(
##     as.matrix(mutate_all_g_signal_eeg$.signal[, c("X", "Y")]),
##     as.matrix(select(mutate_a_tbl, X, Y))
##   )
## })


## test_that("new channels created by mutate shouldn't appear in the events table", {
##   expect_true(nrow(filter(mutate2_g_signal_eeg$.events, .channel == "ZZ")) == 0)
##   expect_true(nrow(filter(mutate5_g_signal_eeg$.events, .channel == "ZZ")) == 0)
## })


## test_that("new channels appear in the channels table", {
##   expect_true(nrow(filter(channels_tbl(mutate2_g_signal_eeg), .channel == "ZZ")) > 0)
##   expect_true(nrow(filter(channels_tbl(mutate5_g_signal_eeg), .channel == "ZZ")) > 0)
## })


## test_that("the classes of channels of signal_tbl remain in grouped eeg_lst", {
##   expect_equal(is_channel_dbl(group_by_eeg_lst$.signal$X), TRUE)
##   expect_equal(is_channel_dbl(group2_by_eeg_lst$.signal$X), TRUE)
##   expect_equal(is_channel_dbl(mutate_g_signal_eeg$.signal$X), TRUE)
##   expect_equal(is_channel_dbl(mutate2_g_signal_eeg$.signal$ZZ), TRUE)
##   expect_equal(is_channel_dbl(mutate3_g_signal_eeg$.signal$Y), TRUE)
##   expect_equal(is_channel_dbl(transmute_g_signal_eeg$.signal$X), TRUE)
##   expect_equal(is_channel_dbl(mutate_all_g_signal_eeg$.signal$X), TRUE)
##   expect_equal(is_channel_dbl(mutate_at_g_signal_eeg$.signal$X), TRUE)
## })

## test_that("use a group in mutate",{
## expect_equal(group8_by_eeg_lst %>% mutate(X = X * trial) %>% ungroup(),
##   data %>% mutate(trial= segment) %>% mutate(X = X * trial))
## })


# check against original data
test_that("data didn't change after grouping and mutate functions", {
  expect_equal(reference_data, data)
})




### test as_time conversion  ###
eeg_time <- suppressWarnings(mutate(data, .time = as_time(.sample, unit = "seconds")) %>%
  summarize(mean = mean(.time)))

tbl_time <- data %>%
  as_tibble() %>%
  summarize(mean = mean(.time))

test_that("as_time works as expected", {
  expect_equal(as.double(eeg_time$.signal[["mean"]]), tbl_time$mean)
  expect_warning(mutate(data, .time = as_time(.sample, unit = "seconds")) %>%
    summarize(mean = mean(.time)))
})




###########################
### test serial mutates ###
###########################

# Bruno's note: Maybe it's fine that the following fails:
# mutate(data, .time = as_time(.sample, unit = "milliseconds")) %>%
#   group_by(.time) %>%
#   summarize(mean(X))

# create new variable with mutate
eeg_mutate_1 <- data %>%
  mutate(bin = ntile(.sample, 5))

tbl_mutate_1 <- data %>%
  as_tibble() %>%
  mutate(bin = ntile(.time, 5))

# use new variable in second variable doesn't work in eeg_lst (#35)
## eeg_mutate_2 <- data %>% mutate(.time = as_time(.sample, unit = "ms"), bin = ntile(time, 5))
# work around:
eeg_mutate_2 <- data %>%
  mutate(.time = as_time(.sample, unit = "ms")) %>%
  mutate(bin = ntile(.time, 5))

tbl_mutate_2 <- data %>%
  as_tibble() %>%
  mutate(test = .time + 1, bin = ntile(test, 5))

# can't summarize by a mutated variable within eeg_lst (#43)
eeg_mutate_3 <- data %>%
  mutate(bin = ntile(.sample, 5)) %>%
  group_by(bin) %>%
  summarize(mean = mean(X))

tbl_mutate_3 <- data %>%
  as_tibble() %>%
  mutate(bin = ntile(.time, 5)) %>%
  group_by(bin) %>%
  summarize(mean = mean(.value[.key == "X"]))


test_that("mutate works the same on eeg_lst as on tibble", {
  expect_equal(eeg_mutate_1$.signal[["bin"]], tbl_mutate_1$bin[tbl_mutate_1$.key == "X"])
  expect_equal(eeg_mutate_2$.signal[["bin"]], tbl_mutate_2$bin[tbl_mutate_1$.key == "X"])
  expect_equal(eeg_mutate_3$.signal[["bin"]], tbl_mutate_3$bin)
})
