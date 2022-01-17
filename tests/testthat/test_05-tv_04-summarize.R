library(eeguana)
options(eeguana.verbose=FALSE)
# tests when factors are used should be done.
expect_equal_plain_df <- eeguana:::expect_equal_plain_df
expect_equal_but_sgl <- eeguana:::expect_equal_but_sgl
expect_equal_but_cnt_sgl <- eeguana:::expect_equal_but_cnt_sgl
expect_equal_but_sgm <- eeguana:::expect_equal_but_sgm
expect_equal_but_cnt_sgm <- eeguana:::expect_equal_but_cnt_sgm
expect_equal_eeg_lst <- eeguana:::expect_equal_eeg_lst
# create fake dataset

data_1 <- eeguana:::data_sincos3id
data_2 <- eeg_mutate(data_1, .recording = "recording2", 
                     X = sin(X + 10),
                     Y = cos(Y - 10),
                     condition = c("b", "a", "b"))
data <- bind(data_1, data_2)

# for checks later
reference_data <- data.table::copy(data)




#################################################
### test dplyr dplyr::summarize on ungrouped eeg_lst ###
#################################################

summarize_eeg <- eeg_summarize(data, mean = mean(X))



test_that("dplyr version of summarize works", {
  expect_equal(
    eeg_summarize(data, mean = mean(X)),
    dplyr::summarize(data, mean = mean(X))
  )})

nsamples <- 100
nsamples_ <- 100

test_that("summarize has the right scope", {
  expect_equal(
    data %>% eeg_summarize(X = mean(X) + nsamples),
    data %>% eeg_summarize(X = mean(X) + 100)
  )
  expect_equal(
    data %>% eeg_summarize(X = mean(X) + nsamples),
    data %>% eeg_summarize(X = mean(X) + nsamples_)
  )
})


# 
# summarize_tbl <- data %>%
#   dplyr::as_tibble() %>%
#   dplyr::filter(.key == "X") %>%
#   dplyr::summarize(mean = mean(.value))
# 
# 
# summarize2_tbl <- data %>%
#   dplyr::as_tibble() %>%
#   dplyr::group_by(.key) %>%
#   dplyr::summarize(mean = mean(.value)) %>%
#   tidyr::spread(key = .key, value = mean)
# 
# 
# summarize4_tbl <- data %>%
#   dplyr::as_tibble() %>%
#   dplyr::group_by(.key) %>%
#   dplyr::summarize(mean = mean(.value)) %>%
#   tidyr::spread(key = .key, value = mean) %>%
#   dplyr::rename(X_mean = X, Y_mean = Y)

test_that("dplyr::summarize works correctly on ungrouped data", {
  expect_equal(
    summarize_eeg$.signal$mean,
    mean(data$.signal$X)
    )
  expect_equal(
    nrow(summarize_eeg$.events),
    0
  )
  expect_equal(
    nrow(summarize_eeg$.segments),
    1
  )
  summarize_all_eeg <- eeg_summarize(data, across(c("X","Y"), mean))
  expect_equal_plain_df(
    summarize_all_eeg$.signal[,c("X","Y")],
    colMeans(data$.signal[,c("X","Y")])
  )
  expect_equal(
    summarize_eeg$.events,
    summarize_all_eeg$.events
  )
  expect_equal(
    summarize_eeg$.segments,
    summarize_all_eeg$.segments
  )
  summarize_all2_eeg <- eeg_summarize(data, across(c("X","Y"), list(mean = mean)))
  expect_equal(
    summarize_all2_eeg,
    summarize_all_eeg %>% eeg_rename(X_mean = X, Y_mean = Y)
  )
})

eeg_summarize(data, across(c("X","Y"), list(mean = mean)))
.eeg_lst <- data
dots <- rlang::quos(across(c("X","Y"), list(mean = mean)))

# check against original data
test_that("data didn't change", {
  expect_equal(reference_data, data)
})



############################################################
### test dplyr dplyr::summarize on grouped eeg_lst signal table ###
############################################################

# create groupings
group_by_eeg_lst <- dplyr::group_by(data, .sample)
group2_by_eeg_lst <- dplyr::group_by(data, .id)
group3_by_eeg_lst <- dplyr::group_by(data, .recording)
group4_by_eeg_lst <- dplyr::group_by(data, .sample, .recording)
group5_by_eeg_lst <- dplyr::group_by(data, .id, .recording)
group6_by_eeg_lst <- dplyr::group_by(data, .id, .sample, .recording)
group7_by_eeg_lst <- dplyr::group_by(data, .sample, condition)
group8_by_eeg_lst <- dplyr::group_by(data, segment)

summarize_g_signal_eeg <- eeg_summarize(group_by_eeg_lst, mean = mean(X))

summarize_g_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.key == "X") %>%
  dplyr::group_by(.time) %>%
  dplyr::summarise(mean = mean(.value))

summarize_at_g_signal_eeg <- eeg_summarize(group_by_eeg_lst, across(channel_names(data), mean))

summarize_at_g_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::group_by(.time, .key) %>%
  dplyr::summarise(mean = mean(.value)) %>%
  tidyr::spread(key = .key, value = mean) %>%
  dplyr::ungroup()

summarize_g2_signal_eeg <- eeg_summarize(group2_by_eeg_lst, mean = mean(X))

summarize_g2_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.key == "X") %>%
  dplyr::group_by(.id) %>%
  dplyr::summarise(mean = mean(.value))

summarize_g3_signal_eeg <- eeg_summarize(group3_by_eeg_lst, mean = mean(X))

summarize_g3_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.key == "X") %>%
  dplyr::group_by(.recording) %>%
  dplyr::summarise(mean = mean(.value))

summarize_g4_signal_eeg <- eeg_summarize(group4_by_eeg_lst, mean = mean(X))

summarize_g4_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.key == "X") %>%
  dplyr::group_by(.recording, .time) %>%
  dplyr::summarise(mean = mean(.value))

summarize_g5_signal_eeg <- eeg_summarize(group5_by_eeg_lst, mean = mean(X))

summarize_g5_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.key == "X") %>%
  dplyr::group_by(.id, .recording) %>%
  dplyr::summarise(mean = mean(.value))

summarize_g6_signal_eeg <- eeg_summarize(group6_by_eeg_lst, mean = mean(X))

summarize_g6_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.key == "X") %>%
  dplyr::group_by(.id, .time, .recording) %>%
  dplyr::summarise(mean = mean(.value))

summarize_g7_signal_eeg <- eeg_summarize(group7_by_eeg_lst, mean = mean(X))

summarize_g7_tbl <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.key == "X") %>%
  dplyr::group_by(condition, .time) %>% # have to reverse order
  dplyr::summarise(mean = mean(.value))



test_that(" summarize can use grouping variable", {
  expect_equal(
    group8_by_eeg_lst %>% dplyr::summarize(X = mean(X) + mean(segment) * 100),
    group8_by_eeg_lst %>% dplyr::mutate(seg = segment) %>%
      dplyr::summarize(X = mean(X) + mean(seg) * 100)
  )
})


test_that("dplyr::summarize works correctly on  data grouped by .sample", {
  expect_equal(as.double(summarize_g_signal_eeg$.signal[["mean"]]), summarize_g_tbl$mean)
  expect_equal(as.matrix(summarize_at_g_signal_eeg$.signal[, c("X", "Y")]), as.matrix(dplyr::select(summarize_at_g_tbl, X, Y)))
  expect_equal(as.double(summarize_g2_signal_eeg$.signal[["mean"]]), summarize_g2_tbl$mean)
  expect_equal(as.double(summarize_g3_signal_eeg$.signal[["mean"]]), summarize_g3_tbl$mean)
  expect_equal(as.double(summarize_g4_signal_eeg$.signal[["mean"]]), summarize_g4_tbl$mean)
  expect_equal(as.double(summarize_g5_signal_eeg$.signal[["mean"]]), summarize_g5_tbl$mean)
  expect_equal(as.double(summarize_g6_signal_eeg$.signal[["mean"]]), summarize_g6_tbl$mean)
  expect_equal(as.double(summarize_g7_signal_eeg$.signal[["mean"]]), summarize_g7_tbl$mean)
})


test_that("dplyr::summarizes don't have any individual events", {
  expect_true(nrow(summarize_g_signal_eeg$.events) == 0)
  expect_true(nrow(summarize_at_g_signal_eeg$.events) == 0)
})


test_that("the classes of channels of signal_tbl remain in non-grouped eeg_lst", {
  expect_equal(is_channel_dbl(summarize_g_signal_eeg$.signal$mean), TRUE)
  expect_equal(is_channel_dbl(summarize_at_g_signal_eeg$.signal$X), TRUE)
})


# check against original data
test_that("data didn't change after grouping and dplyr::summarize functions", {
  expect_equal(reference_data, data)
})



########################################################################################
### test eeguana dplyr::summarize directly on eeg_lst, grouping by segments table variables ###
########################################################################################


test_that("summarizing by groups works as expected for the condition, .sample, .recording", {
  extended_signal <- dplyr::left_join(dplyr::as_tibble(data$.signal), data$.segments, by = ".id")
  
  data_s1 <- data %>%
    eeg_group_by(condition, .sample, .recording) %>%
    eeg_summarize(X = mean(X), Y = mean(Y))
  
  extended_signal_s1 <- dplyr::left_join(dplyr::as_tibble(data_s1$.signal), data_s1$.segments, by = ".id") %>%
    dplyr::arrange(condition, .sample, .recording)
  e_s1 <- extended_signal %>%
    dplyr::group_by(.recording, condition, .sample) %>%
    dplyr::summarize(X = mean(X), Y = mean(Y), .groups = "drop")%>%
    dplyr::arrange(condition, .sample, .recording)
  
  expect_equal_plain_df(dplyr::select(extended_signal_s1, condition, .recording, .sample , X, Y),
  dplyr::select(e_s1, condition, .recording, .sample , X, Y))
  expect_equal(colnames(data_s1$.signal), c(".id",".sample","X","Y"))
  expect_equal(colnames(data_s1$.segments), c(".id",".recording","condition"))
})


test_that("summarizing by groups works as expected for the condition,", {
  
  extended_signal <- dplyr::left_join(dplyr::as_tibble(data$.signal), data$.segments, by = ".id")
  
  data_s1 <- data %>%
    eeg_group_by(condition) %>%
    eeg_summarize(X = mean(X), Y = mean(Y))
  
  extended_signal_s1 <- dplyr::left_join(dplyr::as_tibble(data_s1$.signal), data_s1$.segments, by = ".id") %>%
    dplyr::arrange(condition)
  e_s1 <- extended_signal %>%
    dplyr::group_by(condition) %>%
    dplyr::summarize(X = mean(X), Y = mean(Y), .groups = "drop")%>%
    dplyr::mutate(.recording = NA_character_, .sample = NA_integer_) %>%
    dplyr::arrange(condition, .sample, .recording)
  
  expect_equal_plain_df(dplyr::select(extended_signal_s1, condition, .recording, .sample , X, Y),
                        dplyr::select(e_s1, condition, .recording, .sample , X, Y))
  expect_equal(colnames(data_s1$.signal), c(".id",".sample","X","Y"))
  expect_equal(colnames(data_s1$.segments), c(".id",".recording","condition"))
})

test_that("summarizing by groups works as expected for the condition, .sample,", {
  
  extended_signal <- dplyr::left_join(dplyr::as_tibble(data$.signal), data$.segments, by = ".id")
  
  data_s1 <- data %>%
    eeg_group_by(condition, .sample) %>%
    eeg_summarize(X = mean(X), Y = mean(Y))
  
  extended_signal_s1 <- dplyr::left_join(dplyr::as_tibble(data_s1$.signal), data_s1$.segments, by = ".id") %>%
    dplyr::arrange(condition)
  e_s1 <- extended_signal %>%
    dplyr::group_by(condition, .sample) %>%
    dplyr::summarize(X = mean(X), Y = mean(Y), .groups = "drop")%>%
    dplyr::mutate(.recording = NA_character_) %>%
    dplyr::arrange(condition, .sample, .recording)
  
  expect_equal_plain_df(dplyr::select(extended_signal_s1, condition, .recording, .sample , X, Y),
                        dplyr::select(e_s1, condition, .recording, .sample , X, Y))
  expect_equal(colnames(data_s1$.signal), c(".id",".sample","X","Y"))
  expect_equal(colnames(data_s1$.segments), c(".id",".recording","condition"))
})



test_that("summarizing by groups works as expected for the .sample,", {
  
  extended_signal <- dplyr::left_join(dplyr::as_tibble(data$.signal), data$.segments, by = ".id")
  
  data_s1 <- eeg_group_by(data, .sample) %>%
    eeg_summarize(X_1 = mean(X[condition == "a"] -
                               X[condition == "b"]),
                  Y_1 = mean(Y[condition == "a"] -
                               Y[condition == "b"]))
  
  extended_signal_s1 <- dplyr::left_join(dplyr::as_tibble(data_s1$.signal), data_s1$.segments, by = ".id") %>%
    dplyr::arrange(.sample)
  
  e_s1 <- extended_signal %>%
    dplyr::group_by(.sample) %>%
    dplyr::summarize(X_1 = mean(X[condition == "a"] -
                                   X[condition == "b"]),
                     Y_1 = mean(Y[condition == "a"] -
                                  Y[condition == "b"])
                     , .groups = "drop")%>%
    dplyr::mutate(.recording = NA_character_) %>%
    dplyr::arrange( .sample)
  
  expect_equal_plain_df(dplyr::select(extended_signal_s1,  .recording, .sample , X_1, Y_1),
                        dplyr::select(e_s1, .recording, .sample , X_1, Y_1))
  expect_equal(colnames(data_s1$.signal), c(".id",".sample","X_1", "Y_1"))
  expect_equal(colnames(data_s1$.segments), c(".id",".recording"))
  
  eeg_diff_means_2 <-eeg_group_by(data, .sample) %>%
    eeg_summarize(across(channel_names(data), list(~ mean(.[condition == "a"] -
                                                            .[condition == "b"]))))
  expect_equal(data_s1, eeg_diff_means_2)
})


test_that("summarizing by groups works as expected for the .sample,", {
  
  extended_signal <- dplyr::left_join(dplyr::as_tibble(data$.signal), data$.segments, by = ".id")
  
  data_s1 <- eeg_group_by(data, .sample) %>%
    eeg_summarize(mean = mean(X[condition == "a" & .recording == "recording1"] -
                                X[condition == "b" & .recording == "recording2"]))
  
  extended_signal_s1 <- dplyr::left_join(dplyr::as_tibble(data_s1$.signal), data_s1$.segments, by = ".id") %>%
    dplyr::arrange(.sample)
  
  e_s1 <- extended_signal %>%
    dplyr::group_by(.sample) %>%
    dplyr::summarize(mean = mean(X[condition == "a" & .recording == "recording1"] -
                                   X[condition == "b" & .recording == "recording2"])
                     , .groups = "drop")%>%
    dplyr::mutate(.recording = NA_character_) %>%
    dplyr::arrange( .sample)
  
  expect_equal_plain_df(dplyr::select(extended_signal_s1,  .recording, .sample , mean),
                        dplyr::select(e_s1, .recording, .sample , mean))
  expect_equal(colnames(data_s1$.signal), c(".id",".sample","mean"))
  expect_equal(colnames(data_s1$.segments), c(".id",".recording"))
  
  
})


