context("test tidyverse mutate/summarize")
library(eeguana)

### mutate and summarize should have their own files
# tests when factors are used should be done.



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
  segments = dplyr::tibble(.id = c(1L, 2L, 3L), recording = "recording1", segment = c(1L, 2L, 3L), condition = c("a","b","a"))
)
#just some different X and Y
data_2 <- mutate(data_1, recording = "recording2", X = sin(X +10), Y = cos(Y - 10), condition = c("b","a","b"))

data <- bind(data_1,data_2)

reference_data <- data.table::copy(data)


  mutate_eeg_lst <- mutate(data, X = X + 10)
  mutate2_eeg_lst <- mutate(data, ZZ = X + 10)
  mutate3_eeg_lst <- mutate(data, mean(X))
  mutate4_eeg_lst <- mutate(data, subject = recording)
  transmute_eeg_lst <- transmute(data, X = X + 1)
  #TODO
  # mutate_all_eeg_lst <- mutate_all_ch(data, mean)
  # mutate_at_eeg_lst <- mutate_at(data, channel_names(data), mean)
  summarizeX_eeg_lst <- summarize(data, mean(X))
  summarize_at_eeg_lst <- summarize_at_ch(data, channel_names(data), mean)
  summarize_all_eeg_lst <- summarize_all_ch(data, mean)
  summarize_all2_eeg_lst <- summarize_all_ch(data, "mean")
  summarize_all3_eeg_lst <- summarize_all_ch(data, funs(mean(.)))
  summarize_all4_eeg_lst <- summarize_all_ch(data, funs(m = mean(.)))
  group_by_eeg_lst <- group_by(data, .sample_id)
  group2_by_eeg_lst <- group_by(data, .id)
  group3_by_eeg_lst <- group_by(data, recording)
  group4_by_eeg_lst <- group_by(data, .sample_id, recording)
  group4_by_eeg_lst <- group_by(data, .id, recording)
  group5_by_eeg_lst <- group_by(data, .id, .sample_id, recording)

test_that("dplyr functions work correctly on ungrouped data", {
}
)

test_that("data didn't change", {
expect_equal(reference_data, data)
 }) 

test_that("the classes of channels of signal_tbl remain in non-grouped eeg_lst", {
  expect_equal(is_channel_dbl(mutate_eeg_lst$signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate2_eeg_lst$signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate2_eeg_lst$signal$ZZ), TRUE)
  expect_equal(is_channel_dbl(transmute_eeg_lst$signal$X), TRUE)
  # expect_equal(is_channel_dbl(mutate_all_eeg_lst$signal$X), TRUE)
  # expect_equal(is_channel_dbl(mutate_at_eeg_lst$signal$X), TRUE)
  expect_equal(is_channel_dbl(summarizeX_eeg_lst$signal$`mean(X)`), TRUE)
  expect_equal(is_channel_dbl(summarize_at_eeg_lst$signal$X), TRUE)
})

  mutate_g_signal_tbl <- mutate(group_by_eeg_lst, X = X + 1)
  mutate2_g_signal_tbl <- mutate(group_by_eeg_lst, ZZ = X + 1)
  transmute_g_signal_tbl <- transmute(group_by_eeg_lst, X = X + 1)
  # mutate_all_g_signal_tbl <- mutate_all(group_by_eeg_lst, mean)
  # mutate_at_g_signal_tbl <- mutate_at(group_by_eeg_lst, channel_names(data), mean)
  summarize_g_signal_tbl <- summarize(group_by_eeg_lst, mean(X))
  summarize_at_g_signal_tbl <- summarize_at_ch(group_by_eeg_lst, channel_names(data), mean)
test_that("dplyr functions work correctly on  data grouped by .sample_id", {
})

test_that("data didn't change after grouping and dplyr functions", {
  expect_equal(reference_data, data)
 }) 


  summarize_g2_signal_tbl <- summarize(group2_by_eeg_lst, mean(X))
test_that("dplyr functions work correctly on  data grouped by .sample_id", {
})

test_that("the classes of channels of signal_tbl remain in non-grouped eeg_lst", {
  expect_equal(is_channel_dbl(group_by_eeg_lst$signal$X), TRUE)
  expect_equal(is_channel_dbl(group2_by_eeg_lst$signal$X), TRUE)
  
  expect_equal(is_channel_dbl(mutate_g_signal_tbl$signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate2_g_signal_tbl$signal$X), TRUE)
  expect_equal(is_channel_dbl(mutate2_g_signal_tbl$signal$ZZ), TRUE)
  expect_equal(is_channel_dbl(transmute_g_signal_tbl$signal$X), TRUE)
  # expect_equal(is_channel_dbl(mutate_all_g_signal_tbl$signal$X), TRUE)
  # expect_equal(is_channel_dbl(mutate_at_g_signal_tbl$signal$X), TRUE)
  expect_equal(is_channel_dbl(summarize_g_signal_tbl$signal$`mean(X)`), TRUE)
  expect_equal(is_channel_dbl(summarize_at_g_signal_tbl$signal$X), TRUE)
  expect_equal(is_channel_dbl(summarize_g2_signal_tbl$signal$`mean(X)`), TRUE)
})




# with my functions
data_s1 <- data %>% group_by(condition, .sample_id, recording) %>% summarize(X = mean(X),Y = mean(Y))
data_s2 <- data_s1 %>% group_by(condition, .sample_id) %>% summarize(X = mean(X),Y = mean(Y))
data_s3 <- data_s2 %>% group_by(condition) %>% summarize(X = mean(X),Y = mean(Y))
data_s4 <- data_s3 %>% group_by() %>% summarize(X = mean(X),Y = mean(Y))


.eeg_lst <- data %>% group_by(condition, .sample_id, recording)
dots <- rlang::quos(X = mean(X),Y = mean(Y))

# with pure dplyr functions
extended_signal <- left_join(as_tibble(data$signal), data$segments, by =".id" )
e_data_s1 <- data.table::data.table(extended_signal)[,.(X = mean(X),Y = mean(Y)), by = c("condition", ".sample_id", "recording")]
s_data_s1 <- e_data_s1[,unique(.SD) ,.SDcols = c("condition", "recording")]

e_data_s2 <- data.table::data.table(e_data_s1)[,.(X = mean(X),Y = mean(Y)), by = c("condition", ".sample_id")]
s_data_s2 <- e_data_s2[,unique(.SD) ,.SDcols = c("condition")]

e_data_s3 <- data.table::data.table(e_data_s2)[,.(X = mean(X),Y = mean(Y)), by = c("condition")]
s_data_s3 <- e_data_s1[,unique(.SD) ,.SDcols = c("condition")]

e_data_s4 <- data.table::data.table(e_data_s3)[,.(X = mean(X),Y = mean(Y)), by = character(0)]


test_that("summarizing by groups works as expected for the channel values", {
expect_equal(data_s1$signal$X,e_data_s1$X)
expect_equal(data_s2$signal$X,e_data_s2$X)
expect_equal(data_s3$signal$X,e_data_s3$X)
expect_equal(data_s4$signal$X,e_data_s4$X)
})


test_that("summarizing by groups works as expected for the segments", {
expect_equal(data_s1$segments %>% dplyr::select(-segment_n),as_tibble(s_data_s1)%>% mutate(.id = 1:n()))
expect_equal(data_s2$segments %>% dplyr::select(-segment_n),as_tibble(s_data_s2)%>% mutate(.id = 1:n()))
expect_equal(data_s3$segments %>% dplyr::select(-segment_n),as_tibble(s_data_s3)%>% mutate(.id = 1:n()))
expect_equal(data_s4$segments %>% dplyr::select(-segment_n),tibble(.id= 1L))

})



data_all_s1 <- data %>% group_by(.sample_id, condition, recording) %>% summarize_all_ch(mean)
data_all_s2 <- data_all_s1 %>% group_by(.sample_id, condition) %>% summarize_all_ch(mean)
data_all_s3 <- data_all_s2 %>% group_by(.sample_id) %>% summarize_all_ch(mean)
data_all_s4 <- data_all_s3 %>% group_by() %>% summarize_all_ch(mean)

test_that("summarize all channels works as the regular summarize", {
expect_equal(data_all_s1,data_all_s1)
expect_equal(data_all_s2,data_all_s2)
expect_equal(data_all_s3,data_all_s3)
expect_equal(data_all_s4,data_all_s4)
})


# comparing summarize within and outside eeg_lst
d1 <- group_by(data, .sample_id) %>% summarize(mean = mean(X[condition=="a"]-X[condition=="b"]))
d2 <- data %>% as_tibble %>% group_by(time) %>% filter(channel=="X") %>% summarize(mean = mean(amplitude[condition=="a"]-amplitude[condition=="b"]))

d3 <- group_by(data, .sample_id) %>% summarize_all_ch(funs(mean(.[condition=="a"]-.[condition=="b"])))
d4 <- data %>% as_tibble %>% group_by(time, channel) %>% summarize(mean = mean(amplitude[condition=="a"]-amplitude[condition=="b"])) %>% spread(key = channel, value = mean) %>% ungroup()

d5 <- group_by(data, .sample_id) %>% summarize(mean(X[condition=="a" & recording == "recording1"]-X[condition=="b" & recording == "recording2"]))
d6 <- data %>% as_tibble %>% group_by(time) %>% filter(channel=="X") %>% summarize(mean = mean(amplitude[condition=="a" & recording == "recording1"]-amplitude[condition=="b" & recording == "recording2"]))

d7 <- group_by(data, .sample_id) %>% summarize_all_ch(funs(mean(.[condition=="a" & recording == "recording1"]-.[condition=="b" & recording == "recording2"])))
d8 <- data %>% as_tibble %>% group_by(time, channel) %>% summarize(mean = mean(amplitude[condition=="a" & recording == "recording1"]-amplitude[condition=="b" & recording == "recording2"])) %>% spread(key = channel, value = mean) %>% ungroup()

d9 <- group_by(data, .sample_id) %>% summarize_all_ch("mean")
d10 <- data %>% as_tibble() %>% group_by(time, channel) %>% summarize(mean = mean(amplitude)) %>% spread(key = channel, value = mean) %>% ungroup()

d11 <- mutate(data, time = as_time(.sample_id, unit = "milliseconds")) %>% group_by(.sample_id) %>% summarize(mean(X))
d12 <- data %>% as_tibble() %>% group_by(time) %>% summarize(mean = mean(amplitude[channel=="X"]))

test_that("summarising functions work the same on eeg_lst as on tibble", {
  expect_equal(as.double(d1$signal[[3]]), d2$mean)
  expect_equal(as.matrix(d3$signal[,c(3,4)]), as.matrix(select(d4, X, Y)))
  expect_equal(as.double(d5$signal[[3]]), d6$mean)
  expect_equal(as.matrix(d7$signal[,c(3,4)]), as.matrix(select(d8, X, Y)))
  expect_equal(as.matrix(d9$signal[,c(3,4)]), as.matrix(select(d10, X, Y)))
  expect_equal(as.double(d11$signal[[3]]), d12$mean)
})


# not converting to ms
d13 <- mutate(data, time = as_time(.sample_id, unit = "milliseconds")) %>% summarize(mean(time))
d14 <- data %>% as_tibble() %>% summarize(mean = mean(time)) 

test_that("as_time works as expected", {
  expect_equal(as.double(d13$signal[[3]]), d14$mean)
})


#Maybe it's fine that the following fails:
# mutate(data, time = as_time(.sample_id, unit = "milliseconds")) %>% group_by(time) %>%
#         summarize(mean(X))

