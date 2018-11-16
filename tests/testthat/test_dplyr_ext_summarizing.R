context("test summarize at different levels dplyr functions")
library(eeguana)


data_1 <- eeg_lst(
  signal = signal_tbl(
    signal_matrix = as.matrix(
      data.frame(X = sin(1:30), Y = cos(1:30))
    ),
    ids = rep(c(1L, 2L, 3L), each = 10),
    sample_ids = sample_int(rep(seq(-4L, 5L), times = 3), sampling_rate = 500),
    dplyr::tibble(
      .name = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
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

# with my functions
data_s1 <- data %>% group_by(condition, .sample_id, recording) %>% summarize(X = mean(X),Y = mean(Y))
data_s2 <- data_s1 %>% group_by(condition, .sample_id) %>% summarize(X = mean(X),Y = mean(Y))
data_s3 <- data_s2 %>% group_by(condition) %>% summarize(X = mean(X),Y = mean(Y))
data_s4 <- data_s3 %>% group_by() %>% summarize(X = mean(X),Y = mean(Y))

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
expect_equal(data_s4$signal$X %>% as.numeric,e_data_s4$X)
})


test_that("summarizing by groups works as expected for the segments", {
expect_equal(data_s1$segments %>% select(-segment_n),as_tibble(s_data_s1)%>% mutate(.id = 1:n()))
expect_equal(data_s2$segments %>% select(-segment_n),as_tibble(s_data_s2)%>% mutate(.id = 1:n()))
expect_equal(data_s3$segments %>% select(-segment_n),as_tibble(s_data_s3)%>% mutate(.id = 1:n()))
expect_equal(data_s4$segments %>% select(-segment_n),tibble(.id= 1L))

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

