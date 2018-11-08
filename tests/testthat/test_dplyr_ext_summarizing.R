context("test extended dplyr functions - rollup")
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

data_s1 <- data %>% group_by(condition, .sample_id, recording) %>% summarize(X = mean(X))
data_s2 <- data_s1 %>% group_by(condition, .sample_id) %>% summarize(X = mean(X))
data_s3 <- data_s2 %>% group_by(condition) %>% summarize(X = mean(X))
data_s4 <- data_s3 %>% group_by() %>% summarize(X = mean(X))

d <- left_join(data$signal, data$segments, by =".id" )
d %>% group_by(condition, .sample_id, recording) %>%summarize(X = mean(X))
data_s1$signal
data_r1$signal

data$segments %>% group_by(condition,  recording) %>% summarize()
# data.table(data$segments)[,.SD,.SDcols= c("condition",  "recording"), by = c("condition",  "recording")]

data.table(data$segments)[,unique(.SD),.SDcols= c("condition",  "recording")]

d %>% group_by(condition, .sample_id, recording) %>%summarize(X = mean(X)) %>%
 group_by(condition, .sample_id) %>%summarize(X = mean(X))
data_s2$signal


data_r1 <- data %>% group_by(condition, .sample_id, recording) %>% rollup(X = mean(X),level = c("condition", ".sample_id", "recording")) 
data_r2 <- data %>% group_by(condition, .sample_id, recording) %>% rollup(X = mean(X),level =c("condition", ".sample_id")) 
data_r3 <- data %>% group_by(condition, .sample_id, recording) %>% rollup(X = mean(X),level =c("condition")) 
data_r4 <- data %>% group_by(condition, .sample_id, recording) %>% rollup(X = mean(X),level =c()) 

test_that("rollup does the same as summarizing by groups", {
expect_equal(data_s1,data_r1)
expect_equal(data_s2,data_r2)
expect_equal(data_s3,data_r3)
expect_equal(data_s4,data_r4)
})


data_all_s1 <- data %>% group_by(.sample_id, condition, recording) %>% summarize_all_ch(mean)
data_all_s2 <- data_all_s1 %>% group_by(.sample_id, condition) %>% summarize_all_ch(mean)
data_all_s3 <- data_all_s2 %>% group_by(.sample_id) %>% summarize_all_ch(mean)
data_all_s4 <- data_all_s3 %>% group_by() %>% summarize_all_ch(mean)

data_all_r1 <- data %>% group_by(.sample_id, condition, recording) %>% rollup_all_ch(mean,level = c(".sample_id", "condition", "recording")) 
data_all_r2 <- data %>% group_by(.sample_id, condition, recording) %>% rollup_all_ch(mean,level =c(".sample_id", "condition")) 
data_all_r3 <- data %>% group_by(.sample_id, condition, recording) %>% rollup_all_ch(mean,level =c(".sample_id")) 
data_all_r4 <- data %>% group_by(.sample_id, condition, recording) %>% rollup_all_ch(mean,level =c()) 

test_that("all variants rollup does the same as summarizing by groups", {
expect_equal(data_all_s1,data_all_r1)
expect_equal(data_all_s2,data_all_r2)
expect_equal(data_all_s3,data_all_r3)
expect_equal(data_all_s4,data_all_r4)
})

xx <- data$signal[data.table(data$segments), on = ".id"]

rollup(xx, .(mean(X)), by = c(".sample_id", "condition", "recording"))[!is.na(.sample_id) & !is.na(condition) & is.na(recording)] 
cube(xx, .(mean(X)), by = c(".sample_id", "condition", "recording"))[!is.na(.sample_id) & !is.na(condition) & is.na(recording)] 
xx[,.(X = mean(X)), by = c(".sample_id", "condition")]
xx[,.(X = mean(X)), by = c(".sample_id", "condition", "recording")][,.(X = mean(X)), by = c(".sample_id", "condition")]



x2 <- xx[,.(X = mean(X)), by = c(".sample_id", "condition", "recording")]
x2[,.(X = mean(X)), by = c(".sample_id", "condition")]
data_s1$signal
data_s2$signal

extended_signal[,.(`X` = mean(X)), by = c(by)]
extended_signal[,.(`X` = mean(X)), by = c(".sample_id", "condition")]


mean(xx[,.(X = mean(X)), by = c(".sample_id", "condition", "recording")]$X)
mean(extended_signal$X)