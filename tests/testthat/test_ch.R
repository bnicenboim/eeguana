context("test ch functions")
library(eeguana)


data_eeg <- eeg_lst(
  signal = signal_tbl(
    signal_matrix = as.matrix(
      data.frame(X = sin(1:20), Y = cos(1:20))
    ),
    ids = rep(c(1L, 2L), each = 10),
    sample_ids = sample_int(rep(seq(-4L, 5L), times = 2), sampling_rate = 500),
    dplyr::tibble(
      channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_
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
    2L, "Bad", NA_character_, 2L, 1L, "Y"
  ),
  segments = dplyr::tibble(.id = c(1L, 2L), recording = "recording1", segment = c(1L, 2L))
)



data_M <- transmute(data_eeg, mean = chs_mean(X,Y))

data_M_q <- transmute(data_eeg, mean = chs_mean(c("X","Y")))

test_that("can take the mean of the channels", {
expect_equal(data_M$signal$mean %>% as.numeric(), rowMeans(data_eeg$signal[,.(X,Y)]))
expect_equal(data_M_q$signal$mean %>% as.numeric(), rowMeans(data_eeg$signal[,.(X,Y)]))
})

data_M2 <- chs_mean(data_eeg)
test_that("both .eeg_lst and .channel_dbl give the same output for chs_mean", {
  expect_equal(data_M, data_M2)
})

data_M_f <- transmute(data_eeg, mean = chs_fun(X,Y,.funs = mean))
data_M_fa <- chs_fun(data_eeg,.funs = mean)

test_that("both chs_fun and chs_mean give the same output", {
  expect_equal(data_M, data_M_fa)
})



data_reref <- mutate(data_eeg, X = ch_rereference(X, X, Y))
X_reref <- data_eeg$signal$X - (data_eeg$signal$X+data_eeg$signal$Y)/2
attributes(X_reref)$.reference = "X, Y"

test_that("can reref the mean of the channels", {
  expect_equal(data_reref$signal$X, X_reref)
})

data_reref_all_chs <- ch_rereference(data_eeg, X, Y)

test_that(".reference changes", {
  expect_equal(unique(channels_tbl(data_reref_all_chs)$.reference),"X, Y")
})


data_reref_all <- transmute(data_eeg, X_ref = ch_rereference(X, X, Y), Y_ref = ch_rereference(Y, X, Y))  %>%
                    rename(X = X_ref, Y = Y_ref)


test_that("both .eeg_lst and .channel_dbl give the same values for ch_rereference (it's ok to loose the events and attributes", {
  expect_equal(data_reref_all$signal$X %>% as.numeric, data_reref_all_chs$signal$X %>% as.numeric)
  expect_equal(data_reref_all$signal$Y %>% as.numeric, data_reref_all_chs$signal$Y %>% as.numeric)
})

