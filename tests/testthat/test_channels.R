context("test Channel functions")
library(eeguana)


data_eeg <- eeg_lst(
  signal_tbl =
 dplyr::tibble(X = sin(1:20), Y = cos(1:20),
    .id = rep(c(1L, 2L), each = 10),
    .sample = sample_int(rep(seq(-4L, 5L), times = 2), sampling_rate = 500)),
   channels_tbl = dplyr::tibble(
      .channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = NA_real_, .y = NA_real_, .z = NA_real_
),   events_tbl = dplyr::tribble(
    ~.id, ~.type, ~.description, ~.initial, ~.final, ~.channel,
    1L, "New Segment", NA_character_, -4L, -4L, NA,
    1L, "Bad", NA_character_, -2L, 0L, NA,
    1L, "Time 0", NA_character_, 1L, 1L, NA,
    1L, "Bad", NA_character_, 2L, 3L, "X",
    2L, "New Segment", NA_character_, -4L, -4L, NA,
    2L, "Time 0", NA_character_, 1L, 1L, NA,
    2L, "Bad", NA_character_, 2L, 2L, "Y"
    ),
  segments = dplyr::tibble(.id = c(1L, 2L), .recording = "recording1", segment = c(1L, 2L))
)



data_M <- transmute(data_eeg, mean = chs_mean(X,Y))

data_M_q <- transmute(data_eeg, mean = chs_mean(c("X","Y")))

test_that("can take the mean of the channels", {
expect_equal(data_M$.signal$mean %>% as.numeric(), rowMeans(data_eeg$.signal[,.(X,Y)]))
expect_equal(data_M_q$.signal$mean %>% as.numeric(), rowMeans(data_eeg$.signal[,.(X,Y)]))
})

data_M2 <- chs_mean(data_eeg)
test_that("both .eeg_lst and .channel_dbl give the same output for chs_mean", {
  expect_equal(data_M, data_M2)
})

data_M_f <- transmute(data_eeg, mean = chs_fun(X,Y, .funs = mean))
data_M_fa <- chs_fun(data_eeg, "mean")
data_M_fa2 <- chs_fun(data_eeg, mean)
data_M_fa3 <- chs_fun(data_eeg, list(mean = ~ mean(.)))
data_M_fa4 <- chs_fun(data_eeg, ~ mean(.,na.rm = TRUE)) %>%
  dplyr::rename(mean =  X...mean....na.rm...TRUE.)
data_eeg_NA <- data_eeg %>% mutate(X = if_else(X>.98, channel_dbl(NA), X)) 
data_M_fa_NA1 <- chs_fun(data_eeg_NA, list(mean = ~ mean(.,na.rm = TRUE)))
data_M_fa_NA2 <- chs_fun(data_eeg_NA, mean, list(na.rm=TRUE))

test_that("both chs_fun and chs_mean give the same output", {
  expect_equal(data_M_f, data_M_fa)
  expect_equal(data_M_f, data_M_fa2)
  expect_equal(data_M_f, data_M_fa3)
  expect_equal(data_M_f, data_M_fa4)
  expect_equal(data_M_fa_NA1, data_M_fa_NA2)
  expect_equal(data_M, data_M_f)
})



## data_reref <- mutate(data_eeg, X = ch_rereference(X, X, Y))
data_eeg_Z <- data_eeg %>% mutate(Z = channel_dbl(0))
 X_reref <- data_eeg_Z$.signal$X - (data_eeg$.signal$X+data_eeg$.signal$Y)/2
 Y_reref <- data_eeg_Z$.signal$Y - (data_eeg$.signal$X+data_eeg$.signal$Y)/2
Z_reref <- data_eeg_Z$.signal$Z - (data_eeg$.signal$X+data_eeg$.signal$Y)/2
 attributes(X_reref)$.reference = "X, Y"
attributes(Y_reref)$.reference = "X, Y"
attributes(Z_reref)$.reference = "X, Y"

## test_that("can reref the mean of the channels", {
##   expect_equal(data_reref$.signal$X, X_reref)
## })

data_reref_all_chs <- eeg_rereference(data_eeg_Z, ref = c("X", "Y"))

test_that(".reference changes", {
    expect_equal(unique(channels_tbl(data_reref_all_chs)$.reference),"X, Y")
    expect_equal(data_reref_all_chs$.signal$X %>% as.numeric, X_reref %>% as.numeric)
    expect_equal(data_reref_all_chs$.signal$Y %>% as.numeric, Y_reref %>% as.numeric)
    expect_equal(data_reref_all_chs$.signal$Z %>% as.numeric, Z_reref %>% as.numeric)
})


## data_reref_all <- transmute(data_eeg, X_ref = ch_rereference(X, X, Y), Y_ref = ch_rereference(Y, X, Y))  %>%
##                     rename(X = X_ref, Y = Y_ref)


## test_that("both .eeg_lst and .channel_dbl give the same values for ch_rereference (it's ok to loose the events and attributes", {
##   expect_equal(data_reref_all$.signal$X %>% as.numeric, data_reref_all_chs$.signal$X %>% as.numeric)
##   expect_equal(data_reref_all$.signal$Y %>% as.numeric, data_reref_all_chs$.signal$Y %>% as.numeric)
## })


    
