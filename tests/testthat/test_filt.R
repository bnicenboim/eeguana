context("test filters")
library(eeguana)
set.seed(123)


N <- 1000
signal <- tibble(X1 =  sin(seq_len(N)),  # f = 1/(2* pi)
                 X2 = sin(seq_len(N) *2), # f = 3/(2*pi)
                 X3 =  sin(seq_len(N) *3), # f = 5/(2*pi)
                 X4 = X1+X2+X3)

data_sin <- eeg_lst(
    signal = signal_tbl( 
        signal_matrix = signal ,
        ids = 1L,
        sample_ids = sample_int(seq_len(N), sampling_rate = 500) 
    ),
    events = events_tbl(), 
    segments = dplyr::tibble(.id = 1L, recording = "recording1", segment =  1L)
)


data_sin_more <- eeg_lst(
    signal = signal_tbl( 
        signal_matrix = signal,
        ids = rep(1:4, each =N/4),
        sample_ids = sample_int(rep(seq_len(N/4),times= 4), sampling_rate = 500) 
    ),
    events = events_tbl(), 
    segments = dplyr::tibble(.id = seq.int(4), recording = paste0("recording",c(1,1,2,2)), segment =  seq.int(4))
)


data_sin_X1 <- eeg_filt_low_pass(data_sin, freq = 500*1/(2*pi))
## plot(data_sin_X1)

data_sin_X3 <- eeg_filt_high_pass(data_sin, freq = 500*3/(2*pi))
## plot(data_sin_X3)

data_sin_X2 <- eeg_filt_band_pass(data_sin, freq = c(1.5,2.2)*500/(2*pi))
## plot(data_sin_X2)

data_sin_X1X3 <- eeg_filt_band_stop(data_sin, freq = c(2.8,1.5)*500/(2*pi))
## plot(data_sin_X1X3)

## plot(data_sin_X2_onlyX1X2)

test_that("low pass signal",{
    data_sin_X1 <- data_sin_X1 %>% filter(as_time(.sample_id) %>% between(.25,1.75))
    data_sin <- data_sin %>% filter(as_time(.sample_id) %>% between(.25,1.75))
    expect_equal(data_sin_X1$signal$X1, data_sin_X1$signal$X4, tolerance=.001)
    expect_equal(data_sin_X1$signal$X1, data_sin$signal$X1, tolerance=.005)
    expect_lte(max(data_sin_X1$signal$X2, data_sin_X1$signal$X3),.0005)
})

test_that("high pass signal",{
    data_sin_X3 <- data_sin_X3 %>% filter(as_time(.sample_id) %>% between(.25,1.75))
    data_sin <- data_sin %>% filter(as_time(.sample_id) %>% between(.25,1.75))
    expect_equal(data_sin_X3$signal$X3, data_sin_X3$signal$X4, tolerance=.004)
    expect_equal(data_sin_X3$signal$X3, data_sin$signal$X3, tolerance=.002)
    expect_lte(max(data_sin_X3$signal$X2, data_sin_X3$signal$X1),.004)
})

test_that("band pass signal",{
    data_sin_X2 <- data_sin_X2 %>% filter(as_time(.sample_id) %>% between(.25,1.75))
    data_sin <- data_sin %>% filter(as_time(.sample_id) %>% between(.25,1.75))
    expect_equal(data_sin_X2$signal$X2, data_sin_X2$signal$X4, tolerance=.004)
    expect_equal(data_sin_X2$signal$X2, data_sin$signal$X2, tolerance=.002)
    expect_lte(max(data_sin_X2$signal$X3, data_sin_X2$signal$X1),.004)
})

test_that("band stop signal",{
    data_sin_X1X3 <- data_sin_X1X3 %>% filter(as_time(.sample_id) %>% between(.25,1.75))
    data_sin <- data_sin %>% filter(as_time(.sample_id) %>% between(.25,1.75))
    expect_equal(data_sin_X1X3$signal$X1+data_sin_X1X3$signal$X3, data_sin_X1X3$signal$X4, tolerance=.004)
    expect_equal(data_sin_X1X3$signal$X1, data_sin$signal$X1, tolerance=.003)
    expect_lte(max(data_sin_X1X3$signal$X2),.004)
})


## plot(data_sin_more) + facet_grid(.source~.id)
data_sin_more_X1 <- eeg_filt_low_pass(data_sin_more, freq = 500*1/(2*pi))
## plot(data_sin_more_X1) + facet_grid(.source~.id)

data_sin_more_X3 <- eeg_filt_high_pass(data_sin_more, freq = 500*3/(2*pi))
## plot(data_sin_more_X3)+ facet_grid(.source~.id)


data_sin_more_X2 <- eeg_filt_band_pass(data_sin_more, freq = c(1.5,2.2)*500/(2*pi))
## plot(data_sin_more_X2)+ facet_grid(.source~.id)


data_sin_more_X1X3 <- eeg_filt_band_stop(data_sin_more, freq = c(2.8,1.5)*500/(2*pi))
## plot(data_sin_more_X1X3)+ facet_grid(.source~.id)

data_sin_more_X2_onlyX1X2 <- eeg_filt_band_pass(data_sin_more, X1,X2, freq = c(1.5,2.2)*500/(2*pi))

test_that("low pass signal",{
    data_sin_more_X1 <- data_sin_more_X1 %>% filter(as_time(.sample_id) %>% between(.15,.4))
    data_sin_more <- data_sin_more %>% filter(as_time(.sample_id) %>% between(.15,.4))
    expect_equal(data_sin_more_X1$signal$X1, data_sin_more_X1$signal$X4, tolerance=.001)
    expect_equal(data_sin_more_X1$signal$X1, data_sin_more$signal$X1, tolerance=.005)
    expect_lte(max(data_sin_more_X1$signal$X2, data_sin_more_X1$signal$X3),.0005)
})

test_that("high pass signal",{
    data_sin_more_X3 <- data_sin_more_X3 %>% filter(as_time(.sample_id) %>% between(.15,.4))
    data_sin_more <- data_sin_more %>% filter(as_time(.sample_id) %>% between(.15,.4))
    expect_equal(data_sin_more_X3$signal$X3, data_sin_more_X3$signal$X4, tolerance=.004)
    expect_equal(data_sin_more_X3$signal$X3, data_sin_more$signal$X3, tolerance=.002)
    expect_lte(max(data_sin_more_X3$signal$X2, data_sin_more_X3$signal$X1),.004)
})

test_that("band pass signal",{
    data_sin_more_X2 <- data_sin_more_X2 %>% filter(as_time(.sample_id) %>% between(.15,.4))
    data_sin_more <- data_sin_more %>% filter(as_time(.sample_id) %>% between(.15,.4))
    expect_equal(data_sin_more_X2$signal$X2, data_sin_more_X2$signal$X4, tolerance=.004)
    expect_equal(data_sin_more_X2$signal$X2, data_sin_more$signal$X2, tolerance=.002)
    expect_lte(max(data_sin_more_X2$signal$X3, data_sin_more_X2$signal$X1),.004)
})

test_that("band stop signal",{
    data_sin_more_X1X3 <- data_sin_more_X1X3 %>% filter(as_time(.sample_id) %>% between(.15,.4))
    data_sin_more <- data_sin_more %>% filter(as_time(.sample_id) %>% between(.15,.4))
    expect_equal(data_sin_more_X1X3$signal$X1+data_sin_more_X1X3$signal$X3, data_sin_more_X1X3$signal$X4, tolerance=.004)
    expect_equal(data_sin_more_X1X3$signal$X1, data_sin_more$signal$X1, tolerance=.003)
    expect_lte(max(data_sin_more_X1X3$signal$X2),.004)
})

test_that("band pass signal, sel",{
    data_sin_more_X2_onlyX1X2 <- data_sin_more_X2_onlyX1X2 %>%
        filter(as_time(.sample_id) %>% between(.15,.4))
    data_sin_more_X2 <- data_sin_more_X2 %>% filter(as_time(.sample_id) %>% between(.15,.4))
    data_sin_more <- data_sin_more %>% filter(as_time(.sample_id) %>% between(.15,.4))
    expect_equal(data_sin_more_X2_onlyX1X2$signal$X1, data_sin_more_X2$signal$X1, tolerance=.004)
    expect_equal(data_sin_more_X2_onlyX1X2$signal$X2, data_sin_more$signal$X2, tolerance=.002)
    expect_equal(data_sin_more_X2_onlyX1X2$signal$X3, data_sin_more$signal$X3, tolerance=.002)
    expect_equal(data_sin_more_X2_onlyX1X2$signal$X4, data_sin_more$signal$X4, tolerance=.002)
    ## expect_lte(max(data_sin_more_X2$signal$X3, data_sin_more_X2$signal$X1),.004)
})

