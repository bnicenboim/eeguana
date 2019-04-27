context("test eeg ica")
library(eeguana)
set.seed(123)

## simulate eye movements
##https://stackoverflow.com/questions/53071709/remove-eye-blinks-from-eeg-signal-with-ica

N <- 4000
fs = 100
blink <-  rbinom(N, 1, .003) %>%
    signal::filter(signal::butter(2, c(1*2/fs, 10*2/fs), 'pass'), .)
noise <- rpink(N)
alpha <- (abs(sin(10 * seq_len(N) / fs)) - 0.5)/2

s_tbl <- dplyr::tibble(sample = rep(seq_len(N),times=3), A = c(blink, noise, alpha), component = rep(c("blink","noise", "alpha"), each = N) ) 
#ggplot(s_tbl,aes(x=sample,y=A)) + geom_line() + facet_grid(component~.)

# And they mix depending on the distance of S_pos:

signal_blinks <- dplyr::tibble(Fz = blink * 2 + alpha *.1 + noise,
                 Cz = blink * 1 + alpha * .15 + noise,
                 Pz = blink * .1 + alpha * .1 + noise)  %>%
    dplyr::mutate_all(channel_dbl)


signal <- dplyr::tibble(Fz =  alpha *.1 + noise,
                        Cz =  alpha * .15 + noise,
                        Pz =  alpha * .1 + noise) %>%
    dplyr::mutate_all(channel_dbl)

data <- eeg_lst(
    signal_tbl = signal  %>%
dplyr::mutate(
        .id = 1L,
        .sample_id = sample_int(seq_len(N), sampling_rate = 500) 
    ),
    segments_tbl = dplyr::tibble(.id = 1L, recording = "recording1", segment =  1L)
)

data_blinks <- eeg_lst(
    signal_tbl = signal_blinks  %>%
dplyr::mutate(
        .id = 1L,
        .sample_id = sample_int(seq_len(N), sampling_rate = 500) 
    ),
    segments_tbl = dplyr::tibble(.id = 1L, recording = "recording1", segment =  1L)
)


data_blinks_more <- eeg_lst(
    signal_tbl = signal_blinks  %>%
dplyr::mutate(
        .id = rep(1:4, each =N/4),
        .sample_id = sample_int(rep(seq_len(N/4),times= 4), sampling_rate = 500) 
    ),
    segments_tbl = dplyr::tibble(.id = seq.int(4), recording = paste0("recording",c(1,1,2,2)), segment =  seq.int(4))
)

data_more <- eeg_lst(
    signal_tbl = signal %>%
dplyr::mutate(
        .id = rep(1:4, each =N/4),
        .sample_id = sample_int(rep(seq_len(N/4),times= 4), sampling_rate = 500) 
    ),
    segments_tbl = dplyr::tibble(.id = seq.int(4), recording = paste0("recording",c(1,1,2,2)), segment =  seq.int(4))
)

data_blinks_more_NA <- data_blinks_more
data_blinks_more$signal[1,]$Fz <- NA_real_
data_blinks_more_NA$signal[5,]$Cz <- NA_real_
##plot(data)


data_ica_default <- eeg_ica(data_blinks)
data_rec <- data_ica_default %>% as_eeg_lst()

data_ica_m <- eeg_ica(data_blinks, method = fastICA::fastICA,config = list(verbose = FALSE) )
data_rec_m <- data_ica_m %>% as_eeg_lst()

data_ica_Fz <- eeg_ica(data_blinks, -Fz)
data_rec_Fz <- data_ica_Fz %>% as_eeg_lst()

test_that("ica is a reversible",{
expect_equal(data_blinks,data_rec)
expect_equal(data_blinks,data_rec_m)
expect_equal(data_blinks,data_rec_Fz) 
})

data_ica_red <- data_ica_default %>% select(-ICA1, comp1 = ICA2, comp2 = ICA3)
data_no_blinks <- data_ica_red %>% as_eeg_lst()


test_that("ica can remove blinks",{
expect_equal(data,data_no_blinks, tolerance = .021)
})


test_that("appropiate warnings",{
expect_warning(data_blinks_more %>% eeg_ica())
})

data_blinks_more <-  data_blinks_more %>% group_by(recording)
data_ica_b_m <- data_blinks_more %>% eeg_ica()

data_b_m_rec <-   as_eeg_lst(data_ica_b_m)
data_b_m_rec_Fz <- eeg_ica(data_blinks_more, -Fz) %>% as_eeg_lst()

data_blinks_more_no_blinks <- data_ica_b_m %>%
    select(-ICA1, comp1 = ICA2, comp2 = ICA3) %>%
    as_eeg_lst() 

test_that("ica grouped works",{ 
    expect_equal(data_blinks_more,data_b_m_rec)
    expect_equal(data_blinks_more,data_b_m_rec_Fz)
    expect_equal(data_more,data_blinks_more_no_blinks %>% ungroup() , tolerance = .01) 
})

data_blinks_more_NA <-  data_blinks_more_NA %>% group_by(recording)
data_ica_b_m_NA <- data_blinks_more_NA %>% eeg_ica(na.rm=TRUE)

data_b_m_rec <-   as_eeg_lst(data_ica_b_m)
data_b_m_rec_Fz <- eeg_ica(data_blinks_more, -Fz) %>% as_eeg_lst()

