context("test eeg artifacts")
library(eeguana)
set.seed(123)

N <- 1000
signal <- tibble(Fz = rep(0,N),
                 Cz = rep(0,N),
                 Pz = rep(0,N))

signal$Fz[c(1,2,3, 500,502, 700)] <- 10
signal$Cz[c(200,700,710,999,1000)] <- 10

data <- eeg_lst(
    signal = signal_tbl( 
        signal_matrix = signal ,
        ids = 1L,
        sample_ids = sample_int(seq_len(N), sampling_rate = 500) 
    ),
    events = events_tbl(), 
    segments = dplyr::tibble(.id = 1L, recording = "recording1", segment =  1L)
)


data_more <- eeg_lst(
    signal = signal_tbl( 
        signal_matrix = signal ,
        ids = rep(1:4, each =N/4),
        sample_ids = sample_int(rep(seq_len(N/4),times= 4), sampling_rate = 500) 
    ),
    events = events_tbl(), 
    segments = dplyr::tibble(.id = seq.int(4), recording = paste0("recording",c(1,1,2,2)), segment =  seq.int(4))
)


test_that("window of 1 element",{
    art_events <- data %>%
        eeg_artif_step(step = .01, lim=c(-1/500,0/500), unit = "second") %>%
        events()
    expect_equal(art_events[.channel=="Fz",]$.sample_0,c(3,499,699) )
    expect_equal(art_events[.channel=="Fz",]$.size,c(2, #1 sample is no length, one to the left
                                                     5, # 503 - 499 +1 =5  steps 499-500,500-501,501-502,502-503
                                                     3)) # 701-699 +1 )
    expect_equal(art_events[.channel=="Cz",]$.sample_0,c(199,699, 709,998) )
    expect_equal(art_events[.channel=="Cz",]$.size,c(3,3,3,2) )
    expect_equal(nrow(art_events[.channel == "Pz",]), 0)
})

test_that("window of 22 element",{
    art_events <- data %>%
        eeg_artif_step(step = .01, lim=c(-10/500,10/500), unit = "second") %>%
        events()
    expect_equal(art_events[.channel=="Fz",]$.sample_0,c(1,490,690) )
    expect_equal(art_events[.channel=="Fz",]$.size,c(14,24,22))
    expect_equal(art_events[.channel=="Cz",]$.sample_0,c(190,690,989) )
    expect_equal(art_events[.channel=="Cz",]$.size,c(22,32,12) )
    expect_equal(nrow(art_events[.channel == "Pz",]), 0)
})

test_that("No artifacts",{
    art_events <- data %>%
        eeg_artif_step(step = 100, lim=c(-10/500,10/500), unit = "second") %>%
        events()
    expect_equal(art_events,events_tbl())
})
test_that("window of 22 elements with different ids", {
    art_events <- data_more %>%
        eeg_artif_step(step = .01, lim=c(-10/500,10/500), unit = "second") %>% events()
    expect_equal(art_events[.channel=="Fz",]$.sample_0,c(1,490 -250,1,690-500) )
    expect_equal(art_events[.channel=="Fz",]$.size,c(14,11,13,22))
    expect_equal(art_events[.channel=="Cz",]$.sample_0,c(190,690-500,989-750) )
    expect_equal(art_events[.channel=="Cz",]$.size,c(22,32,12) )
    expect_equal(nrow(art_events[.channel == "Pz",]), 0)
    expect_equal(art_events$.id, c(1L,1L,2L,3L,3L,3L,4L))
})


