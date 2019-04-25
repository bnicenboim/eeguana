context("test eeg artifacts")
library(eeguana)
set.seed(123)

N <- 1000
signal <- dplyr::tibble(.id = 1L,
                        .sample_id = sample_int(seq_len(N), sampling_rate = 500), 
                        Fz = channel_dbl(rep(0,N)),
                        Cz = channel_dbl(rep(0,N)),
                        Pz = channel_dbl(rep(0,N)))

signal$Fz[c(3, 500, 700)] <- 10
signal$Fz[c(1,2, 502 )] <- -10
signal$Cz[c(200,700,710,999,1000)] <- 10
data <- eeg_lst(
    signal_tbl = signal, 
    segments = dplyr::tibble(.id = 1L, recording = "recording1", segment =  1L)
)

data_1minmax <- eeg_lst(
    signal_tbl = dplyr::tibble( 
        X = channel_dbl(c(0,0,0,-10,10,0,0,0,0)),
        .id = 1L,
        .sample_id = sample_int(1:9, sampling_rate = 500) 
    ),
    segments = dplyr::tibble(.id = 1L, recording = "recording1", segment =  1L)
)

data_1step <- eeg_lst(
    signal_tbl = dplyr::tibble( 
        X = channel_dbl(c(0,0,0,0,10,10,10,10,10)),
        .id = 1L,
        .sample_id = sample_int(1:9, sampling_rate = 500) 
    ),
    segments = dplyr::tibble(.id = 1L, recording = "recording1", segment =  1L)
)

data_more <- eeg_lst(
    signal_tbl =  signal %>%
        mutate(.id = rep(1:4, each =N/4),
        .sample_id = sample_int(rep(seq_len(N/4),times= 4), sampling_rate = 500)) 
    ,
    segments = dplyr::tibble(.id = seq.int(4), recording = paste0("recording",c(1,1,2,2)), segment =  seq.int(4))
)

###### voltage steps ######################3

test_that("window of 1 step",{
    art_eventsp1 <- data_1step %>%
        eeg_artif_step(difference = .01, lim=c(0/500,1/500), unit = "second") %>%
        events_tbl()
    expect_equal(art_eventsp1[.channel=="X",]$.initial %>% as.numeric(), 5 )
    expect_equal(art_eventsp1[.channel=="X",]$.final %>% as.numeric(),6)
    art_events0 <- data_1step %>%
        eeg_artif_step(difference = .01, lim=c(0/500,0/500), unit = "second") %>%
        events_tbl()
    expect_equal(art_events0[.channel=="X",]$.initial %>% as.numeric(), 5)
    expect_equal(art_events0[.channel=="X",]$.final %>% as.numeric(),5)
    art_eventsm1 <- data_1step %>%
        eeg_artif_step(difference = .01, lim=c(-1/500,0/500), unit = "second") %>%
        events_tbl()
    expect_equal(art_eventsm1[.channel=="X",]$.initial %>% as.numeric(), 4)
    expect_equal(art_eventsm1[.channel=="X",]$.final %>% as.numeric(),5)
    
    art_eventsl <- data_1step %>%
        eeg_artif_step(difference = .01, lim=c(-1000/500,1000/500), unit = "second") %>%
        events_tbl()
    expect_equal(art_eventsl[.channel=="X",]$.initial %>% as.numeric(), 1)
    expect_equal(art_eventsl[.channel=="X",]$.final %>% as.numeric(),9)
})

test_that("window of 1 element",{
    art_events <- data %>%
        eeg_artif_step(difference = .01, lim=c(-1/500,0/500), unit = "second") %>%
        events_tbl()
    expect_equal(art_events[.channel=="Fz",]$.initial %>% as.numeric(), c(2,499,699) )
    expect_equal(art_events[.channel=="Fz",]$.final %>% as.numeric(),c(4,
                                                     503, # 503 - 499   steps 499-500,500-501,501-502,502-503
                                                     701)) )
    expect_equal(art_events[.channel=="Cz",]$.initial %>% as.numeric(), c(199,699, 709,998) )
    expect_equal(art_events[.channel=="Cz",]$.final %>% as.numeric(),c(201,701,711,999) )
    expect_equal(nrow(art_events[.channel == "Pz",]), 0)
})

test_that("window of 22 element",{
    art_events <- data %>%
        eeg_artif_step(difference = .01, lim=c(-10/500,10/500), unit = "second") %>%
        events_tbl()
    expect_equal(art_events[.channel=="Fz",]$.initial %>% as.numeric(), c(1,490,690) )
    expect_equal(art_events[.channel=="Fz",]$.final %>% as.numeric(),c(14,490+24-1,690+22-1))
    expect_equal(art_events[.channel=="Cz",]$.initial %>% as.numeric(), c(190,690,989) )
    expect_equal(art_events[.channel=="Cz",]$.final %>% as.numeric(),c(190 + 22 -1,690 + 32-1,989+12 -1) )
    expect_equal(nrow(art_events[.channel == "Pz",]), 0)
})

test_that("No artifacts",{
    art_events <- data %>%
        eeg_artif_step(difference = 100, lim=c(-10/500,10/500), unit = "second") %>%
        events_tbl()
    expect_equal(art_events,eeguana:::new_events_tbl() %>% .[,.initial := sample_int(integer(0), 500)])
})
test_that("window of 22 elements with different .id", {
    art_events <- data_more %>%
        eeg_artif_step(difference = .01, lim=c(-10/500,10/500), unit = "second") %>% events_tbl()
    expect_equal(art_events[.channel=="Fz",]$.initial %>% as.numeric(), c(1,490 -250,1,690-500) )
    expect_equal(art_events[.channel=="Fz",]$.final %>% as.numeric(),c(14,490 -250 + 10,13,690 -500 + 21))
    expect_equal(art_events[.channel=="Cz",]$.initial %>% as.numeric(), c(190,690-500,989-750) )
    expect_equal(art_events[.channel=="Cz",]$.final %>% as.numeric(),c(190 + 21,690-500 + 31,989-750 + 11) )
    expect_equal(nrow(art_events[.channel == "Pz",]), 0)
    expect_equal(art_events$.id, c(1L,1L,2L,3L,3L,3L,4L))
    
    art_events <- data_more %>%
        eeg_artif_step(Fz, difference = .01, lim=c(-10/500,10/500), unit = "second") %>% events_tbl()
      expect_equal(art_events[.channel=="Fz",]$.initial %>% as.numeric(), c(1,490 -250,1,690-500) )
    expect_equal(art_events[.channel=="Fz",]$.final %>% as.numeric(),c(14,490 -250 + 10,13,690 -500 + 21))
    expect_equal(art_events[.channel=="Cz",]$.initial %>% as.numeric(), integer(0) )
    expect_equal(art_events[.channel=="Cz",]$.final %>% as.numeric(),integer(0) )
    expect_equal(nrow(art_events[.channel == "Pz",]), 0)
})

test_that("missing samples",{
  data_1skip <- data_1step %>% filter(!.sample_id %in% c(4,5))
    art_events_skipped1 <- data_1skip %>%
        eeg_artif_step(difference = .01, lim=c(-1000/500,1000/500), unit = "second") %>%
        events_tbl()
    expect_equal(nrow(art_events_skipped1),0)
})

test_that("window of 22 element, with NAs",{
    data$signal$Fz[5] <- NA
    art_events <- data %>%
        eeg_artif_step(difference = .01, lim=c(-10/500,10/500), unit = "second") %>%
        events_tbl()
    expect_equal(art_events[.channel=="Fz",]$.initial %>% as.numeric(), c(1,490,690) )
    expect_equal(art_events[.channel=="Fz",]$.final %>% as.numeric(),c(14,490 + 23,690 + 21))
    expect_equal(art_events[.channel=="Cz",]$.initial %>% as.numeric(), c(190,690,989) )
    expect_equal(art_events[.channel=="Cz",]$.final %>% as.numeric(),c(190 + 21,690 + 31,989 + 11) )
    expect_equal(nrow(art_events[.channel == "Pz",]), 0)
})



###### voltage minmax ######################3
test_that("minmax: window of 1 step",{
    art_eventsp1 <- suppressWarnings(  #the window is too small, but it's ok for the test
        data_1step %>%
        eeg_artif_minmax(difference = .01, lim=c(0/500,1/500),window=1/500, unit = "second")
    ) %>%
        events_tbl()
    expect_equal(art_eventsp1[.channel=="X",]$.initial %>% as.numeric(), 5 )
    expect_equal(art_eventsp1[.channel=="X",]$.final %>% as.numeric(),6)

    art_events0 <- suppressWarnings(
        data_1step %>%
        eeg_artif_minmax(difference = .01, lim=c(0/500,0/500), window = 1/500, unit = "second")
    )%>%
        events_tbl()
    expect_equal(art_events0[.channel=="X",]$.initial %>% as.numeric(), 5)
    expect_equal(art_events0[.channel=="X",]$.final %>% as.numeric(),5)
    art_eventsm1 <- suppressWarnings(
        data_1step %>%
        eeg_artif_minmax(difference = .01, lim=c(-1/500,0/500), window = 1/500, unit = "second")
    )%>%
        events_tbl()
    expect_equal(art_eventsm1[.channel=="X",]$.initial %>% as.numeric(), 4)
    expect_equal(art_eventsm1[.channel=="X",]$.final %>% as.numeric(),5)
    
    art_eventsl <- suppressWarnings(
        data_1step %>%
        eeg_artif_minmax(difference = .01, lim=c(-1000/500,1000/500), window =1/500, unit = "second")
    )%>%
        events_tbl()
    expect_equal(art_eventsl[.channel=="X",]$.initial %>% as.numeric(), 1)
    expect_equal(art_eventsl[.channel=="X",]$.final %>% as.numeric(),9)
})

test_that("NAs",{
  data_1step$signal$X[1:2] <- NA
  data_1step$signal$X[5] <- NA
 art_eventsl <- data_1step %>%
        eeg_artif_minmax(difference = .01, lim=c(-1000/500,1000/500), window =3/500, unit = "second") %>%
        events_tbl()
    expect_equal(art_eventsl[.channel=="X",]$.initial %>% as.numeric(), 1)
    expect_equal(art_eventsl[.channel=="X",]$.final %>% as.numeric(),9)
})

test_that("missing samples",{
  data_1skip <- data_1step %>% filter(!.sample_id %in% c(4,5))
    art_events_skipped1 <- data_1skip %>%
        eeg_artif_minmax(difference = .01, lim=c(-1000/500,1000/500), window =1/500, unit = "second") %>%
        events_tbl()
    expect_equal(nrow(art_events_skipped1[.channel=="X",]),0)
     art_events_skipped1w <- data_1skip %>%
        eeg_artif_minmax(difference = .01, lim=c(-1000/500,1000/500), window =3/500, unit = "second") %>%
        events_tbl()
    expect_equal(art_events_skipped1w[.channel=="X",]$.initial %>% as.numeric(), 1)
    expect_equal(art_events_skipped1w[.channel=="X",]$.final %>% as.numeric(),9)
})

test_that("simple window",{
    art_events <- data_1minmax %>%
        eeg_artif_minmax(difference = 19, lim=c(0/500,4/500), unit = "second") %>%
        events_tbl()
    expect_equal(art_events[.channel=="X",]$.initial %>% as.numeric(), c(5) )
    expect_equal(art_events[.channel=="X",]$.final %>% as.numeric(),9 )
})


