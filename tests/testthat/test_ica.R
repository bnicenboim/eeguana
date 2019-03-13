context("test eeg ica")
library(eeguana)


# 5 independent sources
S <- cbind(sin((1:1000)/20),
           cos((1:1000)/20),
           sin((1:1000)/5),
           rep((((1:200)-100)/100), 5),
           rep(((1:200)/100 -1), 5)
           )
# not correlated
cor(S) %>% .[.<1] %>%max()

s_tbl <- `colnames<-`(S, paste0("C", seq_len(ncol(S)))) %>%
    dplyr::as_tibble( .name_repair = "check_unique") %>%
    dplyr::mutate(x=1:n())%>%
    tidyr::gather("C", "y",-x) 
ggplot(s_tbl,aes(x=x,y=y)) + geom_line() + facet_grid(~C)

# eeg electrodes
channels <- dplyr::tibble(
      channel = c("A", "B","C","D","E"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = c(-.6, .6,0,-.6,.6), .y = c(-.6, -.6,0,.6,.6), .z = NA_real_
    )

# And they mix depending on the distance of S_pos:
A <-structure(c(0.762492851663023, 0.572598334313868, 0.854357657716761, 
                0.854357657716761, 0.762492851663023, 0.762492851663023, 0.854357657716761, 
                0.854357657716761, 0.572598334313868, 0.762492851663023, 1, 0.81923192051904, 
                0.81923192051904, 0.81923192051904, 1, 0.762492851663023, 0.572598334313868, 
                0.572598334313868, 0.854357657716761, 0.762492851663023, 0.762492851663023, 
                0.854357657716761, 0.572598334313868, 0.572598334313868, 0.762492851663023
                ), .Dim = c(5L, 5L), .Dimnames = list(c("C1", "C2", "C3", "C4", 
                                                        "C5"), NULL))


signal_matrix <- S %*% A

 
data <- eeg_lst(
  signal = signal_tbl(
    signal_matrix = signal_matrix
    ,
    ids = rep(seq_len(4), each = 250),
    sample_ids = sample_int(rep(seq_len(250), times = 4), sampling_rate = 500),
    channels
  ),
  events = dplyr::tibble(
    .id=integer(0),.sample_0 =integer(0), .size=integer(0), .channel=character(0),
  ),
  segments = dplyr::tibble(.id = seq_len(4), recording = c(rep("recording1",2),rep("recording2",2)), segment =  c(1L, 2L,1L,2L))
)

data <- data %>% group_by(recording)
 
data_ica <- eeg_ica(data,  method = "fastica")

data_2 <- data_ica %>% as_eeg_lst()

testthat::expect_equal(data,data_2, tolerance=.01)
