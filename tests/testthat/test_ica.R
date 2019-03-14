context("test eeg ica")
library(eeguana)
set.seed(123)

# 3 independent sources
## S <- cbind(rt(1000,6),
##            rexp(1000,1),
##            runif(1000)
##            )

S <- cbind(pink_noise(1000)/200,
           pink_noise(1000)/200,
           sin(seq_len(1000)/10)
           )
## not correlated
cor(S) %>% .[.<1] %>%max()

s_tbl <- `colnames<-`(S, paste0("C", seq_len(ncol(S)))) %>%
    dplyr::as_tibble( .name_repair = "check_unique") %>%
    dplyr::mutate(x=1:n())%>%
    tidyr::gather("C", "y",-x) 
## ggplot(s_tbl,aes(x=x,y=y)) + geom_line() + facet_grid(~C)

# And they mix depending on the distance of S_pos:
A <-structure(c(0.415024629739427, -0.396594233424652, 1.5500801401529, 
                -0.199523720507532, 0.873497221011429, 0.877738604024566, -1.78779862971126, 
                -1.28749409074682, -0.640912580158802), .Dim = c(3L, 3L))

signal_matrix <- S %*% A

 
data <- eeg_lst(
  signal = signal_tbl( 
    signal_matrix = signal_matrix ,
    ids = rep(seq_len(4), each = 250),
    sample_ids = sample_int(rep(seq_len(250), times = 4), sampling_rate = 500) 
  ),
  events = dplyr::tibble(
    .id=integer(0),.sample_0 =integer(0), .size=integer(0), .channel=character(0),
  ),
  segments = dplyr::tibble(.id = seq_len(4), recording = c(rep("recording1",2),rep("recording2",2)), segment =  c(1L, 2L,1L,2L))
)

data <- data %>% group_by(recording)


data_ica_default <- eeg_ica(data)
data_ica_default_2ch <- eeg_ica(data, V3)
data_ica_m <- eeg_ica(data, method = fastICA::fastICA,config = list(verbose = FALSE) )

#plot(data_ica_default)+ facet_wrap(segment+.source~recording)

 
data_default_2 <- data_ica_default %>% as_eeg_lst()
data_default_2ch_2 <- data_ica_default_2ch %>% as_eeg_lst()
data_m_2 <- data_ica_m %>% as_eeg_lst()

expect_equal(data,data_default_2, tolerance=.01)
expect_equal(data,data_default_2ch_2, tolerance=.01)
expect_equal(data,data_m_2, tolerance=.01)

