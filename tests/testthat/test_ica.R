context("test eeg ica")
library(eeguana)

library(MASS)

# 6 independent sources
S <- cbind(sin((1:1000)/20),
           cos((1:1000)/20),
           sin((1:1000)/5),
           cos((1:1000)/200),
           rep((((1:200)-100)/100), 5),
           rep(((1:200)/100 -1), 5)
           )
# not correlated
cor(S) %>% .[.<1] %>%max()

s_tbl <- as_tibble(S) %>% mutate(x=1:n())%>% tidyr::gather("C", "y",-x)
ggplot(s_tbl,aes(x=x,y=y)) + geom_line() + facet_grid(~C)

# eeg electrodes
channels <- dplyr::tibble(
      channel = c("A", "B","C","D","E"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = c(-.6, .6,0,-.6,.6), .y = c(-.6, -.6,0,.6,.6), .z = NA_real_
    )

# I assume that the sources are stronger in these areas of the scalp
S_pos <- dplyr::tribble(~x, ~y,
                  0,  0,
                 .0, .7,
                 .7,  0,
                  0, -.7,
                  -.7, 0,
                  0, 0)

# And they mix depending on the distance of S_pos:
A <- S_pos %>% as.list() %>% 
          purrr::transpose() %>% {setNames(.,paste0("C",seq_len(length(.))))} %>%
          purrr::map_dfr(~ 1/ sqrt((.$x -channels$.x)^2 + (.$y -channels$.y)^2 +1) )  %>%
          as.matrix() %>% t()

A %>% dplyr::as_tibble() %>% setNames(c("A", "B","C","D","E")) %>%
      dplyr::mutate(component = paste0("C",1:n())) %>%
       dplyr::bind_cols(S_pos) %>%
      tidyr::gather("channel","amplitude", -component,-x,-y) %>%
      dplyr::left_join(channels) %>% dplyr::group_by(component) %>%
      interpolate_tbl() %>% plot_topo() + facet_grid(~component) +
      geom_point(data= S_pos %>% mutate(component = paste0("C",1:n())), aes(x=x,y=y))


signal_matrix <- S %*% A


data <- eeg_lst(
  signal = signal_tbl(
    signal_matrix = signal_matrix
    ,
    ids = rep(c(1L, 2L), each = 500),
    sample_ids = sample_int(rep(seq(1L, 500L), times = 2), sampling_rate = 500),
    channels
  ),
  events = dplyr::tibble(
    .id=integer(0),.sample_0 =integer(0), .size=integer(0), .channel=character(0),
  ),
  segments = dplyr::tibble(.id = c(1L, 2L), recording = "recording1", segment = c(1L, 2L))
)

plot(data) + facet_grid(channel~.id)


C_eyes <- c(tan(((1:30))/20) %>% ifelse(. > 4,4,.),
                           rep(0,200),
                           tan(((1:30))/20) %>% ifelse(. > 4,4,.),
                           rep(0,400), 
                           tan(((1:30))/20) %>% ifelse(. > 4,4,.),
                           rep(0,310))

A_new <- A %>% rbind(1/sqrt((0-channels$.x)^2 + (1.2-channels$.y)^2)) # add a new element to A that affects the  electrodes near the eye most

A_new %>% dplyr::as_tibble() %>% setNames(c("A", "B","C","D","E")) %>%
      dplyr::mutate(component = paste0("C",1:n())) %>%
      tidyr::gather("channel","amplitude", -component) %>%
      dplyr::left_join(channels) %>% dplyr::group_by(component) %>%
      interpolate_tbl() %>% plot_topo() + facet_grid(~component)



signal_matrix_eyes <- cbind(S,C_eyes) %*% A_new


data_eyes <- eeg_lst(
  signal = signal_tbl(
    signal_matrix = signal_matrix_eyes
    ,
    ids = rep(c(1L, 2L), each = 500),
    sample_ids = sample_int(rep(seq(1L, 500L), times = 2), sampling_rate = 500),
    channels
  ),
  events = dplyr::tibble(
    .id=integer(0),.sample_0 =integer(0), .size=integer(0), .channel=character(0),
  ),
  segments = dplyr::tibble(.id = c(1L, 2L), recording = "recording1", segment = c(1L, 2L))
)

plot(data_eyes) + facet_grid(channel~.id)
.eeg_lst <- data_eyes

plot(data_eyes) + facet_grid(channel~.id)
data %>% filter(.id==1) %>% summarize_all_ch(mean) %>% interpolate_tbl() %>% plot_topo()

.eeg_lst <- data_eyes %>% select(-eye)

data_eyes_fixed <- eeg_lst(
  signal = signal_tbl(
    signal_matrix = signal_out
    ,
    ids = rep(c(1L, 2L), each = 500),
    sample_ids = sample_int(rep(seq(1L, 500L), times = 2), sampling_rate = 500),
    channels
  ),
  events = dplyr::tibble(
    .id=integer(0),.sample_0 =integer(0), .size=integer(0), .channel=character(0),
  ),
  segments = dplyr::tibble(.id = c(1L, 2L), recording = "recording1", segment = c(1L, 2L))
)

plot(data_eyes_fixed) + facet_grid(channel~.id)
.eeg_lst <- data_eyes
# ?interpolate_tbl
# as_tibble(MASS::ginv(ica_res$W) ) %>% 
# mutate(channel=c("A","B","C","D","E")) %>% 
# # mutate(comp=1:n())%>% 
# tidyr::gather(comp, amplitude, -channel) %>% left_join(channels_tbl(.eeg_lst)) %>% filter(comp=="V4") %>% 
# interpolate_tbl() %>% plot_topo() 


# as.matrix(signal_raw) %*% ica_res$W[1,]
# MASS::ginv(ica_res$W)[,1]