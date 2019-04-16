context("test eeg artifacts")
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

s_tbl <- tibble(sample = rep(seq_len(N),times=3), A = c(blink, noise, alpha), component = rep(c("blink","noise", "alpha"), each = N) ) 
                                        #ggplot(s_tbl,aes(x=sample,y=A)) + geom_line() + facet_grid(component~.)

                                        # And they mix depending on the distance of S_pos:

signal_blinks <- tibble(Fz = blink * 2 + alpha *.1 + noise,
                        Cz = blink * 1 + alpha * .15 + noise,
                        Pz = blink * .1 + alpha * .1 + noise)

signal <- tibble(Fz =  alpha *.1 + noise,
                 Cz =  alpha * .15 + noise,
                 Pz =  alpha * .1 + noise)
data_blinks <- eeg_lst(
    signal = signal_tbl( 
        signal_matrix = signal_blinks ,
        ids = 1L,
        sample_ids = sample_int(seq_len(N), sampling_rate = 500) 
    ),
    events = events_tbl(), 
    segments = dplyr::tibble(.id = 1L, recording = "recording1", segment =  1L)
)


data_blinks_more <- eeg_lst(
    signal = signal_tbl( 
        signal_matrix = signal_blinks ,
        ids = rep(1:4, each =N/4),
        sample_ids = sample_int(rep(seq_len(N/4),times= 4), sampling_rate = 500) 
    ),
    events = events_tbl(), 
    segments = dplyr::tibble(.id = seq.int(4), recording = paste0("recording",c(1,1,2,2)), segment =  seq.int(4))
)


data_blinks %>% eeg_grad_artifact(step = .01)


plot(data_blinks)
x <- c(1,2,3,4,5,10,7)

ch_sel=  channel_names(data_blinks)
amp_step = .2
sample_range = c(-100,100) %>% as.integer
.eeg_lst <- data_blinks
   ## a<- data_blinks$signal[,purrr::map(.SD, ~ abs(.x - data.table::shift(.x, n = 1)) > amp_step ), .SDcols =ch_sel][-1,]

artifact_found <- .eeg_lst$signal[,c(list(.sample_id = .sample_id[-1]), purrr::map(.SD, ~ abs(diff(.x)) > amp_step)) , .SDcols = (ch_sel), by = .id]

events_found <- artifact_found %>%
    split(.,.$.id) %>%
    map_dtr( function(.eeg)
        .eeg %>% dplyr::select_at(dplyr::vars(ch_sel)) %>%
        imap_dtr( ~{
            if(all(.x==FALSE)){
                data.table::data.table()
            } else {
                left <-  .eeg$.sample_id[.x] + sample_range[[1]]
                right <-  .eeg$.sample_id[.x] + sample_range[[2]]
                ##merge if there are steps closer than the window for removal 
                left_merge <- c(TRUE, diff(left) > abs(sample_range[[1]]))
                right_merge <- c(diff(right) > abs(sample_range[[2]]), TRUE)
                .sample_0 <- left[left_merge] %>% .[.>= min(.eeg$.sample_id) ]
                cut_right <- right[right_merge] %>% .[.<= max(.eeg$.sample_id) ]
                .size <-   cut_right - .sample_0 +1L
                data.table::data.table(type = "artifact",
                                       description="gradient",
                                       .sample_0 = .sample_0,
                                       .size = .size,
                                       .channel = .y)
            }
        }),.id = TRUE
        )
new_events <- rbind(events_found, events(.eeg_lst), fill = TRUE)
data.table::setorder(new_events,.id, .sample_0, .channel)


imap_dtr(.x, ~ data.frame(a=.x))

events(.eeg_lst)
$events
.id         type description .sample_0 .size .channel
1:   1  New Segment                     1     1     <NA>
                                                  2:   1 Bad Interval Bad Min-Max      2158   738      Fp1
3:   1 Bad Interval Bad Min-Max      2161   731      Fp2
4:   1 Bad Interval Bad Min-Max      2162   729      Fpz
5:   1 Bad Interval Bad Min-Max      2173   689       F8
---                                                      
    4272:   1 Bad Interval Bad Min-Max    524692   204       P8
4273:   1 Bad Interval Bad Min-Max    524725   268      FC5
4274:   1 Bad Interval Bad Min-Max    524777   346       P7
4275:   1 Bad Interval Bad Min-Max    524983   173       Cz
4276:   1 Bad Interval Bad Min-Max    525073   135       O2

{split(.,.$.id)}

.x <- c(FALSE,FALSE,TRUE, TRUE, FALSE, TRUE, FALSE, FALSE,FALSE,TRUE, FALSE,FALSE, FALSE, FALSE, TRUE )
.y <- seq_along(.x)
r <- c(-2,2)

w1 <- {.y[.x] + r[1]}
s_merge <-c(TRUE, diff(w1) > abs(r[1]))
w1[s_merge]

w2 <- .y[.x] + r[2]
?rle
data.table::shift(.y, n = r)

{.[[1]][.x]}
?data.table::shift

diff
