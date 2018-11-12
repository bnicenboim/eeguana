## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----downloading, eval = FALSE-------------------------------------------
#  download.file("https://www.ling.uni-potsdam.de/~nicenboim/files/faces.vhdr",
#                "faces.vhdr", mode="wb")
#  download.file("https://www.ling.uni-potsdam.de/~nicenboim/files/faces.vmrk",
#                "faces.vmrk", mode="wb")
#  download.file("https://www.ling.uni-potsdam.de/~nicenboim/files/faces.dat",
#                "faces.dat", mode="wb")

## ----libs, message = FALSE-----------------------------------------------
library(eeguana)

## ------------------------------------------------------------------------
faces <- read_vhdr("faces.vhdr")

## ------------------------------------------------------------------------
faces

## ------------------------------------------------------------------------
summary(faces)

## ---- fig.dim = c(10,15), out.width = "100%"-----------------------------
plot(faces)

## ---- fig.dim = c(10,15), out.width = "100%"-----------------------------
plot(faces) + coord_cartesian(ylim = c(-500,500))

## ------------------------------------------------------------------------
faces_segs <- faces %>% 
               segment(description %in% c("s70", "s71"), 
                        lim = c(-.2,.25)) %>%
               event_to_ch_NA(type == "Bad Interval") %>% 
               ch_baseline()

## ------------------------------------------------------------------------
faces_segs_some <- faces_segs %>%  
                  mutate(condition =
                  if_else(description == "s70", "faces", "non-faces")) %>% 
                  select(-type)

faces_segs_some

## ---- fig.dim = c(10,15), out.width = "100%"-----------------------------
faces_segs_some %>% 
                  select(O1, O2, P7, P8) %>% 
                  plot_gg(faces_segs_some) + 
                  geom_line(alpha = .1, aes(group = .id, color = condition)) + 
                  stat_summary(fun.y = "mean", geom ="line", alpha = 1, size = 1.5, 
                  aes(color = condition)) +
                  facet_wrap(~ channel) + 
                  geom_vline(xintercept = 0, linetype = "dashed") + 
                geom_vline(xintercept = .17, linetype = "dotted") + 
                theme(legend.position = "bottom") 

## ---- fig.dim = c(10,15), out.width = "100%"-----------------------------
faces_segs_some %>% ungroup(faces_segs_some) %>%
                transmute(Occipital = chs_mean(O1, O2, Oz, na.rm = TRUE),
                          Parietal = chs_mean(P3, P4, P7,  P8, Pz, na.rm = TRUE)) %>% 
                plot_gg() + 
                geom_line(alpha = .1, aes(group = .id, color = condition)) +
                stat_summary(fun.y = "mean", geom ="line", alpha = 1, size = 1, 
                  aes(color = condition)) +
                facet_wrap(~ channel) + 
                theme(legend.position = "bottom") 


## ---- fig.dim = c(10,5), out.width = "100%"------------------------------
faces_segs_some %>% ungroup() %>%
                    filter(between(.sample_id,20,50)) %>% 
                    group_by(condition) %>%
                    plot_topo()

    

## ------------------------------------------------------------------------
df <- faces_segs_some %>% 
      select(O1, O2, P7, P8) %>%
      as_tibble %>%
        # We can use regular dplyr functions now
      group_by(channel, time) %>% 
      summarize(
        `t-value` = t.test(amplitude[condition == "faces"],
          amplitude[condition == "non-faces"])$statistic)

df

## ---- fig.dim = c(10,15), out.width = "100%"-----------------------------
ggplot(df, aes(x = time, y = `t-value`)) + geom_line() +
                facet_wrap(~ channel) 

## ---- fig.dim = c(10,15), out.width = "100%"-----------------------------
faces_segs_some_t <- 
  faces_segs_some %>% 
      select(O1, O2, P7, P8) %>%
      group_by(.sample_id) %>% 
      summarize_all(funs(t = t.test(.[condition == "faces"],
          .[condition == "non-faces"])$statistic))

  plot_gg(faces_segs_some_t) + 
                geom_line(alpha = .1, aes(group = .id)) +
                stat_summary(fun.y = "mean", geom ="line", alpha = 1, size = 1) +
                facet_wrap(~ channel) + 
                theme(legend.position = "bottom") 


