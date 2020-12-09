library(eeguana)

library(httr)
GET("https://osf.io/wv5hp//?action=download",
  write_disk("./s1_faces.vhdr", overwrite = TRUE),
  progress()
)
GET("https://osf.io/c8wea//?action=download",
  write_disk("./s1_faces.vmrk", overwrite = TRUE),
  progress()
)
GET("https://osf.io/uhsde//?action=download",
  write_disk("./s1_faces.eeg", overwrite = TRUE),
  progress()
)

faces <- read_vhdr("s1_faces.vhdr")

data_faces_ERPs <- faces %>%
  eeg_segment(.description %in% c("s70", "s71"),
    .lim = c(-.2, .25)
  ) %>%
  eeg_events_to_NA(.type == "Bad Interval") %>%
  eeg_baseline() %>%
  mutate(
    condition =
      if_else(description == "s70", "faces", "non-faces")
  ) %>%
  select(-type) %>%
  group_by(.sample, condition, .recording) %>%
  summarize_at(channel_names(.), mean, na.rm = TRUE) %>%
  ungroup()

pos_10 <- events_tbl(faces) %>% filter(.type == "Stimulus", .description == "s130") %>% pull(.initial) %>% .[10]

data_faces_10_trials <- faces %>% filter(.sample %>% between(15000, pos_10)) %>% ungroup()

usethis::use_data(data_faces_ERPs, data_faces_10_trials, overwrite = TRUE)
