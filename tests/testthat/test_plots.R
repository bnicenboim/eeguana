context("test eeguana plotting functions")
library(eeguana)


# create fake dataset
data_1 <- eeg_lst(
    signal_tbl = dplyr::tibble(X = sin(1:30),
                               Y = sin(1:30),
    .id = rep(c(1L, 2L, 3L), each = 10),
    .sample = sample_int(rep(seq(-4L, 5L), times = 3), sampling_rate = 500)),
    channels_tbl =  dplyr::tibble(
      .channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = c(1, -10), .y = c(1, 1), .z = c(1, 10)
  ),
  events_tbl = dplyr::tribble(
    ~.id, ~.type, ~.description, ~.initial, ~.final, ~.channel,
    1L, "New Segment", NA_character_, -4L, -4L, NA,
    1L, "Bad", NA_character_, -2L, 0L, NA,
    1L, "Time 0", NA_character_, 1L, 1L, NA,
    1L, "Bad", NA_character_, 2L, 3L, "X",
    2L, "New Segment", NA_character_, -4L, -4L, NA,
    2L, "Time 0", NA_character_, 1L, 1L, NA,
    2L, "Bad", NA_character_, 2L, 2L, "Y",
    3L, "New Segment", NA_character_, -4L, -4L, NA,
    3L, "Time 0", NA_character_, 1L, 1L, NA,
    3L, "Bad", NA_character_, 2L, 2L, "Y"
  ),
  segments_tbl = dplyr::tibble(.id = c(1L, 2L, 3L),
                           .recording = "recording1",
                           segment = c(1L, 2L, 3L),
                           condition = c("a", "b", "a"))
)


data("data_faces_ERPs")
data("data_faces_10_trials")


# helper functions (borrowed from github.com/stan-dev/bayesplot/R/helpers-testthat.R)
expect_gg <- function(x) {
  testthat::expect_s3_class(x, "ggplot")
  invisible(ggplot_build(x))
}

# if eeguana plots were not classed as ggplot, could use something like this:
# expect_eeguanaplot <- function(x) testthat::expect_s3_class(x, "eeguanaplot")

##auto plot
plot <- plot(data_faces_ERPs)

# create line plot
lineplot_eeg <- data_faces_ERPs %>%
    ggplot(aes(x = .time, y=.value)) +
  geom_line(aes(group = .id, colour = condition), alpha = .5) +
  stat_summary(fun.y = "mean", geom = "line",
               aes(colour = condition), alpha = 1, size = 1) +
  # only .key works dynamically but only channel works with test()!
    facet_wrap(~ .key) +
  theme(legend.position = "bottom")

# create topo plot
topoplot_eeg <- data_faces_ERPs %>%
  group_by(condition) %>%
  summarize_at(channel_names(.), mean, na.rm = TRUE) %>%
  plot_topo() + 
  facet_grid(~ condition) +
  annotate_head() +
  geom_contour() +
  geom_text(colour = "black")

ica_plot <- data_faces_ERPs  %>% mutate(.recording =1) %>%
    eeg_ica(-M1,-M2, -EOGV,-EOGH) %>% plot_components()

test_that("plotting doesn't change data", {
  # channel is factor in the plot and character in tibble, is that ok?
  expect_equal(as.matrix(lineplot_eeg$data), as.matrix(as_tibble(data_faces_ERPs)))
  # lengths are very different 
  expect_equal(nrow(topoplot_eeg$data), 
               nrow(data_faces_ERPs %>%
                           group_by(condition) %>%
                           summarize_at(channel_names(.), mean, na.rm = TRUE) %>%
                         eeg_interpolate_tbl() %>%
                         filter(is.na(.key))))
})


test_that("plot functions create ggplots", {
  expect_gg(plot)
  expect_gg(lineplot_eeg)
  expect_gg(topoplot_eeg)
  expect_gg(ica_plot)
  data_shorter <- filter(data_faces_10_trials, between(as_time(.sample) , 91,93))
  expect_gg(plot(data_shorter) + annotate_events())

})


