context("test eeguana plotting functions")
library(eeguana)


# create fake dataset
data_1 <- eeg_lst(
  signal = signal_tbl(
    signal_matrix = as.matrix(
      data.frame(X = sin(1:30), Y = sin(1:30))
    ),
    ids = rep(c(1L, 2L, 3L), each = 10),
    sample_ids = sample_int(rep(seq(-4L, 5L), times = 3), sampling_rate = 500),
    dplyr::tibble(
      channel = c("X", "Y"), .reference = NA, theta = NA, phi = NA,
      radius = NA, .x = c(1, -10), .y = c(1, 1), .z = c(1, 10)
    )
  ),
  events = as_events_tbl(dplyr::tribble(
    ~.id, ~type, ~description, ~.sample_0, ~.size, ~.channel,
    1L, "New Segment", NA_character_, -4L, 1L, NA,
    1L, "Bad", NA_character_, -2L, 3L, NA,
    1L, "Time 0", NA_character_, 1L, 1L, NA,
    1L, "Bad", NA_character_, 2L, 2L, "X",
    2L, "New Segment", NA_character_, -4L, 1L, NA,
    2L, "Time 0", NA_character_, 1L, 1L, NA,
    2L, "Bad", NA_character_, 2L, 1L, "Y",
    3L, "New Segment", NA_character_, -4L, 1L, NA,
    3L, "Time 0", NA_character_, 1L, 1L, NA,
    3L, "Bad", NA_character_, 2L, 1L, "Y"
  )),
  segments = dplyr::tibble(.id = c(1L, 2L, 3L),
                           recording = "recording1",
                           segment = c(1L, 2L, 3L),
                           condition = c("a", "b", "a"))
)


data("data_faces_ERPs")


# helper functions (borrowed from github.com/stan-dev/bayesplot/R/helpers-testthat.R)
expect_gg <- function(x) {
  testthat::expect_s3_class(x, "ggplot")
  invisible(ggplot_build(x))
}

# if eeguana plots were not classed as ggplot, could use something like this:
# expect_eeguanaplot <- function(x) testthat::expect_s3_class(x, "eeguanaplot")


test_that("plotting functions don't throw errors", {
  
  # line plot
    expect_silent(data_faces_ERPs %>%
                  plot_gg() +
                  geom_line(aes(group = .id, colour = condition), alpha = .5) +
                  stat_summary(fun.y = "mean", geom = "line",
                               aes(colour = condition), alpha = 1, size = 1) +
                  facet_wrap(~ .source) +
                  theme(legend.position = "bottom")
                ) 
  
  # topo plot
    expect_silent(data_faces_ERPs %>%
                  group_by(condition) %>%
                  summarise_all_ch(mean, na.rm = TRUE) %>%
                  plot_topo() + 
                  facet_grid(~ condition) +
                  annotate_head() +
                  geom_contour() +
                  geom_text(colour = "black")
                )
})


# create line plot
lineplot_eeg <- data_faces_ERPs %>%
  plot_gg() +
  geom_line(aes(group = .id, colour = condition), alpha = .5) +
  stat_summary(fun.y = "mean", geom = "line",
               aes(colour = condition), alpha = 1, size = 1) +
  # only .source works dynamically but only channel works with test()!
    facet_wrap(~ .source) +
  theme(legend.position = "bottom")

# create topo plot
topoplot_eeg <- data_faces_ERPs %>%
  group_by(condition) %>%
  summarise_all_ch(mean, na.rm = TRUE) %>%
  plot_topo() + 
  facet_grid(~ condition) +
  annotate_head() +
  geom_contour() +
  geom_text(colour = "black")

test_that("plotting doesn't change data", {
  # channel is factor in the plot and character in tibble, is that ok?
  expect_equal(as.matrix(lineplot_eeg$data), as.matrix(as_tibble(data_faces_ERPs)))
  # lengths are very different 
  expect_equal(nrow(topoplot_eeg$data), 
               nrow(data_faces_ERPs %>%
                           group_by(condition) %>%
                           summarise_all_ch(mean, na.rm = TRUE) %>%
                         eeg_interpolate_tbl() %>%
                         filter(is.na(.source))))
})

test_that("warnings", {
 expect_warning(data_1 %>%
    group_by(condition) %>%
    summarise_all_ch(mean, na.rm = TRUE) %>%
     plot_topo(projection = "orthographic"))
})

test_that("plot functions create ggplots", {
  expect_gg(lineplot_eeg)
  expect_gg(topoplot_eeg)
})




