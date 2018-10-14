# group_by_.eegbl<- function(.data, ..., add = add) {
#   dots <- list(...)$.dots
#   # dots is a lazy_dots, that looks like this:
#   # [[1]]
#   # <lazy>
#   #   expr: recording
#   # env:  <environment: R_GlobalEnv>

#   #   [[2]]
#   # <lazy>
#   #   expr: description.y
#   # env:  <environment: R_GlobalEnv>

#   #   [[3]]
#   # <lazy>
#   #   expr: sample
#   # env:  <environment: R_GlobalEnv>

#   #   attr(,"class")
#   # [1] "lazy_dots"

#   # Make a list of names from the NSE variables
#   all_vars <- purrr::map_chr(dots, ~  rlang::quo_name(.x$expr))
#   # Divide the variables into the relevant columns
#   signal_col <- intersect(all_vars, colnames(.data$signal))
#   segments_col <-intersect(all_vars, colnames(.data$segments))
#   extra <- setdiff(union(signal_col, segments_col), all_vars)

#   if(length(extra)>0){
#    warning(paste("The following grouping variables were not found: ",
#      paste(extra, collapse = ", ") ))
#   }
#   .data$signal <- dplyr::group_by_at(.data$signal, vars(signal_col), add = add)
#   .data$segments <- dplyr::group_by_at(.data$segments, vars(segments_col), add = add)
#   update_chans(.data) %>%   validate_eegbl
# }



# #' Plots a summary based on a statistics.
# #'
# #' @param x An \code{eegble} object.
# #' @param .funs A statistics to be used on every segment
# #' @param ... Other arguments passed on to \code{.funs}. See \link{dplyr-package} help.
# #'
# #' @return A ggplot.
# #'
# #'
# #' @export
# #'
# plot_segment_summary <- function(x, .funs = mean, ...){
#   funs_name <- rlang::enquo(.funs)
#   seg_sum <- as_segment_summary(x, .funs = .funs, ...)

#   # Field triplike plots
#   # plot_down <- ggplot2::ggplot(seg_sum, aes(x= segment, y = .funs)) + ggplot2::geom_point()
#   # plot_right <- ggplot2::ggplot(seg_sum, aes(x= .funs, y = channel)) + ggplot2::geom_point()
#   # plot_center <- ggplot2::ggplot(seg_sum, aes(x= segment, y = channel, fill = .funs)) + ggplot2::geom_raster() +
#   #     ggplot2::scale_colour_brewer(type = "qual", palette = "Dark2") +
#   #     ggplot2::theme_bw()
#   # # plot_center
#   # cowplot::plot_grid(plot_center, plot_right, plot_down, ncol = 2)
#   ggplot2::ggplot(seg_sum, aes(x= channel, y = .funs, label = segment)) + ggplot2::geom_jitter() +
#       # ggrepel::geom_text_repel() +
#       ggplot2::scale_colour_brewer(type = "qual", palette = "Dark2") +
#       ggplot2::theme_bw()

# }


#' Mutate/transmute/select/rename channels or segments.
#'
#' Manipulate the signal and the segments of an eegble.
#'
#' Wrappers for \link{dplyr}'s commands for eegble objects. These commands always return the entire eegble so that they can be
#' piped using \link{magrittr}'s pipe, %>%.
#'
#' Manipulation type:
#' \itemize{
#' \item \code{mutate_*()} adds new variables and preserves existing..
#' \item \code{transmute_*()} drops existing variables.
#' \item \code{select_*()} keeps only the mentioned variables.
#' \item \code{rename_*()}: keeps all variables.
#' \item \code{filter_*()}: find segments/samples where conditions are true. Segments/samples where the condition evaluates to NA are dropped.
#' \item \code{left_join_*()}: Left-joins an external dataframe to one of the dataframes of the eegble.
#' \item \code{semi_join_*()}: Semi-joins an external dataframe to one of the dataframes of the eegble and updates the remaining dataframes correspondingly.
#' \item \code{anti_join_*()}: Anti-joins an external dataframe to one of the dataframes of the eegble and updates the remaining dataframes correspondingly.


#' }
#' Manipulation effect:
#' \itemize{
#' \item \code{*_signal()} affects the channels of the data and adapt the `chan_info` data frame.
#' \item \code{*_segments()} affect the segment information of the `seg_info` data frame.
#' \item \code{*_events()} affect the event information of the `events` data frame.
#' }
#'
#' See also \link{dplyr-package}.
#' @param .data An eegbl.
#' @param ... Name-value pairs of expressions, for `_chan` variations they will include channel names, for `_segments` variations they will include segments descriptors.
#' @return An eegble object.
#'
mutate_signal <- function(.data, ...) {
  dots <- validate_dots(...)
  .data$data <- dplyr::mutate(.data$data, !!!dots)
  update_signals(.data) %>% validate_eegbl()
}

#' @rdname mutate_signal
transmute_signal <- function(.data, ...) {
  dots <- validate_dots(...)
  .data$data <- dplyr::transmute(.data$data, sample, !!!dots)
  update_signals(.data) %>% validate_eegbl()
}


#' @rdname mutate_signal
select_signal <- function(.data, ...) {
  dots <- validate_dots(...)
  .data$data <- dplyr::group_by(.data$data, .id, sample) %>%
    dplyr::select(!!!dots) %>%
    ungroup()
  update_signals(.data) %>% validate_eegbl()
}

#' @rdname mutate_signal
rename_signal <- function(.data, ...) {
  dots <- validate_dots(...)
  .data$data <- dplyr::rename(.data$data, !!!dots)
  update_signals(.data) %>% validate_eegbl()
}


#' @rdname mutate_signal
mutate_segments <- function(.data, ...) {
  dots <- validate_dots(...)
  .data$seg_info <- dplyr::mutate(.data$seg_info, !!!dots)
  validate_eegbl(.data)
}

#' @rdname mutate_signal
transmute_segments <- function(.data, ...) {
  dots <- validate_dots(...)
  .data$seg_info <- dplyr::mutate(.data$seg_info, .id, !!!dots)
  validate_eegbl(.data)
}

#' @rdname mutate_signal
rename_segments <- function(.data, ...) {
  dots <- validate_dots(...)
  .data$seg_info <- dplyr::rename(.data$seg_info, !!!dots)
  validate_eegbl(.data)
}

#' @rdname mutate_signal
select_segments <- function(.data, ...) {
  dots <- validate_dots(...)
  .data$seg_info <- dplyr::group_by(.data$seg_info, .id, sample) %>%
    dplyr::select(!!!dots) %>%
    ungroup()
  validate_eegbl(.data)
}

#' @rdname mutate_signal
filter_segments <- function(.data, ...) {
  dots <- rlang::enquos(...)
  .data$seg_info <- dplyr::filter(.data$seg_info, !!!dots)
  .data$data <- dplyr::semi_join(.data$data, .data$seg_info, by = ".id")
  .data$events <- dplyr::semi_join(.data$events, .data$seg_info, by = ".id")

  validate_eegbl(.data)
}

#' @rdname mutate_signal
left_join_segments <- function(x, y, by = NULL, suffix = c(".x", ".y"), ...) {
  if (!is_eegble(x)) {
    stop("x must be an eegble.")
  }
  dplyr::left_join(x$seg_info, y, by = NULL, suffix = c(".x", ".y"), ...)
}

#' @rdname mutate_signal
left_join_events <- function(x, y, by = NULL, suffix = c(".x", ".y"), ...) {
  if (!is_eegble(x)) {
    stop("x must be an eegble.")
  }
  dplyr::left_join(x$events, y, by = NULL, suffix = c(".x", ".y"), ...)
}

#' @rdname mutate_signal
semi_join_segments <- function(x, y, by = NULL, suffix = c(".x", ".y"), ...) {
  if (!is_eegble(x)) stop("x must be an eegble.")
  x$segments <- dplyr::semi_join(x$segments, y, by = NULL, suffix = c(".x", ".y"), ...)
  x$signal <- dplyr::semi_join(x$signal, x$segments, by = ".id")
  x$events <- dplyr::semi_join(x$events, x$segments, by = ".id")

  validate_eegbl(x)
}

#' @rdname mutate_signal
semi_join_events <- function(x, y, by = NULL, suffix = c(".x", ".y"), ...) {
  if (!is_eegble(x)) stop("x must be an eegble.")
  x$events <- dplyr::semi_join(x$events, y, by = NULL, suffix = c(".x", ".y"), ...)
  x$signal <- dplyr::semi_join(x$signal, x$events, by = ".id")
  x$segments <- dplyr::semi_join(x$segments, x$events, by = ".id")

  validate_eegbl(x)
}

#' @rdname mutate_signal
anti_join_segments <- function(x, y, by = NULL, suffix = c(".x", ".y"), ...) {
  if (!is_eegble(x)) stop("x must be an eegble.")
  x$segments <- dplyr::anti_join(x$segments, y, by = NULL, suffix = c(".x", ".y"), ...)
  x$signal <- dplyr::semi_join(x$signal, x$segments, by = ".id")
  x$events <- dplyr::semi_join(x$events, x$segments, by = ".id")

  validate_eegbl(x)
}

#' @rdname mutate_signal
anti_join_events <- function(x, y, by = NULL, suffix = c(".x", ".y"), ...) {
  if (!is_eegble(x)) stop("x must be an eegble.")
  x$events <- dplyr::anti_join(x$events, y, by = NULL, suffix = c(".x", ".y"), ...)
  x$signal <- dplyr::semi_join(x$signal, x$events, by = ".id")
  x$segments <- dplyr::semi_join(x$segments, x$events, by = ".id")

  validate_eegbl(x)
}
