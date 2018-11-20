#' Simple plot an eeg_lst object.
#' @param x An `eeg_lst` object.
#' @param max_sample Downsample to approximately 2000 samples by default.
#'
#' @family plot
#' 
#' @return A ggplot object
#'
#' @importFrom magrittr %>%
#'
#' @export
plot.eeg_lst <- function(x, max_sample = 64000) {
  if (is.numeric(max_sample) & max_sample != 0 &
    # it will downsample if the samples are at least twice as large than the max_sample
    max(duration(x)) * sampling_rate(x) * 2 > max_sample) {
    x <- downsample(x, max_sample = max_sample)
  }

  df <- as_tibble(x)
  plot <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = time, y = amplitude, group = .id)
  ) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(channel ~ .,
      labeller = ggplot2::label_wrap_gen(multi_line = FALSE)
    ) +
    ggplot2::scale_y_reverse() +
    theme_eeguana
  plot
}

#' ggplot object based on an eeg_lst object.
#' @param x An `eeg_lst` object.
#' @param max_sample Downsample to approximately 2000 samples by default.
#'
#' @family plot
#' @return A ggplot object
#'
#' @importFrom magrittr %>%
#'
#' @export
plot_gg <- function(.data, ...) {
  UseMethod("plot_gg")
}

#' @export
plot_gg.eeg_lst <- function(.data, x = time, y = amplitude, ..., max_sample = 2000) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  if (!all(is.na(.data$signal$.sample_id)) && is.numeric(max_sample) && max_sample != 0 &&
    # it will downsample if the samples are at least twice as large than the max_sample
    max(duration(.data)) * sampling_rate(.data) * 2 > max_sample) {
    .data <- downsample(.data, max_sample = max_sample)
  }

  dots <- rlang::enquos(...)
  df <- dplyr::as_tibble(.data)
  plot <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = !!x, y = !!y, !!!dots)
  ) +
    ggplot2::scale_colour_brewer(type = "qual", palette = "Dark2") +
    theme_eeguana
  plot
}

#' @export
plot_gg.tbl_df <- function(.data, x = x, y = y,  ...) {
  dots <- rlang::enquos(...)
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  plot <- ggplot2::ggplot(
    .data,
    ggplot2::aes(x = !!x, y = !!y, !!!dots)
  ) +
    ggplot2::scale_colour_brewer(type = "qual", palette = "Dark2") +
    theme_eeguana
  plot
}


#' A topographic plot of an eeg_lst object.
#'
#' Create a default topographic plot based on the segments of the `eeg_lst` object.
#'
#' The following methods of interpolation are available :
#'
#' * `"MBA"` (Default) Multilevel B-splines using the function `mba.surf`; requires the package `MBA`.

#'
#' @param x An `eeg_lst` object.
#' @param method Method of interpolation.
#' @param ... Various arguments passed to the interpolation method.
#'
#' @family plot
#'
#' @return A ggplot object
#'
#'
#' @export
plot_topo <- function(data,  ...) {
  UseMethod("plot_topo")
}

#' @export
plot_topo.tbl_df <- function(data, x = .x, y =.y, value= amplitude,  label=channel) {

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  value <- rlang::enquo(value)
  label <- rlang::enquo(label)


  # grid <- interpolate_xy(s_x, x = x, y = y, value = A, method = "MBA", ...)

  plot <- filter(data, !is.na(!!x), !is.na(!!y), is.na(!!label)) %>%
    ggplot(aes(x=!!x, y=!!y)) +
    geom_raster(aes(fill = !!value), interpolate = TRUE, hjust = 0.5, vjust = 0.5) +
    geom_contour(aes(z = !!value)) +
    geom_text(data = filter(data, !is.na(!!x), !is.na(!!y), !is.na(!!label)), aes(x = !!x, y = !!y, label = !!label), colour = "black") +
    # scale_fill_distiller(palette = "Spectral", guide = "colourbar", oob = scales::squish) + #, oob = scales::squish
    scale_fill_gradientn(
      colours = c("darkred", "yellow", "green", "darkblue"),
      values = c(1.0, 0.75, 0.5, 0.25, 0)
    ) +
    theme_eeguana

  # if (length(grouping_vars) > 0) {
  #   plot <- plot + facet_wrap(grouping_vars)
  # }

  plot

}

#' @export
plot_topo.eeg_lst <- function(data, x = .x, y =.y, value= amplitude,  label=channel) {

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  value <- rlang::enquo(value)
  label <- rlang::enquo(label)


  # grid <- interpolate_xy(s_x, x = x, y = y, value = A, method = "MBA", ...)

  plot <- data %>%
    ggplot(aes(x=!!x, y=!!y)) +
    geom_raster(aes(fill = !!value), interpolate = F, hjust = 0.5, vjust = 0.5) +
    geom_contour(aes(z = !!value)) +
    geom_text(data = filter(data, !is.na(!!x), !is.na(!!y)), aes(x = !!x, y = !!y, label = !!label), colour = "black") +
    # scale_fill_distiller(palette = "Spectral", guide = "colourbar", oob = scales::squish) + #, oob = scales::squish
    scale_fill_gradientn(
      colours = c("darkred", "yellow", "green", "darkblue"),
      values = c(1.0, 0.75, 0.5, 0.25, 0)
    ) +
    theme_eeguana

  # if (length(grouping_vars) > 0) {
  #   plot <- plot + facet_wrap(grouping_vars)
  # }

  plot
  # scale_fill_distiller(palette = "RdBu", guide = "colourbar") + #, oob = scales::squish
  # scale_fill_viridis_c(option="B") +
}
