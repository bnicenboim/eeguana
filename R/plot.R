#' Simple plot an eeg_lst object.
#' @param x An `eeg_lst` object.
#' @param max_sample Downsample to approximately 64000 samples by default.
#' @param ... Not in use.
#' @family plot
#' 
#' @return A ggplot object
#'
#' @importFrom magrittr %>%
#'
#' @export
plot.eeg_lst <- function(x, max_sample = 64000, ...) {
  if (is.numeric(max_sample) & max_sample != 0 &
    # it will downsample if the samples are at least twice as large than the max_sample
    max(duration(x)) * sampling_rate(x) * 2 > max_sample) {
    x <- downsample(x, max_sample = max_sample)
  }

  df <- dplyr::as_tibble(x) %>% 
        dplyr::mutate(channel = factor(channel, levels = unique(channel)))
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
#' @param .data An `eeg_lst` object.
#' @inheritParams  ggplot2::aes
#' @param max_sample Downsample to approximately 64000 samples by default.
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
#' @rdname plot_gg
#' @export
plot_gg.eeg_lst <- function(.data, x = time, y = amplitude, ..., max_sample = 64000) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  if (!all(is.na(.data$signal$.sample_id)) && is.numeric(max_sample) && max_sample != 0 &&
    # it will downsample if the samples are at least twice as large than the max_sample
    max(duration(.data)) * sampling_rate(.data) * 2 > max_sample) {
    .data <- downsample(.data, max_sample = max_sample)
  }

  dots <- rlang::enquos(...)
  df <- dplyr::as_tibble(.data) %>% 
        dplyr::mutate(channel = factor(channel, levels = unique(channel)))

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


#' A topographic plot.
#'
#' Create a default topographic plot based on an interpolation table.
#'
#'
#' @param data A table of interpolated electrodes as produced by [interpolate_tbl]
#' @param ... Others.
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

#' @param x x
#' @param y y
#' @param value value 
#' @param label label, generally channel

#' @rdname plot_topo
#' @export
plot_topo.tbl_df <- function(data, x = .x, y =.y, value= amplitude,  label=channel, ...) {

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  value <- rlang::enquo(value)
  label <- rlang::enquo(label)


  # grid <- interpolate_xy(s_x, x = x, y = y, value = A, method = "MBA", ...)

  plot <- dplyr::filter(data, !is.na(!!x), !is.na(!!y), is.na(!!label)) %>%
    ggplot2::ggplot(ggplot2::aes(x=!!x, y=!!y)) +
    ggplot2::geom_raster(ggplot2::aes(fill = !!value), interpolate = TRUE, hjust = 0.5, vjust = 0.5) +
    ggplot2::geom_contour(ggplot2::aes(z = !!value)) +
    ggplot2::geom_text(data = dplyr::filter(data, !is.na(!!x), !is.na(!!y), !is.na(!!label)), ggplot2::aes(x = !!x, y = !!y, label = !!label), colour = "black") +
    # scale_fill_distiller(palette = "Spectral", guide = "colourbar", oob = scales::squish) + #, oob = scales::squish
    ggplot2::scale_fill_gradientn(
      colours = c("darkred", "yellow", "green", "darkblue"),
      values = c(1.0, 0.75, 0.5, 0.25, 0)
    ) +
    theme_eeguana

  # if (length(grouping_vars) > 0) {
  #   plot <- plot + facet_wrap(grouping_vars)
  # }

  plot

}
#' 
#' #' @export
#' plot_topo.eeg_lst <- function(data, x = .x, y =.y, value= amplitude,  label=channel) {
#' 
#'   x <- rlang::enquo(x)
#'   y <- rlang::enquo(y)
#'   value <- rlang::enquo(value)
#'   label <- rlang::enquo(label)
#' 
#' 
#'   # grid <- interpolate_xy(s_x, x = x, y = y, value = A, method = "MBA", ...)
#' 
#'   plot <- data %>%
#'     ggplot2::ggplot(ggplot2::aes(x=!!x, y=!!y)) +
#'     ggplot2::geom_raster(ggplot2::aes(fill = !!value), interpolate = F, hjust = 0.5, vjust = 0.5) +
#'     ggplot2::geom_contour(ggplot2::aes(z = !!value)) +
#'     ggplot2::geom_text(data = dplyr::filter(data, !is.na(!!x), !is.na(!!y)), ggplot2::aes(x = !!x, y = !!y, label = !!label), colour = "black") +
#'     # scale_fill_distiller(palette = "Spectral", guide = "colourbar", oob = scales::squish) + #, oob = scales::squish
#'     ggplot2::scale_fill_gradientn(
#'       colours = c("darkred", "yellow", "green", "darkblue"),
#'       values = c(1.0, 0.75, 0.5, 0.25, 0)
#'     ) +
#'     theme_eeguana
#' 
#'   # if (length(grouping_vars) > 0) {
#'   #   plot <- plot + facet_wrap(grouping_vars)
#'   # }
#' 
#'   plot
#'   # scale_fill_distiller(palette = "RdBu", guide = "colourbar") + #, oob = scales::squish
#'   # scale_fill_viridis_c(option="B") +
#' }
