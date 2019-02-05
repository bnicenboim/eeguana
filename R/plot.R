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
    theme_eeguana_empty

  # if (length(grouping_vars) > 0) {
  #   plot <- plot + facet_wrap(grouping_vars)
  # }

  plot

}

#' @export
plot_in_layout <- function(plot,  ...) {
    UseMethod("plot_in_layout")
}

#' @export
plot_in_layout.gg <- function(plot, projection = "polar", size = 1, ...) {
    plot_grob <- ggplot2::ggplotGrob(plot)
    plot <- plot + facet_wrap(~ channel)

    #detect the number of panels
    ## n_panels <- length(unique(ggplot_build(plot)$data[[1]]$PANEL))
    ## dim_facets <- ggplot2::wrap_dims(n_panels)

    # channels <- df$channel %>% levels() #not too good, below a better alternative:
    # facet_names <- matrix(c(channels,rep(NA,dim_facets[[1]]* dim_facets[[2]] - length(channels))),
    #   nrow =dim_facets[[1]], ncol = dim_facets[[2]], byrow = TRUE )

    layout <- ggplot2::ggplot_build(plot)$layout$layout

    ## PANEL ROW COL channel SCALE_X SCALE_Y
    ## 1      1   1   1     Fp1       1       1
    ## 2      2   1   2     Fpz       1       1
    ## 3      3   1   3     Fp2       1       1
    ## 4      4   1   4      F7       1       1
    ## 5      5   1   5      F3       1       1
    ## 6      6   1   6      Fz       1       1
    ## 7      7   2   1      F4       1       1

    channel_grobs <- purrr::map(layout$channels, function(ch){
        ## pos <- which(facet_names==ch, arr.ind =  TRUE)
        panel_txt <- paste0("panel-",layout$ROW,"-",layout$COL,"|strip-t-",layout$COL,"-",layout$ROW)
        g_filter(plot_grob,panel_txt)
    }) %>% setNames(layout$channel)

    #Discard facet panels from the original plot:
    rest_grobs <- g_filter_out(plot_grob,"panel|strip-t|axis")

    # How much larger than the electrode position should the plot be?
    cmin <- -1 - 0.1 * size
    cmax <- 1 + 0.1 * size

    new_plot <- ggplot(data.frame(x =  c(cmin,cmax), y = c(cmin,cmax)), aes_(x = ~x, y = ~y)) +
        geom_blank() +
        scale_x_continuous(limits = c(cmin,cmax), expand = c(0, 0)) +
        scale_y_continuous(limits = c(cmin,cmax), expand = c(0, 0)) +
        theme_void() +
        annotation_custom(rest_grobs,
                          xmin =cmin,
                          xmax = cmax,
                          ymin = cmin,
                          ymax = cmax) 

    if  (stringr::str_to_lower(projection) == "orthographic"){
        project <- orthographic
    } else if(stringr::str_to_lower(projection) == "polar" ){
        project <- polar
    } else if (stringr::str_to_lower(projection) == "stereographic"){
        project <- stereographic
    }

    if(all(is.na(channels_tbl(.data)$.z)) & !identical(project,orthographic)){
        warning("Z coordinates are missing, using 'ortographic' projection ")
        project <- orthographic
    }

    for(i in seq_len(length(channel_grobs))) {
        location <- channels_tbl(.data) %>%
            dplyr::filter(channel == names(channel_grobs)[[i]])
        if(is.na(location$.x) && is.na(location$.y)){
            new_plot
        } else if (is.na(location$.z) & !identical(project,orthographic)){
            warning("Z coordinates are missing for electrode ", names(channel_grobs)[[i]])

        } else if (is.na(location$.x) | is.na(location$.y)){
             warning("X or Y coordinates are missing for electrode ", names(channel_grobs)[[i]])

        } else {
            new_coord <- project(location$.x, location$.y, location$.z) 
            new_plot<- new_plot +  annotation_custom(channel_grobs[[i]],
                                                     xmin = coord$x- .1 * scale,
                                                     xmax = coord$x + .1 * scale,
                                                     ymin = coord$y - .1 * scale,
                                                     ymax = coord$y + .1 * scale) 
        }
    }
    new_plot                               
}

#' Looks for grobs not matching a pattern
g_filter_out <-
    function (x, pattern) {
    matches <- grepl(pattern, x$layout$name, fixed = fixed)
    x$layout <- x$layout[!matches, , drop = FALSE]
    x$grobs <- x$grobs[!matches]
    x
}
#' Looks for grobs matching a pattern
g_filter <-
    function (x, pattern) {
        matches <- grepl(pattern, x$layout$name, fixed = fixed)
        x$layout <- x$layout[matches, , drop = FALSE]
        x$grobs <- x$grobs[matches]
        x
    }

#' stereographic projection over a 2d plane
stereographic <- function(x,y,z){
    mu <- 1 / (sqrt(x^2 + y^2 + z^2) + z)
    x <- x * mu
    y <- y * mu
    list(x = x,y = y)
}
#' polar projection over a 2d plane
polar <- function(x,y,z){
    az <- atan2(y, x)
    el <-  atan2(z, sqrt(x^2 + y^2))
    x <-  (pi/2 - el) * cos(az)
    y <- (pi/2 - el) * sin(az)
    list(x = x,y = y)
}
#' orthographic projection over a 2d plane
orthographic <- function(x,y,z){
    list(x = x,y = y)
}

