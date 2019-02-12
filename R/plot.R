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
#' @param data A table of interpolated electrodes as produced by [interpolate_tbl], or an eeg_lst appropiately grouped. 
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
#' @param value value 
#' @param label label, generally channel
#' @rdname plot_topo
#' @export
plot_topo.tbl_df <- function(data, value= amplitude,  label=channel, ...) {

  value <- rlang::enquo(value)
  label <- rlang::enquo(label)
  
  # Labels positions mess up with geom_raster, they need to be excluded 
  # and then add the labels to the data that was interpolated
  d <- dplyr::filter(data, !is.na(.x), !is.na(.y), is.na(!!label)) %>%
    dplyr::select(-!!label)
  label_pos <- dplyr::filter(data, !is.na(.x), !is.na(.y), !is.na(!!label)) %>% 
        dplyr::distinct(.x,.y, !!label)
  label_corrected_pos <- purrr::map_df(label_pos %>% dplyr::select(.x,.y,!!label) %>% purrr::transpose(), function(l){ 
    d %>% dplyr::select(-!!value) %>%    
      dplyr::filter((.x - l$.x)^2 + (.y - l$.y)^2 == min((.x - l$.x)^2 + (.y - l$.y)^2) )  %>%
      # does the original grouping so that I add a label to each group
      dplyr::group_by_at(vars(colnames(.)[!colnames(.) %in% c(".x",".y")]) ) %>%
      slice(1) %>%
      dplyr::mutate(!!label := l[[3]])
            }
                )
  d <- suppressMessages(dplyr::left_join(d, label_corrected_pos))
  
  
  #remove all the AES from the geoms, to remove later the geoms
  #see if geom_text can work with NA or something
  plot <- 
    ggplot2::ggplot(d, ggplot2::aes(x = .x, y = .y, 
                                 fill = !!value, z = !!value, label =  dplyr::if_else(!is.na(!!label), !!label, ""))) +
    ggplot2::geom_raster(interpolate = TRUE, hjust = 0.5, vjust = 0.5)  +
    # Non recommended "rainbow" Matlab palette from https://www.mattcraddock.com/blog/2017/02/25/erp-visualization-creating-topographical-scalp-maps-part-1/
    #    scale_fill_gradientn(colours = colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")),guide = "colourbar",oob = scales::squish)+ 
    # Not that bad scale:
    #    scale_fill_distiller(palette = "Spectral", guide = "colourbar", oob = scales::squish) + #
    scale_fill_distiller(type = "div",palette = "RdBu",guide = "colourbar",  oob = scales::squish) +
    theme_eeguana_empty
  plot

}

#' @inheritParams plot_in_layout
#' @param ... passed to the interpolation method
#' @rdname plot_topo
#' @export
plot_topo.eeg_lst <- function(data, size= 1.2, value= amplitude,  label=channel, projection = "polar", ...) {
  
  amplitude <- rlang::enquo(value)
  channel <- rlang::enquo(label)
  channels_tbl(data)  <- change_coord(channels_tbl(data), projection) 
  interpolate_tbl(data, size,...) %>%
    plot_topo(value= amplitude,  label=channel,...)
  }

#' Place channels in a layout.
#'
#' Reorganizes a ggplot so that the plot of each channel is in their correct position in the scalp.
#'
#' @param plot A ggplot object with channels
#'
#' @param ... 
#'
#' @family plot
#' @return A ggplot object
#' 
#' @export
plot_in_layout <- function(plot,  ...) {
    UseMethod("plot_in_layout")
}


#' @param projection "polar" (default), "orthographic", or  "stereographic"
#' @param ratio Ratio
#' @param ... 
#'
#' @rdname plot_in_layout
#' @export
plot_in_layout.gg <- function(plot, projection = "polar", ratio = c(1,1), ...) {
  size_x <- ratio[[1]]
  size_y <- ratio[[2]]
  eeg_data <- plot$data
  if (!"channel" %in% colnames(eeg_data)) {
    stop("Channels are missing from the data.")
  }
  if (!all(c(".x", ".y", ".z") %in% colnames(eeg_data))) {
    stop("Coordinates are missing from the data.")
  }
  plot <- plot  + ggplot2::facet_wrap(.~channel)
  plot_grob <- ggplot2::ggplotGrob(plot)
  layout <- ggplot2::ggplot_build(plot)$layout$layout

  ## PANEL ROW COL channel SCALE_X SCALE_Y
  ## 1      1   1   1     Fp1       1       1
  ## 2      2   1   2     Fpz       1       1
  ## 3      3   1   3     Fp2       1       1
  ## 4      4   1   4      F7       1       1
  ## 5      5   1   5      F3       1       1
  ## 6      6   1   6      Fz       1       1
  ## 7      7   2   1      F4       1       1

  # The facet in the bottom left has both axis, I'll extract and use everywhere:
  maxrow <- max(layout$ROW) # bottom
  # first I extract the axis and I fill the grob with it.
  axisl <- g_filter(plot_grob, paste0("axis-l-", maxrow, "-1"))
  axisb <- g_filter(plot_grob, paste0("axis-b-1-", maxrow))

  # # then I also extract the labels, which I'll use for each facet
  axes_labels <- g_filter(plot_grob, ".lab-.")

  # This the complete facet with axis
  panel_txt <- paste0("panel-", maxrow, "-1")
  strip_txt <- paste0("strip-t-1-", maxrow)
  axisl_txt <- paste0("axis-l-", maxrow, "-1")
  axisb_txt <- paste0("axis-b-1-", maxrow)
  pattern_txt <- paste0(c(panel_txt, strip_txt, axisl_txt, axisb_txt), collapse = "|")
  full_facet_grob <- g_filter(plot_grob, pattern_txt, trim = TRUE)

  rowsize <- full_facet_grob$heights[3] # bottom
  colsize <- full_facet_grob$widths[1] # left
  
  #THESE ARE NOT IN ORDER!!!
  panels <- subset(plot_grob$layout, grepl("panel", plot_grob$layout$name)) %>%
    dplyr::arrange(b,l)
  strips <- subset(plot_grob$layout, grepl("strip", plot_grob$layout$name))%>%
    dplyr::arrange(b,l)
  
  # won't work for free scales, need to add an if-else inside
  
  channel_grobs <- purrr::map(layout$channel, function(ch) {
    ## pos <- which(facet_names==ch, arr.ind =  TRUE)
    ch_pos <- layout %>% dplyr::filter(channel == ch)
    # panel_txt <- paste0("panel-", ch_pos$ROW, "-", ch_pos$COL)
    # strip_txt <- paste0("strip-t-", ch_pos$COL, "-", ch_pos$ROW)
    # axisl_txt <- paste0("axis-l-", ch_pos$ROW, "-", ch_pos$COL)
    # axisb_txt <- paste0("axis-b-", ch_pos$COL, "-", ch_pos$ROW)
    # # pattern_txt <- paste0(c(panel_txt,strip_txt,axisl_txt,axisb_txt), collapse = "|")
    # pattern_txt <- paste0(c(panel_txt, strip_txt), collapse = "|")
    # # plot_grob[[1]][[which(plot_grob$layout$name == axisl_txt)]] <- axisl[[1]][[1]]
    # # plot_grob[[1]][[which(plot_grob$layout$name == axisb_txt)]] <- axisb[[1]][[1]]
    pattern_txt <- paste0(panels[ch_pos$PANEL,]$name,"|",strips[ch_pos$PANEL,]$name)
    ch_grob <- g_filter(plot_grob, pattern_txt, trim = TRUE) %>%
      gtable::gtable_add_rows(rowsize) %>%
      gtable::gtable_add_grob(axisb[[1]][[1]], 3, 1) %>%
      gtable::gtable_add_cols(colsize, 0) %>%
      gtable::gtable_add_grob(axisl[[1]][[1]], 2, 1)

    #  #if there is no bottom axis, add one:
    #  if(is.null(g_filter(ch_grob,"axis-b")[[1]][[1]]$height)){
    #    ch_grob <- ch_grob %>%
    #     gtable::gtable_add_grob( axisb[[1]][[1]],3,2) %>%
    #     gtable::gtable_add_rows(rowsize)
    #  }
    #  if(is.null(g_filter(ch_grob,"axis-l")[[1]][[1]]$width)){
    #    ch_grob <- ch_grob %>% gtable::gtable_add_grob(axisl[[1]][[1]],2,1) %>%
    #    gtable::gtable_add_cols(colsize,0)
    # }

    ch_grob
  }) %>% setNames(layout$channel)
  # #gtable::gtable_height(ch_grob)
  # grid::heightDetails(ch_grob)
  # grid::heightDetails(ch_grob)
  # ch_grob$widths
  #
  # # grid::heightDetails()
    # grid::grid.newpage()
    # grid::grid.draw(channel_grobs[[4]])
    # grid::grid.draw(ch_grob)
   # 
  # Discard facet panels from the original plot:
  rest_grobs <- g_filter_out(plot_grob, "panel|strip-t|axis|xlab|ylab", trim = FALSE)

  # How much larger than the electrode position should the plot be?
 

  eeg_data <- change_coord(eeg_data, projection)

  xmin <- min(eeg_data$.x,na.rm=TRUE) - 0.3 #* size
  xmax <- max(eeg_data$.x,na.rm=TRUE) + 0.3 #* size
  ymin <- min(eeg_data$.y,na.rm=TRUE) - 0.3 #* size
  ymax <- max(eeg_data$.y,na.rm=TRUE) + 0.3 #* size
  new_plot <- ggplot(data.frame(x = c(xmin, xmax), y = c(ymin, ymax)), aes_(x = ~x, y = ~y)) +
    geom_blank() +
    scale_x_continuous(limits = c(xmin, xmax), expand = c(0, 0)) +
    scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0)) +
    theme_void() +
    annotation_custom(rest_grobs,
                      xmin = xmin,
                      xmax = xmax,
                      ymin = ymin,
                      ymax = ymax
    )
  
  for (i in seq_len(length(channel_grobs))) {
    new_coord <- eeg_data %>%
      dplyr::filter(channel == names(channel_grobs)[[i]]) %>%
      dplyr::distinct(.x, .y)
    if(is.na(new_coord$.x) && is.na(new_coord$.y)){
      new_plot
    
    } else if (is.na(new_coord$.x) | is.na(new_coord$.y)){
      warning("X or Y coordinates are missing for electrode ", names(channel_grobs)[[i]])

    } else {
    
    new_plot <- new_plot + annotation_custom(channel_grobs[[i]],
      xmin = new_coord$.x - .13 * size_x,
      xmax = new_coord$.x + .13 * size_x,
      ymin = new_coord$.y - .13 * size_y,
      ymax = new_coord$.y + .13 * size_y
      )
    }
  }  
  new_plot
}


#' Annotates a head in a ggplot
#'
#' @param size Size of the head
#' @param color Color of the head
#' @param stroke Line thickness
#'
#' @return A layer for a ggplot
#' @export
#'
annotate_head <- function(size = 1.1, color ="black", stroke=1) {
  head <- dplyr::tibble(angle = seq(-pi, pi, length = 50), x = sin(angle)*size, y = cos(angle)*size)
  nose <- data.frame(x = c(size*sin(-pi/18),0, size*sin(pi/18)),y=c(size*cos(-pi/18),1.15*size,size*cos(pi/18)))
  list(ggplot2::annotate("polygon",x =head$x, y =head$y, color = color, fill =NA, size = 1* stroke),
   ggplot2::annotate("line", x = nose$x, y =nose$y, color = color, size = 1* stroke))
  
} 
