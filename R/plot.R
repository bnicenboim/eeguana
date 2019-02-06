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
#' @param size 
#' @param ... 
#'
#' @rdname plot_in_layout
#' @export
plot_in_layout.gg <- function(plot, projection = "polar", size = 1, ...) {
    
    eeg_data <- plot$data  
    if(!"channel" %in% colnames(eeg_data)){
      stop("Channels are missing from the data.")
    }
    if(!all(c(".x",".y",".z") %in% colnames(eeg_data))){
      stop("Coordinates are missing from the data.")
    }
    plot <- plot + facet_wrap(~ channel)
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
    
    #The facet in the bottom left has both axis, I'll extract and use everywhere:
    maxrow <- max(layout$ROW) #bottom
    # first I extract the axis and I fill the grob with it.
    axisl <- g_filter(plot_grob,paste0("axis-l-",maxrow,"-1"))
    axisb <- g_filter(plot_grob,paste0("axis-b-1-",maxrow))

    # # then I also extract the labels, which I'll use for each facet
    axes_labels <- g_filter(plot_grob,".lab-.")
    
    # This the complete facet with axis
    panel_txt <- paste0("panel-",maxrow,"-1")
    strip_txt <- paste0("strip-t-1-",maxrow)
    axisl_txt <- paste0("axis-l-",maxrow,"-1")
    axisb_txt <- paste0("axis-b-1-",maxrow)
    pattern_txt <- paste0(c(panel_txt,strip_txt,axisl_txt,axisb_txt), collapse = "|")
    full_facet_grob <- g_filter(plot_grob,pattern_txt, trim = TRUE)
    
    rowsize <- full_facet_grob$heights[3] #bottom
    colsize <- full_facet_grob$widths[1]  #left
    
#won't work for free scales, need to add an if-else inside
    
    channel_grobs <- purrr::map(layout$channel, function(ch){
        ## pos <- which(facet_names==ch, arr.ind =  TRUE)
      ch_pos <- layout %>% dplyr::filter(channel ==ch)
      panel_txt <- paste0("panel-",ch_pos$ROW,"-",ch_pos$COL)
      strip_txt <- paste0("strip-t-",ch_pos$COL,"-",ch_pos$ROW)
      axisl_txt <- paste0("axis-l-",ch_pos$ROW,"-",ch_pos$COL)
      axisb_txt <- paste0("axis-b-",ch_pos$COL,"-",ch_pos$ROW)
      # pattern_txt <- paste0(c(panel_txt,strip_txt,axisl_txt,axisb_txt), collapse = "|")
      pattern_txt <- paste0(c(panel_txt,strip_txt), collapse = "|")
      # plot_grob[[1]][[which(plot_grob$layout$name == axisl_txt)]] <- axisl[[1]][[1]] 
      # plot_grob[[1]][[which(plot_grob$layout$name == axisb_txt)]] <- axisb[[1]][[1]]
       ch_grob <- g_filter(plot_grob,pattern_txt, trim = TRUE) %>% 
                            gtable::gtable_add_rows(rowsize) %>%
                            gtable::gtable_add_grob( axisb[[1]][[1]],3,1) %>%
          gtable::gtable_add_cols(colsize,0) %>%
         gtable::gtable_add_grob(axisl[[1]][[1]],2,1)
       
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
     # grid::grid.draw(ch_grob)
    
    #Discard facet panels from the original plot:
    rest_grobs <- g_filter_out(plot_grob,"panel|strip-t|axis|xlab|ylab", trim = FALSE)

    # How much larger than the electrode position should the plot be?
    cmin <- -1 - 0.3 * size
    cmax <- 1 + 0.3 * size

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

    if(all(is.na(eeg_data$.z)) & !identical(project,orthographic)){
        warning("Z coordinates are missing, using 'ortographic' projection ")
        project <- orthographic
    }
      

        for(i in seq_len(length(channel_grobs))) {
        location <- eeg_data %>%
            dplyr::filter(channel == names(channel_grobs)[[i]]) %>% 
            dplyr::distinct(.x,.y,.z)
        if(is.na(location$.x) && is.na(location$.y)){
          new_plot
        } else if (is.na(location$.z) & !identical(project,orthographic)){
            warning("Z coordinates are missing for electrode ", names(channel_grobs)[[i]])

        } else if (is.na(location$.x) | is.na(location$.y)){
             warning("X or Y coordinates are missing for electrode ", names(channel_grobs)[[i]])

        } else {
            new_coord <- project(location$.x, location$.y, location$.z) 
            new_plot<- new_plot +  annotation_custom(channel_grobs[[i]],
                                                     xmin = new_coord$x- .13 * size,
                                                     xmax = new_coord$x + .13 * size,
                                                     ymin = new_coord$y - .13 * size,
                                                     ymax = new_coord$y + .13 * size) 
        }
    }
    new_plot                               
}


