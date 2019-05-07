#' Create a basic signal plot
#' 
#' `plot` creates a ggplot object in which the EEG signal over the whole 
#' recording is plotted by electrode. Useful as a quick visual check for major
#' noise issues in the recording.
#' 
#' Note that for normal-size datasets, the plot may take some time to compile.
#' If necessary, `plot` will first downsample the `eeg_lst` object so that there is a 
#' maximum of 6,400 samples. The `eeg_lst` object is then converted to a long-format
#' tibble via `as_tibble`. In this tibble, the `.key` variable is the 
#' channel/component name and `.value` its respective amplitude. The sample 
#' number (`.sample_id` in the `eeg_lst` object) is automatically converted to seconds
#' to create the variable `time`. By default, time is then plotted on the 
#' x-axis and amplitude on the y-axis, and uses `scales = "free"`; see [ggplot2::facet_grid()].
#' 
#' To add additional components to the plot such as titles and annotations, simply
#' use the `+` symbol and add layers exactly as you would for `ggplot::ggplot`.
#' 
#' 
#' @param x An `eeg_lst` object.
#' @param max_sample Downsample to approximately 6400 samples by default.
#' @param ... Not in use.
#' @family plot
#' 
#' @return A ggplot object
#' 
#' @examples 
#' 
#' # Basic plot
#' plot(data_faces_ERPs)
#' 
#' # Add ggplot layers
#' plot(data_faces_ERPs) + 
#'     coord_cartesian(ylim = c(-500,500))
#' 
#' @importFrom magrittr %>%
#'
#' @export
plot.eeg_lst <- function(x, max_sample = 6400, ...) {
    ellipsis::check_dots_unnamed()
    plot <- ggplot2::ggplot(x, ggplot2::aes(x = .time, y = .value, group = .id)) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(.key ~ .,
                        labeller = ggplot2::label_wrap_gen(multi_line = FALSE),
                        scales = "free"
    ) +
      ggplot2::scale_x_continuous("Time (s)")+
      ggplot2::scale_y_continuous("Amplitude")+
    theme_eeguana()
  plot
}

#' Create an ERP plot
#' 
#' `plot_gg` initializes a ggplot object which takes an `eeg_lst` object as
#' its input data. Layers can then be added in the same way as for a 
#' `ggplot2::ggplot` object.
#' 
#' If necessary, `plot_gg` will first downsample the `eeg_lst` object so that there is a 
#' maximum of 6400 samples. The `eeg_lst` object is then converted to a long-format
#' tibble via [as_tibble]. In this tibble, the `.key` variable is the 
#' channel/component name and `.value` its respective amplitude. The sample 
#' number (`.sample_id` in the `eeg_lst` object) is automatically converted to milliseconds
#' to create the variable `time`. By default, time is plotted on the 
#' x-axis and amplitude on the y-axis.
#' 
#' To add additional components to the plot such as titles and annotations, simply
#' use the `+` symbol and add layers exactly as you would for `ggplot::ggplot`.
#' 
#' @param .data An `eeg_lst` object.
#' @inheritParams  ggplot2::aes
#' @param max_sample Downsample to approximately 2400 samples by default.
#'
#' @family plot
#' @return A ggplot object
#' 
#' @examples 
#' 
#' # Plot grand averages for selected channels
#' data_faces_ERPs %>% 
#'   # select the desired electrodes
#'   select(O1, O2, P7, P8) %>% 
#'   plot_gg() + 
#'       # add a grand average wave
#'       stat_summary(fun.y = "mean", geom ="line", alpha = 1, size = 1.5, 
#'                aes(color = condition)) +
#'       # facet by channel
#'       facet_wrap(~ .key) + 
#'       theme(legend.position = "bottom") 
#'
#' @export
plot_gg <- function(.data, ...) {
  UseMethod("plot_gg")
}
#' @rdname plot_gg
#' @export
plot_gg.eeg_lst <- function(.data, x = .time, y = .value, ..., max_sample = 2400) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  dots <- rlang::enquos(...)
   plot <- ggplot2::ggplot(.data, ggplot2::aes(x = !!x, y = !!y, !!!dots)) +
    ggplot2::scale_colour_brewer(type = "qual", palette = "Dark2") +
    theme_eeguana()
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
    theme_eeguana()
  plot
}


#' Create a topographic plot
#'
#' `plot_topo` initializes a ggplot object which takes an `eeg_lst` object
#' as its input data. Layers can then be added in the same way as for a 
#' `ggplot2::ggplot` object.
#' 
#' Before calling `plot_topo`, the `eeg_lst` object object must be appropriately 
#' grouped (e.g. by condition) and/or 
#' summarized into mean values such that each .x .y coordinate has only one 
#' amplitude value. By default, `plot_topo` interpolates amplitude values via
#' `eeg_interpolate_tbl`, which generates a tibble with `.key` (channel), 
#'  `.value` (amplitude), and .x .y coordinate variables. .x .y coordinates are 
#' extracted from the `eeg_lst` object, which in turn reads the coordinates logged
#' by your EEG recording software. By default, `plot_topo` will display electrodes 
#' in polar arrangement, but can be changed with the `projection` 
#' argument. Alternatively, if `eeg_interpolate_tbl` is called after grouping/summarizing
#' but before `plot_topo`, the resulting electrode layout will be stereographic.
#' 
#' `plot_topo` called alone 
#' without any further layers will create an unannotated topographical plot. 
#' To add a head and nose, add the layer `annotate_head`. Add 
#' contour lines with `ggplot2::geom_contour` and electrode labels 
#' with `ggplot2::geom_text`. These arguments are deliberately not
#' built into the function so as to allow flexibility in choosing colour, font 
#' size, and even head size, etc.
#' 
#' To add additional components to the plot such as titles and annotations, simply
#' use the `+` symbol and add layers exactly as you would for `ggplot::ggplot`.
#'
#'
#' @param data A table of interpolated electrodes as produced by `eeg_interpolate_tbl`, or an `eeg_lst`, or `ica_lst` appropiately grouped. 
#' @param ... If data are an `eeg_lst` or `ica_lst`, these are arguments passed to `eeg_interpolate_tbl`, such as, radius, size, etc.
#'
#' @family plot
#'
#' @return A ggplot object
#' 
#' @examples 
#' 
#' # Calculate mean amplitude between 100-200 ms and plot the topography
#' data_faces_ERPs %>% 
#'     # select the time window of interest
#'     filter(between(as_time(.sample_id, unit = "milliseconds"),100,200)) %>% 
#'     # compute mean amplitude per condition
#'     group_by(condition) %>%
#'     summarize_all_ch(mean, na.rm = TRUE) %>%
#'     plot_topo() +
#'         # add a head and nose shape
#'         annotate_head() + 
#'         # add contour lines
#'         geom_contour() +
#'         # add electrode labels
#'         geom_text(colour = "black") +
#'         facet_grid(~condition)
#'
#' # The same but with interpolation
#' data_faces_ERPs %>% 
#'     filter(between(as_time(.sample_id, unit = "milliseconds"),100,200)) %>% 
#'     group_by(condition) %>%
#'     summarize_all_ch(mean, na.rm = TRUE) %>%
#'     eeg_interpolate_tbl() %>%
#'     plot_topo() +
#'         annotate_head() + 
#'         geom_contour() +
#'         geom_text(colour = "black") +
#'         facet_grid(~condition)
#' 
#' 
#' @export
plot_topo <- function(data,  ...) {
  UseMethod("plot_topo")
}
#' @rdname plot_topo
#' @export
plot_topo.tbl_df <- function(data, value= .value,  label=.key) {

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
      dplyr::group_by_at(dplyr::vars(colnames(.)[!colnames(.) %in% c(".x",".y")]) ) %>%
      dplyr::slice(1) %>%
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
    ggplot2::scale_fill_distiller(type = "div",palette = "RdBu",guide = "colourbar",  oob = scales::squish) +
    theme_eeguana2()
  plot

}

#' @inheritParams plot_in_layout
#' @inheritParams eeg_interpolate_tbl
#' @rdname plot_topo
#' @export
plot_topo.eeg_lst <- function(data, projection = "polar", ...) {
  
  channels_tbl(data)  <- change_coord(channels_tbl(data), projection) 
    eeg_interpolate_tbl(data,...) %>%
    plot_topo()
}

#' @export
plot_components <- function(data,  ...) {
  UseMethod("plot_components")
}
#' @rdname plot_topo
#' @export
plot_components.eeg_ica_lst <- function(data,  projection = "polar", ...) {
    
    channels_tbl(data)  <- change_coord(channels_tbl(data), projection)  
    ##TODO: move to data.table, ignore group, just do it by .recording
    long_table <- map_dtr(data$ica, ~ data.table::as.data.table(.x$mixing_matrix) %>%
                            .[, .ICA := {.ICA = paste0("ICA",seq_len(.N));
                            factor(.ICA, levels = .ICA)}],.id = "recording") %>%
      data.table::melt(variable.name = ".key",
                       id.vars = c(".ICA","recording"),
                       value.name = ".value")
    long_table[,.key := as.character(.key)]
    
    long_table <- left_join_dt(long_table, data.table::as.data.table(channels_tbl(data)), by = c(".key"= ".channel")) %>% 
      dplyr::group_by(recording,.ICA) 
      
       long_table %>% eeg_interpolate_tbl(...) %>%
        plot_topo() +
      facet_wrap(~.ICA)+
      annotate_head() + 
      geom_contour() +
      geom_text(colour = "black")+
         theme(legend.position = "none")
    
}




#' Arrange ERP plots according to scalp layout
#'
#' Arranges a ggplot so that the facet for each channel appears in its position 
#' on the scalp.
#' 
#' This function requires two steps: first, a ggplot object must be created with 
#' ERPs facetted by channel (`.key`). 
#' Then, the ggplot object is called in `plot_in_layout`. The function uses grobs
#' arranged according to .x .y coordinates extracted from the `eeg_lst` object, by
#' default in polar arrangement. The arrangement can be changed with the `projection`
#' argument. White space in the plot can be reduced by changing `ratio`.
#' 
#' Additional components such as titles and annotations should be added to the 
#' plot object using `+` exactly as you would for `ggplot::ggplot`.
#' Title and legend adjustments will be treated as applying to the 
#' whole plot object, while other theme adjustments will be treated as applying 
#' to individual facets. x-axis and y-axis labels cannot be added at this stage.
#'
#' @param plot A ggplot object with channels
#'
#' @family plot
#' @return A ggplot object
#' 
#' 
#' @examples 
#' 
#' # Create a ggplot object with some grand averaged ERPs
#' ERP_plot <- data_faces_ERPs %>% 
#'    # select a few electrodes
#'    select(Fz, FC1, FC2, C3, Cz, C4, CP1, CP2, Pz) %>%
#'    # group by time point and condition
#'    group_by(.sample_id, condition) %>%
#'    # compute averages
#'    summarize_all_ch(mean,na.rm=TRUE) %>%
#'    plot_gg() + 
#'        # plot the averaged waveforms
#'        geom_line(aes(color = condition)) +
#'        # facet by channel
#'        facet_wrap(~ .key) +  
#'        # add a legend and title
#'        theme(legend.position = "bottom") + 
#'        ggtitle("ERPs for faces vs non-faces") 
#'
#' # Call the ggplot object with the layout function
#' plot_in_layout(ERP_plot)
#' @export
plot_in_layout <- function(plot,  ...) {
    UseMethod("plot_in_layout")
}


#' @param projection Projection type for converting the 3D coordinates of the electrodes into 2d coordinates. Projection types available: "polar" (default), "orthographic", or  "stereographic"
#' @param ratio Ratio of the individual panels
#' @param ... Not in use. 
#'
#' @rdname plot_in_layout
#' @export
plot_in_layout.gg <- function(plot, projection = "polar", ratio = c(1,1), ...) {
  size_x <- ratio[[1]]
  size_y <- ratio[[2]]
  ch_location <- plot$data_channels
  ## if (!"channel" %in% colnames(ch_location)) {
  ##   stop("Channels are missing from the data.")
  ## }
  if (!all(c(".x", ".y", ".z") %in% colnames(ch_location))) {
    stop("Coordinates are missing from the data.")
  }
  plot <- plot  + ggplot2::facet_wrap(.~.key)
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

  # needed for passing checks:
  b <- NULL
  l <- NULL
  #THESE ARE NOT IN ORDER!!!
  panels <- subset(plot_grob$layout, grepl("panel", plot_grob$layout$name)) %>%
    dplyr::arrange(b,l)
  strips <- subset(plot_grob$layout, grepl("strip", plot_grob$layout$name))%>%
    dplyr::arrange(b,l)
  
  # won't work for free scales, need to add an if-else inside
  
  channel_grobs <- purrr::map(layout$.key, function(ch) {
    ## pos <- which(facet_names==ch, arr.ind =  TRUE)
      ch_pos <- layout %>% dplyr::filter(.key == ch)
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
  }) %>% stats::setNames(layout$.key)
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
 

  ch_location <- change_coord(ch_location, projection)

  xmin <- min(ch_location$.x,na.rm=TRUE) - 0.3 #* size
  xmax <- max(ch_location$.x,na.rm=TRUE) + 0.3 #* size
  ymin <- min(ch_location$.y,na.rm=TRUE) - 0.3 #* size
  ymax <- max(ch_location$.y,na.rm=TRUE) + 0.3 #* size
  new_plot <- ggplot2::ggplot(data.frame(x = c(xmin, xmax), y = c(ymin, ymax)),
                             ggplot2::aes_(x = ~x, y = ~y)) +
    ggplot2::geom_blank() +
    ggplot2::scale_x_continuous(limits = c(xmin, xmax), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0)) +
    ggplot2::theme_void() +
    ggplot2::annotation_custom(rest_grobs,
                      xmin = xmin,
                      xmax = xmax,
                      ymin = ymin,
                      ymax = ymax
    )
  
  for (i in seq_len(length(channel_grobs))) {
    new_coord <- ch_location %>%
        dplyr::filter(.channel == names(channel_grobs)[[i]]) %>%
      dplyr::distinct(.x, .y)
    if(is.na(new_coord$.x) && is.na(new_coord$.y)){
      new_plot
    
    } else if (is.na(new_coord$.x) | is.na(new_coord$.y)){
      warning("X or Y coordinates are missing for electrode ", names(channel_grobs)[[i]])

    } else {
    
    new_plot <- new_plot + ggplot2::annotation_custom(channel_grobs[[i]],
      xmin = new_coord$.x - .13 * size_x,
      xmax = new_coord$.x + .13 * size_x,
      ymin = new_coord$.y - .13 * size_y,
      ymax = new_coord$.y + .13 * size_y
      )
    }
  }  
  new_plot
}


#' Add a head shape to a ggplot
#' 
#' Adds the outline of a head and nose to a ggplot.
#'
#' @param size Size of the head
#' @param color Color of the head
#' @param stroke Line thickness
#'
#' @return A layer for a ggplot
#' 
#' @examples
#' 
#' data_faces_ERPs %>% 
#'     filter(between(as_time(.sample_id, unit = "milliseconds"),100,200)) %>% 
#'     group_by(condition) %>%
#'     summarize_all_ch(mean, na.rm = TRUE) %>%
#'     plot_topo() +
#'     annotate_head(size = .9, color = "black", stroke = 1)
#' 
#' 
#' @export
#'
annotate_head <- function(size = 1.1, color ="black", stroke=1) {
  angle <- NULL # to avoid a note in the checks afterwards:
  head <- dplyr::tibble(angle = seq(-pi, pi, length = 50),
                        x = sin(angle)*size,
                        y = cos(angle)*size)
  nose <- dplyr::tibble(x = c(size*sin(-pi/18),0, size*sin(pi/18)),
                        y=c(size*cos(-pi/18),1.15*size,size*cos(pi/18)))
  list(ggplot2::annotate("polygon",x =head$x, y =head$y, color = color, fill =NA, size = 1* stroke),
   ggplot2::annotate("line", x = nose$x, y =nose$y, color = color, size = 1* stroke))
  
} 

#' @export
annotate_events <- function(data=NULL, alpha = .2){
    layer <- geom_rect(data=data, alpha = alpha,
              ymin = -Inf, ymax= Inf,
              inherit.aes = FALSE,
              aes(xmin = xmin,
                  xmax =  xmax ,
                  color = Event,
                  fill = Event,
                  group = .id)
              
              )
    structure(list(layer = layer), class = "layer_events")
}

#' @export
ggplot_add.layer_events <- function(object, plot, object_name) {
    if(length(object$layer$data)==0) {
        events_tbl <- plot$data_events
   } else {
       events_tbl <- object$layer$data
    }
    info_events   <- setdiff(colnames(events_tbl), obligatory_cols[["events"]])
    events_tbl <- data.table::as.data.table(events_tbl)
    events_tbl[,xmin:= as_time(.initial) ]
    events_tbl[,xmax:= as_time(.final) ]
    events_tbl[,Event := (do.call(paste,c(.SD, sep ="."))), .SDcols= c(info_events)]
                                        #single events
    segs <- plot$data %>%
        dplyr::select(-.time, -.key, -.value) %>%
        dplyr::distinct()
  
    events_tbl <- left_join_dt(events_tbl, data.table::as.data.table(segs), by = ".id")
    ## if(all_chs){
    ##   events_tbl[, .channel := NA]
    ## }
    chs <- list(unique(as.character(plot$data$.key)))
   events_tbl[,.key:=lapply(.channel, function(x) as.character(x))]
   events_tbl[is.na(.channel), .key:=list(rep(chs,.N))]
   events_tbl <- tidyr::unnest(events_tbl)
   
   events_tbl[,.key:=factor(.key, levels = levels(plot$data$.key))]
  # events_all_chs <- events_tbl[is.na(.channel),]
  # events_all_chs[,.key := rep(list(unique(events_tbl$.channel)),.N)] 
  # events_all_chs <- tidyr::unnest(events_all_chs)
  # events_spec_chs <- events_tbl[!is.na(.channel),]
  # events_spec_chs[, .key := .channel]
  # events_tbl <- rbind(events_all_chs, events_spec_chs)
  #   to_plot<- list()
  #   to_plot$events_all <- filter_dt(events_tbl, .initial == .final, is.na(.channel) )
  #   to_plot$events_ch <- filter_dt(events_tbl, .initial == .final, !is.na(.channel) ) %>%
  #       .[, .key := as.factor(.channel)]
  #   to_plot$intervals_all <- filter_dt(events_tbl, .initial < .final, is.na(.channel) )
  #   to_plot$intervals_ch <- filter_dt(events_tbl, .initial < .final, !is.na(.channel) )  %>%
  #       .[, .key := as.factor(.channel)]
  #   add_to_plot <- purrr::map(to_plot, ~ if(nrow(.x)>0){
  #                                            layer <- data.table::copy(object$layer)
  #                                            layer$data <-  .x
  #                                            layer
  #                                        }
  #                                        else {
  #                                            NULL
  #                                        }) %>%
  #     purrr::keep(function(x) !is.null(x))
  #   
  object$layer$data <- events_tbl
   plot + object$layer
} 

#' @export
ggplot.eeg_lst <- function(data = NULL,
                           mapping = ggplot2::aes(),
                           ...,
                           max_sample = 2400,
                            environment = parent.frame()) {
    df <- try_to_downsample(data, max_sample) %>%
        data.table::as.data.table()

    df[,.key := factor(.key, levels = unique(.key))]
    p <- ggplot2::ggplot(data=df,mapping=mapping,..., environment=environment)

    p$data_channels <- channels_tbl(data)
    p$data_events <- events_tbl(data)
    p$data_channels <- channels_tbl(data)
    p
}
#'  Eeguana ggplot themes
#'  These are complete light themes based on [ggplot2::theme_bw()] which control all non-data display. 
#'  @name theme
NULL
# > NULL

#' @rdname theme
#' @export
theme_eeguana <- function() {
  ggplot2::theme_bw() %+replace% 
                ggplot2::theme(
                    strip.background = ggplot2::element_rect(colour = "transparent", fill = "transparent"),
                    panel.spacing  = ggplot2::unit(.01, "points"),
                    panel.border = ggplot2::element_rect(colour = "transparent", fill = "transparent"))
}
#' @rdname theme
#' @export
theme_eeguana2 <- function() {
 theme_eeguana() %+replace% 
                    theme(panel.grid = ggplot2::element_line(colour = "transparent"),
                      axis.ticks= ggplot2::element_line(colour = "transparent"),
                      axis.text= ggplot2::element_blank(),
                      axis.title= ggplot2::element_blank())
}
