#' Display information of the eegble object.
#'
#' \itemize{
#' \item \code{nchannels()}: Returns the number of channels.
#' \item \code{chan_names()}: Returns a vector with the name of the channels.
#' \item \code{channels()}: Returns a data frame (tibble) with information about the channels.
#' \item \code{info()}: Returns a data frame (tibble) with information about the EEG recording.
#' \item \code{srate()}: Returns the sampling rate.
#' \item \code{reference()}: Returns the reference.
#' \item \code{duration()}: Returns the duration of the recording (or segments).
#' }
#' @param x An eegble object.
#' 
#' @name info
NULL
#> NULL


#' 
#' @rdname info
#' @export
nchannels <- function(x, ...){
  UseMethod("nchannels")
}

#' @export
nchannels.eegbl <- function(x){
  length(x$channels$labels)
}



#' 
#' @rdname info
#' @export
channel_names <- function(x, ...){
  UseMethod("channel_names")
}

#' @export
channel_names.eegbl <- function(x){
  levels(x$channels$labels)
}

#' 
#' @rdname info
#' @export
channels <- function(x, ...){
  UseMethod("channels")
}

#' @export
channels.eegbl <- function(x){
  x$channels
}




#' 
#' @rdname info
#' @export
info <- function(x, ...){
  UseMethod("info")
}


#' @export
info.eegbl <- function(x){
  x$info
}


#' 
#' @rdname info
#' @export
srate <- function(x, ...){
  UseMethod("srate")
}

#' @export
srate.eegbl <- function(x){
  x$info$srate
}

#' @rdname info
#' @export
reference <- function(x, ...){
  UseMethod("reference")
}

#' @export
reference.eegbl <- function(x){
  x$info$reference
}



#' @rdname info
#' @export
duration <- function(x, ...){
  UseMethod("duration")
}
#' @export
duration.eegbl <- function(x){
  x$signal %>% dplyr::group_by(.id) %>% 
              dplyr::summarize(duration = max(sample) / srate(x)) %>% 
              .$duration

}


#' Simple plot an eegble object.
#' @param x An \code{eegble} object.
#' @param thinning Automatic by default, but integers can be used.
#' 
#' 
#' @return A ggplot object 
#' 
#' @importFrom magrittr %>%
#' 
#' @export
plot.eegbl <- function(x, thinning = "auto"){

  df <- as_tibble(x, thinning = thinning) 
  plot <- ggplot2::ggplot(df, 
      ggplot2::aes(x = time, y = amplitude)) + 
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~ channel + recording + segment, 
        labeller = ggplot2::label_wrap_gen(multi_line=FALSE)) + 
      ggplot2::scale_y_reverse() + 
      ggplot2::theme_bw()
  plot
}

#' ggplot object based on an eegble object.
#' @param x An \code{eegble} object.
#' @param thinning Automatic by default, but integers can be used.
#' 
#' 
#' @return A ggplot object 
#' 
#' @importFrom magrittr %>%
#' 
#' @export
plot_gg <- function(x, ...){
  UseMethod("plot_gg")
}

#' @export
plot_gg.eegbl <- function(x, ..., thinning = "auto"){

  dots = rlang::enquos(...) 
  df <- as_tibble(x, thinning = thinning) 
  plot <- ggplot2::ggplot(df, 
      ggplot2::aes(x = time, y = amplitude, !!!dots)) + 
      ggplot2::scale_y_reverse() + 
      ggplot2::scale_colour_brewer(type = "qual", palette = "Dark2") +
      ggplot2::theme_bw()
  plot
}

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

#' A topographic plot of an eegble object.
#' 
#' Create a default topographic plot based on the segments of the \code{eegble} object.
#' 
#' The following methods of interpolation are available :
#' \itemize{
#' \item \code{"MBA"} (Default) Multilevel B-splines using the function \code{mba.surf}; requires the package \code{MBA}.
#' }
#' 
#' @param x An \code{eegble} object.
#' @param method Method of interpolation.
#' @param ... Various arguments passed to the interpolation method.
#' 
#' 
#' @return A ggplot object 
#' 
#' 
#' @export 
plot_topo <- function(x, method = "MBA", ...) {
  UseMethod("plot_topo")
}

#' @export 
plot_topo.eegbl <- function(x, method = "MBA", ...){
  grouping_vars <- colnames(x$segments) %>% setdiff(c(".id","segment"))
  chan_vars <- colnames(x$channels) %>% setdiff("labels")
  s_x <- summarize_id_as_tibble(x, mean, na.rm = TRUE) %>%
            dplyr::group_by_at(c(grouping_vars,chan_vars, "channel")) %>%
            dplyr::summarize(A = mean(mean, na.rm = TRUE)) %>% 
            dplyr::group_by_at(grouping_vars)


  grid <- interpolate_xy(s_x, x = x, y = y, value = A, ...)

  grid  %>% ggplot(aes(x, y))+  facet_wrap(grouping_vars) + 
  geom_raster(aes(fill = A), interpolate = F, hjust = 0.5, vjust = 0.5)+
  geom_contour(aes(z = A)) + 
  geom_text(data = filter(s_x, !is.na(x),!is.na(y)) , aes(x = x, y = y, label = channel), colour = 'black')  +
   # scale_fill_distiller(palette = "Spectral", guide = "colourbar", oob = scales::squish) + #, oob = scales::squish
  scale_fill_gradientn(colours = c("darkred","yellow","green","darkblue"),
                         values=c(1.0,0.75,0.5,0.25,0)) + 
   # scale_fill_distiller(palette = "RdBu", guide = "colourbar") + #, oob = scales::squish
      ggplot2::theme_bw()
  # scale_fill_viridis_c(option="B") +
}
# .data <- s_x
# plot_topo3d <- function(x, method){

#   grouping <- groups(x$segments)

#   seg_sum <- as_segment_summary(x, .funs = mean, na.rm=TRUE) %>% 
#             group_by(channel,x,y, !!!grouping) %>% 
#             summarize(amplitude = mean(mean, na.rm=TRUE))

#' @export
interpolate_xy <- function(.data, x, y, value, method = "MBA",...){
  # x <- rlang::quo(x)
  # y <- rlang::quo(y)
  # value <- rlang::quo(A)
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  value <- rlang::enquo(value)

  .data <- select(.data, one_of(dplyr::group_vars(.data)), !!x, !!y, !!value, channel) 
  l <- select(.data, one_of(dplyr::group_vars(.data))) 

  #REMOVE EMPTY
  if(method == "MBA") {
  if(!"MBA" %in% rownames(utils::installed.packages())){
      stop("Package MBA needs to be installed to interpolate using multilevel B-splines ")
    }
   #change to sp = FALSE and adapt, so that I remove the sp package
  interpolation_alg <- function(xyz, ...) MBA::mba.surf(xyz = xyz, 100, 100, sp = TRUE, extend = TRUE, ...)
   
    } else {
  stop("Non supported method.")
  }
  grid <- split(.data, l) %>% 
          purrr::discard(~ nrow(.x)==0) %>% 
          purrr::map_dfr(function(.d) {
                          common <- .d %>% 
                          dplyr::ungroup() %>%
                          dplyr::select(-channel, -!!x, -!!y, -!!value) %>%
                          distinct()

                          if(nrow(common)>1) {stop("Bad grouping.")}
                        
                          
                          mba_interp <- .d %>% 
                          dplyr::ungroup() %>%
                          dplyr::select(!!x, !!y, !!value) %>% 
                          dplyr::filter(!is.na(x) | !is.na(y)) %>%
                          interpolation_alg() 
                           

                          dplyr::tibble(!!quo_name(x) := mba_interp$xyz.est@coords[,1],
                          !!quo_name(y) := mba_interp$xyz.est@coords[,2], 
                          !!quo_name(value) := mba_interp$xyz.est@data$z) %>% 
                          dplyr::filter(((!!x)^2 + (!!y)^2 < 1.1)) %>% 
                          {dplyr::bind_cols(dplyr::slice(common, rep(1, each = nrow(.))),.) } 
                                      })

   grid 
}




# ggplot(data=grid, aes(x, y))+
#   geom_raster(aes(fill = z), interpolate = F, hjust = 0.5, vjust = 0.5)+
#   geom_contour(aes(z = z)) + 
#   scale_fill_viridis_c(option="B") +
#   geom_point(data = ampl, aes(x, y), colour = 'black') 

# }



#' Summary of eegble information.
#' 
#' @param object An eegble object.
#' @param ... Other options passed to print.tbl for the display of summaries.
#'
#' @export
summary.eegbl <- function(object, ...){
  # to add later as an option, and add ... to the prints

  dots <- rlang::enquos(...)
  message(paste0("# EEG data (eegble) from ", nchannels(object), " channels:"))
  message(paste0( channel_names(object), collapse = ", "))
  message(paste0("# Sampling rate: ", srate(object), " Hz."))

  message(paste0("# Size in memory: ", capture.output(pryr::object_size(object)), "."))
  message(paste0("# Recordings from: ", paste0(unique(object$segments$recording), collapse = ", ")))
  
  message("# Summary of segments")
  object$segments %>% dplyr::group_by(recording) %>% 
                      dplyr::count(segment) %>%
                      print(., !!!dots)

  message("# Summary of events")
    object$events %>% 
                      dplyr::group_by_at(dplyr::vars(-size, -channel, -sample)) %>% 
                      dplyr::count() %>%
                      print(., !!!dots)

  invisible(object)
}

