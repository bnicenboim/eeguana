#' Display information of the eegble object.
#'
#' \itemize{
#' \item \code{nchannels()}: Returns the number of channels.
#' \item \code{chan_names()}: Returns a vector with the name of the channels.
#' \item \code{channels()}: Returns a data frame (tibble) with information about the channels.
#' \item \code{info()}: Returns a data frame (tibble) with information about the EEG recording.
#' \item \code{srate()}: Returns the sampling rate.
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
nchannels <- function(x){
  length(x$channels$labels)
}

#' 
#' @rdname info
#' @export
channel_names <- function(x){
  levels(x$channels$labels)
}

#' 
#' @rdname info
#' @export
channels <- function(x){
  x$channels
}

#' 
#' @rdname info
#' @export
info <- function(x){
  x$info
}

#' 
#' @rdname info
#' @export
srate <- function(x){
  x$info$srate
}

#' 
#' @rdname info
#' @export
duration <- function(x){
  x$signal %>% group_by(.id) %>% summarize(duration = max(sample) / srate(x)) %>% .$duration

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
plot_gg <- function(x, ..., thinning = "auto"){

  dots = rlang::enquos(...) 
  df <- as_tibble(x, thinning = thinning) 
  plot <- ggplot2::ggplot(df, 
      ggplot2::aes(x = time, y = amplitude, !!!dots)) + 
      ggplot2::scale_y_reverse() + 
      ggplot2::scale_colour_brewer(type = "qual", palette = "Dark2") +
      ggplot2::theme_bw()
  plot
}

#' 
plot_segment_summary <- function(x, .funs = mean, ...){
  funs_name <- rlang::enquo(.funs)
  seg_sum <- as_segment_summary(x, .funs = !!funs_name, ...)
  print(seg_sum)
  plot_down <- ggplot2::ggplot(seg_sum, aes(x= segment, y = !!funs_name)) + ggplot2::geom_point()
  plot_right <- ggplot2::ggplot(seg_sum, aes(x= !!funs_name, y = channel)) + ggplot2::geom_point()
  plot_center <- ggplot2::ggplot(seg_sum, aes(x= segment, y = channel, fill = !!funs_name)) + ggplot2::geom_raster() + 
      ggplot2::scale_colour_brewer(type = "qual", palette = "Dark2") +
      ggplot2::theme_bw()

 
  plot_center
  # cowplot::plot_grid(plot_center, plot_right, plot_down,ncol = 3)
}


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

