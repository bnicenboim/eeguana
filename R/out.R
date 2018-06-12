#' Display information of the eegble object.
#'
#' \itemize{
#' \item \code{nchan()}: Returns the number of channels.
#' \item \code{chan_names()}: Returns a vector with the name of the channels.
#' \item \code{chan_info()}: Returns a data frame (tibble) with information about the channels.
#' \item \code{eeg_info()}: Returns a data frame (tibble) with information about the EEG recording.
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
nchan <- function(x){
  length(x$chan_info$labels)
}

#' 
#' @rdname info
#' @export
chan_names <- function(x){
  levels(x$chan_info$labels)
}

#' 
#' @rdname info
#' @export
chan_info <- function(x){
  x$chan_info
}

#' 
#' @rdname info
#' @export
eeg_info <- function(x){
  x$eeg_info
}

#' 
#' @rdname info
#' @export
srate <- function(x){
  x$eeg_info$srate
}

#' 
#' @rdname info
#' @export
duration <- function(x){
  x$data %>% group_by(.id) %>% summarize(duration = max(sample) / srate(x)) %>% .$duration

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

  df <- as_tibble(x, thinning = "auto") 
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
  df <- as_tibble(x, thinning = "auto") 
  plot <- ggplot2::ggplot(df, 
      ggplot2::aes(x = time, y = amplitude, !!!dots)) + 
      ggplot2::scale_y_reverse() + 
      ggplot2::scale_colour_brewer(type = "qual", palette = "Dark2") +
      ggplot2::theme_bw()
  plot
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
  message(paste0("# EEG data (eegble) from ", nchan(object), " channels:"))
  message(paste0( chan_names(object), collapse = ", "))
  message(paste0("# Sampling rate: ", srate(object), " Hz."))

  message(paste0("# Size in memory: ", capture.output(pryr::object_size(object)), "."))
  message(paste0("# Recordings from: ", paste0(unique(object$seg_info$recording), collapse = ", ")))
  
  message("# Summary of segments")
  object$seg_info %>% dplyr::group_by(recording) %>% 
                      dplyr::count(segment) %>%
                      print(., !!!dots)

  message("# Summary of events")
    object$events %>% 
                      dplyr::group_by_at(dplyr::vars(-size, -channel, -sample)) %>% 
                      dplyr::count() %>%
                      print(., !!!dots)

  invisible(object)
}

