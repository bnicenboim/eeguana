#' Display information of the eegble object.
#'
#' \itemize{
#' \item \code{nchan()}: Returns the number of channels.
#' \item \code{chan_names()}: Returns a vector with the name of the channels.
#' \item \code{chan_info()}: Returns a dataframe (tibble) with information about the channels.
#' \item \code{gral_info()}: Returns a dataframe (tibble) with information about the EEG recording.
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
gral_info <- function(x){
  x$gral_info
}

#' 
#' @rdname info
#' @export
srate <- function(x){
  x$gral_info$srate
}

#' 
#' @rdname info
#' @export
duration <- function(x){
  purrr::map_dbl(x$data, function(f)
   purrr::map_dbl(f$signals, ~ nrow(.x)) %>% unique()/ srate(x) )

}


#' Simple plot an eegble object.
#' @param x An \code{eegble} object.
#' @param chans Channel names to be used; all of them by default.
#' @param thinning Automatic by default, but integers can be used.
#' 
#' 
#' @return A ggplot object 
#' 
#' @importFrom magrittr %>%
#' 
#' @export
plot.eegbl <- function(x, chans = NULL, thinning = "auto"){

  if(is.null(chans)) {chans = chan_names(x)}

  if(thinning=="auto"){ 
    by <- length(chans) * 2
  } else if(is.null(thinning)) {
    by <- 1
  } else {
    by <- thinning
  }
  df <- as_tibble(x, chans) %>% 
    dplyr::group_by(id, segment, channel) %>% 
        dplyr::filter(time %in%  time[seq(1,length(time), by = by)])

  plot <- ggplot2::ggplot(df, 
      ggplot2::aes(x = time, y = amplitude)) + 
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~ channel + id + segment, 
        labeller = ggplot2::label_wrap_gen(multi_line=FALSE)) + 
      ggplot2::scale_y_reverse() + 
      ggplot2::theme_bw()
  plot
}


#' Print an eegble object.
#' 
#' @param x An eegble object.
#' @param ... Other options passed to print.tbl.
#' @param file If it is specified, prints a summary of a single file.
#'
#' @export
print.eegbl <- function(x, ..., id = NULL){
  # to add later as an option, and add ... to the prints

  dots <- rlang::enquos(...)
  message(paste0("# EEG data (eegble) from ", nchan(x), " channels:"))
  message(paste0( chan_names(x), collapse = ", "))
  message(paste0("# Sampling rate: ", srate(x), " Hz."))
  if(is.null(id)){
    message(paste0("# Size in memory: ", capture.output(pryr::object_size(x)), "."))
    message(paste0("# Recording ids: ", paste0(names(x$data), collapse = ", ")))
    segs <- purrr::map_dbl(x$data, ~ length(.x$signals))
    if(length(unique(segs)) == 1) {
        message(paste0("# Number of segments in each recordings: ",
         unique(segs) ))
    } else {
      message("# Recordings have different number of segments: ", 
        paste0(segs, collapse = ", "))
    }
    message("# Summary of events for all recordings")
    purrr::map_dfr(x$data, ~ .x$events) %>% 
      dplyr::group_by_at(dplyr::vars(-size, -channel, -sample)) %>% 
      dplyr::count() %>%
    print(., !!!dots)

    message("# Segments info: ")
    print(x$seg_info, !!!dots)
  } else  {

    segs <- length(x$data[[id]]$signals)
    message(paste0("# Number of segments: ",segs ))
    x$seg_info %>% dplyr::filter(id == id) %>% 
      print(., !!!dots)

    message("# Summary of events")
    purrr::map_dfr(x$data, ~ .x$events) %>% 
      dplyr::group_by_at(dplyr::vars(-size, -channel, -sample)) %>% 
      dplyr::count() %>%
    print(., !!!dots)
  }
  invisible(x)
}

