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



#' Summary of an eegble object.
#' @export
summary.eegbl <- function(object, ...){
  message(paste0("# EEG data (eegble) from ", nchan(object), " channels:"))
  message(paste0( chan_names(object), collapse = ", "))
  message(paste0("# Sampling rate: ", srate(object), " Hz."))
  message(paste0("# Duration: ", duration(object), " ms."))
  # message(paste0("# Events (markers/triggers): ", 
  #                   paste0(event_names(object),collapse =", "), "."))
  message(paste0("# Size in memory: ", capture.output(pryr::object_size(object)), "."))
  message(paste0("# Files: ", names(object$data), collapse = ", "))
  

  message("# Summary of signals")
  summary_data <- purrr::map_dfr(object$data, function(f)
   purrr::map_dfr(f$signals, ~ .x) ) %>% summary() %>%
  print()
  
  message("# Summary of events")
  summary_events <- purrr::map_dfr(object$data, function(f)
   f$events) %>% summary() %>%
  print()
  invisible(list(signals = summary_data, events = summary_events))
}

#' Print an eegble object.
#' 
#' @param x An eegble object.
#' @param ... Other options passed to print.tbl.
#' @param file If it is specified, prints a summary of a single file.
#'
#' @export
print.eegbl <- function(x, ..., file = NULL){
  # to add later as an option, and add ... to the prints

  dots <- rlang::enquos(...)
  message(paste0("# EEG data (eegble) from ", nchan(x), " channels:"))
  message(paste0( chan_names(x), collapse = ", "))
  message(paste0("# Sampling rate: ", srate(x), " Hz."))
  if(is.null(file)){
    message(paste0("# Size in memory: ", capture.output(pryr::object_size(x)), "."))
    message(paste0("# Files: ", paste0(names(x$data), collapse = ", ")))
    segs <- purrr::map_dbl(x$data, ~ length(.x$signals))
    if(length(unique(segs)) == 1) {
        message(paste0("# Number of segments in each file: ",
         unique(segs) ))
    } else {
      message("# Files have different number of segments: ", 
        paste0(segs, collapse = ", "))
    }
    message("# Summary of events for all files")
    purrr::map_dfr(x$data, ~ .x$events) %>% 
      dplyr::group_by_at(vars(-size, -channel, -sample)) %>% 
      dplyr::count() %>%
    print(., !!!dots)
  } else  {

    segs <- length(x$data[[file]]$signals)
    message(paste0("# Number of segments: ",segs ))
    message("# Summary of events")
    purrr::map_dfr(x$data, ~ .x$events) %>% 
      dplyr::group_by_at(vars(-size, -channel, -sample)) %>% 
      dplyr::count() %>%
    print(., !!!dots)
  }
  invisible(x)
}

