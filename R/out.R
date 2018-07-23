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
              dplyr::summarize(duration = (max(sample) - min(sample)) / srate(x)) %>% 
              .$duration

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

