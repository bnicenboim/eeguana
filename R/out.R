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
#' \item \code{nsamples()}: Returns the number of samples of the recording (or segments).
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

#' @rdname info
#' @export
nsamples <- function(x, ...){
  UseMethod("nsamples")
}
#' @export
nsamples.eegbl <- function(x){
  duration(x) * srate(x)
}


#' count number of complete segments of an eegble object.
#' @param x An \code{eegble} object.
#' @param ... Variables to group by.
#' 
#' 
#' @return A tbl. 
#' 
#' @importFrom magrittr %>%
#' 
#' @examples
#' \dontrun{
#'
#' faces_segs_some %>% count_complete_cases(recording, description)
#' }
#' @export
count_complete_cases <- function(x, ...){
  UseMethod("count_complete_cases")
}

#' @export
count_complete_cases.eegbl <- function(x, ...){
    dots <- rlang::enquos(...)

  x$signal %>% dplyr::group_by(.id) %>%
   dplyr::filter_at(dplyr::vars(dplyr::one_of(channel_names(x))), 
            dplyr::all_vars(all(!is.na(.)))) %>% 
   dplyr::summarize() %>% 
   dplyr::semi_join(x$segments,., by = ".id") %>%  
   dplyr::select(-.id,-segment) %>%
   dplyr::count(!!!dots)
}

#' Convert time to sample number.
#'
#' @param x An eegble. 
#' @param t A vector of times. 
#' @param unit "seconds" (or "s"), "milliseconds" (or "ms")
#' 
#' @return A vector of sample numbers.
#' 
#' @importFrom magrittr %>%
#' 
#' @export
in_samples <- function(x, t, unit = "seconds"){
  t * scaling(x, unit)
}



#' Summary of eegble information.
#' 
#' @param object An eegble object.
#' @param ... Other options passed to print.tbl for the display of summaries.
#'
#' @export
summary.eegbl <- function(object, ...){

  dots <- rlang::enquos(...)
  message(paste0("# EEG data (eegble) from ", nchannels(object), " channels:"))
  message(paste0( channel_names(object), collapse = ", "))
  message(paste0("# Sampling rate: ", srate(object), " Hz."))

  message(paste0("# Size in memory: ", capture.output(pryr::object_size(object)), "."))

  message("# Summary of segments")
  object$segments %>% dplyr::count(recording) %>% 
                      dplyr::rename(segment_n = n) %>%
                      print(., !!!dots)

  message("# Summary of events")
    object$events %>% 
                      dplyr::group_by_at(dplyr::vars(-size, -channel, -sample, -.id)) %>% 
                      dplyr::count() %>%
                      print(., !!!dots)

  invisible(object)
}

