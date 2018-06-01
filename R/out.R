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
	map_dbl(x$data, function(f)
	 map_dbl(f$signals, ~ nrow(.x)) %>% unique()/ srate(x) )

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
  
  map_chr(object$data, function(f) names(f))

  map_dfr(object$data, function(f)
	 map_dfr(f$signals, ~ .x) ) %>% summary() %>%
  print()
  
  map_dfr(object$data, function(f)
	 f$events) %>% summary() %>%
  print()
  

  # print(eegble$data, ...)
}

