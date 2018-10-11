#' Builds an eegble.
#' @return A valid eggble.
#' @export
eegble <- function(signal, events, segments) {
  validate_eegble(new_eegble(signal, events, segments))
}


#' Builds a signal table.
#' @return A valid signal table.
#' @export
signal <- function(signal_matrix, ids, sample_ids) {
  validate_signal(new_signal(signal_matrix, ids, sample_ids))
}

#' Test if the object is a  signal
#' This function returns  TRUE for signals.
#'
#' @param x An object.
#' @return `TRUE` if the object inherits from the `signal_tbl` class.
#' @export
is_signal <- function(x) {
  class(x) == "signal_tbl"
}


#' Test if the object is an eegble.
#' This function returns  TRUE for eegbles.
#'
#' @param x An object.
#' @return `TRUE` if the object inherits from the `eegble` class.
#' @export
is_eegble <- function(x) {
  class(x) == "eegble"
}


#' Builds a serie of sample numbers.
#' @export
#' @examples
#' 
#' sample_id(1:100, sampling_rate = 500)
sample_id <- function(values, sampling_rate ) {
  validate_sample_id(new_sample_id(values, sampling_rate))
}

#' Test if the object is a sample
#' This function returns  TRUE for samples.
#'
#' @param x An object.
#' @return `TRUE` if the object inherits from the `sample` class.
#' @export
is_sample_id <- function(x) {
  class(x) == "sample_id"
}


#' Builds a channel.
#' @export
#' @examples
#' 
#' Cz <- channel(runif(100,-5,5))
channel <- function(values, x = NA_real_ ,y = NA_real_,z = NA_real_,reference = NA, ...) {
  validate_channel(new_channel(values,x,y,z,reference, ...))
}

#' Test if the object is a channel
#' This function returns  TRUE for channels.
#'
#' @param x An object.
#' @return `TRUE` if the object inherits from the `sampl` class.
#' @export
is_channel <- function(x) {
  class(x) == "channel"
}

