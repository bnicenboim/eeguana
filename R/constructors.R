#' Builds an eegble.
#' @return A valid eggble.
#' @export
eegble <- function(signal, events, segments) {
  validate_eegble(new_eegble(signal, events, segments))
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
#' sample(1:100, sampling_rate = 500)
sample_n <- function(values, sampling_rate ) {
  validate_sample_n(new_sample_n(values, sampling_rate))
}

#' Test if the object is a sample
#' This function returns  TRUE for samples.
#'
#' @param x An object.
#' @return `TRUE` if the object inherits from the `sample` class.
#' @export
is_sample <- function(x) {
  class(x) == "sample"
}


#' Builds a channel.
#' @export
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

