#' Builds an eegble.
#' @export
eegble  <- function(signal, events, channels, info, segments) {
  validate_eegbl(new_eegbl(signal, events, channels, info, segments))
}

#' Test if the object is a tibble
#' This function returns  TRUE for eegbles.
#' 
#' @param x An object.
#' @return `TRUE` if the object inherits from the `eegbl` class.
#' @export
is_eegble <- function(x){
  class(x) == "eegbl"
}

#' @rdname is_eegble
#' @usage NULL
#' @export
is.eegble <- is_eegble