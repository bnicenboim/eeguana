#' Builds an eeg_lst.
#' @return A valid eggble.
#' @export
eeg_lst <- function(signal, events, segments) {
  validate_eeg_lst(new_eeg_lst(signal, events, segments))
}


#' Builds a signal table.
#' @return A valid signal table.
#' @export
signal <- function(signal_matrix, ids, sample_ids, channel_info) {
  validate_signal(new_signal(signal_matrix, ids, sample_ids, channel_info))
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


#' Test if the object is an eeg_lst.
#' This function returns  TRUE for eeg_lsts.
#'
#' @param x An object.
#' @return `TRUE` if the object inherits from the `eeg_lst` class.
#' @export
is_eeg_lst <- function(x) {
  class(x) == "eeg_lst"
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
  validate_channel(new_channel(values, channel_info = list(.x =x ,.y= y ,.z =z,.reference =reference, ...)))
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

