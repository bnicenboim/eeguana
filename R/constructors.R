#' Builds an eeg_lst.
#'
#' @param signal_tbl 
#' @param events 
#' @param segments 
#' 
#' @family eeg_lst
#'
#' @return A valid eggble.
#' @export
eeg_lst <- function(signal_tbl, events, segments) {
  validate_eeg_lst(new_eeg_lst(signal_tbl, events, segments))
}


#' Builds a signal_tbl table.
#'
#' @param signal_matrix 
#' @param ids 
#' @param sample_ids 
#' @param channel_info 
#' 
#' @family signal_tbl
#' 
#' @return A valid signal_tbl table.
#' @export
signal_tbl <- function(signal_matrix, ids, sample_ids, channel_info) {
  validate_signal_tbl(new_signal_tbl(signal_matrix, ids, sample_ids, channel_info))
}

#' Test if the object is a  signal_tbl
#' This function returns  TRUE for signals.
#'
#' @param x An object.
#' 
#' @family signal_tbl
#'
#' @return `TRUE` if the object inherits from the `signal_tbl` class.
#' @export
is_signal_tbl <- function(x) {
  class(x) == "signal_tbl"
}


#' Test if the object is an eeg_lst.
#' This function returns  TRUE for eeg_lsts.
#'
#' @param x An object.
#'
#' @return `TRUE` if the object inherits from the `eeg_lst` class.
#' 
#' @family eeg_lst
#' @export
is_eeg_lst <- function(x) {
  class(x) == "eeg_lst"
}


#' Builds a serie of sample numbers.
#'
#' @param values 
#' @param sampling_rate 
#'
#' @family sample_int
#' 
#' @export
#' @examples
#'
#' sample_int(1:100, sampling_rate = 500)
sample_int <- function(values, sampling_rate) {
  validate_sample_int(new_sample_int(values, sampling_rate))
}

#' Test if the object is a sample
#' This function returns  TRUE for samples.
#'
#' @param x An object.
#' 
#' @family sample_int
#' 
#' @return `TRUE` if the object inherits from the `sample` class.
#' @export
is_sample_int <- function(x) {
  if(class(x) == "sample_int") {
  	message("sample_id class is deprecated")
  	return(TRUE)
  }
  class(x) == "sample_int"
}


#' Builds a channel.
#'
#' @param values 
#' @param x 
#' @param y 
#' @param z 
#' @param reference 
#' @param ... 
#'
#' @family channel
#'
#' @export
#' @examples
#'
#' Cz <- channel(runif(100,-5,5))
channel <- function(values, x = NA_real_, y = NA_real_, z = NA_real_, reference = NA, ...) {
  validate_channel(new_channel(values, channel_info = list(.x = x, .y = y, .z = z, .reference = reference, ...)))
}

#' Test if the object is a channel
#' This function returns  TRUE for channels.
#'
#' @param x An object.
#'
#' @family channel
#' 
#' @return `TRUE` if the object inherits from the `sampl` class.
#' @export
is_channel <- function(x) {
  class(x) == "channel"
}
