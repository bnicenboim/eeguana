#' Builds an eeg_lst.
#'
#' @param signal signal
#' @param events events
#' @param segments segments
#' 
#' @family eeg_lst
#'
#' @return A valid eggble.
#' @export
eeg_lst <- function(signal = NULL, events = NULL, segments = NULL) {
  if(is.null(signal)) {
    signal <- data.table::data.table(.id= integer(0),.sample_id= integer(0))
    data.table::setkey(signal,.id,.sample_id)
  }
  if(!data.table::is.data.table(signal) && is.data.frame(signal)) {
   signal <- data.table::as.data.table(signal)
   data.table::setkey(signal,.id,.sample_id)
 }
  if(is.null(events)) {
    events <- data.table::data.table(.id= integer(0),.sample_0= integer(0),.size= integer(0),.channel= integer(0))
  }
  if(!data.table::is.data.table(events) && is.data.frame(events)) {
   events <- data.table::as.data.table(events)
 }
 if(is.null(segments)) {
    segments <- dplyr::tibble(.id = integer(0))
  }
  validate_eeg_lst(new_eeg_lst(signal, events, segments))
}


#' Builds a signal_tbl table.
#'
#' @param signal_matrix Matrix or table of channels with their signal.
#' @param ids Integers indicating to which group the row of the signal matrix belongs.
#' @param sample_ids Vector of integers.
#' @param channel_info A table with information about each channel (such as the one produced by `channels_tbl``)
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
  "signal_tbl" %in% class(x) 
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
#' @param values Sequence of integers.
#' @param sampling_rate Double indicating the sampling rate in Hz.
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
#' @param values Vector of doubles indicating amplitudes.
#' @param x Position in the scalp.
#' @param y Position in the scalp.
#' @param z Position in the scalp.
#' @param reference Reference electrode.
#' @inheritParams base::mean
#'
#' @family channel
#'
#' @export
#' @examples
#'
#' Cz <- channel_dbl(runif(100,-5,5))
channel_dbl <- function(values, x = NA_real_, y = NA_real_, z = NA_real_, reference = NA, ...) {
  validate_channel_dbl(new_channel_dbl(values, channel_info = list(.x = x, .y = y, .z = z, .reference = reference, ...)))
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
is_channel_dbl <- function(x) {
   if(class(x) == "channel") {
    message("channel class is deprecated")
    return(TRUE)
  }
  class(x) == "channel_dbl"
}


#' @export
`[.channel_dbl` <- function(x,i,...) {
  attrs <- attributes(x)
  class(x) <- NULL
  r <- NextMethod("[")
  mostattributes(r) <- attrs
  r
}

#' @export
`[[.channel_dbl` <- function(x,i,...) {
  attrs <- attributes(x)
  r <- NextMethod("[[")
  mostattributes(r) <- attrs
  r
}

#' @export
mean.channel_dbl <- function(x,...) {
  attrs <- attributes(x)
  class(x) <- NULL
  r <- NextMethod("mean")
  mostattributes(r) <-attrs
  r
}



#' @export
subset.channel_dbl <- function(x, ... ) {
  attrs <- attributes(x)
  class(x) <- NULL
  r <- NextMethod("subset")
  mostattributes(r) <- attrs
 r
}
