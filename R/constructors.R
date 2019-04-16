#' Create an eeg_lst
#' 
#' Builds an eeg_lst object composed of two `data.table::data.table` 
#' objects and one  `tibble::tibble`. All three are linked by a unique
#' identifier `.id`. Amplitude values and timestamps appear in the `signal`
#' table. Triggers, blinks, artefact rejection markings, and other
#' events logged by the EEG recording software appear in the `events` table. 
#' Segment information and recording IDs appear in the `segments` tibble. 
#' 
#' The `signal` table is organised into columns representing timestamps 
#' (`.sample_id`) and individual electrodes. Each `.sample_id` corresponds to
#' 1 sample in the original recording, i.e. if the sampling rate of the EEG
#' recording is 500 Hz, then each `.sample_id` corresponds to 2 milliseconds. 
#' These timestamps correspond to `.sample_0` in the `events` table, which 
#' displays only the timestamps where logged events began.
#' 
#' The `events` table is organised into columns representing the `type` of event
#' associated with the trigger listed under `description`. The timestamp marking
#' the beginning of the event is listed under `.sample_0` and the length of the
#' event (in timestamps) is listed under `.size`. The `.channel` column will 
#' generally only contain NAs, unless the 
#' event is specific to a certain channel.
#' 
#' The `segments` tibble contains the subject ID under `recording`, which is 
#' the file name unless otherwise specified. If the data has been segmented in 
#' BrainVision, the segment number will be listed under `segment`. The data can
#' also be segmented according to trigger labels in `eeguana`, see `segment`. 
#' `segment` will be place the segment number under `segment`, the trigger name 
#' under `type.x`, and the trigger label under `description.x`. Other information 
#' such as condition labels or response times can be added by the user by merging
#' into the `segments` tibble using non-eeguana merge functions, e.g. the `dplyr`
#' join series.
#'
#' @param signal See [signal_tbl()].
#' @param events See [events_tbl()].
#' @param segments A tibble of segment numbers and related information. 
#' 
#' @family eeg_lst
#'
#' @return A valid eeg_lst.
#' @export
eeg_lst <- function(signal = NULL, events = NULL, segments = NULL) {
  validate_eeg_lst(new_eeg_lst(signal, events, segments))
}


#' Builds a signal_tbl table
#' 
#' The eeg_lst `signal` table is organised into columns representing timestamps 
#' (`.sample_id`) and individual electrodes. Each `.sample_id` corresponds to
#' 1 sample in the original recording, i.e. if the sampling rate of the EEG
#' recording is 500 Hz, then each `.sample_id` corresponds to 2 milliseconds. 
#' These timestamps correspond to `.sample_0` in the `events` table, which 
#' displays only the timestamps where logged events began.
#'
#' @param signal_matrix Matrix or table of channels with their signal.
#' @param ids Integers indicating to which group the row of the signal matrix belongs.
#' @param sample_ids Vector of integers.
#' @param channel_info A table with information about each channel (such as the one produced by `channels_tbl``)
#' 
#' @family signal_tbl
#' 
#' @return A valid `signal_tbl` table.
#' @export
signal_tbl <- function(signal_matrix = NULL, ids=NULL, sample_ids=NULL, channel_info=NULL) {
  validate_signal_tbl(new_signal_tbl(signal_matrix, ids, sample_ids, channel_info))
}

#' Builds an events_tbl table
#' 
#' The eeg_lst `events` table is organised into columns representing the `type` of event
#' associated with the trigger listed under `description`. The timestamp marking
#' the beginning of the event is listed under `.sample_0` and the length of the
#' event (in timestamps) is listed under `.size`. The `.channel` column is a
#' linking variable only, so will generally only contain NAs, unless the 
#' event is specific to a certain channel.
#'
#' @param ids Integers indicating to which group the row of the signal matrix belongs.
#' @param sample_0s Vector of integers that indicate at which sample an events starts.
#' @param sizes Vector of integers that indicate at which sample an events starts.
#' @param channels Vector of characters that indicate to which channel the event is relevant or NA for all the channels.
#' 
#' @family events_tbl
#' 
#' @return A valid `events_tbl` table.
#' @export
events_tbl <- function(.id = NULL, .sample_0=NULL, .size=NULL, .channel=NULL, descriptions_dt=NULL) {
    validate_events_tbl(new_events_tbl(.id, .sample_0, .size, .channel, descriptions_dt))
}

#' @export
as_events_tbl <- function(.data,...){
    UseMethod("as_events_tbl")
}

#' @export
as_events_tbl.data.table <- function(.data){
    class(.data) <- c("events_tbl",class(.data))
    validate_events_tbl(.data)
}

#' @export
as_events_tbl.events_tbl <- function(.data){
    validate_events_tbl(.data)
}


#' @export
as_events_tbl.data.frame <- function(.data){
    .data <- data.table::as.data.table(.data)
    class(.data) <- c("events_tbl",class(.data))
    validate_events_tbl(.data)
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

#' Test if the object is an events_tbl 
#' This function returns  TRUE for events_tbl.
#'
#' @param x An object.
#' 
#' @family events_tbl
#'
#' @return `TRUE` if the object inherits from the `events_tbl` class.
#' @export
is_events_tbl <- function(x) {
    "events_tbl" %in% class(x) 
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
    "eeg_lst" %in% class(x)
}


#' Builds a series of sample numbers.
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

#' Builds a component.
#'
#' @param values Vector of doubles indicating amplitudes.
#'
#' @family component
#'
#' @export
#' @examples
#'
#' Cz <- component_dbl(runif(100,-5))
component_dbl <- function(values) {
  validate_component_dbl(new_component_dbl(values))
}

#' Test if the object is a component
#' This function returns  TRUE for components.
#'
#' @param x An object.
#' 
#' @family component
#' 
#' @return `TRUE` if the object inherits from the `sample_id` class.
#' @export
is_component_dbl <- function(x) {
  class(x) == "component_dbl"
}


#' @export
`[.component_dbl` <- function(x,i,...) {
  attrs <- attributes(x)
  class(x) <- NULL
  r <- NextMethod("[")
  mostattributes(r) <- attrs
  r
}

#' @export
`[[.component_dbl` <- function(x,i,...) {
  attrs <- attributes(x)
  r <- NextMethod("[[")
  mostattributes(r) <- attrs
  r
}

#' @export
mean.component_dbl <- function(x,...) {
  attrs <- attributes(x)
  class(x) <- NULL
  r <- NextMethod("mean")
  mostattributes(r) <-attrs
  r
}



#' @export
subset.component_dbl <- function(x, ... ) {
  attrs <- attributes(x)
  class(x) <- NULL
  r <- NextMethod("subset")
  mostattributes(r) <- attrs
  r
}
#' Builds an eeg_lst.
#'
#' @param signal signal
#' @param events events
#' @param segments segments
#' 
#' @family eeg_lst
#'
#' @return A valid eeg_lst.
#' @export
ica_lst <- function(signal = NULL, mixing = NULL, events = NULL, segments = NULL) {
    validate_ica_lst(new_ica_lst(signal, mixing, events, segments))
}

#' Builds a mixing_tbl table.
#'
#' @param mixing_matrix Matrix or table of channels with their mixing.
#' @param ids Integers indicating to which group the row of the mixing matrix belongs.
#' @param sample_ids Vector of integers.
#' @param channel_info A table with information about each channel (such as the one produced by `channels_tbl``)
#' 
#' @family mixing_tbl
#' 
#' @return A valid mixing_tbl table.
#' @export
mixing_tbl <- function(mixing_matrix,means_matrix, groups, channel_info) {
    validate_mixing_tbl(new_mixing_tbl(mixing_matrix = mixing_matrix,
                                       means_matrix = means_matrix,
                                       groups = groups,
                                       channel_info =channel_info))
}
