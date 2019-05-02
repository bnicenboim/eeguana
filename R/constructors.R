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
#' These timestamps correspond to `.initial` in the `events` table, which 
#' displays only the timestamps where logged events began.
#' 
#' The `events` table is organised into columns representing the `type` of event
#' associated with the trigger listed under `description`. The timestamp marking
#' the beginning and the end of the event is listed under `.initial` and `.final` (in samples).
#' The `.channel` column is a  linking variable only, so will generally only contain NAs, unless the 
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
#' @param signal_tbl See [signal_tbl()].
#' @param events_tbl See [events_tbl()].
#' @param segments_tbl A tibble of segment numbers and related information. 
#' 
#' @family eeg_lst
#'
#' @return A valid eeg_lst.
#' @export
eeg_lst <- function(signal_tbl = NULL, events_tbl = NULL, segments_tbl = NULL, channels_tbl = NULL) {
  if(is.null(signal_tbl) || !is_signal_tbl(signal_tbl)){  
  signal_tbl <- data.table::as.data.table(signal_tbl) 
    if(!is.null(channels_tbl)){
        data.table::set(signal_tbl,
                        ##columns with channels
                        j = channels_tbl$.channel,
                        ## columns that need to be updated with attributes
                        value=  signal_tbl[, (update_channel_meta_data(.SD, channels_tbl)),
                                           .SDcols=(channels_tbl$.channel)])
    }
  
    signal_tbl <- as_signal_tbl(signal_tbl)
  } else {
      signal_tbl <- validate_signal_tbl(signal_tbl)
  }
  if(is.null(events_tbl) || !is_events_tbl(events_tbl)){
      events_tbl <- as_events_tbl(events_tbl, sampling_rate = sampling_rate(signal_tbl))
  } else {
      events_tbl <- validate_events_tbl(events_tbl)
  }
    segments_tbl <- validate_segments(segments_tbl)
    validate_eeg_lst(new_eeg_lst(signal_tbl,
                                 events_tbl,
                                 segments_tbl),
                     recursive = FALSE)
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
`[[.eeg_lst` <- function(x,i,...) {
  if(is.character(i) ){
    if(i %in% names(x)){
      return(NextMethod())
    } else if(i %in% colnames(x$signal)){
        x <- x$signal
    } else if(i %in% colnames(x$segments)){
      x <- x$segments
    } else {
      warning("`[[` can only be used with elements of the signal and segments table.")
      return(NULL)
    }
  # attrs <- attributes(x)
  # r <- NextMethod("[[")
  # mostattributes(r) <- attrs
  # print(r)
  return(x[[i]])
  } else if(is.numeric(i)){
   return(dplyr::filter(x, .id == i) )
  }
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

#' wrapper for signal::decimate that allows a vector in q, for decimating several times serially
#' When using IIR downsampling, it is recommended to call decimate multiple times for downsampling factors higher than 13. reference: https://docs.scipy.org/doc/scipy/reference/generated/scipy.signal.decimate.html
#' @noRd
decimate_ch <- function (.channel, q, n = if (ftype == "iir") 8 else 30, ftype = "iir") {
    attrs <- attributes(.channel)
    class(.channel) <- NULL
    if(length(q)>1){
       r<- Reduce(function(x,q) signal::decimate(x=x,q=q, n=n,ftype = ftype), x = q, init = .channel)
    } else {
        r <- signal::decimate(x =.channel, q=q, n = n , ftype = ftype)
    }
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

