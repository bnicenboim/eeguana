#' Deprecated functions in eeguana
#' 
#' These functions still work but will be removed (defunct) in the next version.
#' 
#'  * 'downsample()' should be changed to 'eeg_downsample()' 
#'  * 'segment()' should be changed to 'eeg_segment()' 
#'  * 'interpolate_tbl()' should be changed to 'eeg_interpolate_tbl()' 
#'  * 'event_to_ch_NA()' should be changed to 'eeg_intervals_to_NA()' 
#' @name eeguana-deprecated
NULL

#' @rdname eeguana-deprecated
#' @inheritParams eeg_downsample
#' @export
downsample <- function(x, q = 2, max_sample = NULL, ...) {
  .Deprecated("eeg_downsample")
  UseMethod("eeg_downsample")
}

#' @rdname eeguana-deprecated
#' @inheritParams eeg_segment
#' @export
segment <- function(x, ...) {
  .Deprecated("eeg_segment")
  UseMethod("eeg_segment")
}
#' @rdname eeguana-deprecated
#' @inheritParams eeg_interpolate_tbl
#' @export
interpolate_tbl <- function(.data, ...) {
  .Deprecated("eeg_interpolate_tbl")
    UseMethod("eeg_interpolate_tbl")
}
#' @rdname eeguana-deprecated
#' @inheritParams eeg_events_to_NA
#' @export
event_to_ch_NA <- function(x, ...) {
  .Deprecated("eeg_events_to_NA")
    UseMethod("eeg_events_to_NA")
}
 
#' @rdname eeguana-deprecated
#' @inheritParams eeg_events_to_NA
#' @export
eeg_intervals_to_NA <- function(x, ...) {
    .Deprecated("eeg_events_to_NA")
    UseMethod("eeg_events_to_NA")
}
#' @rdname eeguana-deprecated
#' @inheritParams eeg_filt_low_pass
#' @export
ch_filt_low_pass <- function(x, ...) {
    .Deprecated("eeg_filt_low_pass")
    UseMethod("eeg_filt_low_pass")
}

#' @rdname eeguana-deprecated
#' @inheritParams eeg_filt_high_pass
#' @export
ch_filt_high_pass <- function(x, ...) {
    .Deprecated("eeg_filt_high_pass")
    UseMethod("eeg_filt_high_pass")
}

#' @rdname eeguana-deprecated
#' @inheritParams eeg_filt_band_pass
#' @export
ch_filt_band_pass <- function(x, ...) {
    .Deprecated("eeg_filt_band_pass")
    UseMethod("eeg_filt_band_pass")
}

#' @rdname eeguana-deprecated
#' @inheritParams eeg_filt_stop_pass
#' @export
ch_filt_stop_pass <- function(x, ...) {
    .Deprecated("eeg_filt_stop_pass")
    UseMethod("eeg_filt_stop_pass")
}

#' @rdname eeguana-deprecated
#' @inheritParams events_tbl 
#' @export
events <- function(x, ...) {
    .Deprecated("events_tbl")
    UseMethod("events_tbl")
}

#' @rdname eeguana-deprecated
#' @inheritParams events_tbl 
#' @export
`events<-` <- function(x, ...) {
    .Deprecated("events_tbl<-")
    UseMethod("events_tbl<-")
}
