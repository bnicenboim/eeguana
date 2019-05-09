#' Defunct functions in eeguana
#' 
#' These functions are defunct.
#' 
#'  * 'downsample()' should be changed to 'eeg_downsample()'.
#'  * 'segment()' should be changed to 'eeg_segment()'.
#'  * 'interpolate_tbl()' should be changed to 'eeg_interpolate_tbl()'.
#'  * 'event_to_ch_NA()' should be changed to 'eeg_intervals_to_NA()'.
#'  * 'ch_filt_low_pass()' should be changed to 'eeg_filt_low_pass()'.
#'  * 'ch_filt_high_pass()' should be changed to 'eeg_filt_high_pass()'.
#'  * 'ch_filt_band_pass()' should be changed to 'eeg_filt_band_pass()'.
#'  * 'ch_filt_stop_pass()' should be changed to 'eeg_filt_band_stop()'.
#'  * 'events()' should be changed to 'events_tbl()'.
#'  * 'plot_gg()' should be changed to 'ggplot(aes(x=.time, y=.value))'.
#'  * 'summarize_all_ch(...)' should be changed to `summarize_at(channel_names(.),...)`.
#'  * 'summarize_at_ch(...)' should be changed to `summarize_at(...)`.
#' @name eeguana-defunct
NULL

#' @rdname eeguana-defunct
#' @inheritParams eeg_downsample
#' @export
downsample <- function(x, q = 2, max_sample = NULL, ...) {
  .Defunct("eeg_downsample")
}
#' @rdname eeguana-defunct
#' @inheritParams eeg_segment
#' @export
segment <- function(x, ...) {
  .Defunct("eeg_segment")
}
#' @rdname eeguana-defunct
#' @inheritParams eeg_interpolate_tbl
#' @export
interpolate_tbl <- function(.data, ...) {
  .Defunct("eeg_interpolate_tbl")
}
#' @rdname eeguana-defunct
#' @inheritParams eeg_events_to_NA
#' @export
event_to_ch_NA <- function(x, ...) {
  .Defunct("eeg_events_to_NA")
}
 
#' @rdname eeguana-defunct
#' @inheritParams eeg_events_to_NA
#' @export
eeg_intervals_to_NA <- function(x, ...) {
    .Defunct("eeg_events_to_NA")
    UseMethod("eeg_events_to_NA")
}
#' @rdname eeguana-defunct
#' @inheritParams eeg_filt_low_pass
#' @export
ch_filt_low_pass <- function(x, ...) {
    .Defunct("eeg_filt_low_pass")
}

#' @rdname eeguana-defunct
#' @inheritParams eeg_filt_high_pass
#' @export
ch_filt_high_pass <- function(x, ...) {
    .Defunct("eeg_filt_high_pass")
}

#' @rdname eeguana-defunct
#' @inheritParams eeg_filt_band_pass
#' @export
ch_filt_band_pass <- function(x, ...) {
    .Defunct("eeg_filt_band_pass")
}

#' @rdname eeguana-defunct
#' @inheritParams eeg_filt_band_stop
#' @export
ch_filt_stop_pass <- function(x, ...) {
    .Defunct("eeg_filt_band_stop")
}

#' @rdname eeguana-defunct
#' @inheritParams events_tbl 
#' @export
events <- function(x,value, ...) {
    .Defunct("events_tbl")
}
#' @rdname eeguana-defunct
#' @inheritParams events_tbl 
#' @export
`events<-` <- function(x, ...) {
    .Defunct("events_tbl<-")
}
#' @rdname eeguana-defunct
#' @export
summarize_all_ch <- function(x, ...) {
    .Defunct("summarize_at")
}
#' @rdname eeguana-defunct
#' @export
summarize_at_ch <- function(x, ...) {
    .Defunct("summarize_at")
}
#' @rdname eeguana-defunct
#' @export
summarise_all_ch <- function(x, ...) {
    .Defunct("summarize_at")
}
#' @rdname eeguana-defunct
#' @export
summarise_at_ch <- function(x, ...) {
    .Defunct("summarize_at")
}
#' @rdname eeguana-defunct
#' @export
plot_gg <- function(x, ...) {
    .Defunct("ggplot")
}
