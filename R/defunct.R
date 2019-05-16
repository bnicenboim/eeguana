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
#'  * 'ch_baseline()' should be changed to 'eeg_baseline()'.
#'  * 'events()' should be changed to 'events_tbl()'.
#'  * 'plot_gg()' should be changed to 'ggplot(aes(x=.time, y=.value))'.
#'  * 'summarize_all_ch(...)' should be changed to `summarize_at(channel_names(.),...)`.
#'  * 'summarize_at_ch(...)' should be changed to `summarize_at(...)`.
#' @name defunct
NULL

#' @rdname defunct
#' @inheritParams eeg_downsample
#' @keywords internal
#' @export
downsample <- function(x, q = 2, max_sample = NULL, ...) {
  .Defunct("eeg_downsample")
}
#' @rdname defunct
#' @inheritParams eeg_segment
#' @keywords internal
#' @export
segment <- function(x, ...) {
  .Defunct("eeg_segment")
}
#' @rdname defunct
#' @inheritParams eeg_interpolate_tbl
#' @keywords internal
#' @export
interpolate_tbl <- function(.data, ...) {
  .Defunct("eeg_interpolate_tbl")
}
#' @rdname defunct
#' @inheritParams eeg_events_to_NA
#' @keywords internal
#' @export
event_to_ch_NA <- function(x, ...) {
  .Defunct("eeg_events_to_NA")
}

#' @rdname defunct
#' @inheritParams eeg_events_to_NA
#' @keywords internal
#' @export
eeg_intervals_to_NA <- function(x, ...) {
  .Defunct("eeg_events_to_NA")
  UseMethod("eeg_events_to_NA")
}
#' @rdname defunct
#' @inheritParams eeg_filt_low_pass
#' @keywords internal
#' @export
ch_filt_low_pass <- function(x, ...) {
  .Defunct("eeg_filt_low_pass")
}

#' @rdname defunct
#' @inheritParams eeg_filt_high_pass
#' @keywords internal
#' @export
ch_filt_high_pass <- function(x, ...) {
  .Defunct("eeg_filt_high_pass")
}

#' @rdname defunct
#' @inheritParams eeg_filt_band_pass
#' @keywords internal
#' @export
ch_filt_band_pass <- function(x, ...) {
  .Defunct("eeg_filt_band_pass")
}

#' @rdname defunct
#' @inheritParams eeg_filt_band_stop
#' @keywords internal
#' @export
ch_filt_stop_pass <- function(x, ...) {
  .Defunct("eeg_filt_band_stop")
}

#' @rdname defunct
#' @inheritParams events_tbl
#' @keywords internal
#' @export
events <- function(x, ...) {
  .Defunct("events_tbl")
}
#' @rdname defunct
#' @inheritParams events_tbl
#' @keywords internal
#' @export
`events<-` <- function(x, value) {
  .Defunct("events_tbl<-")
}
#' @rdname defunct
#' @keywords internal
#' @export
summarize_all_ch <- function(x, ...) {
  .Defunct("summarize_at")
}
#' @rdname defunct
#' @keywords internal
#' @export
summarize_at_ch <- function(x, ...) {
  .Defunct("summarize_at")
}
#' @rdname defunct
#' @keywords internal
#' @export
summarise_all_ch <- function(x, ...) {
  .Defunct("summarize_at")
}
#' @rdname defunct
#' @keywords internal
#' @export
summarise_at_ch <- function(x, ...) {
  .Defunct("summarize_at")
}
#' @rdname defunct
#' @keywords internal
#' @export
plot_gg <- function(x, ...) {
  .Defunct("ggplot")
}
#' @rdname defunct
#' @keywords internal
#' @export
ch_baseline <- function(x, ...) {
  .Defunct("eeg_baseline")
}