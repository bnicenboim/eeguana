#' Baseline an eeg_lst
#'
#' Subtract the average or baseline of the points in a defined interval from all points in the segment.
#'
#' @param x An `eeg_lst` object.
#' @param time A negative number indicating from when to baseline; the interval is defined as [time,0]. The default is to use all the negative times.
#' @param sample_id A negative number indicating from when to baseline. The default is to use all the negative times.
#'
#' @family channel
#' @return An eeg_lst.
#'
#' @importFrom magrittr %>%
#'
#' @export
ch_baseline <- function(x, ...) {
  UseMethod("ch_baseline")
}

#' @export
ch_baseline.eeg_lst <- function(x, time = -Inf, sample_id = NULL) {

  # ch_baseline.eeg_lst uses grouping by .id by default, more flexible baseline
  x <- group_by_id(x)


  if (is.null(sample_id) & is.numeric(time)) {
    sample_id <- time * sampling_rate(x)
  } else if (is.numeric(sample_id) & is.numeric(time)) {
    message("# Ignoring time parameter.")
  }

  x$signal <-
    dplyr::mutate_at(
      x$signal,
      channel_names(x),
      dplyr::funs(. - mean(.[between(.sample_id, sample_id, 0)]))
    )
  x
}
