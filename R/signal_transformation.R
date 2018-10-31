#' Baseline an eeg_lst
#'
#' Subtract the average or baseline of the points in a defined interval from all points in the segment.
#'
#' @param x An `eeg_lst` object.
#' @param time A negative number indicating from when to baseline; the interval is defined as [time,0]. The default is to use all the negative times.
#' @param sample_id A negative number indicating from when to baseline. The default is to use all the negative times. (`time` is ignored if  sample_id is used).
#'
#' @family channel
#' @return An eeg_lst.
#'
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#'
#' @export
ch_baseline <- function(x, ...) {
  UseMethod("ch_baseline")
}

#' @export
ch_baseline.eeg_lst <- function(x, time = -Inf, sample_id = NULL) {

  if (is.null(sample_id) & is.numeric(time)) {
    sample_id <- time * sampling_rate(x)
  } else if (is.numeric(sample_id) & is.numeric(time)) {
    message("# Ignoring time parameter.")
  }
 
 # dplyr code
  # ch_baseline.eeg_lst uses grouping by .id by default, more flexible baseline
  # x <- group_by_id(x)
  # x$signal <-
  #   dplyr::mutate_at(
  #     x$signal,
  #     channel_names(x),
  #     dplyr::funs(. - mean(.[between(.sample_id, sample_id, 0)], na.rm = TRUE))
  #   )

 
  new_signal <- data.table::copy(x$signal)
  new_signal[, (channel_names(x)) := lapply(.SD, fun_baseline, .sample_id, sample_id) ,
             .SDcols = (channel_names(x)),
              by = .id ]
  
  x$signal <- new_signal


  x
}

# TODO: it doesn't work with grouped data because of a bug of dplyr
#' @export
ch_baseline.channel_dbl <- function(x, time = -Inf, sample_id = NULL) {

  signal <- signal_from_parent_frame(env = parent.frame(1))

  if(is.null(sample_id) & is.numeric(time)){
   sample_id <- time * attributes(signal$.sample_id)$sampling_rate
  } else if( is.numeric(sample_id) & is.numeric(time)) {
    message("# Ignoring time parameter.")
  }

  print(signal$.sample_id)
  fun_baseline(x, signal[[".sample_id"]], sample_id)  
}


fun_baseline <- function(x,.sample_id, lower) {
  x - mean(x[between(.sample_id, lower, 0)],na.rm=TRUE)
}

