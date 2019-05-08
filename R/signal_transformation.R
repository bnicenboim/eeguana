#' Baseline an eeg_lst
#'
#' Subtract the average or baseline of the points in a defined interval from all points in the segment.
#'
#' @param x An `eeg_lst` object or a channel.
#' @param time A negative number indicating from when to baseline; the interval is defined as \[time,0\]. The default is to use all the negative times.
#' @param sample_id A negative number indicating from when to baseline. The default is to use all the negative times. (`time` is ignored if  sample_id is used).
#' @param ... Not in use. 
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
#' @name ch_baseline
#' @export
ch_baseline.eeg_lst <- function(x, time = -Inf, sample_id = NULL, ...) {

  if (is.null(sample_id) & is.numeric(time)) {
    sample_id <- time * sampling_rate(x)
  } else if (is.numeric(sample_id) & is.numeric(time)) {
    message("# Ignoring time parameter.")
  }
 
 x$.signal <- data.table::copy(x$.signal)
 x$.signal <- x$.signal[, (channel_names(x)) := lapply(.SD, fun_baseline, .sample, sample_id) ,
             .SDcols = (channel_names(x)),
              by = .id ]
  x
}
#' @name ch_baseline
#' @export
ch_baseline.channel_dbl <- function(x, time = -Inf, sample_id = NULL,...) {

  signal <- signal_from_parent_frame(env = parent.frame(1))

  if(is.null(sample_id) & is.numeric(time)){
   sample_id <- time * attributes(signal$.sample)$sampling_rate
  } else if( is.numeric(sample_id) & is.numeric(time)) {
    message("# Ignoring time parameter.")
  }
# 
  fun_baseline(x, signal[[".sample"]], sample_id)  
}


fun_baseline <- function(x,.sample, lower) {
  x - mean(x[between(.sample, lower, 0)],na.rm=TRUE)
}

