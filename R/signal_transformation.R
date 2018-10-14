#' Baseline an eegble
#' 
#' Subtract the average or baseline of the points in a defined interval from all points in the segment.
#'
#' @param x An \code{eegble} object.
#' @param time A negative number indicating from when to baseline; the interval is defined as [time,0]. The default is to use all the negative times.
#' @param sample_id A negative number indicating from when to baseline
#'    (TO COMPLETE). The default is to use all the negative times.
#'
#' @examples
#' @return An eegble.
#'
#' @importFrom magrittr %>%
#'
#' @export
ch_baseline <- function(x, ...) {
  UseMethod("ch_baseline")
}

#' @export
ch_baseline.eegble <- function(x, time = -Inf, sample_id = NULL) {

  # ch_baseline.eegble uses grouping by .id by default, more flexible baseline
  # should be done with ch.baseline.channel
  orig_groups <- dplyr::group_vars(x$signal)
   #if there are many groupings
  if(length(orig_groups) > 1 | 
    #or if the only one is not .id
     (length(orig_groups) == 1 & orig_groups[1] != ".id")) { 
    message("# Grouping by .id.")
  }

  if(is.null(sample_id) & is.numeric(time)){
   sample_id <- time * sampling_rate(x)
  } else if( is.numeric(sample_id) & is.numeric(time)) {
    message("# Ignoring time parameter.")
  }

  x$signal <- dplyr::group_by(x$signal, .id) %>%
     dplyr::mutate_at(
      channel_names(x),
      dplyr::funs(. - mean(.[between(.sample_id, sample_id, 0)]))
    )
  x
}


ch_baseline.channel <- function(x, time = -Inf, sample_id = NULL) {
#TODO
  # sample <- get("sample", envir = parent.frame(), inherit = TRUE)
  # sample <- rlang::env(parent.frame())$sample
  # x - mean(x[between(sample, from_sample, 0)])
}

