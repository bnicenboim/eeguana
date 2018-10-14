#'  The by-sample (or by-row) mean of the specified channels.
#'
#' Wrapper of \code{rowMeans} that performs a by-sample mean of the specified channels. 
#' It works only inside mutate* and summarize*
#' 
#' @param ... A channel or a group of channels

#' @return A new channel.

#' @family 
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' faces_segs_some %>%  
#'                transmute(Occipital = chan_means(O1, O2, Oz, na.rm = TRUE),
#'                          Parietal = chan_means(P3, P4, P7,  P8, Pz, na.rm = TRUE))
#' }

chs_mean <- function(..., na.rm = FALSE) {
	dots <- rlang::enquos(...)

	# This is the environment where I can find the columns of signal
	signal_env <- rlang::env_get(env = parent.frame(), '.top_env', inherit = TRUE)
	signal <- dplyr::as_tibble(rlang::env_get_list(signal_env, rlang::env_names(signal_env)))
	 # %>%
			  # validate_signal()
	rowMeans(dplyr::select(signal, !!!dots), na.rm = na.rm)
}




#' Convert a time point into a sample point.
#' 
#' 
#' @param x A vector of times.
#' @param unit "seconds" (or "s"), "milliseconds" (or "ms")
#' @param sampling_rate
#'
#' @return A \code{sample_id} object.
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#'
#' faces_segs_some %>% filter(between(.sample_id, as_sample_id(100, unit = "ms", sampling_rate = 500),
#'                               as_sample_id(300, unit = "ms", sampling_rate = 500)))
#' }
#' @export
as_sample_id <- function(x, unit = "seconds", sampling_rate) {
	#TODO I could check if it's been called inside the eegble and extract the sampling rate.
  x * scaling(sampling_rate, unit)
}


#' Convert a sample point into a time point.
#'
#'
#' @param x A \code{sample_id} object.
#' @param unit "seconds" (or "s"), "milliseconds" (or "ms")
#'
#' @return A vector of times.
#'
#' @importFrom magrittr %>%
#'
#' @export
as_time <- function(x, unit = "second") {
  UseMethod("as_time")
}

#' @export
as_time.sample_id <- function(x, unit = "second") {
  # add unit

  time <- x / scaling(sampling_rate = attributes(x)$sampling_rate, unit)

  attributes(time) <- NULL
  time
}




