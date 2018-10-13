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




#' Convert time to sample number.
#'
#' @param x An eegble.
#' @param t A vector of times.
#' @param unit "seconds" (or "s"), "milliseconds" (or "ms")
#'
#' @return A vector of sample numbers.
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#'
#' faces_segs_some %>% filter(between(sample, in_samples(., 100, unit = "ms"),
#'                               in_samples(., 300, unit = "ms")))
#' }
#' @export
in_samples <- function(x, t, unit = "seconds") {
  t * scaling(x, unit)
}
