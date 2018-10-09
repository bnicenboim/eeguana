#'  For the by sample (or by row) mean of the specified channels.
#'
#' Wrapper of rowMeans that performs a by sample mean of the specified channels. It works only inside mutate* and summarize*
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

mean_by_sample_ch <- function(..., na.rm = FALSE) {
	dots <- rlang::enquos(...)

	# This is the environment where I can find the columns of signal
	signal_env <- rlang::env_get(env = parent.frame(), '.top_env', inherit = TRUE)
	signal <- dplyr::as_tibble(rlang::env_get_list(signal_env, rlang::env_names(signal_env))) %>%
			  validate_signal()
	rowMeans(dplyr::select(signal, !!!dots), na.rm = na.rm)
}

