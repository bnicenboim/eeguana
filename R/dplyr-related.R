#' Get the by-sample (or by-row) mean of the specified channels.
#'
#' Wrapper of `rowMeans` that performs a by-sample mean of the specified channels.
#'
#' @param ... A channel or a group of channels
#' @return A new channel.
#' @family channel
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' faces_segs_some %>%
#'                transmute(Occipital = chs_mean(O1, O2, Oz, na.rm = TRUE),
#'                          Parietal = chs_mean(P3, P4, P7,  P8, Pz, na.rm = TRUE))
#'
#'
#' }
# chs_mean <- function(x, ...) {
#   UseMethod("chs_mean")
# }


## ' @export
chs_mean <- function(..., na.rm = FALSE) {
  dots <- rlang::enquos(...)

  # signal_tbl <- signal_from_parent_frame(env = parent.frame())
  # This is the environment where I can find the columns of signal_tbl
  signal_env <- rlang::env_get(env = parent.frame(), ".top_env", inherit = TRUE)
  signal_tbl <- dplyr::as_tibble(rlang::env_get_list(signal_env, rlang::env_names(signal_env)))


  rowMeans(dplyr::select(signal_tbl, !!!dots), na.rm = na.rm)
}

# #' @export
# chs_mean.eeg_lst <- function(x, na.rm = FALSE) {
# 	all_channels <- rlang::syms(channel_names(x))
# 	dplyr::transmute(x, MEAN = chs_mean.default(x = NULL,!!!all_channels, na.rm = na.rm) )
# }





#' Convert a sample point into a time point.
#'
#'
#' @param x A `sample_id` object.
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
  time <- x / scaling(sampling_rate = attributes(x)$sampling_rate, unit)
  time
}
