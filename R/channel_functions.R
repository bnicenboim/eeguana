#' Get the by-sample (or by-row) mean of the specified channels.
#'
#' Wrapper of `rowMeans` that performs a by-sample mean of the specified channels.
#'
#' @param ... A channel or a group of channels, or an `eeg_lst` object.
#' @param na.rm 
#' @return A new channel or an `eeg_lst` object with a `mean` channel instead of the previous channels.
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
#' faces_segs_some %>%
#'                chs_mean(na.rm = TRUE)
#' }
#' @export
chs_mean <- function(x, ...,na.rm= FALSE) {
  UseMethod("chs_mean")
}


#' @export
chs_mean.channel_dbl <- function(..., na.rm = FALSE) {
  dots <- rlang::quos(...)

  # # signal_tbl <- signal_from_parent_frame(env = parent.frame())
  # # This is the environment where I can find the columns of signal_tbl
  # signal_env <- rlang::env_get(env = parent.frame(), ".top_env", inherit = TRUE)
  # signal_tbl <- dplyr::as_tibble(rlang::env_get_list(signal_env, rlang::env_names(signal_env)))

 # https://stackoverflow.com/questions/17133522/invalid-internal-selfref-in-data-table

	 rowMeans(data.table::data.table(...), na.rm = na.rm)  # throws a warning

  # rowMeans(copy(data.table::data.table(...)), na.rm = na.rm)  # throws a warning
  # rowMeans(.SD, na.rm = na.rm), .SDcols = cols # should be the way, but it's hard to implement it in my template

  # rowMeans(dplyr::select(signal_tbl, !!!dots), na.rm = na.rm)
}

#' @export
chs_mean.eeg_lst <- function(x, na.rm = FALSE) {
  channels_info <- channels_tbl(x)
  signal <- data.table::copy(x$signal)
  signal[,mean := rowMeans(.SD, na.rm = na.rm),.SDcols = channel_names(x)][,`:=`(channel_names(x), NULL)]
  x$signal <- signal
  update_events_channels(x) %>% update_channels_tbl(channels_info) %>%
      validate_eeg_lst()
}


#' Rereference a channel or group of channels.
#'
#' Rereference a channel or group of channels.
#'
#' Notice that this function will update the channels one by one when used inside a mutate and not all at the same time.
#' 
#' @param x A channel.
#' @param ... Channels that will be averaged as the reference.
#' @param na.rm 
#' @return A rereferenced channel or an eeg_lst with all channels re-referenced.
#' @export
#'
#' @family channel
#'
#' @examples
#' \dontrun{
#' # Rereference all channels used the linked mastoids (average of the two mastoids)
#'
#' faces_segs %>% ch_rereference(M1, M2)
#' }
#' @export
ch_rereference <- function(x, ...,na.rm= FALSE) {
  UseMethod("ch_rereference")
}

#' @export
ch_rereference.channel_dbl <- function(x, ..., na.rm = FALSE) {
  x - rowMeans(data.table::data.table(...), na.rm = na.rm)
}

#' @export
ch_rereference.eeg_lst <- function(x,..., na.rm = FALSE) {
  channels_info <- channels_tbl(x)
  signal <- data.table::copy(x$signal)
  dots <- rlang::enquos(...)
  cols <- rlang::quos_auto_name(dots) %>% names()
  ref <- rowMeans(x$signal[,..cols], na.rm = na.rm)

  signal[, (channel_names(x)) := purrr::map(.SD, ~ .x - ref),.SDcols = channel_names(x)]
  x$signal <- signal
  update_events_channels(x) %>% update_channels_tbl(channels_info) %>%
      validate_eeg_lst()

}
