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


#' Rereference channel.
#'
#' This function is meant to be used together with `mutate` or `mutate_all`. See the example
#'
#' @param x A channel.
#' @param ... Channels that will be averaged as the reference.
#' @return A rereferenced channel.
#' @export
#'
#' @family channel
#'
#' @examples
#' \dontrun{
#' # Rereference all channels used the linked mastoids (average of the two mastoids)
#'
#' faces_segs %>% act_on(signal_tbl) %>%
#'                  mutate_all(funs(rereference(., M1, M2)))
#' }#'
ch_rereference <- function(x, ..., na.rm = FALSE) {
  x - vec_mean(..., na.rm = na.rm)
}