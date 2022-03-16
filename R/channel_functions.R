#' Get the by-sample (or by-row) mean of the specified channels.
#'
#' Wrapper of `rowMeans` that performs a by-sample mean of the specified channels.
#'
#' @param x An `eeg_lst` object.
#' @param ... A group of channels, it can be used in combination with `across`.
#' @inheritParams base::mean
#' @return A new channel or an `eeg_lst` object with a `mean` channel instead of the previous channels.
#' @family channel functions
#'
#' @export
#'
#' @examples
#' \dontrun{
#' faces_segs_some %>%
#'   eeg_transmute(
#'     Occipital = chs_mean(O1, O2, Oz, na.rm = TRUE),
#'     Parietal = chs_mean(P3, P4, P7, P8, Pz, na.rm = TRUE)
#'   )
#'
#' faces_segs_some %>%
#'   eeg_transmute(
#'     Occipital = chs_mean(O1, O2, Oz, na.rm = TRUE),
#'     Parietal = chs_mean(P3, P4, P7, P8, Pz, na.rm = TRUE)
#'   )
#' faces_seg %>%
#'   eeg_transmute(
#'     Occipital = chs_mean(across(starts_with("O")), na.rm = TRUE), # O1, O2, Oz
#'     Parietal = chs_mean(across(starts_with("O")), na.rm = TRUE) # P3, P4, P7, P8, Pz
#'   )
#' faces_segs_some %>%
#'   chs_mean(na.rm = TRUE)
#' }
#' @export
chs_mean <- function(x, ..., na.rm = FALSE) {
  UseMethod("chs_mean")
}
#' @export
chs_mean.channel_dbl <- function(..., na.rm = FALSE) {
  dt_chs <- data.table::data.table(...)
  rowMeans_ch(dt_chs, na.rm = na.rm)
}

#' @export
chs_mean.data.frame <- function(..., na.rm = FALSE) {
  rowMeans_ch(..., na.rm = na.rm)
}

## This should work with tidyselect #115
## chs_mean.character <- function(..., na.rm = FALSE) {
##   dt_chs <- data.table::as.data.table(mget(..., envir = rlang::caller_env()))
##   print(dt_chs)
##   rowMeans_ch(dt_chs, na.rm = na.rm)
## }
#' @export
chs_mean.eeg_lst <- function(x, ..., na.rm = FALSE) {
  signal <- data.table:::shallow(x$.signal)
  signal[, mean := rowMeans_ch(.SD, na.rm = na.rm), .SDcols = channel_names(x)][, `:=`(channel_names(x), NULL)]
  x$.signal <- signal
  update_events_channels(x) %>%
    validate_eeg_lst()
}

#' Re-reference a channel or group of channels.
#'
#' Re-reference a channel or group of channels.
#'
#' Notice that this function will also rereference the eye electrodes unless excluded. See examples.
#'
#' @param .data An eeg_lst object.
#' @param ... Channels to include. All the channels by default, but eye channels should be removed.
#' @param .ref Channels that will be averaged as the reference. tidyselect compatible.
#' @inheritParams base::mean
#' @return An  eeg_lst with some channels re-referenced.
#' @export
#'
#' @family preprocessing functions
#'
#' @examples
#' # Re-reference all channels using the left mastoid excluding the eye electrodes.
#' data_faces_ERPs_M1 <- data_faces_ERPs %>%
#'   eeg_rereference(-EOGV, -EOGH, .ref = M1)
#'
#' # Re-reference using the linked mastoids excluding the eye electrodes.
#' data_faces_ERPs_M1M2 <- data_faces_ERPs %>%
#'   eeg_rereference(-EOGV, -EOGH, .ref = c(M1, M2))
#' @export
eeg_rereference <- function(.data, ..., .ref = NULL, na.rm = FALSE) {
  UseMethod("eeg_rereference")
}
#' @export
eeg_rereference.eeg_lst <- function(.data, ..., .ref = NULL, na.rm = FALSE) {
  chs <- sel_ch(.data, ...)
  .ref <- rlang::enquo(.ref)
  ref_v <- .data$.signal %>% select.(tidyselect::all_of(channel_names(.data))) %>% select.( !!.ref)
  ref_value <- rowMeans(ref_v)
  reref <- function(x, ref_value) {
    x <- x - ref_value
    attributes(x)$.reference <- paste0(colnames(ref_v), collapse = ", ")
    x
  }
  .data$.signal <- .data$.signal %>% mutate.(across.(tidyselect::all_of(!!chs), reref, ref_value))
  update_events_channels(.data) %>% validate_eeg_lst()
}

sel_ch <- function(data, ...) {
  dots <- rlang::enquos(...)
  if (rlang::is_empty(dots)) {
    ch_sel <- channel_names(data)
  } else {
    ch_sel <- tidyselect::vars_select(channel_names(data), !!!dots)
  }
  ch_sel
}


#' Get the by-sample (or by-row) function of the specified channels.
#'
#' @inheritParams chs_mean
#' @return A new channel or an `eeg_lst` object with a channel where a function was instead of the previous channels.
#' @inheritParams dplyr::summarize_at
#' @param x An eeg_lst.
#' @param .pars List that contains the additional arguments for the function calls in .funs.
#' @family channel functions
#' @export
chs_fun <- function(x, .funs, ..., .pars = list()) {
  UseMethod("chs_fun")
}
#' @export
chs_fun.channel_dbl <- function(..., .funs, .pars = list()) {
  .funs <- rlang::as_function(.funs)
  row_fun_ch(data.table::data.table(...), .funs, .pars)
}
#' @export
chs_fun.character <- function(..., .funs, .pars = list()) {
  dt_chs <- data.table::as.data.table(mget(..., envir = rlang::caller_env()))
  .funs <- rlang::as_function(.funs)
  row_fun_ch(data.table::data.table(...), .funs, .pars)
}

#' @export
chs_fun.eeg_lst <- function(x, .funs, .pars = list(), ...) {
  signal <- data.table::copy(x$.signal)
  if (is.list(.funs)) {
    .funs <- lapply(.funs, rlang::as_function)
  } else {
    fun_txt <- toString(substitute(.funs)) %>% make_names()
    .funs <- list(rlang::as_function(.funs))
    names(.funs) <- fun_txt
  }
  signal[, names(.funs) := lapply(.funs, function(x) row_fun_ch(.SD, x, .pars)), .SDcols = channel_names(x)][, `:=`(channel_names(x), NULL)]

  x$.signal <- signal
  update_events_channels(x) %>% # update_channels_tbl(channels_info) %>%
    validate_eeg_lst()
}
#' Baseline an eeg_lst
#'
#' Subtract the average or baseline of the points in a defined interval from all points in the segment.
#'
#' @param ... Channels to include. All channels by default.
#' @param x An `eeg_lst` object or a channel.
#' @param .lim A negative number indicating from when to baseline; the interval is defined as \[lim,0\]. The default is to use all the negative times.
#' @inheritParams eeg_artif
#' 
#' @family preprocessing functions
#' @return An eeg_lst.
#'
#'
#' @export
eeg_baseline <- function(x, ..., .lim = -Inf, .unit = "s") {
  UseMethod("eeg_baseline")
}
#' @export
eeg_baseline.eeg_lst <- function(x, ..., .lim = -Inf, .unit = "s") {
  ch_sel <- sel_ch(x, ...)
  sample_id <- as_sample_int(.lim, sampling_rate = sampling_rate(x), .unit)

  x$.signal <- data.table::copy(x$.signal)
  x$.signal <- x$.signal[, (ch_sel) := lapply(.SD, fun_baseline, .sample, sample_id),
    .SDcols = (ch_sel),
    by = .id
  ]
  x
}

#' @noRd
fun_baseline <- function(x, .sample, lower) {
  x - mean(x[between(.sample, lower, 0)], na.rm = TRUE)
}
