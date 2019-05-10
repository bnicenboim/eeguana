#' Get the by-sample (or by-row) mean of the specified channels.
#'
#' Wrapper of `rowMeans` that performs a by-sample mean of the specified channels.
#'
#' @param x An `eeg_lst` object.
#' @param ... A channel or a group of unquoted or quoted channels (if an `eeg_lst` is not specified).
#' @inheritParams base::mean
#' @return A new channel or an `eeg_lst` object with a `mean` channel instead of the previous channels.
#' @family channel
#'
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' faces_segs_some %>%
#'   transmute(
#'     Occipital = chs_mean(O1, O2, Oz, na.rm = TRUE),
#'     Parietal = chs_mean(P3, P4, P7, P8, Pz, na.rm = TRUE)
#'   )
#' 
#' faces_segs_some %>%
#'   chs_mean(na.rm = TRUE)
#' }
#' @export
chs_mean <- function(x, ..., na.rm = FALSE) {
  UseMethod("chs_mean")
}
#' @rdname chs_mean
#' @export
chs_mean.channel_dbl <- function(..., na.rm = FALSE) {
  dt_chs <- data.table::data.table(...)
  rowMeans_ch(dt_chs, na.rm = na.rm)
}
#' @rdname chs_mean
#' @export
chs_mean.character <- function(..., na.rm = FALSE) {
  dt_chs <- data.table::as.data.table(mget(..., envir = rlang::caller_env()))
  rowMeans_ch(dt_chs, na.rm = na.rm)
}
#' @rdname chs_mean
#' @export
chs_mean.eeg_lst <- function(x, ..., na.rm = FALSE) {
  # channels_info <- channels_tbl(x)
  signal <- data.table::copy(x$.signal)
  signal[, mean := rowMeans_ch(.SD, na.rm = na.rm), .SDcols = channel_names(x)][, `:=`(channel_names(x), NULL)]
  x$.signal <- signal
  update_events_channels(x) %>% # update_channels_tbl(channels_info) %>%
    validate_eeg_lst()
}


#' Re-reference a channel or group of channels.
#'
#' Re-reference a channel or group of channels.
#'
#' Notice that this function will update the channels one by one when used inside a mutate and not all at the same time.
#' @param .data An eeg_lst object.
#' @param ref Character vector of channels that will be averaged as the reference.
#' @inheritParams base::mean
#' @param ... Channels to include. All the channels by default, but eye channels channels should be removed.
#' @return An  eeg_lst with some channels re-referenced.
#' @export
#'
#' @family channel
#'
#' @examples
#' \dontrun{
#' # Re-reference all channels used the linked mastoids (average of the two mastoids)
#' 
#' faces_segs %>% eeg_rereference(ref = c("M1", "M2"))
#' }
#' @export
eeg_rereference <- function(.data, ..., ref = NULL, na.rm = FALSE) {
  UseMethod("eeg_rereference")
}
#' @export
eeg_rereference.eeg_lst <- function(.data, ..., ref = NULL, na.rm = FALSE) {
  signal <- data.table::copy(.data$.signal)
  sel_ch <- sel_ch(.data, ...)
  ref <- unlist(ref) # rlang::quos_auto_name(dots) %>% names()

  # ref <- rowMeans(.data$.signal[,..ref], na.rm = na.rm)
  reref <- function(x, ref_value) {
    x <- x - ref_value
    attributes(x)$.reference <- paste0(ref, collapse = ", ")
    x
  }
  # signal[, (ch_sel) := {ref= rowMeans()   ;lapply(.SD, reref, ref = ref)},.SDcols = c(ch_sel)]
  signal[, (sel_ch) := {
    ref <- rowMeans(.SD)
    lapply(mget(sel_ch, inherits = TRUE), reref, ref_value = ref)
  }, .SDcols = c(ref)]
  .data$.signal <- signal
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
#' @export
chs_fun <- function(x, .funs, ...) {
  UseMethod("chs_fun")
}
#' @rdname chs_fun
#' @param pars List that contains the additional arguments for the function calls in .funs.
#' @export
chs_fun.channel_dbl <- function(..., .funs, pars = list()) {
  .funs <- rlang::as_function(.funs)
  row_fun_ch(data.table::data.table(...), .funs, pars)
}
#' @rdname chs_fun
#' @export
chs_fun.character <- function(..., .funs, pars = list()) {
  dt_chs <- data.table::as.data.table(mget(..., envir = rlang::caller_env()))
  .funs <- rlang::as_function(.funs)
  row_fun_ch(data.table::data.table(...), .funs, pars)
}

#' @rdname chs_fun
#' @export
chs_fun.eeg_lst <- function(x, .funs, pars = list(), ...) {
  signal <- data.table::copy(x$.signal)
  if (is.list(.funs)) {
    .funs <- lapply(.funs, rlang::as_function)
  } else {
    fun_txt <- toString(substitute(.funs)) %>% make_names()
    .funs <- list(rlang::as_function(.funs))
    names(.funs) <- fun_txt
  }
  signal[, names(.funs) := lapply(.funs, function(x) row_fun_ch(.SD, x, pars)), .SDcols = channel_names(x)][, `:=`(channel_names(x), NULL)]

  x$.signal <- signal
  update_events_channels(x) %>% # update_channels_tbl(channels_info) %>%
    validate_eeg_lst()
}
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
  x$.signal <- x$.signal[, (channel_names(x)) := lapply(.SD, fun_baseline, .sample, sample_id),
    .SDcols = (channel_names(x)),
    by = .id
  ]
  x
}
#' @name ch_baseline
#' @export
ch_baseline.channel_dbl <- function(x, time = -Inf, sample_id = NULL, ...) {
  signal <- signal_from_parent_frame(env = parent.frame(1))

  if (is.null(sample_id) & is.numeric(time)) {
    sample_id <- time * attributes(signal$.sample)$sampling_rate
  } else if (is.numeric(sample_id) & is.numeric(time)) {
    message("# Ignoring time parameter.")
  }
  #
  fun_baseline(x, signal[[".sample"]], sample_id)
}


fun_baseline <- function(x, .sample, lower) {
  x - mean(x[between(.sample, lower, 0)], na.rm = TRUE)
}
