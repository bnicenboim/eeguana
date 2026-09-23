#' Display information of the eeg_lst object.
#'
#' * `*_names()` functions return a vector of names, 
#' * `n*()` return the number of elements channels or components. 
#' 
#' Components are only available after running [eeg_ica()]. `channel_ica_names()` refers to channels used in the ICA.
#'
#' @param x An eeg_lst object.
#' @param ... Not in use.
#'
#' @family summary functions
#'
#' @name summaries
NULL
# > NULL
#' @rdname summaries
#' @export
channel_names <- function(x, ...) {
  UseMethod("channel_names")
}
#' @export
channel_names.signal_tbl <- function(x, ...) {
  NextMethod()
}
#' @export
channel_names.data.table <- function(x, ...) {
  colnames(x)[x[, map_lgl(.SD, is_channel_dbl)]]
}
#' @export
channel_names.eeg_lst <- function(x, ...) {
  channel_names(x$.signal)
}
#' @export
channel_names.psd_lst <- function(x, ...) {
  channel_names(x$.psd)
}
#' @rdname summaries
#' @export
channel_ica_names <- function(x, ...) {
  UseMethod("channel_ica_names")
}
#' @export
channel_ica_names.eeg_ica_lst <- function(x, ...) {
  map(x$.ica, function(recording) {
    recording$unmixing_matrix %>%
      rownames()
  }) %>%
    unlist() %>%
    unique()
}
#' @export
channel_ica_names.eeg_lst <- function(x, ...) {
  NULL
}
#' @rdname summaries
#' @export
nchannels <- function(x, ...) {
  UseMethod("nchannels")
}
#' @export
nchannels.default <- function(x, ...) {
  channel_names(x) %>% length()
}

#' @rdname summaries
#' @export
component_names <- function(x, ...) {
  UseMethod("component_names")
}
#' @export
component_names.eeg_ica_lst <- function(x, ...) {
  map(x$.ica, function(recording) {
    recording$unmixing_matrix %>%
      colnames()
  }) %>%
    unlist() %>%
    unique()
}
#' @export
component_names.default <- function(x, ...) {
  stop("Component names can only be extracted after running `eeg_ica()` on an eeg_lst object.", call. = FALSE)
}
#' @rdname summaries
#' @export
ncomponents <- function(x, ...) {
  UseMethod("ncomponents")
}
#' @export
ncomponents.eeg_lst <- function(x, ...) {
  component_names(x) %>% length()
}

#' @rdname summaries
#' @export
sampling_rate <- function(x, ...) {
  UseMethod("sampling_rate")
}

#' @export
sampling_rate.eeg_lst <- function(x,...) {
  attributes(x$.signal$.sample)$sampling_rate
}
#' @export
sampling_rate.signal_tbl <- function(x,...) {
  attributes(x$.sample)$sampling_rate
}
#' @export
sampling_rate.events_tbl <- function(x,...) {
  attributes(x$.initial)$sampling_rate
}
#' @export
sampling_rate.sample_int <- function(x,...) {
  attributes(x)$sampling_rate
}

duration <- function(x) {
  x$.signal %>%
    tidytable::summarize(
      duration = (max(.sample) - min(.sample) + 1) / sampling_rate(x),
      .by = .id, .sort = TRUE
    ) %>%
    .$duration
}
#' @rdname summaries
#' @export
nsamples <- function(x, ...) {
  UseMethod("nsamples")
}
#' @export
nsamples.eeg_lst <- function(x, ...) {
  x$.signal[, max(.sample) - min(.sample) + 1, by = .id]$V1
}
#' Summary of eeg_lst information.
#'
#' It's also possible to extract the elements by accessing it as a list.
#'
#' @family summary functions
#' @param object An eeg_lst object.
#' @inheritParams base::summary
#'
#' @examples
#'
#' summary(data_faces_ERPs)
#' @export
summary.eeg_lst <- function(object, ...) {
  # to avoid no visible binding for global variable
  incomplete <- NULL

  segments_with_incomp_col <- object %>%
    signal_tbl() %>%
    tidytable::select(-.sample) %>%
    split(by = ".id", keep.by = FALSE) %>%
    lapply(anyNA) %>%
    unlist() %>%
    tidytable::tidytable(".id" = as.integer(names(.)), "incomplete" = .) %>%
    tidytable::left_join(object$.segments, by = ".id")


  ## everything in .events except the four columns that vary within an event
  event_by_cols <- setdiff(
    colnames(object$.events), c(".final", ".channel", ".initial", ".id")
  )

  summ <- list(
    sampling_rate = sampling_rate(object),
    segments = segments_with_incomp_col %>%
      tidytable::summarize(
        n_segments = tidytable::n(), n_incomplete = sum(incomplete),
        .by = .recording, .sort = TRUE
      ) %>%
      data.table::data.table(),
    events = object$.events %>%
      tidytable::summarize(
        n = tidytable::n(),
        .by = tidyselect::all_of(event_by_cols), .sort = TRUE
      ) %>%
      data.table::data.table(),
    size = utils::capture.output(print(utils::object.size(object), units = "auto")),
    duration = format(.POSIXct(nrow(object$.signal) / sampling_rate(object), tz = "GMT"), "%H:%M:%S")
  )
  class(summ) <- c("eeg_summary", class(summ))
  summ
}


#' @export
summary.eeg_ica_lst <- function(object, ...) {
  summ <- NextMethod()
  class(summ) <- c("ica_summary", class(summ))
  summ
}


#' Show correlations between ICA sources and eye (EOG) electrodes.
#'
#' @param .data An `eeg_ica_lst` object
#' @param ... If left empty, the function will assume that EOG channels include eog/EOG in their names, alternatively, EOG channels can be selected here.
#' @family ICA functions
#' @family summary functions
#'
#' @return A table with the correlations between each component and each EOG channel in each recording.
#' @export
eeg_ica_cor_tbl <- function(.data, ...) {
  UseMethod("eeg_ica_cor_tbl")
}

#' @export
eeg_ica_cor_tbl.eeg_ica_lst <- function(.data, ...) {
  if (length(rlang::enquos(...)) == 0) {
    eogs <- # sel_ch(.data, c(tidyselect::starts_with("eog"), tidyselect::ends_with("eog")))
      tidyselect::vars_select(channel_names(.data), c(tidyselect::starts_with("eog"), tidyselect::ends_with("eog")))
    message_verbose("EOG channels detected as: ", toString(eogs))
  } else {
    eogs <- sel_ch(.data, ...)
  }

  names(eogs) <- eogs
  comps <- .data %>%
    eeg_ica_show(component_names(.data)) %>%
    eeg_select(tidyselect::all_of(eogs), component_names(.data))
  signal <- extended_signal(comps, ".recording")

  # new cols:
  cor <- NULL


  dt_cor <- lapply(eogs, function(eog) {
    signal[, lapply(.SD, function(ica) {
      stats::cor(x = ica, y = comps$.signal[[eog]], use = "complete")
    }), .SDcols = component_names(comps), by = .recording] %>%
      data.table::melt(
        variable.name = ".ICA",
        id.vars = c(".recording"),
        value.name = "cor"
      )
  }) %>% data.table::rbindlist(idcol = "EOG")
  data.table::setcolorder(dt_cor, c(".recording", "EOG", ".ICA", "cor"))
  # data.table::setorder(dt_cor, .recording, -abs(cor))
  dt_cor[order(.recording, -abs(cor))]
  # split(dt_cor, keep.by = FALSE, by=c(".recording", "EOG"))
}
#' Show the variance explained for each ICA sources.
#'
#' This function shows the proportion of the variance of the channels that each
#' ICA component explains, as the function `eeg_pvaf()` of EEGLAB does: one minus
#' the variance left in the channels after removing everything but that
#' component, divided by the variance of the channels, where the variance of
#' several channels is the mean of their variances.
#'
#' If the components are uncorrelated, as the ones found with fastICA in the
#' data they were fitted to, the proportions of each recording add up to one.
#'
#' @param .data An `eeg_ica_lst` object
#' @param .max_sample The maximum number of samples to use for calculating the
#'   variance explained. `NULL`, the default, uses all of them; a number
#'   downsamples the data to about that many samples first.
#' @param ... Not in use.
#' @family ICA functions
#' @family summary functions
#'
#' @return A table with the variance explained by each component in each recording.
#
#' @export
eeg_ica_var_tbl <- function(.data, ..., .max_sample = NULL) {
  UseMethod("eeg_ica_var_tbl")
}

#' @export
eeg_ica_var_tbl.eeg_ica_lst <- function(.data, ..., .max_sample = NULL) {
  # to avoid no visible global function definition
  var <- NULL

  .data <- try_to_downsample(.data, max_sample = .max_sample)
  signal <- extended_signal(.data, ".recording") %>%
    split(by = ".recording", keep.by = FALSE)

  ## Keeping only component k leaves the residual X_c - s_k a_kc in each
  ## channel c, where s_k = X w_k are the centered activations, w_k is column k
  ## of the unmixing matrix, and a_kc is an entry of the mixing matrix. Its
  ## variance is var(X_c) - 2 a_kc cov(X_c, s_k) + a_kc^2 var(s_k), so every
  ## component is computed from the activations, without reconstructing the
  ## signal once per component. The covariances are computed from the
  ## activations rather than as w_k' Sigma w_k: with rank-deficient channels,
  ## such as average-referenced ones, the unmixing weights are large and
  ## the product loses digits.
  vars <- map_dtr(names(.data$.ica), function(rec) {
    ica <- .data$.ica[[rec]]
    chs <- rownames(ica$unmixing_matrix)
    X <- scale(as.matrix(signal[[rec]][, chs, with = FALSE]), scale = FALSE)
    S <- X %*% ica$unmixing_matrix
    n1 <- nrow(X) - 1
    B <- crossprod(X, S) / n1
    s <- colSums(S^2) / n1
    A <- ica$mixing_matrix[, chs, drop = FALSE]
    m_var <- mean(colSums(X^2) / n1)
    m_res <- m_var - 2 * rowMeans(A * t(B)) + s * rowMeans(A^2)
    data.table::data.table(
      .recording = rec,
      .ICA = colnames(ica$unmixing_matrix),
      var = 1 - m_res / m_var
    )
  })
  vars[order(.recording, -var)]
}

#' Show a table with a summary of the results of the ICA.
#'
#' This function generates a table with the variance explained by each ICA component, and the correlations
#' between ICA components and EOG channels. See more details in [eeg_ica_cor_tbl] and [eeg_ica_var_tbl].
#'
#'
#' @param .data An `eeg_ica_lst` object
#' @inheritParams eeg_ica_cor_tbl
#' @inheritParams eeg_ica_var_tbl
#' @family ICA functions
#' @family summary functions
#'
#' @return A table with the variance explained by each component, and the correlation between EOG channels and each components in each recording.
##' @export
eeg_ica_summary_tbl <- function(.data, ...) {
  UseMethod("eeg_ica_summary_tbl")
}

#' @export
eeg_ica_summary_tbl.eeg_ica_lst <- function(.data, ..., .max_sample = NULL) {
  # to avoid no visible global function definition
  var <- NULL
  cor <- NULL

  summ <- left_join_dt(eeg_ica_var_tbl(.data, .max_sample = .max_sample),
    eeg_ica_cor_tbl(.data, ...),
    by = c(".recording", ".ICA")
  ) %>%
    .[order(.recording, -var, -abs(cor))]
  summ[, .ICA := as.character(.ICA)][]
}

#' @export
print.ica_summary <- function(x, ...) {
  NextMethod()
  cat_line("")
  invisible(x)
}


#' @export
print.eeg_summary <- function(x, ...) {
  cat_line(paste0("# EEG data:"))
  cat(paste0("# Sampling rate: ", x$sampling_rate, " Hz.\n"))
  cat(paste0("# Size in memory: ", x$size, ".\n"))
  cat(paste0("# Total duration: ", x$duration, ".\n"))
  cat("# Summary of segments\n")
  x$segments %>%
    print(., ...)


  cat("# Summary of events\n")

  x$events %>%
    print(., ...)

  invisible(x)
}

#' @export
print.eeg_lst <- function(x, ...) {
  cat_line("# EEG data:")
  if (length(eeg_group_vars(x)) > 0) {
    cat_line("# Grouped by: ", paste0(eeg_group_vars(x), sep = ", "))
  }
  cat_line("")
  cat_line("# Signal table:")
  print(x$.signal, ...)

  cat_line("")
  cat_line("# Events table:")
  if (nrow(x$.events) > 0) {
    print(x$.events, ...)
  } else {
    cat_line("No events.")
  }

  cat_line("")
  cat_line("# Segments table:")
  print(x$.segments, ...)
  invisible(x)
}


#' @export
print.psd_lst <- function(x, ...) {
  cat_line("# PSD data:")
  if (length(eeg_group_vars(x)) > 0) {
    cat_line("# Grouped by: ", paste0(eeg_group_vars(x), sep = ", "))
  }
  cat_line("")
  cat_line("# PSD table:")
  print(x$.psd, ...)
  cat_line("")
  cat_line("# Segments table:")
  print(x$.segments, ...)
  invisible(x)
}

#' @export
print.eeg_ica_lst <- function(x, ...) {
  cat_line("# EEG data:")
  if (length(eeg_group_vars(x)) > 0) {
    cat_line("# Grouped by: ", paste0(eeg_group_vars(x), sep = ", "))
  }
  cat_line("")
  cat_line("# Signal table:")
  print(x$.signal, ...)

  cat_line("")
  cat_line("## Events table:")
  if (nrow(x$.events) > 0) {
    print(x$.events, ...)
  } else {
    cat_line("No events.")
  }
  cat_line("")
  cat_line("# ICA:")
  cat_line(paste0("# Component_names:", paste0(component_names(x), collapse = ", ")))
  cat_line(paste0("# Channels_used: ", paste0(channel_ica_names(x), collapse = ", ")))
  cat_line("")
  cat_line("# Segments table:")
  print(x$.segments, ...)
  invisible(x)
}


#' Count number of complete segments of an eeg_lst object.
#'
#' @param x An `eeg_lst` object.
#' @param ... Variables from the segment table to group by.
#'
#'
#' @return A tbl.
#'
#' @family summary functions
#'
#' @examples
#' \dontrun{
#' faces_segs_some %>% count_complete_cases(.recording, .description)
#' }
#' @family summarize
#' @export
count_complete_cases_tbl <- function(x, ...) {
  UseMethod("count_complete_cases_tbl")
}
#' @export
count_complete_cases_tbl.eeg_lst <- function(x, ...) {
  dots <- rlang::enquos(...)
  by <- tidytable::map_chr(dots, rlang::quo_text)
  chs <- channel_names(x)
  x$.signal %>%
    tt_summarize(N = as.integer(!anyNA(c_across(tidyselect::one_of(!!chs)))), .by =".id") %>%
    tidytable::left_join(x$.segments) %>%
    tt_summarize(N = sum(N), .by = by) %>%
    data.table::as.data.table()

}

#' Drop segments with NAs from the eeg_lst
#'
#' Drop segments with NAs from the eeg_lst.
#'
#' @param x eeg_lst
#' @returns An eeg_lst object
#' @family tidyverse-like functions
#' @export
drop_incomplete_segments <- function(x) {
  UseMethod("drop_incomplete_segments")
}

#' @export
drop_incomplete_segments.eeg_lst <- function(x) {
  ## keep the segments in which every channel is free of NAs. filter_at() with
  ## all_vars() is superseded, and the predicate is constant within a segment,
  ## so it reads better as a per-.id test than as a grouped row filter.
  chs <- channel_names(x)
  complete <- x$.signal[, all(!is.na(unlist(.SD))), by = .id, .SDcols = chs]
  keep <- complete[[".id"]][complete[["V1"]]]
  eeg_filter(x, .id %in% !!keep)
}
