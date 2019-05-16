#' Display information of the eeg_lst object.
#' `*_names()` functions return a vector of names, `n*()` return the number of 
#' elements channels or components. Components are only available after running [eeg_ica()]. `channel_ica_names()` refers to channels used in the ICA.
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
channel_names.default <- function(x, ...) {
  colnames(x)[x[, purrr::map_lgl(.SD, is_channel_dbl)]]
}
#' @export
channel_names.eeg_lst <- function(x, ...) {
  channel_names(x$.signal)
}
#' @rdname summaries
#' @export
channel_ica_names <- function(x, ...) {
  UseMethod("channel_ica_names")
}
#' @export
channel_ica_names.eeg_ica_lst <- function(x, ...) {
  rownames(x$ica[[1]]$unmixing_matrix)
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
  colnames(x$ica[[1]]$unmixing_matrix)
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
sampling_rate <- function(x, ...) {
  UseMethod("sampling_rate")
}
sampling_rate.eeg_lst <- function(x) {
  attributes(x$.signal$.sample)$sampling_rate
}
sampling_rate.signal_tbl <- function(x) {
  attributes(x$.sample)$sampling_rate
}
sampling_rate.events_tbl <- function(x) {
  attributes(x$.initial)$sampling_rate
}
duration <- function(x) {
  x$.signal %>%
    dplyr::group_by(.id) %>%
    dplyr::summarize(duration = (max(.sample) - min(.sample)) /
      sampling_rate(x)) %>%
    .$duration
}
#' @rdname summaries
#' @export
nsamples <- function(x, ...) {
  UseMethod("nsamples")
}
#' @export
nsamples.eeg_lst <- function(x, ...) {
  duration(x) * sampling_rate(x)
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
#' 
#' @export
summary.eeg_lst <- function(object, ...) {
  segments_with_incomp_col <- object %>%
    signal_tbl() %>%
    dplyr::select(-.sample) %>%
    split(by = ".id", keep.by = FALSE) %>%
    lapply(anyNA) %>%
    unlist() %>%
    dplyr::tibble(".id" = as.integer(names(.)), "incomplete" = .) %>%
    dplyr::left_join(object$.segments, by = ".id")


  summ <- list(
    sampling_rate = sampling_rate(object),
    segments = segments_with_incomp_col %>%
      dplyr::group_by(.recording) %>%
      dplyr::summarize(n_segments = dplyr::n(), n_incomplete = sum(incomplete)) %>%
      data.table::data.table(),
    events = object$.events %>%
      dplyr::group_by_at(dplyr::vars(-.final, -.channel, -.initial, -.id)) %>%
      dplyr::count() %>%
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
  summ$ica_cor <- eeg_ica_cor_lst(object)

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
#' @return A list of correlations.
#' @export
eeg_ica_cor_lst <- function(.data, ...) {
  UseMethod("eeg_ica_cor_lst")
}

#' @export
eeg_ica_cor_lst.eeg_ica_lst <- function(.data, ...) {
  if (length(list(...)) == 0) {
    eogs <- channel_names(.data)[channel_names(.data) %>%
      stringr::str_detect(stringr::regex("eog", ignore_case = TRUE))]
  } else {
    eogs > -sel_ch(.data, ...)
  }
  names(eogs) <- eogs
  comps <- .data %>% eeg_ica_show(component_names(.))

  # new cols:
  .ICA <- NULL
  cor <- NULL

  lapply(eogs, function(eog)
    comps$.signal %>%
      dplyr::summarize_at(
        component_names(comps),
        ~ stats::cor(x = ., y = comps$.signal[[eog]], use = "complete")
      ) %>%
      t() %>%
      {
        dplyr::tibble(.ICA = rownames(.), cor = .[, 1])
      } %>%
      dplyr::arrange(dplyr::desc(abs(cor))))
}

#' @export
print.ica_summary <- function(x, ...) {
  NextMethod()
  cat_line("")
  cat_line("# ICA data:")
  cat_line("")
  cat_line("# Correlations with EOG channels:")
  print(x$ica_cor, ...)
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
  if (length(dplyr::group_vars(x)) > 0) {
    cat_line("# Grouped by: ", paste0(dplyr::group_vars(x), sep = ", "))
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
print.eeg_ica_lst <- function(x, ...) {
  cat_line("# EEG data:")
  if (length(dplyr::group_vars(x)) > 0) {
    cat_line("# Grouped by: ", paste0(dplyr::group_vars(x), sep = ", "))
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
#' @param ... Variables to group by.
#'
#'
#' @return A tbl.
#'
#' @family summary functions
#'
#' @examples
#' \dontrun{
#' 
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

  x$.signal %>%
    dplyr::group_by(.id) %>%
    dplyr::filter_at(
      dplyr::vars(dplyr::one_of(channel_names(x))),
      dplyr::all_vars(all(!is.na(.)))
    ) %>%
    dplyr::summarize() %>%
    dplyr::semi_join(x$.segments, ., by = ".id") %>%
    dplyr::select(-.id, -segment) %>%
    dplyr::count(!!!dots)
}
