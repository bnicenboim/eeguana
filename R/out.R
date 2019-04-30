#' Display information of the eeg_lst object.
#'
#' * `nchannels()`: Returns the number of channels.
#' * `channel_names()`: Returns a vector with the name of the channels.
#' * `nchannels()`: Returns the number of channels.
#' * `nsamples()`: Returns the number of samples of the recording (or segments).
#' * `summary()`: Prints a summary of the eeg_lst object. (It also returns a list.)
#'
#' @param x An eeg_lst object.
#' 
#' @family summarize
#' 
#' @name summary
NULL
# > NULL

#' @rdname summary
#' @export
channel_names <- function(x, ...) {
  UseMethod("channel_names")
}
#' @rdname summary
#' @export
channel_names.signal_tbl <- function(x, ...) {
NextMethod()
  }
#' @rdname summary
#' @export
channel_names.default <- function(x, ...) {
  colnames(x)[x[,purrr::map_lgl(.SD, is_channel_dbl )]]
}
#' @rdname summary
#' @export
channel_names.eeg_lst <- function(x, ...) {
  channel_names(x$signal)
}
#' @rdname summary
#' @export
channel_names.mixing_tbl <- function(x, ...) {
NextMethod()
}
#' @rdname summary
#' @export
channel_names.ica_lst <- function(x, ...) {
c(channel_names.eeg_lst(x), channel_names(x$mixing))
  }
#' @rdname summary
#' @export
nchannels <- function(x, ...) {
  UseMethod("nchannels")
}


#' @rdname summary
#' @export
nchannels.default <- function(x, ...) {
    channel_names(x) %>% length()
}

####
#' @rdname summary
#' @export
component_names <- function(x, ...) {
  UseMethod("component_names")
}
#' @rdname summary
#' @export
component_names.signal_tbl <- function(x, ...) {
  colnames(x)[x[,purrr::map_lgl(.SD, is_component_dbl )]]
}
#' @rdname summary
#' @export
component_names.eeg_lst <- function(x, ...) {
  component_names(x$signal)
}

#' @rdname summary
#' @export
ncomponents <- function(x, ...) {
  UseMethod("ncomponents")
}


#' @rdname summary
#' @export
ncomponents.eeg_lst <- function(x, ...) {
    component_names(x) %>% length()
}

sampling_rate <- function(x, ...) {
  UseMethod("sampling_rate")
}
sampling_rate.eeg_lst <- function(x) {
  attributes(x$signal$.sample_id)$sampling_rate
}
sampling_rate.signal_tbl<- function(x) {
  attributes(x$.sample_id)$sampling_rate
}
sampling_rate.events_tbl<- function(x) {
  attributes(x$.initial)$sampling_rate
}

duration <- function(x) {
  x$signal %>%
    dplyr::group_by(.id) %>%
    dplyr::summarize(duration = (max(.sample_id) - min(.sample_id)) /
      sampling_rate(x)) %>%
    .$duration
}

#' @rdname summary
#' @export
nsamples <- function(x, ...) {
  UseMethod("nsamples")
}

#' @rdname summary
#' @export
nsamples.eeg_lst <- function(x, ...) {
  duration(x) * sampling_rate(x)
}


#' Summary of eeg_lst information.
#'
#' @param object An eeg_lst object.
#' @inheritParams base::summary
#' @rdname summary
#'
#' @export
summary.eeg_lst <- function(object, ...) {
  summ <- list(
    channels = channels_tbl(object) %>% data.table::data.table(),
    sampling_rate = sampling_rate(object),
    segments = object$segments %>%
      dplyr::count(recording) %>%
      dplyr::rename(segment_n = n) %>% data.table::data.table(),
    events = object$events %>%
      dplyr::group_by_at(dplyr::vars(-.final, -.channel, -.initial, -.id)) %>%
      dplyr::count() %>% data.table::data.table(),
    size = utils::capture.output(print(utils::object.size(object), units = "auto")),
    duration= format(.POSIXct(nrow(object$signal) / sampling_rate(object) ,tz="GMT"), "%H:%M:%S")
  )
  class(summ) <- c("eeg_summary", class(summ))
  summ
}

#' @rdname summary
#' @export
print.eeg_summary <- function(x, ...) {
  cat(paste0("# EEG data (eeg_lst) from the following channels:\n"))

  x$channels %>%
    print(.)

  cat(paste0("# Sampling rate: ", x$sampling_rate, " Hz.\n"))

  cat(paste0("# Size in memory: ", x$size, ".\n"))

  cat(paste0("# Total duration: ", x$duration, ".\n"))

  cat("# Summary of segments\n")
  x$segments %>%
    print(.)

  cat("# Summary of events\n")

  x$events %>%
    print(.)

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
#' @importFrom magrittr %>%
#' @family summarize
#'
#' @examples
#' \dontrun{
#'
#' faces_segs_some %>% count_complete_cases(recording, description)
#' }
#' @family summarize
#' @export
count_complete_cases_tbl <- function(x, ...) {
  UseMethod("count_complete_cases_tbl")
}
#' @export
count_complete_cases_tbl.eeg_lst <- function(x, ...) {
  dots <- rlang::enquos(...)

  x$signal %>%
    dplyr::group_by(.id) %>%
    dplyr::filter_at(
      dplyr::vars(dplyr::one_of(channel_names(x))),
      dplyr::all_vars(all(!is.na(.)))
    ) %>%
    dplyr::summarize() %>%
    dplyr::semi_join(x$segments, ., by = ".id") %>%
    dplyr::select(-.id, -segment) %>%
    dplyr::count(!!!dots)
}
