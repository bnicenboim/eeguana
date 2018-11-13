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

#' @export
channel_names <- function(x, ...) {
  UseMethod("channel_names")
}

#' @rdname summary
#' @export
channel_names.eeg_lst <- function(x) {
  setdiff(colnames(x$signal), obligatory_cols$signal)
}


#' @export
nchannels <- function(x, ...) {
  UseMethod("nchannels")
}

#' @rdname summary
#' @export
nchannels.eeg_lst <- function(x) {
  ncol(x$signal) - length(obligatory_cols$signal)
}


#' @export
channels_tbl <- function(x, ...) {
  UseMethod("channels_tbl")
}


#' @rdname summary
#' @export
channels_tbl.eeg_lst <- function(x, ...) {
  dplyr::tibble(channel = channel_names(x)) %>%
    # first row is enough and it makes it faster
    dplyr::bind_cols(x$signal[1,] %>%
      dplyr::select(channel_names(x)) %>%
      purrr::map_dfr(~attributes(.x))) %>%
    select(-channel)
}

#' @export
`channels_tbl<-` <- function(x, value) {
  UseMethod("channels_tbl<-")
}

#' @rdname summary
#' @export
`channels_tbl<-.eeg_lst` <- function(x, value) {
  orig_names <- channel_names(x)
  channels <- dplyr::select(x$signal, orig_names)
  nochannels <- dplyr::select(x$signal, -dplyr::one_of(channel_names(x)))
  x$signal <- dplyr::bind_cols(nochannels, update_channel_meta_data(channels, value))
  new_names <- channel_names(x)

  for (i in seq_len(nchannels(x))) {
    x$events <- mutate(x$events, .channel = dplyr::if_else(.channel == orig_names[i], new_names[i], .channel)) %>%
                data.table::as.data.table()
  }

  x
}

sampling_rate <- function(x) {
  attributes(x$signal$.sample_id)$sampling_rate
}

duration <- function(x) {
  x$signal %>%
    dplyr::group_by(.id) %>%
    dplyr::summarize(duration = (max(.sample_id) - min(.sample_id)) /
      sampling_rate(x)) %>%
    .$duration
}

#' @export
nsamples <- function(x, ...) {
  UseMethod("nsamples")
}

#' @rdname summary
#' @export
nsamples.eeg_lst <- function(x) {
  duration(x) * sampling_rate(x)
}


#' Summary of eeg_lst information.
#'
#' @param object An eeg_lst object.
#' @rdname summary
#'
#' @export
summary.eeg_lst <- function(object) {
  summ <- list(
    channels = channels_tbl(object),
    sampling_rate = sampling_rate(object),
    segments = object$segments %>%
      dplyr::count(recording) %>%
      dplyr::rename(segment_n = n),
    events = object$events %>%
      dplyr::group_by_at(dplyr::vars(-.size, -.channel, -.sample_0, -.id)) %>%
      dplyr::count(),
    size = capture.output(object_size(object))
  )
  class(summ) <- c("eeg_summary", class(summ))
  summ
}

#' @rdname summary
#' @export
print.eeg_summary <- function(x, ...) {
  cat(paste0("# EEG data (eeg_lst) from the following channels:\n"))

  x$channels %>%
    print(., ...)

  cat(paste0("# Sampling rate: ", x$sampling_rate, " Hz.\n"))

  cat(paste0("# Size in memory: ", x$size, ".\n"))

  cat("# Summary of segments\n")
  x$segments %>%
    print(., ...)

  cat("# Summary of events\n")

  x$events %>%
    print(., ...)

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
