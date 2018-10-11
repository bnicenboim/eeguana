#' Display information of the eegble object.
#'
#' \itemize{
#' \item \code{nchannels()}: Returns the number of channels.
#' \item \code{chan_names()}: Returns a vector with the name of the channels.
#' \item \code{channels()}: Returns a data frame (tibble) with information about the channels.
#' \item \code{info()}: Returns a data frame (tibble) with information about the EEG recording.
#' \item \code{sampling_rate()}: Returns the sampling rate.
#' \item \code{reference()}: Returns the reference.
#' \item \code{duration()}: Returns the duration of the recording (or segments).
#' \item \code{nsamples()}: Returns the number of samples of the recording (or segments).
#' }
#' @param x An eegble object.
#'
#' @name info
NULL
# > NULL

#'
#' @rdname info
#' @export
channel_names <- function(x, ...) {
  UseMethod("channel_names")
}

#' @export
channel_names.eegble <- function(x) {
  setdiff(colnames(x$signal), reserved_cols_signal)
}


#'
#' @rdname info
#' @export
nchannels <- function(x, ...) {
  UseMethod("nchannels")
}

#' @export
nchannels.eegble <- function(x) {
  ncol(x$signal) - length(reserved_cols_signal)
}


## TILL HERE

#'
#' @rdname info
#' @export
channels_tbl <- function(x, ...) {
  UseMethod("channels")
}

#' @export
channels_tbl.eegble <- function(x) {
  # x$channels
  #returns channels, locations and sampling_rate
}







#'
#' @rdname info
#' @export
sampling_rate <- function(x, ...) {
  UseMethod("sampling_rate")
}

#' @export
sampling_rate.eegble <- function(x) {
  attributes(x$signal$.sample_id)$sampling_rate
}



#' @rdname info
#' @export
duration <- function(x, ...) {
  UseMethod("duration")
}

#' @export
duration.eegble <- function(x) {
  x$signal %>%
    dplyr::group_by(.id) %>%
    dplyr::summarize(duration = (max(.sample_id) - min(.sample_id)) / 
      sampling_rate(x)) %>%
    .$duration
}

#' @rdname info
#' @export
nsamples <- function(x, ...) {
  UseMethod("nsamples")
}
#' @export
nsamples.eegble <- function(x) {
  duration(x) * sampling_rate(x)
}


#' count number of complete segments of an eegble object.
#' @param x An \code{eegble} object.
#' @param ... Variables to group by.
#'
#'
#' @return A tbl.
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#'
#' faces_segs_some %>% count_complete_cases(recording, description)
#' }
#' @export
count_complete_cases_tbl <- function(x, ...) {
  UseMethod("count_complete_cases")
}

#' @export
count_complete_cases_tbl.eegble <- function(x, ...) {
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




#' Summary of eegble information.
#'
#' @param object An eegble object.
#' @param ... Other options passed to print.tbl for the display of summaries.
#'
#' @export
summary.eegble <- function(object, ...) {
  dots <- rlang::enquos(...)
  message(paste0("# EEG data (eegble) from ", nchannels(object), " channels:"))
  message(paste0(channel_names(object), collapse = ", "))
  message(paste0("# Sampling rate: ", sampling_rate(object), " Hz."))

  message(paste0("# Size in memory: ", capture.output(pryr::object_size(object)), "."))

  message("# Summary of segments")
  object$segments %>%
    dplyr::count(recording) %>%
    dplyr::rename(segment_n = n) %>%
    print(., !!!dots)

  message("# Summary of events")
  object$events %>%
    dplyr::group_by_at(dplyr::vars(-.size, -.channel, -.sample_0, -.id)) %>%
    dplyr::count() %>%
    print(., !!!dots)

  invisible(object)
}
