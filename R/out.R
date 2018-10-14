#' Display information of the eegble object.
#'
#' \itemize{
#' \item \code{nchannels()}: Returns the number of channels.
#' \item \code{channel_names()}: Returns a vector with the name of the channels.
#' \item \code{nchannels()}: Returns the number of channels.
#' \item \code{nsamples()}: Returns the number of samples of the recording (or segments).
#' \item \code{summary()}: Prints a summary of the eegble object. (It also returns a list.)
#' \item \code{count_complete_cases_tbl()}: Returns a table with the number of complete segments by specified groups.
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
  setdiff(colnames(x$signal), obligatory_cols[["signal"]])
}


#'
#' @rdname info
#' @export
nchannels <- function(x, ...) {
  UseMethod("nchannels")
}

#' @export
nchannels.eegble <- function(x) {
  ncol(x$signal) - length(obligatory_cols[["signal"]])
}




channels_tbl <- function(x) {
  dplyr::tibble(channel = channel_names(x)) %>%
   dplyr::bind_cols(dplyr::select(x$signal, channel_names(x)) %>% 
      purrr::map_dfr( ~ attributes(.x))) %>% 
      hd_add_column(sampling_rate = attributes(x$signal$.sample_id)$sampling_rate) %>%
      select(-class)
  # x$channels
  #returns channels, locations and sampling_rate
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

#' @rdname info
#' @export
nsamples <- function(x, ...) {
  UseMethod("nsamples")
}
#' @export
nsamples.eegble <- function(x) {
  duration(x) * sampling_rate(x)
}


#' Summary of eegble information.
#'
#' @param object An eegble object.
#' @param ... Other options passed to print.tbl for the display of summaries.
#'
#' @export
summary.eegble <- function(object, ...) {
  dots <- rlang::enquos(...)
  summ <- list(channels = select(channels_tbl(object), -sampling_rate),
                  sampling_rate = unique(channels_tbl(object)$sampling_rate),
                  segments = object$segments %>%
                              dplyr::count(recording) %>%
                              dplyr::rename(segment_n = n),
                  events = object$events %>%
                          dplyr::group_by_at(dplyr::vars(-.size, -.channel, -.sample_0, -.id)) %>%
                          dplyr::count(),
                  size = capture.output(pryr::object_size(object)))
  
  
  print(paste0("# EEG data (eegble) from the following channels:"))
  summ$channels %>% 
    print(., !!!dots)

  print(paste0("# Sampling rate: ", summ$sampling_rate, " Hz."))

  print(paste0("# Size in memory: ", summ$size, "."))

  print("# Summary of segments")
  summ$segments %>% 
    print(., !!!dots)

  print("# Summary of events")
  summ$events %>% 
    print(., !!!dots)

  invisible(summ)
}


#' Count number of complete segments of an eegble object.
#' 
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
  UseMethod("count_complete_cases_tbl")
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
