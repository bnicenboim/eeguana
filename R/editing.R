#' Functions to get or set the events table of an eeg_lst object.
#'
#'
#' @param x An eeg_lst object.
#' @param value An events table.
#' @param ... Not in use.
#' 
#' @return A table.
#' @export
events <- function(x, ...) {
  UseMethod("events")
}
#' @rdname events
#' @export
events.eeg_lst <- function(x,...){
  x$events
}
#' @rdname events
#' @export
`events<-` <- function(x, value) {
  UseMethod("events<-")
}
#' @rdname events
#' @export
`events<-.eeg_lst` <- function(x, value) {
  x$events <- data.table::data.table(value)
  x
}



#' Functions to get or set the channel information of an eeg_lst object.
#'
#'
#' @param x An eeg_lst object.
#' @param ... Not in use.
#' 
#' @return A table.
#' @export
channels_tbl <- function(x, ...) {
  UseMethod("channels_tbl")
}

#' @rdname channels_tbl
#' @export
channels_tbl.eeg_lst <- function(x, ...) {
  dplyr::tibble(channel = channel_names(x)) %>%
    # first row is enough and it makes it faster
    dplyr::bind_cols(x$signal[1,] %>%
      dplyr::select(channel_names(x)) %>%
      purrr::map_dfr(~attributes(.x))) %>%
    dplyr::select(-class)
}
#' @rdname channels_tbl
#' @param value A channel table.
#' @export
`channels_tbl<-` <- function(x, value) {
  UseMethod("channels_tbl<-")
}

#' @rdname channels_tbl
#' @export
`channels_tbl<-.eeg_lst` <- function(x, value) {
  orig_names <- channel_names(x)
  channels <- dplyr::select(x$signal, orig_names)
  nochannels <- dplyr::select(x$signal, -dplyr::one_of(channel_names(x)))
  x$signal <- dplyr::bind_cols(nochannels, update_channel_meta_data(channels, value))
  new_names <- channel_names(x)

  for (i in seq_len(nchannels(x))) {
    x$events <- dplyr::mutate(x$events, .channel = dplyr::if_else(.channel == orig_names[i], new_names[i], .channel)) %>%
                data.table::as.data.table()
  }

  x
}