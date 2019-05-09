#' Functions to get or set the events table of an eeg_lst object.
#'
#'
#' @param .data An eeg_lst object.
#' @param value An events table.
#' @param ... Not in use.
#' 
#' @return A table.
#' @export
events_tbl <- function(.data, ...) {
  UseMethod("events_tbl")
}
#' @export
events_tbl.eeg_lst <- function(.data,...){
  .data$.events
}
#' @rdname events_tbl
#' @export
`events_tbl<-` <- function(.data, value) {
  UseMethod("events_tbl<-")
}
#' @export
`events_tbl<-.eeg_lst` <- function(.data, value) {
  .data$.events <- as_events_tbl(value)
  .data
}

#' Functions to get or set the channel information of an eeg_lst object.
#'
#'
#' @param .data An eeg_lst object.
#' @param ... Not in use.
#' 
#' @return A table.
#' @export
channels_tbl <- function(.data, ...) {
  UseMethod("channels_tbl")
}

#' @export
channels_tbl.eeg_lst <- function(.data, ...) {
channels_tbl(.data$.signal)
}


#' @export
channels_tbl.data.frame <- function(.data, ...) {

    channels <- dplyr::select_if(.data,is_channel_dbl) %>% colnames()
    ## first row is enough and it makes it faster
    tbl <- .data[1,] %>%
        dplyr::select(channels) %>%
        purrr::map_dfr(~attributes(.x)) %>%
        dplyr::bind_cols( dplyr::tibble(.channel = channels),. ) %>%
        dplyr::select(-contains("class", ignore.case = FALSE))
    if(tbl %>% nrow== 0) {
        tibble()
    } else {
        tbl
    }
}

#' @rdname channels_tbl
#' @param value A channel table.
#' @export
`channels_tbl<-` <- function(.data, value) {
  UseMethod("channels_tbl<-")
}

#' @export
`channels_tbl<-.eeg_lst` <- function(.data, value) {
  orig_names <- channel_names(.data$.signal)
  channels_sg <- .data$.signal[, channel_names(.data$.signal), with =FALSE]
  nochannels_sg <- .data$.signal[, setdiff(colnames(.data$.signal), channel_names(.data$.signal)), with = FALSE]
  .data$.signal <- cbind(nochannels_sg, 
                    data.table::as.data.table(
                      update_channel_meta_data(channels_sg, 
                                               value))) %>%
    as_signal_tbl()
  new_names <- channel_names(.data$.signal)
  
  .data$.signal <- data.table::copy(.data$.signal)
  for (i in seq_len(nchannels(.data$.signal))) {
    data.table::set(.data$.signal,which(.data$.signal$.channel == orig_names[i]), ".channel", new_names[i])
  }
.data
}

#' @export
`channels_tbl<-.data.frame` <- function(.data, value) {
  orig_names <- channel_names(.data)
  channels <- dplyr::select(.data, orig_names)
  nochannels <- dplyr::select(.data, -dplyr::one_of(channel_names(.data)))
  .data <- dplyr::bind_cols(nochannels, update_channel_meta_data(channels, value))
  .data
}


#' Function to get the signal table of an eeg_lst object.
#'
#'
#' @param .data An eeg_lst object.
#' @param ... Not in use.
#' 
#' @return A table.
#' @export
signal_tbl <- function(.data, ...) {
    UseMethod("signal_tbl")
}
#' @export
signal_tbl.eeg_lst <- function(.data,...){
    .data$.signal
}

#' Function to get the segments table of an eeg_lst object.
#'
#'
#' @param .data An eeg_lst object.
#' @param ... Not in use.
#' 
#' @return A table.
#' @export
segments_tbl <- function(.data, ...) {
    UseMethod("segments_tbl")
}
#' @export
segments_tbl.eeg_lst <- function(.data,...){
    .data$.segments
}

