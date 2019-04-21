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
  x$events <- as_events_tbl(value)
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
channels_tbl(x$signal)
}

#' @rdname channels_tbl
#' @export
channels_tbl.ica_lst <- function(x, ...) {
    signal_chs <- channels_tbl(x$signal)
    mixing_chs <- channels_tbl(x$mixing)
    if(nrow(signal_chs)==0)  signal_chs <- NULL
    if(nrow(mixing_chs)==0)  mixing_chs <- NULL
   dplyr::bind_rows(signal_chs, mixing_chs)
}


#' @rdname channels_tbl
#' @export
channels_tbl.data.frame <- function(x, ...) {

    channels <- dplyr::select_if(x,is_channel_dbl) %>% colnames()
    ## first row is enough and it makes it faster
    tbl <- x[1,] %>%
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
    #TODO do it in data.table
    x$events <- dplyr::mutate(x$events, 
                              .channel = dplyr::if_else(.channel == orig_names[i], new_names[i], .channel)) %>%
                as_events_tbl()
  }

  x
}

#' @rdname channels_tbl
#' @export
`channels_tbl<-.data.frame` <- function(x, value) {
  orig_names <- channel_names(x)
  channels <- dplyr::select(x, orig_names)
  nochannels <- dplyr::select(x, -dplyr::one_of(channel_names(x)))
  x <- dplyr::bind_cols(nochannels, update_channel_meta_data(channels, value))
  x
}
