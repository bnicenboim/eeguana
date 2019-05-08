#' Functions to get or set the events table of an eeg_lst object.
#'
#'
#' @param x An eeg_lst object.
#' @param value An events table.
#' @param ... Not in use.
#' 
#' @return A table.
#' @export
events_tbl <- function(x, ...) {
  UseMethod("events_tbl")
}
#' @rdname events
#' @export
events_tbl.eeg_lst <- function(x,...){
  x$.events
}
#' @rdname events
#' @export
`events_tbl<-` <- function(x, value) {
  UseMethod("events_tbl<-")
}
#' @rdname events
#' @export
`events_tbl<-.eeg_lst` <- function(x, value) {
  x$.events <- as_events_tbl(value)
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
channels_tbl(x$.signal)
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
  orig_names <- channel_names(x$.signal)
  channels_sg <- x$.signal[, channel_names(x$.signal), with =FALSE]
  nochannels_sg <- x$.signal[, setdiff(colnames(x$.signal), channel_names(x$.signal)), with = FALSE]
  x$.signal <- cbind(nochannels_sg, 
                    data.table::as.data.table(
                      update_channel_meta_data(channels_sg, 
                                               value))) %>%
    as_signal_tbl()
  new_names <- channel_names(x$.signal)
  
  x$.signal <- data.table::copy(x$.signal)
  for (i in seq_len(nchannels(x$.signal))) {
    data.table::set(x$.signal,which(x$.signal$.channel == orig_names[i]), ".channel", new_names[i])
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
#' 
#' #' @export
#' `[[.eeg_lst` <- function(x,i,...) {
#'   pull.eeg_lst(.data = x,var =  i)
#' }



#' Function to get the signal table of an eeg_lst object.
#'
#'
#' @param x An eeg_lst object.
#' @param ... Not in use.
#' 
#' @return A table.
#' @export
signal_tbl <- function(x, ...) {
    UseMethod("signal_tbl")
}
#' @rdname signal
#' @export
signal_tbl.eeg_lst <- function(x,...){
    x$.signal
}
#' @export
`signal_tbl<-` <- function(x, value) {
    UseMethod("signal_tbl<-")
}
#' @export
`signal_tbl<-.eeg_lst` <- function(x, value) {

stop("Not implemented, please use mutate", call. = FALSE)
}
