#' Functions to get or set the events table of an eeg_lst object.
#'
#'
#' @param .data An eeg_lst object.
#' @param value An events table.
#' @param ... Not in use.
#'
#' @return A table.
#' @family functions to expose internal parts of eeg_(ica_)_lst
#' @export
events_tbl <- function(.data, ...) {
  UseMethod("events_tbl")
}
#' @export
events_tbl.eeg_lst <- function(.data, ...) {
  .data$.events
}
#' @rdname events_tbl
#' @export
`events_tbl<-` <- function(.data, value) {
  UseMethod("events_tbl<-")
}
#' @export
`events_tbl<-.eeg_lst` <- function(.data, value) {
  atti <- attributes(.data$.events$.initial)
  attf <- attributes(.data$.events$.final)
  attributes(value$.initial) <- atti
  attributes(value$.final) <- attf
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
#' @family functions to expose internal parts of eeg_(ica_)_lst
#'
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' # Get channel information:
#' channels_tbl(faces_seg)
#' # Set channel information using dplyr's mutate and replace
#' channels_tbl(faces_seg) <- mutate(channels_tbl(faces_seg),
#'   .channel = replace(.channel, .channel == "HEOG", "EOGH"),
#'   .channel = replace(.channel, .channel == "VEOG", "EOGV")
#' )
#' }
#'
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
  channels <- dplyr::select_if(.data, is_channel_dbl) %>% colnames()
  ## first row is enough and it makes it faster
  tbl <- .data[1, ] %>%
    dplyr::select(tidyselect::all_of(channels)) %>%
    purrr::map_dfr(~ {
      attrs <- attributes(.x)
      attrs[names(attrs) != "class"]
    }) %>%
    dplyr::bind_cols(dplyr::tibble(.channel = channels), .)

  if (tbl %>% nrow() == 0) {
    dplyr::tibble()
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
  .data$.signal <- data.table::copy(.data$.signal)
  data.table::setnames(.data$.signal, channel_names(.data$.signal), value$.channel)
  purrr::iwalk(.data$.signal, function(col, name) {
    if (is_channel_dbl(col)) {
      # remove attributes first (except class)
      remove_attr <- names(attributes(col))[-1]
      purrr::walk(remove_attr, function(attr_n) {
        data.table::setattr(col, attr_n, NULL)
      })
      # list of attributes for each channel without .channel
      attr_list <- c(data.table::as.data.table(value)[.channel == name, ])[-1]
      # check unnessary attributes besides class (the first one)
      purrr::iwalk(attr_list, function(attr_v, attr_n) {
        data.table::setattr(col, attr_n, attr_v)
      })
    }
  })
  .data
}

#old version:
# `channels_tbl<-.eeg_lst` <- function(.data, value) {
#   
#   
#   orig_names <- channel_names(.data$.signal)
#   channels_sg <- .data$.signal[, channel_names(.data$.signal), with = FALSE]
#   nochannels_sg <- .data$.signal[, setdiff(colnames(.data$.signal), channel_names(.data$.signal)), with = FALSE]
#   .data$.signal <- cbind(
#     nochannels_sg,
#     data.table::as.data.table(
#       update_channel_meta_data(
#         channels_sg,
#         value
#       )
#     )
#   ) %>%
#     as_signal_tbl()
#   new_names <- channel_names(.data$.signal)
# 
#   .data$.signal <- data.table::copy(.data$.signal)
#   for (i in seq_len(nchannels(.data$.signal))) {
#     data.table::set(.data$.signal, which(.data$.signal$.channel == orig_names[i]), ".channel", new_names[i])
#   }
#   .data
# }

#' @export
`channels_tbl<-.data.frame` <- function(.data, value) {
  orig_names <- channel_names(.data)
  channels <- dplyr::select(.data, orig_names)
  nochannels <- dplyr::select(.data, -tidyselect::all_of(orig_names))
  dplyr::bind_cols(nochannels, update_channel_meta_data(channels, value))
}

#' @export
`channels_tbl<-.data.table` <- function(.data, value) {
  orig_names <- channel_names(.data)
  channels <- .data[, ..orig_names]
  nochannels <- .data[, -..orig_names]
  update <- data.table::setDT(update_channel_meta_data(channels, value))
  cbind(nochannels, update)
}

#' @export
`channels_tbl<-.signal_tbl` <- function(.data, value) {
  .data <- NextMethod()
  # cbind from data.table method removes the class
  data.table::setattr(.data, "class", c("signal_tbl", class(.data)))
}

#' Function to get the signal table of an eeg_lst object.
#'
#'
#' @param .data An eeg_lst object.
#' @param ... Not in use.
#'
#' @return A table.
#' @family functions to expose internal parts of eeg_(ica_)_lst
#' @export
signal_tbl <- function(.data, ...) {
  UseMethod("signal_tbl")
}
#' @export
signal_tbl.eeg_lst <- function(.data, ...) {
  .data$.signal
}

#' Function to get the segments table of an eeg_lst object.
#'
#'
#' @param .data An eeg_lst object.
#' @param ... Not in use.
#'
#' @return A table.
#' @family functions to expose internal parts of eeg_(ica_)_lst
#' @export
segments_tbl <- function(.data, ...) {
  UseMethod("segments_tbl")
}
#' @export
segments_tbl.eeg_lst <- function(.data, ...) {
  .data$.segments
}

#' Function to get the list of mixing and unmixing matrices of an eeg_ica_lst object.
#'
#'
#' @param .data An eeg_ica_lst object.
#' @param ... Not in use.
#'
#' @return A list.
#' @family functions to expose internal parts of eeg_(ica_)_lst
#' @family ica methods
#' @export
ica_matrix_lst <- function(.data, ...) {
  UseMethod("ica_matrix_lst")
}
#' @export
ica_matrix_lst.eeg_ica_lst <- function(.data, ...) {
  .data$.ica
}
