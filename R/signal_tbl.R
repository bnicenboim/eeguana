#' Builds a signal_tbl table
#'
#' The eeg_lst `signal` table is organised into columns representing timestamps
#' (`.sample`) and individual electrodes. Each `.sample` corresponds to
#' 1 sample in the original recording, i.e. if the sampling rate of the EEG
#' recording is 500 Hz, then each `.sample` corresponds to 2 milliseconds.
#' These timestamps correspond to `.initial` in the `events` table, which
#' displays only the timestamps where logged events began.
#'
#' @param .id Integers indicating to which group the row of the signal matrix belongs.
#' @param signal_matrix Matrix or table of channels with their signal.
#' @param .sample Vector of integers.
#' @param channels_tbl A table with information about each channel (such as the one produced by `channels_tbl``)
#'
#' @family signal_tbl
#'
#' @return A valid `signal_tbl`.
#' @noRd
new_signal_tbl <- function(.id = integer(0), .sample = integer(0), signal_matrix = NULL, channels_tbl = NULL) {
  if (!data.table::is.data.table(signal_matrix)) {
    signal_matrix <- data.table::data.table(signal_matrix)
  }
  ## if(is.null(channels_tbl)){
  ##   channels_tbl <- dplyr::tibble(channel= colnames(signal_matrix))
  ## }
  signal_tbl <- signal_matrix[, (update_channel_meta_data(.SD, channels_tbl)), .SDcols = colnames(signal_matrix)]

  signal_tbl[, .id := .id][, .sample := .sample]
  data.table::setnames(signal_tbl, make_names(colnames(signal_tbl)))
  data.table::setcolorder(signal_tbl, c(".id", ".sample"))
  data.table::setattr(signal_tbl, "class", c("signal_tbl", class(signal_tbl)))
  data.table::setkey(signal_tbl, .id, .sample)
  signal_tbl[]
}

#' @noRd
as_signal_tbl <- function(.data, ...) {
  UseMethod("as_signal_tbl")
}
#' @noRd
as_signal_tbl.data.table <- function(.data) {
  .data <- data.table::copy(.data)
  set_signal_tbl(.data)[]
}

set_signal_tbl <- function(.data){
  .data[, .id := as.integer(.id)]
  data.table::setattr(.data, "class", c("signal_tbl", class(.data)))
  data.table::setkey(.data, .id, .sample)
  validate_signal_tbl(.data)
}

#' @noRd
as_signal_tbl.signal_tbl <- function(.data) {
  validate_signal_tbl(.data)
}
#' @noRd
as_signal_tbl.data.frame <- function(.data) {
  .data <- data.table::as.data.table(.data)
  set_signal_tbl(.data)[]
}

#' @noRd
as_signal_tbl.NULL <- function(.data) {
  .data <- data.table::data.table(.id = integer(0), .sample = sample_int(integer(0), integer(0)))
  as_signal_tbl(.data)
}
#' @param signal_tbl
#'
#' @noRd


#' Test if the object is a  signal_tbl
#' 
#' This function returns  TRUE for signals.
#'
#' @param x An object.
#'
#' @family signal_tbl
#'
#' @return `TRUE` if the object inherits from the `signal_tbl` class.
#' @export
is_signal_tbl <- function(x) {
  "signal_tbl" %in% class(x)
}

#' @noRd
as_eeg_ica_lst <- function(.data, ...) {
  UseMethod("as_eeg_ica_lst")
}
as_eeg_ica_lst.eeg_ica_lst <- function(.data, ...) {
  .data
}
as_eeg_ica_lst.eeg_lst <- function(.data, ...) {
  class(.data) <- c("eeg_ica_lst", class(.data))
  .data
}
validate_signal_tbl <- function(signal_tbl) {
  ## if(is.null(signal_tbl)) {
  ##     signal_tbl <- data.table::data.table(.id= integer(0),.sample= integer(0))
  ##     data.table::setkey(signal_tbl,.id,.sample)
  ## }
  ##  if(!data.table::is.data.table(signal_tbl) && is.data.frame(signal_tbl)) {
  ##      signal <- data.table::as.data.table(signal_tbl)
  ##      data.table::setkey(signal_tbl,.id,.sample)
  # fs# }
  if (!data.table::is.data.table(signal_tbl)) {
    warning("'signal' should be a data.table.",
            call. = FALSE
    )
  }
  if (!is_signal_tbl(signal_tbl)) {
    warning("Class is not signal_tbl", call. = FALSE)
  }
  if (!is.integer(signal_tbl$.id)) {
    warning(".id should be an integer.",
            call. = FALSE
    )
  }
  
  if (!identical(data.table::key(signal_tbl), c(".id", ".sample"))) {
    warning("`keys` of signal table are missing.",
            call. = FALSE
    )
  }
  
  ## Validates .sample
  if (!is_sample_int(signal_tbl$.sample)) {
    warning("Values of .sample should be samples",
            call. = FALSE
    )
  }
  
  ## checks if there are channels
  if (nrow(signal_tbl) > 0) {
    nchannels <- sum(sapply(signal_tbl, is_channel_dbl))
    ncomponents <- sum(sapply(signal_tbl, is_component_dbl))
    if (nchannels == 0 & ncomponents == 0) {
      warning("No channels or components found.")
    }
  }
  
  ## Validates channels
  signal_tbl[, lapply(.SD, validate_channel_dbl), .SDcols = sapply(signal_tbl, is_channel_dbl)]
  ## reorders
  data.table::setcolorder(signal_tbl, obligatory_cols[[".signal"]])
}

