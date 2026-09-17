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
  data.table::setnames(signal_tbl, make_names(colnames(signal_tbl), allow_init_dot = TRUE))
  data.table::setcolorder(signal_tbl, c(".id", ".sample"))
  data.table::setattr(signal_tbl, "class", c("signal_tbl", class(signal_tbl)))
  signal_tbl[]
}

#' @noRd
as_signal_tbl <- function(.data, ...) {
  UseMethod("as_signal_tbl")
}
#' @noRd
#' @exportS3Method
as_signal_tbl.tidytable <- function(.data, ...) {
  as_signal_tbl.data.table(.data)
}
#' @noRd
#' @exportS3Method
as_signal_tbl.data.table <- function(.data, ...) {
    .data <- .data %>% tt_mutate(.id = as.integer(.id))
    class(.data) <- c("signal_tbl","data.table", "data.frame")
    validate_signal_tbl(.data)
}
# 
# set_signal_tbl <- function(.data){
#   .data[, .id := as.integer(.id)]
#   data.table::setattr(.data, "class", c("signal_tbl", class(.data)))
#   data.table::setkey(.data, .id, .sample)
#   validate_signal_tbl(.data)
# }

#' @noRd
#' @exportS3Method
as_signal_tbl.signal_tbl <- function(.data, ...) {
  validate_signal_tbl(.data)
}
#' @noRd
#' @exportS3Method
as_signal_tbl.data.frame <- function(.data, ...) {
  .data <- data.table::as.data.table(.data)
  as_signal_tbl.data.table(.data)
}

#' @noRd
#' @exportS3Method
as_signal_tbl.NULL <- function(.data, ...) {
  .data <- data.table::data.table(.id = integer(0), .sample = sample_int(integer(0), integer(0)))
  as_signal_tbl(.data)
}


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
#' @exportS3Method
as_eeg_ica_lst.eeg_ica_lst <- function(.data, ...) {
  .data
}
#' @exportS3Method
as_eeg_ica_lst.eeg_lst <- function(.data, ...) {
  class(.data) <- c("eeg_ica_lst", class(.data))
  .data
}
#' @noRd
## TRUE when the obligatory columns are not already the first ones. The vectors
## in obligatory_cols are named, and identical() compares names too, so they
## have to be dropped before the comparison can ever match.
needs_reorder <- function(tbl, cols) {
  !identical(names(tbl)[seq_along(cols)], unname(cols))
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
  ## Put the obligatory columns first when they are not already. setcolorder()
  ## moves the column names without the data on a table with 64 or more columns
  ## whose over-allocation is gone, which is the state tidytable verbs return,
  ## so such a table is copied before it is reordered.
  ##
  ## Until 2026-09-13 this compared the names against the *named* vector in
  ## obligatory_cols, so identical() never matched. Every call counted as out of
  ## order and copied the whole table whenever it had lost its over-allocation:
  ## for the signal table, one full extra copy on each eeg_mutate() and
  ## eeg_select(). An earlier note here said the reorder was skipped in
  ## practice; that rested on the same broken comparison.
  ##
  ## Returns the table, so callers must assign the result.
  cols <- obligatory_cols[[".signal"]]
  if (needs_reorder(signal_tbl, cols)) {
    if (data.table::truelength(signal_tbl) < ncol(signal_tbl)) {
      signal_tbl <- data.table::copy(signal_tbl)
    }
    data.table::setcolorder(signal_tbl, cols)
  }
  signal_tbl
}

