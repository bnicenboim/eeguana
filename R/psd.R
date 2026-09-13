#' @param .id Integers indicating to which group the row of the psd matrix belongs.
#' @param psd_matrix Matrix or table of channels with their psd.
#' @param .freq Vector of integers.
#' @param channels_tbl A table with information about each channel (such as the one produced by `channels_tbl``)
#'
#' @family psd_tbl
#'
#' @return A valid `psd_tbl`.
#' @noRd
new_psd_tbl <- function(.id = integer(0), .freq = numeric(0), psd_matrix = NULL, channels_tbl = NULL) {
  if (!data.table::is.data.table(psd_matrix)) {
    psd_matrix <- data.table::data.table(psd_matrix)
  }

  psd_tbl <- psd_matrix[, (update_channel_meta_data(.SD, channels_tbl)), .SDcols = colnames(psd_matrix)]
  
  psd_tbl[, .id := .id][, .freq := .freq]
  data.table::setnames(psd_tbl, make_names(colnames(psd_tbl)))
  data.table::setcolorder(psd_tbl, c(".id", ".freq"))
  data.table::setattr(psd_tbl, "class", c("psd_tbl", class(psd_tbl)))
  data.table::setkey(psd_tbl, .id, .freq)
  psd_tbl[]
}

#' @noRd
as_psd_tbl <- function(.data, ...) {
  UseMethod("as_psd_tbl")
}
#' @noRd
#' @exportS3Method
as_psd_tbl.data.table <- function(.data, ...) {
  .data <- data.table::copy(.data)
  set_psd_tbl(.data)[]
}

set_psd_tbl <- function(.data){
  .data[, .id := as.integer(.id)]
  data.table::setattr(.data, "class", c("psd_tbl", class(.data)))
  data.table::setkey(.data, .id, .freq)
  validate_psd_tbl(.data)
}

#' @noRd
#' @exportS3Method
as_psd_tbl.psd_tbl <- function(.data, ...) {
  validate_psd_tbl(.data)
}
#' @noRd
#' @exportS3Method
as_psd_tbl.data.frame <- function(.data, ...) {
  .data <- data.table::as.data.table(.data)
  set_psd_tbl(.data)[]
}

#' @noRd
#' @exportS3Method
as_psd_tbl.NULL <- function(.data, ...) {
  .data <- data.table::data.table(.id = integer(0), .freq = numeric(0))
  as_psd_tbl(.data)
}


#' Test if the object is a  psd_tbl
#' 
#' This function returns  TRUE for psds.
#'
#' @param x An object.
#'
#' @family psd_tbl
#'
#' @return `TRUE` if the object inherits from the `psd_tbl` class.
#' @export
is_psd_tbl <- function(x) {
  "psd_tbl" %in% class(x)
}

#' @noRd
validate_psd_tbl <- function(psd_tbl) {
  if (!data.table::is.data.table(psd_tbl)) {
    warning("'psd' should be a data.table.",
            call. = FALSE
    )
  }
  if (!is_psd_tbl(psd_tbl)) {
    warning("Class is not psd_tbl", call. = FALSE)
  }
  if (!is.integer(psd_tbl$.id)) {
    warning(".id should be an integer.",
            call. = FALSE
    )
  }
  if (!identical(data.table::key(psd_tbl), c(".id", ".freq"))) {
    warning("`keys` of psd table are missing.",
            call. = FALSE
    )
  }
  # if (!is.numeric(psd_tbl$.freq)) {
  #   warning("Values of .freq should be numbers",
  #           call. = FALSE
  #   )
  # }
  ## checks if there are channels
  if (nrow(psd_tbl) > 0) {
    nchannels <- sum(sapply(psd_tbl, is_channel_dbl))
    ncomponents <- sum(sapply(psd_tbl, is_component_dbl))
    if (nchannels == 0 & ncomponents == 0) {
      warning("No channels or components found.")
    }
  }
  ## Validates channels
  psd_tbl[, lapply(.SD, validate_channel_dbl), .SDcols = sapply(psd_tbl, is_channel_dbl)]
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
  cols <- obligatory_cols[[".psd"]]
  if (needs_reorder(psd_tbl, cols)) {
    if (data.table::truelength(psd_tbl) < ncol(psd_tbl)) {
      psd_tbl <- data.table::copy(psd_tbl)
    }
    data.table::setcolorder(psd_tbl, cols)
  }
  psd_tbl
}
