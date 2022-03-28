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
as_psd_tbl.data.table <- function(.data) {
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
as_psd_tbl.psd_tbl <- function(.data) {
  validate_psd_tbl(.data)
}
#' @noRd
as_psd_tbl.data.frame <- function(.data) {
  .data <- data.table::as.data.table(.data)
  set_psd_tbl(.data)[]
}

#' @noRd
as_psd_tbl.NULL <- function(.data) {
  .data <- data.table::data.table(.id = integer(0), .freq = numeric(0))
  as_psd_tbl(.data)
}
#' @param psd_tbl
#'
#' @noRd


#' Test if the object is a  psd_tbl
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
  ## Validates .sample
  if (!is.numeric(psd_tbl$.freq)) {
    warning("Values of .freq should be numbers",
            call. = FALSE
    )
  }
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
  ## reorders
  data.table::setcolorder(psd_tbl, obligatory_cols[[".psd"]])
}
