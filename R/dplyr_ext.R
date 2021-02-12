#' Bind eeg_lst objects.
#'
#' Binds eeg_lst and throws a warning if there is a mismatch in the channel information.
#'
#' @param ... `eeg_lst` objects to combine.
#'
#' @return An `eeg_lst` object.
#' @family tidyverse-like functions
#' @examples 
#' \dontrun{
#' 
#' # Load multiple subjects using purrr::map, extracting subject IDs from file names.
#' faces_list <- purrr::map(list.files("./","vhdr"), ~ 
#'     read_vhdr(.x)
#' )
#' # Bind all the eeg_lsts into a large one:
#' faces <- bind(faces_list)
#' }
#'  
#' @export
bind <- function(...) {
  eeg_lsts <- list(...)
  # hack to allow that "..." would already be a list
  if (class(eeg_lsts[[1]]) != "eeg_lst") {
    eeg_lsts <- list(...)[[1]]
  }

  # Checks:
  purrr::iwalk(
    eeg_lsts[seq(2, length(eeg_lsts))],
    ~ if (!identical(channels_tbl(eeg_lsts[[1]]), channels_tbl(.x))) {
      warning("Objects with different channels information, see below\n\n", "File ",
        as.character(as.numeric(.y) + 1), " ... \n",
        paste0(
          utils::capture.output(setdiff(channels_tbl(eeg_lsts[[1]]), channels_tbl(.x))),
          collapse = "\n"
        ),
        "\n\n ... in comparison with file 1 ...\n\n",
        paste0(
          utils::capture.output(setdiff(channels_tbl(.x), channels_tbl(eeg_lsts[[1]]))),
          collapse = "\n"
        ),
        call. = FALSE
      )
    }
  )

  # Binding
  # .id of the new eggbles needs to be adapted

  signal <- purrr::map(eeg_lsts, ~ .x$.signal) %>% data.table::rbindlist(idcol = ".sid", fill = TRUE)
  signal[, .id := .GRP, by = .(.sid, .id)][, .sid := NULL]
  data.table::setkey(signal, .id, .sample)

  data.table::setattr(signal, "class", c("signal_tbl", class(signal)))
  events <- purrr::map(eeg_lsts, ~ .x$.events) %>% data.table::rbindlist(idcol = ".sid", fill = TRUE)
  events[, .id := .GRP, by = .(.sid, .id)][, .sid := NULL]
  events <- as_events_tbl(events)

  segments <- purrr::map(eeg_lsts, ~ data.table::data.table(.x$.segments)) %>% data.table::rbindlist(idcol = ".sid", fill = TRUE)
  segments[, .id := .GRP, by = .(.sid, .id)][, .sid := NULL]
  data.table::setkey(segments, .id)
  new_eeg_lst <- new_eeg_lst(
    .signal = signal, .events = events, .segments = segments
  ) %>%
    validate_eeg_lst()
  message(say_size(new_eeg_lst))
  new_eeg_lst
}

#' Choose samples by the position
#'
#' Choose samples by their ordinal position in the signal table. Grouped eeg_lst object use the ordinal position in the signal table within the group.
#'
#' @param .data An eeg_lst object.
#' @inheritParams dplyr::slice
#' @param .preserve Not in use.
#' @family tidyverse-like functions
#'
#' @export
slice_signal <- function(.data, ..., .preserve = FALSE){
UseMethod("slice_signal")
}


#' @export
slice_signal.eeg_lst <- function(.data, ..., .preserve = FALSE){
 if(.preserve){
   warning("`.preserve`` is not implemented.")
 }
  slice_signal_eeg_lst(.eeg_lst=.data,...)
}

slice_signal_eeg_lst <- function(.eeg_lst,...) {
  extended_signal <- extended_signal(.eeg_lst)
  by <- as.character(dplyr::group_vars(.eeg_lst))
  if(length(by)!=0){
    cols_signal <- colnames(.eeg_lst$.signal)
    .eeg_lst$.signal <- extended_signal[extended_signal[,.I[...],by=by]$V1] %>%
      .[, ..cols_signal]
    } else{
      .eeg_lst$.signal <- .eeg_lst$.signal[list(...)[[1]],]  
    }
    
  if (nrow(.eeg_lst$.events) > 0) {
      range_s <- .eeg_lst$.signal[, .(.lower = min(.sample), .upper = max(.sample)), by = .id]
      .eeg_lst$.events <- update_events(.eeg_lst$.events, range_s)
  }
    .eeg_lst$.segments <- dplyr::semi_join(.eeg_lst$.segments, .eeg_lst$.signal, by = ".id")
    validate_eeg_lst(.eeg_lst)
}
