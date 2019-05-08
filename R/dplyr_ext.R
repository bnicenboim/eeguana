#' Bind eeg_lst objects.
#'
#' Binds eeg_lst and throws a warning if there is a mismatch in the channel information.
#'
#' @param ... `eeg_lst` objects to combine.
#'
#' @return An `eeg_lst` object.
#'
#' @importFrom magrittr %>%
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
    ~if (!identical(channels_tbl(eeg_lsts[[1]]), channels_tbl(.x))) {
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
        )
        ,
        call. = FALSE
      )
    }
  )

  # Binding
  # .id of the new eggbles needs to be adapted

  signal <- purrr::map(eeg_lsts, ~.x$.signal) %>% data.table::rbindlist(idcol=".sid", fill = TRUE)
  signal[, .id := .GRP, by = .(.sid,.id)][,.sid := NULL]
  data.table::setkey(signal,.id,.sample)
  
  data.table::setattr(signal,"class",c("signal_tbl",class(signal)))
  events <- purrr::map(eeg_lsts, ~.x$.events) %>% data.table::rbindlist(idcol=".sid", fill = TRUE)
  events[, .id := .GRP, by = .(.sid,.id)][,.sid := NULL]
  events <- as_events_tbl(events)

  segments <- purrr::map(eeg_lsts, ~data.table::data.table(.x$.segments)) %>% data.table::rbindlist(idcol=".sid", fill = TRUE)
  segments[, .id := .GRP, by = .(.sid,.id)][,.sid := NULL] 
  segments <- segments %>% dplyr::as_tibble()

  new_eeg_lst <- new_eeg_lst(
    .signal = signal, .events = events, .segments = segments
  ) %>%
    validate_eeg_lst()
  message(say_size(new_eeg_lst))
  new_eeg_lst
}
