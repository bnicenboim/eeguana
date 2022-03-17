#' Convert an eeg_lst to a long table in [`data.table`][data.table::data.table] format.
#'
#' Convert the signal_tbl table from wide to long format.
#'
#' @param x An `eeg_lst` object.
#' @param .unit Unit for the `.time` column of the transformed object: "s" (default), "ms", "samples".
#' @return  A [`data.table`][data.table::data.table].
#'
#'
#'
as.data.table.eeg_lst <- function(x, .unit = "s") {
  keys <- x$.signal %>%
    select.(where(~ is_channel_dbl(.) || is_component_dbl(.))) %>%
    colnames()
  if (length(keys) == 0) {
    stop("No channels found.", call. = TRUE)
  }
  long_signal <- x$.signal %>%
    data.table::melt(
      variable.name = ".key",
      measure.vars = keys,
      value.name = ".value"
    )
  long_signal[, .key := as.character(.key)][
    , .value := `attributes<-`(.value, NULL)
  ]

  long_table <- long_signal %>%
    left_join_dt(., data.table::as.data.table(x$.segments), by = ".id")

  long_table[, .time := as_time(.sample, .unit = .unit)]
  long_table[, .sample := NULL]
  long_table %>% dplyr::select(.time, dplyr::everything())
}


#' Convert an eeg_lst to a long table in [`tibble`][tibble::tibble] format.
#'
#' Convert the signal_tbl table from wide to long format.
#'
#' @inheritParams as.data.table.eeg_lst
#' @return A [`tibble`][tibble::tibble]
#'
#'
#' @family tibble
#'
as_tibble.eeg_lst <- function(x, .unit = "second") {
  data.table::as.data.table(x, .unit) %>%
    dplyr::as_tibble(.name_repair = "unique")
}

#' Convert an eeg_lst to a long table in [`tidytable`][tidytable::tidytable] format.
#'
#' Convert the signal_tbl table from wide to long format.
#'
#' @param x An `eeg_lst` object.
#' @param .unit Unit for the `.time` column of the transformed object: "s" (default), "ms", "samples".
#' @return  A [`tidytable`][tidytable::tidytable].
#'
#'
#'
as_tidytable.eeg_lst <- function(x, .unit = "s") {
  data.table::as.data.table(x, .unit) %>%
    tidytable::as_tidytable(.name_repair = "unique")
}

as_tibble.signal_tbl <- function(x, ..., .rows = NULL,
                                 .name_repair = c("check_unique", "unique", "universal", "minimal"),
                                 rownames) {
  NextMethod()
}




#' @rdname as_tibble.eeg_lst
as_data_frame.eeg_lst <- as_tibble.eeg_lst




#' Convert an eeg_lst to a (base) data frame.
#'
#' @param ... Other arguments passed on to individual methods.
#'
#' @return A tibble.
#'
#'
#' @family tibble
#' @export
as.data.frame.eeg_lst <- function(...) {
  as.data.frame(as_tibble.eeg_lst(...))
}



#' @rdname as_tibble.eeg_lst
as_long_tbl.eeg_lst <- as_tibble.eeg_lst

as_long_tbl <- function(x, ...) {
  UseMethod("as_long_tbl")
}

as_long_tbl.mixing_tbl <- function(x, add_channels_info = TRUE, ...) {
  x %>%
    .[, lapply(.SD, `attributes<-`, NULL)] %>%
    tidyr::gather(key = ".key", value = ".value", channel_names(x)) %>%
    dplyr::mutate(.type = ".channel") %>%
    {
      if (add_channels_info) {
        dplyr::left_join(., channels_tbl(x), by = c(".key" = ".channel"))
      } else {
        .
      }
    }
}
