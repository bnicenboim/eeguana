#' Convert an eeg_lst to a wide table.
#'
#' Convert the signal_tbl table from wide to long format, and optionally `left_join`s the segment table
#'
#' @param x An `eeg_lst` object.
#' @param add_segments Whether the segments table is included.
#' @param add_channels_info Whether the channels information (`channels_tbl`) is included. 
#' `as_data_frame` is an alias.
#' @return A tibble.
#'
#' @importFrom magrittr %>%
#'
#' @family tibble
#'
as_tibble.eeg_lst <- function(x, add_segments = TRUE, add_channels_info = TRUE) {
   x$signal[,lapply(.SD, `attributes<-`, NULL )] %>% 
    tidyr::gather(key = "channel", value = "amplitude", channel_names(x)) %>%
    {
      if (add_segments) {
        dplyr::left_join(., x$segments, by = ".id")
      } else {
        .
      }
    } %>% 
     {
      if (add_channels_info) {
        dplyr::left_join(., channels_tbl(x), by = c("channel"))
      } else {
        .
      }
    } %>% 
    dplyr::group_by(.id, channel) %>%
    dplyr::mutate(time = (unclass(.sample_id) - 1) / sampling_rate(x)) %>%
    dplyr::ungroup() %>%
    dplyr::select(time, dplyr::everything()) %>%
    dplyr::select(-.sample_id) 
 
}


as_tibble.signal_tbl <- function(x) {
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
#' @importFrom magrittr %>%
#'
#' @family tibble
#' @export
as.data.frame.eeg_lst <- function(...) {
  as.data.frame(as_tibble.eeg_lst(...))
}



