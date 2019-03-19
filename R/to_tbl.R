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

    if(any(channel_names(x) %in% colnames(x$signal))){
        channels <- x$signal %>% dplyr::select_at(vars(-one_of(component_names(x)))) %>%
            .[,lapply(.SD, `attributes<-`, NULL )] %>%
            tidyr::gather(key = ".source", value = ".value", channel_names(x)) %>%
            dplyr::mutate(.type = "channel")

    } else {
        channels <- dplyr::tibble()
    }

    if(ncomponents(x)!=0){
        components <-  x$signal %>% dplyr::select_if(purrr::negate(is_channel_dbl)) %>%
            .[,lapply(.SD, `attributes<-`, NULL )] %>%
            tidyr::gather(key = ".source", value = ".value", component_names(x)) %>%
    dplyr::mutate(type = "component")
    } else {
        components = dplyr::tibble()
    }

    dplyr::bind_rows(channels,components) %>%
    {
      if (add_segments) {
        dplyr::left_join(., x$segments, by = ".id")
      } else {
        .
      }
    } %>% 
     {
      if (add_channels_info) {
        dplyr::left_join(., channels_tbl(x), by = c(".source"="channel"))
      } else {
        .
      }
    } %>% 
    dplyr::group_by(.id, .source) %>%
    dplyr::mutate(time = (unclass(.sample_id) - 1) / sampling_rate(x)) %>%
    dplyr::ungroup() %>%
    dplyr::select(time, dplyr::everything()) %>%
    dplyr::select(-.sample_id) 
 
}

as_tibble.mixing_lst <- function(x, ..., .rows = NULL,
    .name_repair = c("check_unique","unique", "universal", "minimal"),
    rownames) {
        NextMethod()
}




as_tibble.signal_tbl <- function(x, ..., .rows = NULL,
                                 .name_repair = c("check_unique","unique", "universal", "minimal"),
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
#' @importFrom magrittr %>%
#'
#' @family tibble
#' @export
as.data.frame.eeg_lst <- function(...) {
  as.data.frame(as_tibble.eeg_lst(...))
}



#' @rdname as_tibble.eeg_lst
as_long_tbl.eeg_lst <- as_tibble.eeg_lst

as_long_tbl <- function(x,...){
    UseMethod("as_long_tbl")
}

as_long_tbl.mixing_tbl <- function(x, add_channels_info = TRUE,...){

    x %>% 
        .[,lapply(.SD, `attributes<-`, NULL )] %>%
        tidyr::gather(key = ".source", value = ".value", channel_names(x)) %>%
        dplyr::mutate(.type = "channel") %>% 
     {
      if (add_channels_info) {
        dplyr::left_join(., channels_tbl(x), by = c(".source"="channel"))
      } else {
        .
      }
       }%>%
        dplyr::group_by(.ICA,.group)

}
