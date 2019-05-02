#' Convert an eeg_lst to a long table in [`data.table`][data.table::data.table] format.
#'
#' Convert the signal_tbl table from wide to long format.
#'
#' @param x An `eeg_lst` object.
#' @param add_segments Whether the segments table is included.
#' @param add_channels_info Whether the channels information (`channels_tbl`) is included. 
#' @return  A [`data.table`][data.table::data.table].
#' 
#'
#'
as.data.table.eeg_lst <- function(x, unit = "second") {
    ## check if there are channels in the signal tbl (in ica_lst they may not be there)
    if(any(channel_names(x) %in% colnames(x$signal))){
        ## channels <- x$signal %>% dplyr::select_at(vars(-one_of(component_names(x)))) %>%
        ##     .[,lapply(.SD, `attributes<-`, NULL )] %>%
        ##     tidyr::gather(key = ".source", value = ".value",
        ##                   (intersect(colnames(x$signal), channel_names(x)))) %>%
        ##     dplyr::mutate(.type = "channel")
  
        channels <- x$signal %>% dplyr::select_at(dplyr::vars(-dplyr::one_of(component_names(x)))) %>%
            data.table::melt(variable.name = ".source",
                             measure.vars = intersect(colnames(x$signal), channel_names(x)),
                             value.name = ".value")
        channels[,.type := "channel"][
           ,.source := as.character(.source)][
           ,.value := `attributes<-`(.value,NULL)   
           ]
    } else {
        channels <- data.table::data.table()
    }

    if(ncomponents(x)!=0){
        ## components <-  x$signal %>% dplyr::select_if(purrr::negate(is_channel_dbl)) %>%
        ##     .[,lapply(.SD, `attributes<-`, NULL )] %>%
        ##     tidyr::gather(key = ".source", value = ".value", component_names(x)) %>%
        ##     dplyr::mutate(.type = "component")
        components <- x$signal %>% dplyr::select_at(dplyr::vars(-dplyr::one_of(channel_names(x$signal)))) %>%
            data.table::melt(variable.name = ".source",
                             measure.vars = component_names(x),
                             value.name = ".value")
        components[,.type := "component"][
           ,.source := as.character(.source)][
           ,.value := `attributes<-`(.value,NULL)   
           ]
    } else {
        components =  data.table::data.table()
    }

    long_table <- rbind(channels,components) %>%
                left_join_dt(., data.table::as.data.table(x$segments), by = ".id")

    ## inf_events <- setdiff(colnames(events_tbl(x)),  obligatory_cols[["events"]])
    ## events_all_ch <- data.table::copy(events_tbl(x))[is.na(.channel), ends := .initial + .size]

    ## events_all_ch[long_table ,on = .(.id, .initial <= .sample_id, ends > .sample_id), c(colnames(long_table),inf_events), with = FALSE]

    ##unit inside the data.table was creating problems, I rename it to .unit
    .unit <- unit
    long_table[, time := as_time(.sample_id, unit = .unit)]
    long_table[, .sample_id := NULL]
    long_table %>% dplyr::select(time, dplyr::everything())
    #%>%
    #.[,lapply(.SD, `attributes<-`, NULL )]
}


#' Convert an eeg_lst to a long table in [`tibble`][tibble::tibble] format.
#'
#' Convert the signal_tbl table from wide to long format.
#'
#' @param x An `eeg_lst` object.
#' @inheritParams as.data.table
#' @return A [`tibble`][tibble::tibble]
#'
#' @importFrom magrittr %>%
#'
#' @family tibble
#'
as_tibble.eeg_lst <- function(x, unit = "second") {
    data.table::as.data.table(x, unit) %>%
        dplyr::as_tibble()
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
        dplyr::mutate(.type = ".channel") %>% 
     {
      if (add_channels_info) {
        dplyr::left_join(., channels_tbl(x), by = c(".source"=".channel"))
      } else {
        .
      }
       }

}
