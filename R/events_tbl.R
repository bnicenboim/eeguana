#' Builds an events_tbl table
#' 
#' The eeg_lst `events` table is organised into columns representing the `type` of event
#' associated with the trigger listed under `description`. The timestamp marking
#' the beginning of the event is listed under `.sample_0` and the length of the
#' event (in timestamps) is listed under `.size`. The `.channel` column is a
#' linking variable only, so will generally only contain NAs, unless the 
#' event is specific to a certain channel.
#'
#' @param ids Integers indicating to which group the row of the signal matrix belongs.
#' @param sample_0s Vector of integers that indicate at which sample an events starts.
#' @param sizes Vector of integers that indicate at which sample an events starts.
#' @param channels Vector of characters that indicate to which channel the event is relevant or NA for all the channels.
#' 
#' @family events_tbl
#' 
#' @return A valid `events_tbl` table.
#' @noRd
new_events_tbl <- function(.id=NULL, .sample_0=NULL, .size=NULL, .channel=NULL, descriptions_dt=NULL) {
    
    if(is.null(.id) &&
       is.null(.sample_0) &&
       is.null(.size) &&
       is.null(.channel) &&
       is.null(descriptions_dt)) {
        events <- data.table::data.table(.id= integer(0),
                                         .sample_0= integer(0),
                                         .size= integer(0),
                                         .channel= character(0))
    } else {
        if(is.null(.size)) .size <- 1
        if(is.null(.channel)) .channel <- NA_character_

        if(is.null(descriptions_dt)){
        events <- data.table::data.table(.id = .id,
                           .sample_0 = .sample_0,
                           .size = .size,
                           .channel = .channel)
        } else {
            events <- data.table::data.table(.id = .id,
                                             descriptions_dt,
                                             .sample_0 = .sample_0,
                                             .size = .size,
                                             .channel = .channel)
        }
    }
    data.table::setattr(events, "class", c("events_tbl",class(events)))
    events
}
as_events_tbl <- function(.data,...){
    UseMethod("as_events_tbl")
}

as_events_tbl.data.table <- function(.data){
    .data <- data.table::copy(.data)
    .data[,.id := as.integer(.id)]
    .data[,.size := as.integer(.size)]
    .data[,.sample_0 := as.integer(.sample_0)]
    data.table::setattr(.data, "class", c("events_tbl",class(.data)))
    validate_events_tbl(.data)
}

as_events_tbl.events_tbl <- function(.data){
    validate_events_tbl(.data)
}


as_events_tbl.data.frame <- function(.data){
    .data <- data.table::as.data.table(.data)
    as_events_tbl(.data)
}

#' @noRd
as_events_tbl.NULL <- function(.data){
    new_events_tbl()
}

#' Test if the object is an events_tbl 
#' This function returns  TRUE for events_tbl.
#'
#' @param x An object.
#' 
#' @family events_tbl
#'
#' @return `TRUE` if the object inherits from the `events_tbl` class.
is_events_tbl <- function(x) {
    "events_tbl" %in% class(x) 
}

#' @param events 
#'
#' @param channels 
#'
#' @noRd
validate_events_tbl <- function(events) {
     if(!is_events_tbl(events)){
        warning("Class is not events_tbl", call. = FALSE)
    }
    if (!data.table::is.data.table(events)) {
        warning("'events' should be a data.table.",
                call. = FALSE
                )
    }

    if (!is.integer(events$.sample_0)) {
        warning("Values of .sample_0 should be integers",
                call. = FALSE
                )
    }

    if (!is.integer(events$.size)) {
        warning("Values of .size should be integers",
                call. = FALSE
                )
    }

    events
}
