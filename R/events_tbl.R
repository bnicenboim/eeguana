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
new_events_tbl <- function(.id=integer(0), 
                           .sample_0=sample_int(integer(0), integer(0)),
                           .size=integer(0),
                           .channel=character(0), 
                           descriptions_dt=data.table::data.table()) {
    
    if(length(c(.id, .sample_0,.size, .channel, descriptions_dt))==0) {
        events <- data.table::data.table(.id= .id,
                                         .sample_0= .sample_0,
                                         .size= .size,
                                         .channel= .channel)
    } else {
        if(length(.size)==0) .size <- 1
        if(length(.channel)==0) .channel <- NA_character_

        if(length(descriptions_dt)==0){
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

as_events_tbl.data.table <- function(.data, sampling_rate = NULL){
    .data <- data.table::copy(.data)
    .data[,.id := as.integer(.id)]
    .data[,.size := as.integer(.size)]
    if(!is.null(sampling_rate)){
     .data[, .sample_0 := sample_int(as.integer(.sample_0),
                                             sampling_rate =sampling_rate )]
    }
    data.table::setattr(.data, "class", c("events_tbl",class(.data)))
    validate_events_tbl(.data)
}

as_events_tbl.events_tbl <- function(.data, sampling_rate = NULL){
  if(!is.null(sampling_rate) && sampling_rate != sampling_rate(.data)){
    .data <- data.table::copy(.data)
    .data[, .sample_0 := sample_int(as.numeric(.sample_0), sampling_rate = sampling_rate)]
  }  
  validate_events_tbl(.data)
}


as_events_tbl.data.frame <- function(.data, sampling_rate = NULL){
    .data <- data.table::as.data.table(.data)
    as_events_tbl(.data, sampling_rate =  sampling_rate)
}

#' @noRd
as_events_tbl.NULL <- function(.data, sampling_rate = NULL){
    new_events_tbl(.sample_0 = sample_int(integer(0), sampling_rate = sampling_rate))
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

    if (!is_sample_int(events$.sample_0)) {
        warning("Values of .sample_0 should be samples",
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
