#' @param values 
#'
#' @param sampling_rate 
#'
#' @noRd
new_sample_int <- function(values, sampling_rate) {
  if (!all(is.na(values)) && !all.equal(values, round(values))) {
    stop("Sample integer values should be round numbers.",
      call. = FALSE
    )
  } else {
    values <- as.integer(values)
  }
  values <- unclass(values)
  structure(values,
    class = "sample_int",
    sampling_rate = sampling_rate
  )
}

#' @param sample_id 
#'
#' @noRd
validate_sample_int <- function(sample_id) {
  if (!is.integer(sample_id)) {
    stop("Values should be integers.",
      call. = FALSE
    )
  }
  if (length(sample_id) > 0) {
    if (attributes(sample_id)$sampling_rate <= 0) {
      warning("Attribute sampling_rate should be a positive value.",
        call. = FALSE
      )
    }
  }
  sample_id
}

#' @noRd
new_channel_dbl <- function(values, channel_info = list()) {
  values <- unclass(values) %>% as.double
  attributes(values) <- c(
    class = "channel_dbl",
    channel_info
  )
  values
}


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

#' @param channel 
#'
#' @noRd
validate_channel_dbl <- function(channel) {
  if (!is.double(channel)) {
    stop("Values should be double.",
      call. = FALSE
    )
  }
  purrr::walk(c(".x", ".y", ".z"), ~
  if (!is.numeric(attr(channel, .))) {
    warning(sprintf("Attribute %s should be a number.", .),
      call. = FALSE
    )
  } )

  if (is.null(attributes(channel)$.reference)) {
    warning("Attribute .reference is missing.",
      call. = FALSE
    )
  }
  channel
}

#' @param signal_matrix 
#'
#' @param ids 
#' @param sample_ids 
#' @param channel_info 
#'
#' @noRd
new_signal_tbl <- function(signal_matrix=NULL , ids=NULL , sample_ids=NULL , channel_info=NULL ) {

    if(!data.table::is.data.table(signal_matrix)) {
        signal_matrix <- data.table::data.table(signal_matrix)
    }
    ## if(is.null(channel_info)){
    ##   channel_info <- dplyr::tibble(channel= colnames(signal_matrix))
    ## }
  signal_tbl <- signal_matrix[, (update_channel_meta_data(.SD, channel_info)),.SDcols=colnames(signal_matrix)]

  signal_tbl[, .id := ids][, .sample_id := sample_ids]
  data.table::setcolorder(signal_tbl, c(".id", ".sample_id"))
  data.table::setattr(signal_tbl, "class",c("signal_tbl",class(signal_tbl)))
  data.table::setkey(signal_tbl, .id, .sample_id)
  signal_tbl[]
}

#' @param channels 
#'
#' @param channel_info 
#'
#' @noRd
update_channel_meta_data <- function(channels, channel_info) {
  if (nrow(channel_info) == 0 || is.null(channel_info)) {
    channels <- purrr::map(
      channels,
      function(sig) {
        channel <- new_channel_dbl(values = sig,channel_info = list(.x=NA_real_,.y= NA_real_,.z =NA_real_, .reference=NA_real_))
      }
    )
  } else {
    channels <- purrr::map2(
      channels %>% stats::setNames(make.names(channel_info$channel)), purrr::transpose(dplyr::select(channel_info, -channel)),
      function(sig, chan_info) {
        channel <- new_channel_dbl(values = sig, as.list(chan_info))
      }
    )
  }
  channels
}

# purrr::map2(
#       channels , purrr::transpose(channel_info),
#       function(sig, chan_info) {
#         channel <- new_channel_dbl(value = sig, as.list(chan_info))
#       }
# )

#' @param signal_tbl 
#'
#' @param events 
#' @param segments 
#'
#' @noRd
new_eeg_lst <- function(signal = NULL, events = NULL, segments = NULL) {
  x <- list(
    signal = signal, events = events,
    segments = segments
  )
  x <- unclass(x)
  structure(x,
    class = c("eeg_lst"),
    vars = character(0)
  )
}

#' @param x 
#'
#' @noRd
validate_eeg_lst <- function(x) {
  if(!is_eeg_lst(x)){
    warning("Class is not eeg_lst", call. = FALSE)
  }
    x$signal <- validate_signal_tbl(x$signal)
    x$events <- validate_events_tbl(x$events)
    x$segments <- validate_segments(x$segments)
    diff_channels <- setdiff(x$events$.channel, channel_names(x))
    if (length(diff_channels) != 0 & any(!is.na(diff_channels))) {
        warning("Unknown channel in table of events",
                call. = FALSE
                )
    }
    if (!all.equal(unique(x$signal$.id), unique(x$segments$.id))) {
        warning("The values of .ids mismatch between tables.",
                call. = FALSE
                )
    }

    if(any(!group_vars(x) %in% c(colnames(x$signal),colnames(x$segments)))){
        warning("Grouping variables are missing.",
                call. = FALSE
                )
    }
x
}

#' @param signal_tbl 
#'
#' @noRd
validate_signal_tbl <- function(signal_tbl) {
    if(is.null(signal_tbl)) {
        signal_tbl <- data.table::data.table(.id= integer(0),.sample_id= integer(0))
        data.table::setkey(signal_tbl,.id,.sample_id)
    }
    if(!data.table::is.data.table(signal_tbl) && is.data.frame(signal_tbl)) {
        signal <- data.table::as.data.table(signal_tbl)
        data.table::setkey(signal_tbl,.id,.sample_id)
   }     
  if (!data.table::is.data.table(signal_tbl)) {
    warning("'signal' should be a data.table.",
      call. = FALSE
    )
  }
  if(!is_signal_tbl(signal_tbl)){
    warning("Class is not signal_tbl", call. = FALSE)
  }
  if (!is.integer(signal_tbl$.id)) {
    warning(".id should be an integer.",
      call. = FALSE
    )
  }


  if(!identical(data.table::key(signal_tbl), c(".id",".sample_id"))) {
    warning("`keys` of signal table are missing.",
      call. = FALSE
    )
  }

  # Validates sample_id
  validate_sample_int(signal_tbl$.sample_id)

  #checks if there are channels
    if(nrow(signal_tbl)>0){
      nchannels <- sum(sapply(signal_tbl, is_channel_dbl))
      ncomponents <- sum(sapply(signal_tbl, is_component_dbl))
      if(nchannels ==0 & ncomponents ==0  )
        warning("No channels or components found.")
  }

  # Validates channels 
  signal_tbl[, lapply(.SD,validate_channel_dbl), .SDcols= sapply(signal_tbl, is_channel_dbl)] 

  signal_tbl
}
#' @param events 
#'
#' @param channels 
#'
#' @noRd
validate_events_tbl <- function(events) {
    if(!data.table::is.data.table(events) && is.data.frame(events)) {
        events <- data.table::as.data.table(events)
    }
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
#' @param segments 
#'
#' @noRd
validate_segments <- function(segments) {
    if(is.null(segments)) {
        segments <- dplyr::tibble(.id = integer(0))
    }
   if( length(segments$.id) != length(unique(segments$.id)) ){
     warning("Some .ids are repeated in the segments table, there is something wrong going on. Please open an issue with a reproducible example in https://github.com/bnicenboim/eeguana/issues",
      call. = FALSE
    )
  }
  segments
}

#' @param values
#' 
#' @noRd
new_component_dbl <- function(values)  {
    values <- unclass(values) %>% as.double
    attributes(values) <- list(
        class = "component_dbl"
    )
    values
}



#' @param component 
#'
#' @noRd
validate_component_dbl <- function(component) {
    if (!is.double(component)) {
        stop("Values should be double.",
             call. = FALSE
             )
    }
    component
}

#' @param signal_tbl 
#'
#' @param events 
#' @param segments 
#'
#' @noRd
new_ica_lst <- function(signal = NULL, mixing = NULL, events = NULL, segments = NULL) {
    x <- list(
        signal = signal,
        mixing = mixing,
        events = events,
        segments = segments
    )
    x <- unclass(x)
    structure(x,
              class = c("ica_lst","eeg_lst"),
              vars = character(0)
              )
}

#' @param x 
#'
#' @noRd
validate_ica_lst <- function(x) {
    x <- validate_eeg_lst(x)
    x$mixing <- validate_mixing_tbl(x$mixing)
    x
}

#' @param mixing_tbl 
#'
#' @noRd
validate_mixing_tbl <- function(mixing_tbl) {
    
    if (!data.table::is.data.table(mixing_tbl)) {
        warning("'mixing' should be a data.table.",
                call. = FALSE
                )
    }

     if(all(!sapply(mixing_tbl, is_channel_dbl)) && nrow(mixing_tbl)>0){
        warning("No channels found.")
    }
    # Validates channels 
    mixing_tbl[, lapply(.SD,validate_channel_dbl), .SDcols= sapply(mixing_tbl, is_channel_dbl)] 

    mixing_tbl
}

#' @param mixing_matrix  matrix or a list o matrices
#' @param groups 
#' @param channel_info 
#'
#' @noRd
new_mixing_tbl <- function(mixing_matrix, means_matrix , groups, channel_info) {
    ## if mixing_mat is not a list I convert it to always use the same map
    if(!is.list(mixing_matrix)) mixing_matrix <- list(mixing_matrix)
    if(!is.list(means_matrix))  means_matrix <- list(means_matrix)
    mixing_matrix_dt <-
        map2_dtr( mixing_matrix, means_matrix, function(mixm, meansm)  {
            .ICA <- data.table::data.table(.ICA =c("mean",paste0("ICA",seq_len(nrow(mixm)))))
            mm <- rbind(meansm,mixm) %>% data.table::data.table() %>%
                .[, (update_channel_meta_data(.SD, channel_info))] %>%
                cbind(.ICA, .)
            mm
        }
                    ,.id = ".group")
    data.table::setattr(mixing_matrix_dt, "class",c("mixing_tbl",class(mixing_matrix_dt)))
    mixing_matrix_dt[]
}
