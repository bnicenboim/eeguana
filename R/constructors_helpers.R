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
new_signal_tbl <- function(signal_matrix = matrix(), ids = c(), sample_ids = c(), channel_info = dplyr::tibble()) {

  if(!data.table::is.data.table(signal_matrix)) {
    signal_matrix <- data.table::data.table(signal_matrix)
  }

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
  if (nrow(channel_info) == 0 | is.null(channel_info)) {
    channels <- purrr::map(
      channels,
      function(sig) {
        channel <- new_channel_dbl(values = sig)
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
<<<<<<< HEAD
    x$signal <- validate_signal_tbl(x$signal)
    x$events <- validate_events(x$events, channel_names(x))
    x$segments <- validate_segments(x$segments)
    if (!all.equal(unique(x$signal$.id), unique(x$segments$.id))) {
        warning("The values of .ids mismatch between tables.",
                call. = FALSE
                )
    }
=======
  x$signal <- validate_signal_tbl(x$signal)
  x$events <- validate_events(x$events, channel_names(x))
  x$segments <- validate_segments(x$segments)
  if (!all.equal(unique(x$signal$.id), unique(x$segments$.id))) {
    warning("The values of .ids mismatch between tables.",
      call. = FALSE
    )
  }
>>>>>>> feature-ica

    if(any(!group_chr(x) %in% c(colnames(x$signal),colnames(x$segments)))){
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
    warning("'signal' be a data.table.",
      call. = FALSE
    )
  }

  if (!is.integer(signal_tbl$.id)) {
    warning(".id should be an integer.",
      call. = FALSE
    )
  }

  if (length(signal_tbl$.id) >0 && all(unique(signal_tbl$.id) != seq_len(max(signal_tbl$.id)))) {
    warning("Missing .ids, some functions might fail.",
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
validate_events <- function(events, channels) {
    if(is.null(events)) {
        events <- data.table::data.table(.id= integer(0),
                                         .sample_0= integer(0),
                                         .size= integer(0),
                                         .channel= integer(0))
    }
    if(!data.table::is.data.table(events) && is.data.frame(events)) {
        events <- data.table::as.data.table(events)
    }

if (!data.table::is.data.table(events)) {
    warning("'events' be a data.table.",
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

  diff_channels <- setdiff(events$.channel, channels)
  if (length(diff_channels) != 0 & any(!is.na(diff_channels))) {
    warning("Unknown channel in table of events",
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
  # Validates .id
  if (length(segments$.id) >0 && all(segments$.id != seq_len(max(segments$.id)))) {
    warning("Missing .ids, some functions might fail.",
      call. = FALSE
    )
  }
  if( length(segments$.id) != length(unique(segments$.id)) ){
        warning("Some .ids are repeated in the segments table, some functions might fail.",
      call. = FALSE
    )
  }
  segments
}
#' @param values
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

#' @param mixing_matrix 
#' @param components 
#' @param channel_info 
#'
#' @noRd
new_mixing_tbl <- function( mixing_matrix = matrix(), components = c(), channel_info = dplyr::tibble()) {

    if(!data.table::is.data.table(mixing_matrix)) {
        mixing_matrix <- data.table::data.table(mixing_matrix)
    }

    mixing_tbl <- mixing_matrix[, (update_channel_meta_data(.SD, channel_info)),
                                .SDcols=colnames(mixing_matrix)]

    data.table::setattr(mixing_tbl, "class",c("mixing_tbl",class(mixing_tbl)))
    mixing_tbl[]
}
