new_sample_n <- function(values, sampling_rate) {
  if(any(values != round(values))) {
    stop("Values should be round numbers.",
      call. = FALSE
    )
  } else {
    values <- as.integer(values)
  }
  values <- unclass(values)
  structure(values,
    class = "sample_n",
    sampling_rate = sampling_rate
  )
}

validate_sample_n <- function(sample_n){
  if (!is.integer(sample_n)){
    stop("Values should be integers.",
      call. = FALSE)
  }
  if(attributes(sample_n)$sampling_rate <=0){
  stop("Attribute sampling_rate should be a positive value.",
      call. = FALSE)
  }
  sample_n
}


new_channel <- function(values, channel_info = list()) {
  values <- unclass(values)
  attributes(values) <- c(class = "channel",
    channel_info)
  values
}


validate_channel <- function(channel){
  if (!is.numeric(channel)){
    stop("Values should be numeric.",
      call. = FALSE)
  }
  purrr::walk(c("x","y","z"), ~ 
    if(!is.numeric(attr(channel, .))) 
      warning(sprintf("Attribute %s should be a number.",.),
      call. = FALSE))

  if(is.null(attributes(channel)$reference)){
  warning("Attribute reference is missing.",
      call. = FALSE)
  }
  channel
}



new_eegble <- function(signal = NULL, events = NULL, segments = NULL) {
  x <- list(
    signal = signal, events = events,
 segments = segments
  )
  x <- unclass(x)
  structure(x,
    class = c("eegble")
  )
}

validate_eegble <- function(x) {
  validate_signal(x$signal)
  validate_events(x$events, channel_names(x))
  validate_segments(x$segments)
  if(any(unique(x$signal$.id) != unique(x$segments$.id)) | 
    any(unique(x$signal$.id) != unique(x$events$.id))){
    warning("The values of .ids mismatch between tables.",
      call. = FALSE
    )
  }
  x
}

validate_signal <- function(signal) {
  # Validates .id
  if(all(unique(signal$.id) != seq.int(max(signal$.id)))) {
    warning("Missing .ids, some functions might fail.",
      call. = FALSE
    )
  }
  # Validates sample_n
  # validate_sample_n(signal$.sample_n)

  # Validates channels
  dplyr::select(signal, -.id, -.sample_n) %>%
    purrr::walk( ~ validate_channel(.x))

  signal
}


validate_events <- function(events, channels) {

  if(!is.integer(events$.sample_0)){
     warning("Values of .sample_0 should be integers",
      call. = FALSE
    )
  }   

  if(!is.integer(events$.size)){
     warning("Values of .size should be integers",
      call. = FALSE
    )
  }   

  diff_channels <- setdiff(events$.channel, channels)
  if(length(diff_channels) != 0 & any(!is.na(diff_channels))){
     warning("Unknown channel in table of events",
      call. = FALSE
    )
  } 

  events
}

validate_segments <- function(segments) {
  # Validates .id
  if(all(unique(segments$.id) != seq.int(max(segments$.id)))) {
    warning("Missing .ids, some functions might fail.",
      call. = FALSE
    )
  }

}

reserved_cols_signal <- c(".id",".sample_n")

