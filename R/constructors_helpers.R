
#' @noRd
declass <- function(signal) {
  # extracting attributes
  attr <- purrr::imap(signal, ~attributes(.x))

  class(signal) <- class(signal)[class(signal) != "signal_tbl"]
  # removes the classes of the sample_id and channels so that the attributes are ignored
  declassed_signal <- mutate_all(signal, unclass) %>%
    purrr::modify(~`attributes<-`(.x, NULL))
  list(tbl = declassed_signal, attr = attr)
}

#' @param tbl 
#'
#' @param attr 
#'
#' @noRd
reclass <- function(tbl, attr) {
  old_attr_tbl <- attributes(tbl)
  tbl <- purrr::imap_dfc(tbl, ~`attributes<-`(
    .x,
    if (.y == ".id") {
      list("class" = NULL)
    } else if (.y %in% names(attr)) {
      attr[[.y]]
    } else {
      list(
        "class" = "channel",
        ".x" = NA_real_,
        ".y" = NA_real_,
        ".z" = NA_real_,
        ".reference" = NA
      )
    }
  ))
  attributes(tbl) <- old_attr_tbl
  class(tbl) <- c("signal_tbl", class(tbl))
  tbl
}

#' @param values 
#'
#' @param sampling_rate 
#'
#' @noRd
new_sample_id <- function(values, sampling_rate) {
  if (all(!is.na(values)) & any(values != round(values))) {
    stop("Values should be round numbers.",
      call. = FALSE
    )
  } else {
    values <- as.integer(values)
  }
  values <- unclass(values)
  structure(values,
    class = "sample_id",
    sampling_rate = sampling_rate
  )
}




#' @param sample_id 
#'
#' @noRd
validate_sample_id <- function(sample_id) {
  if (!is.integer(sample_id)) {
    stop("Values should be integers.",
      call. = FALSE
    )
  }
  if (length(sample_id) > 0) {
    if (attributes(sample_id)$sampling_rate <= 0) {
      stop("Attribute sampling_rate should be a positive value.",
        call. = FALSE
      )
    }
  }
  sample_id
}

#' @noRd
new_channel <- function(values, channel_info = list()) {
  values <- unclass(values)
  attributes(values) <- c(
    class = "channel",
    channel_info
  )
  values
}

#' @param channel 
#'
#' @noRd
validate_channel <- function(channel) {
  if (!is.numeric(channel)) {
    stop("Values should be numeric.",
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
new_signal <- function(signal_matrix = matrix(), ids = c(), sample_ids = c(), channel_info = dplyr::tibble()) {
  raw_signal <- dplyr::as_tibble(signal_matrix)

  raw_signal <- update_channel_meta_data(raw_signal, channel_info)

  signal <- tibble::tibble(.id = ids, .sample_id = sample_ids) %>%
    dplyr::bind_cols(raw_signal)

  class(signal) <- c("signal_tbl", class(signal))
  signal
}

#' @param channels 
#'
#' @param channel_info 
#'
#' @noRd
update_channel_meta_data <- function(channels, channel_info) {
  if (nrow(channel_info) == 0 | is.null(channel_info)) {
    channels <- purrr::map_dfc(
      channels,
      function(sig) {
        channel <- new_channel(value = sig)
      }
    )
  } else {
    channels <- purrr::map2_dfc(
      channels %>% setNames(channel_info$.name), purrr::transpose(channel_info),
      function(sig, chan_info) {
        channel <- new_channel(value = sig, as.list(chan_info))
      }
    )
  }
  channels
}

#' @param signal 
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
    class = c("eeg_lst")
  )
}

#' @param x 
#'
#' @noRd
validate_eeg_lst <- function(x) {
  validate_signal(x$signal)
  validate_events(x$events, channel_names(x))
  validate_segments(x$segments)
  if (any(unique(x$signal$.id) != unique(x$segments$.id))) {
    warning("The values of .ids mismatch between tables.",
      call. = FALSE
    )
  }
  x
}

#' @param signal 
#'
#' @noRd
validate_signal <- function(signal) {
  # Validates .id
  if (!is.integer(signal$.id)) {
    warning(".id should be an integer.",
      call. = FALSE
    )
  }

  if (all(unique(signal$.id) != seq_len(max(signal$.id)))) {
    warning("Missing .ids, some functions might fail.",
      call. = FALSE
    )
  }
  # Validates sample_id
  validate_sample_id(signal$.sample_id)

  # Validates channels (first row is enough, and takes less memory)
  dplyr::slice(ungroup(signal), 1) %>%
    purrr::walk(~if (is_channel(.x)) validate_channel(.x))

  signal
}

#' @param events 
#'
#' @param channels 
#'
#' @noRd
validate_events <- function(events, channels) {
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
  # Validates .id
  if (all(unique(segments$.id) != seq_len(max(segments$.id)))) {
    warning("Missing .ids, some functions might fail.",
      call. = FALSE
    )
  }
  segments
}

obligatory_cols <- list(
  signal = c(.id = ".id", .sample_id = ".sample_id"),
  events = c(.id = ".id", .sample_0 = ".sample_0", .size = ".size", .channel = ".channel"),
  segments = c(.id = ".id")
)
