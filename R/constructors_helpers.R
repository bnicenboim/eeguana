#' @noRd
new_eeg_lst <- function(.signal = NULL, .events = NULL, .segments = NULL) {
  x <- list(
    .signal = .signal,
    .events = .events,
    .segments = .segments
  )
  x <- unclass(x)
  structure(x,
    class = c("eeg_lst"),
    vars = character(0)
  )
}



#' @param values
#'
#' @param sampling_rate
#'
#' @noRd
new_sample_int <- function(values, sampling_rate) {
  if (!all(is_wholenumber(values))) {
    stop("Sample integer values should be round numbers.",
      call. = FALSE
    )
  } else {
    values <- as_integer(values)
  }
  values <- unclass(values)
  structure(values,
    class = "sample_int",
    sampling_rate = sampling_rate
  )
}

#' @param .sample
#'
#' @noRd
validate_sample_int <- function(.sample) {
  if (!is.integer(.sample) && 
      ## I also want to accept one Inf number, for e.g., baseline
      !(length(.sample)==1 && is.infinite(.sample))) {
    warning("Samples should be integers.",
      call. = FALSE
    )
  }
  if (length(.sample) > 0) {
    if (attributes(.sample)$sampling_rate <= 0) {
      warning("Attribute sampling_rate should be a positive value.",
        call. = FALSE
      )
    }
  }
  .sample
}

#' @noRd
new_channel_dbl <- function(values, channel_info = list()) {
  values <- unclass(values) %>% as.double()
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
  })

  # if (is.null(attributes(channel)$.reference)) {
  #   warning("Attribute .reference is missing.",
  #     call. = FALSE
  #   )
  # }
  channel
}


#' @param channels
#'
#' @param channels_tbl
#'
#' @noRd
update_channel_meta_data <- function(channels, channels_tbl) {
  if (nrow(channels_tbl) == 0 || is.null(channels_tbl)) {
    channels <- purrr::map(
      channels,
      function(sig) {
        .channel <- new_channel_dbl(
          values = sig,
          channel_info = list(
            .x = NA_real_,
            .y = NA_real_,
            .z = NA_real_,
            .reference = NA_real_
          )
        )
      }
    )
  } else {
    channels <- purrr::map2(
      channels %>% stats::setNames(make_names(channels_tbl$.channel)),
      purrr::transpose(dplyr::select(channels_tbl, -.channel)),
      function(sig, chan_info) {
        .channel <- new_channel_dbl(values = sig, as.list(chan_info))
      }
    )
  }
  channels
}

#
#' @param x
#'
#' @noRd
validate_eeg_lst <- function(x, recursive = TRUE) {
  if (!is_eeg_lst(x)) {
    warning("Class is not eeg_lst", call. = FALSE)
  }

  if (recursive) {
    x$.signal <- validate_signal_tbl(x$.signal)
    x$.events <- validate_events_tbl(x$.events)
    x$.segments <- validate_segments(x$.segments)
  }
  diff_channels <- setdiff(x$.events$.channel, channel_names(x))
  if (length(diff_channels) != 0 & any(!is.na(diff_channels))) {
    warning("Unknown channel in table of events",
      call. = FALSE
    )
  }
  if (!all.equal(unique(x$.signal$.id), unique(x$.segments$.id))) {
    warning("The values of .id mismatch between tables.",
      call. = FALSE
    )
  }

  if (any(!dplyr::group_vars(x) %in% c(colnames(x$.signal), colnames(x$.segments)))) {
    warning("Grouping variables are missing.",
      call. = FALSE
    )
  }
  ## nulls should be caught by the recursive=TRUE
  if (!is.null(sampling_rate(x$.events)) && !is.null(sampling_rate(x$.signal)) &&
    sampling_rate(x$.events) != sampling_rate(x$.signal)) {
    warning("Sampling rates in events and signal table are different",
      call. = FALSE
    )
  }
  x
}

#' @param segments
#'
#' @noRd
validate_segments <- function(segments) {
  if (is.null(segments)) {
    segments <- dplyr::tibble(.id = integer(0))
  }
  if (!is.integer(segments$.id)) {
    warning("Column .id of segments table is not an integer.")
  }
  if (length(segments$.id) != length(unique(segments$.id))) {
    warning("Some .id are repeated in the segments table, there is something wrong going on. Please open an issue with a reproducible example in https://github.com/bnicenboim/eeguana/issues",
      call. = FALSE
    )
  }
  segments
}

#' @param values
#'
#' @noRd
new_component_dbl <- function(values) {
  values <- unclass(values) %>% as.double()
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
