#' Downsample EEG data
#'
#' Downsample a signal_tbl by a factor `q`, using an FIR or IIR filter.
#' This is a wrapper for [`decimate`][signal::decimate] from the
#' [`signal`][signal::signal] package, see its documentation for details. Notice that
#' the code of the signal package might be outdated. This function is used in plotting functions.
#' 
#' A factor q larger than 13 can result in NAs. To avoid this,
#' the downsampling can be done in steps. For example, instead of setting
#' `q = 20`, it is possible to set `q = c(2,10)`.
#'
#' @param .data An eeg_lst object.
#' @param .q integer factor(s) to downsample by.
#' @param .max_sample Optionally, the (approximated) maximum sample number can be defined here, which is at least half of the total number of samples
#' @param .multiple_times Indicates whether to factorize `q`` and apply the downsampling in steps.
#' @inheritParams signal::decimate
#' @param ... Not in use.
#'
#' @family preprocessing functions
#' @family plotting functions
#'
#' @export
eeg_downsample <- function(.data, .q = 2, .max_sample = NULL,
                           .n = if (.ftype == "iir") 8 else 30,
                           .ftype = "iir", .multiple_times = FALSE, ...) {
  UseMethod("eeg_downsample")
}

#' @export
eeg_downsample.eeg_lst <- function(.data, .q = 2, .max_sample = NULL,
                                   .n = if (.ftype == "iir") 8 else 30,
                                   .ftype = "iir", .multiple_times = FALSE, ...) {

  # if(stringr::str_to_lower(.q) == "min") {
  #   .q <- mindiv(sampling_rate(.data), start = 2)
  #  message(paste0("Using .q = ", .q))
  # }

  if (any(.q < 2)) {
    stop("# The factor .q must be 2 or more.")
  }

  if (any(round(.q) != .q)) {
    .q <- round(.q)
    warning(paste0("# The factor .q needs to be round number, using .q = ", .q))
  }

  if (!is.null(.max_sample) && .ftype == "iir") {
    len_samples <- max(nsamples(.data))
    if (.max_sample > len_samples / 2) {
      stop("The maximum value for .max_sample allowed is half the number of samples.")
    }
    approx_q <- len_samples / .max_sample
    .q <- factors(round(approx_q))
  }
  channels_to_decimate <- purrr::map_lgl(.data$.signal, ~ is_channel_dbl(.x) ||
    is_component_dbl(.x)) %>% {
    colnames(.data$.signal)[.]
  }

  ## add missing samples in case of a discontinuity
  .data$.signal <- add_missing_samples(.data$.signal)
  discontinuity <- .data$.signal %>% dplyr::select(channels_to_decimate) %>% anyNA()
  if (discontinuity) {
    warning("Some parts of the signal won't be filtered before the downsampling due to NA values or discontinuities")
  }

  if (.multiple_times == TRUE) {
    .q <- factors(round(.q))
  }

  .q <- as.integer(.q)
  factor <- prod(.q)
  new_sampling_rate <- sampling_rate(.data) / factor
  message(paste0(
    "# Downsampling from ", sampling_rate(.data), "Hz to ",
    round(new_sampling_rate, 2), "Hz."
  ))
  if (!is.null(.max_sample) || multiple_times) {
    message("# Using the following factor(s) .q: ", paste0(.q, collapse = ", "))
  }



  channels_info <- channels_tbl(.data)

  .data$.signal <-
    ## The first sample corresponds to the min(time) in sample units
    .data$.signal[, c(
      list(.sample = new_sample_int(seq.int(
        from = ceiling(min(.sample) / factor),
        length.out = .N / factor
      ), sampling_rate = new_sampling_rate)),
      lapply(.SD, decimate_ch, q = .q, n = .n, ftype = .ftype)
    ),
    .SDcols = c(channels_to_decimate), by = c(".id")
    ]


  data.table::setkey(.data$.signal, .id, .sample)
  data.table::setcolorder(.data$.signal, c(".id", ".sample"))

  # even table needs to be adapted, starts from 1,
  # and the size is divided by two with a min of 1
  .data$.events <- data.table::copy(.data$.events)[, .initial :=
    sample_int(ceiling(.initial / factor), new_sampling_rate)][, .final := sample_int(ceiling(.final / factor), new_sampling_rate) ][]

  ## this shouldn't be needed:
  ## just in case I update the .id from segments table
  ## .data$.segments <- dplyr::mutate(.data$.segments, .id = seq_len(dplyr::.n()))

  message(say_size(.data))

  .data %>% # update_channels_tbl(channels_info) %>%
    validate_eeg_lst()
}
