#' @noRd
window_samples <- function(window, sampling_rate, unit) {
  if (!is.numeric(window) || window < 0) {
    stop("`window` should be a positive number.", call. = FALSE)
  }
  window_s <- round(as_sample_int(window, 
                                  .sampling_rate = sampling_rate,
                                  .unit = unit) - 1L)
  if (window_s <= 0) {
    stop("The `window` needs to contain at least one sample.")
  }
  window_s
}

#' @noRd
lim_samples <- function(lim, sampling_rate, unit) {
  if (length(lim) < 2) {
    stop("Two values for `lim` are needed", call. = FALSE)
  }
  as_sample_int(lim, .sampling_rate = sampling_rate, .unit = unit)
}



#' @noRd
events_artif_custom <- function(.signal, ...,
                                fun,
                                args) {
  if ("window_samples" %in% names(args) &&
    args$window_samples >= (args$lim_samples[2] - args$lim_samples[1])) {
    warning(
      "The number of samples in `window` (", args$window_samples,
      ") should be smaller than half of the samples contained in  `lim` (",
      (args$lim_samples[2] - args$lim_samples[1]) / 2, ")."
    )
  }

  artifacts_found <- search_artifacts(.signal,
    ...,
    fun = fun,
    args = args
  )

  fun_txt <- substitute(fun) %>%
    chr_remove("detect_")
  args_txt <- imap_chr(args, ~ paste(.y, toString(.x), sep = "=")) %>%
    paste(collapse = "_")

  # new events table:
  add_intervals_from_artifacts(
    sampling_rate = sampling_rate(.signal),
    artifacts_tbl = artifacts_found,
    sample_range = args$lim_samples,
    .type = paste(fun_txt, args_txt, sep = "_")
  )
}

#' @noRd
detect_minmax <- function(x, args = list(window_samples = NULL, threshold = NULL, direction = "above")) {
  rmin <- RcppRoll::roll_minr(x, n = args$window_samples, na.rm = TRUE) # na.rm  allows for comparing vectors that include some NA
  rmax <- RcppRoll::roll_maxr(x, n = args$window_samples, na.rm = TRUE)
  ## If there is only one non NA value, there should be an NA
  rmin[rmin == Inf] <- NA
  rmax[rmax == -Inf] <- NA
  if (args$direction %in% tolower(c("above", ">", ">="))) {
    abs(rmin - rmax) >= args$threshold
  } else if (args$direction %in% tolower(c("below", "<", "<="))) {
    abs(rmin - rmax) <= args$threshold
  } else {
    stop("The argument `direction` can only include 'above' or 'below'", call. = FALSE)
  }
}

#' @noRd
detect_peak <- function(x, args = list(window_samples = NULL, threshold = NULL)) {
  ## TODO better version of findpeaks
  peaks <- pracma::findpeaks(c(x),
    minpeakheight = args$threshold,
    minpeakdistance = args$window_samples,
    ## setting threshold for avoiding flat peaks
    threshold = .0001
  )[, 2]
  x <- rep(FALSE, length(x))
  x[peaks] <- TRUE
  x
}

#' @noRd
detect_step <- function(x, args = list(window_samples = NULL, threshold = NULL)) {
  means <- RcppRoll::roll_meanr(x, n = args$window_samples / 2, na.rm = FALSE) # na.rm  allows for comparing vectors that include some NA
  lmean <- means
  rmean <- c(means[seq.int(from = args$window_samples / 2 + 1L, to = length(means))], rep(NA, args$window / 2))
  abs(rmean - lmean) >= args$threshold
}

#' @noRd
detect_amplitude <- function(x, args = list(threshold = NULL)) {
  x <= args$threshold[1] | x >= args$threshold[2]
}

#' @noRd
search_artifacts <- function(signal, ..., fun, args = list()) {
  ch_sel <- sel_ch(signal, ...)

    ## in case there are missing .samples
  add_missing_samples(signal) %>%
    .[, c(
      list(.sample = .sample),
      lapply(.SD, fun, args)
    ),
    .SDcols = (ch_sel), by = .id
    ]
}

#' @noRd
add_missing_samples <- function(signal) {
  signal[, list(.sample = sample_int(seq.int(min(.sample), max(.sample)),
    .sampling_rate = sampling_rate(signal)
  )), by = .id] %>%
    left_join.(signal, by = c(".id", ".sample"))
}

#' add events from a table similar to signal, but with TRUE/FALSE depending if an artifact was detected.
#' @noRd
add_intervals_from_artifacts <- function(sampling_rate, artifacts_tbl, sample_range, .type) {
  events_found <- artifacts_tbl %>%
    split(., .$.id) %>%
    map_dtr(function(.eeg) {
      .eeg %>%
        select.(-tidyselect::one_of(obligatory_cols[[".signal"]])) %>%
        imap_dtr(~ {
          if (all(.x[!is.na(.x)] == FALSE)) {
            out <- new_events_tbl()
            out[, .id := NULL][ ## I need to remove .id because it gets added by map
              , .initial := sample_int(integer(0),
                .sampling_rate =
                  sampling_rate
              )
            ][]
          } else {
            ## left and right values of the window of bad values (respecting the min max samples)
            left <- .eeg$.sample[.x] + sample_range[[1]] - 1L
            ## the smallest sample is one less than the present one because diff() in artifact_found reduces the vector by 1
            left[left < (min(.eeg$.sample) - 1L)] <- (min(.eeg$.sample))
            right <- .eeg$.sample[.x] + sample_range[[2]] - 1L
            right[right > max(.eeg$.sample)] <- max(.eeg$.sample)
            ## merge if there are steps closer than the window for removal
            intervals <- data.table::data.table(start = left, stop = right) %>%
              stats::na.omit() %>%
              .[, .(start = min(start), stop = max(stop)),
                by = .(group = cumsum(c(1, tail((start - 1), -1) > head(stop, -1))))
              ]
            data.table::data.table(
              .type = "artifact",
              .description = .type,
              .initial = sample_int(intervals$start,
                .sampling_rate =
                  sampling_rate
              ),
              .final = sample_int(intervals$stop,
                .sampling_rate =
                  sampling_rate
              ),
              .channel = .y
            )
          }
        })
    }, .id = TRUE)
  events_found[, .id := as.integer(.id)]
  message_verbose(paste0("# Number of intervals with artifacts: ", nrow(events_found)))
  events_found
}
