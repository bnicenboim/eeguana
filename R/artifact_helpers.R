#' @noRd
eeg_artif_custom <- function(.data, ...,
                             fun,
                             threshold,
                             window = .2,
                             lim = c(-window, window),
                             unit = "s") {
  if (length(lim) < 2) {
    stop("Two values for `lim` are needed", call. = FALSE)
  }

  lim_s <- as_sample_int(lim, sampling_rate = sampling_rate(.data), unit = unit)

  if (!is.null(window)) {
    window_s <- round(as_sample_int(window, sampling_rate = sampling_rate(.data), unit = unit) - 1L)

    if (window_s <= 0) stop("The `window` needs to contain at least one sample.")
    if (window_s >= (lim_s[2] - lim_s[1])) {
      warning(
        "The number of samples in `window` (", window_s,
        ") should be smaller than half of the samples contained in  `lim` (",
        (lim_s[2] - lim_s[1]) / 2, ")."
      )
    }

    args <- list(threshold = threshold, window = window_s)
  } else {
    args <- list(threshold = threshold)
  }

  artifacts_found <- search_artifacts(.data$.signal,
    ...,
    fun = fun,
    args = args
  )

  fun_txt <- substitute(fun) %>%
    stringr::str_remove("detect_")
  args_txt <- purrr::imap_chr(args, ~ paste(.y, toString(.x), sep = "=")) %>%
    paste(collapse = "_")
  events_tbl(.data) <- add_intervals_from_artifacts(
    old_events = events_tbl(.data),
    artifacts_tbl = artifacts_found,
    sample_range = lim_s,
    .type = paste(fun_txt, args_txt, sep = "_")
  )

  validate_eeg_lst(.data)
}

#' @noRd
detect_minmax <- function(x, args = list(window = NULL, threshold = NULL)) {
  rmin <- RcppRoll::roll_minr(x, n = args$window, na.rm = TRUE) # na.rm  allows for comparing vectors that include some NA
  rmax <- RcppRoll::roll_maxr(x, n = args$window, na.rm = TRUE)
  ## If there is only one non NA value, there should be an NA
  rmin[rmin == Inf] <- NA
  rmax[rmax == -Inf] <- NA
  abs(rmin - rmax) >= args$threshold
}
#' @noRd
detect_step <- function(x, args = list(window = NULL, threshold = NULL)) {
  means <- RcppRoll::roll_meanr(x, n = args$window / 2, na.rm = FALSE) # na.rm  allows for comparing vectors that include some NA
  lmean <- means
  rmean <- c(means[seq.int(from = args$window / 2 + 1L, to = length(means))], rep(NA, args$window / 2))
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

add_missing_samples <- function(signal) {
  signal[, list(.sample = sample_int(seq.int(min(.sample), max(.sample)),
    sampling_rate = sampling_rate(signal)
  )), by = .id] %>%
    left_join_dt(signal, by = c(".id", ".sample"))
}

#' add events from a table similar to signal, but with TRUE/FALSE depending if an artifact was detected.
#' @noRd
add_intervals_from_artifacts <- function(old_events, artifacts_tbl, sample_range, .type) {
  events_found <- artifacts_tbl %>%
    split(., .$.id) %>%
    map_dtr(function(.eeg)
      .eeg %>%
        dplyr::select(-dplyr::one_of(obligatory_cols[[".signal"]])) %>%
        imap_dtr(~ {
          if (all(.x[!is.na(.x)] == FALSE)) {
            out <- new_events_tbl()
            out[, .id := NULL][ ## I need to remove .id because it gets added by map
              , .initial := sample_int(integer(0),
                sampling_rate =
                  sampling_rate(old_events)
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
              na.omit() %>%
              .[, .(start = min(start), stop = max(stop)),
                by = .(group = cumsum(c(1, tail((start - 1), -1) > head(stop, -1))))
              ]
            data.table::data.table(
              .type = "artifact",
              .description = .type,
              .initial = sample_int(intervals$start,
                sampling_rate =
                  sampling_rate(old_events)
              ),
              .final = sample_int(intervals$stop,
                sampling_rate =
                  sampling_rate(old_events)
              ),
              .channel = .y
            )
          }
        }), .id = TRUE)
  events_found[, .id := as.integer(.id)]
  message(paste0("# Number of intervals with artifacts: ", nrow(events_found)))
  new_events <- rbind(events_found, old_events, fill = TRUE)
  data.table::setorder(new_events, .id, .initial, .channel)
  new_events
}
