#' Detect artifacts and add them in the events table of an `eeg_lst`
#'
#' These functions search for artifacts on the signal table based on a threshold and a sliding window (when relevant), and annotate an event in the events table that spans from `-lim` to `+lim`. The signal table remains unchanged until [eeg_events_to_NA()].
#'
#' `eeg_artif_peak()` is wrapper around [pracma::findpeaks],  `.threshold` is the minimum (absolute) height a peak has to have to be recognized as such and `.window` is the minimum distance  peaks have to have to be counted.
#'
#' `eeg_artif_minmax()` is also refered as a peak-to-peak artifact detector.  It is less sensitive to drifts than `eeg_artif_peak()`.
#'
#' `eeg_artif_minmax()` and `eeg_artif_step()` can be used to detect blinks and horizontal eye movements in the electro-oculographic (V/HEOG) channels or large voltage jumps in other channels. See chapter 6 of Luck (2014).
#'
#' For the EOG channels, a relatively low threshold (e.g., 30 µV) is recommended. For non EOG channels, a relatively high threshold (e.g., 100 µV) would be more appropriate.
#'
#' @param .data An `eeg_lst` object.
#' @param ... Channels to include. All the channels by default, but eye channels should be removed.
#' @param .threshold Voltage threshold that indicates an artifact
#' @param .direction Whether to look "above" or "below" the threshold.
#' @param .window Sliding window length for the artifact detection (same unit as `lim`). This is the full width of the step function: this means that we are looking for a period of one voltage for half of the window  immediately followed by a period of a different voltage (indicated by the threshold) for half of the window.
#' @param .lim Vector with two values indicating the time before and after the artifact that will be included in events_tbl (by default the size the window before and afterwards).
#' @param .freq Vector with two values indicates whether to prefilter the signal prior to the artifact detection. (The filtering is not saved in the signal). For a low pass filter the first value should be `NA`, for a high-pass filter the second value should be `NA`.
#' @param .config List with the configuration of the filter.
#' @inheritParams as_time
#' @return An `eeg_lst`.
#'
#' @family artifact detection functions
#' @family events functions
#'
#' @name eeg_artif
#' @references
#' Luck, S. J. (2014). An introduction to the event-related potential technique. MIT press.
#'
#' @examples
#' \dontrun{
#'
#' # Artifacts are annotated in the events table:
#' faces_seg_artif <- faces_seg %>%
#'   eeg_artif_minmax(-HEOG, -VEOG, .threshold = 100, .window = 150, unit = "ms") %>%
#'   eeg_artif_step(-HEOG, -VEOG, .threshold = 50, .window = 200, unit = "ms")
#'
#' # Signals with artifacts are turned into NA values:
#' faces_clean <- faces_seg_artif %>%
#'   eeg_events_to_NA(.type == "artifact", .entire_seg = TRUE, .drop_events = TRUE)
#' }
#'
NULL
# > NULL


#' @rdname eeg_artif
#' @export
eeg_artif_minmax <- function(.data,
                             ...,
                             .threshold = 100,
                             .direction = "above",
                             .window = .2,
                             .lim = c(-.window, .window),
                             .unit = "s",
                             .freq = NULL,
                             .config = list()) {
  UseMethod("eeg_artif_minmax")
}

#' @export
eeg_artif_minmax.eeg_lst <- function(.data,
                                     ...,
                                     .threshold = 100,
                                     .direction = "above",
                                     .window = .2,
                                     .lim = c(-.window, .window),
                                     .unit = "s",
                                     .freq = NULL,
                                     .config = list()) {
  .signal <- .data$.signal
  if (!is.null(.freq)) {
    h <- create_filter(
      l_freq = .freq[[1]],
      h_freq = .freq[[2]],
      sampling_rate = sampling_rate(.data),
      config = .config
    )
    .signal <- filt_eeg_lst(.signal, ..., h = h, na.rm = TRUE)
  }


  events_found <- events_artif_custom(
    .signal,
    ...,
    fun = detect_minmax,
    args = list(
      threshold = .threshold,
      lim_samples = lim_samples(.lim, sampling_rate(.data), .unit),
      window_samples = window_samples(.window, sampling_rate(.data), .unit),
      direction = .direction
    )
  )
  events_tbl(.data) <- rbind(events_found, .data$.events, fill = TRUE)
  data.table::setorder(.data$.events, .id, .initial, .channel)
  .data
}

#' @rdname eeg_artif
#' @export
eeg_artif_step <- function(.data,
                           ...,
                           .threshold = 50,
                           .window = .2,
                           .lim = c(-.window, .window),
                           .unit = "s",
                           .freq = NULL,
                           .config = list()) {
  UseMethod("eeg_artif_step")
}
#' @export
eeg_artif_step.eeg_lst <- function(.data,
                                   ...,
                                   .threshold = 50,
                                   .window = .2,
                                   .lim = c(-.window, .window),
                                   .unit = "s",
                                   .freq = NULL,
                                   .config = list()) {
  .signal <- .data$.signal
  if (!is.null(.freq)) {
    h <- create_filter(
      l_freq = .freq[[1]],
      h_freq = .freq[[2]],
      sampling_rate = sampling_rate(.data),
      config = .config
    )
    .signal <- filt_eeg_lst(.signal, ..., h = h, na.rm = TRUE)
  }


  events_found <- events_artif_custom(
    .signal,
    ...,
    fun = detect_step,
    args = list(
      threshold = .threshold,
      lim_samples = lim_samples(.lim, sampling_rate(.data), .unit),
      window_samples = window_samples(.window, sampling_rate(.data), .unit)
    )
  )
  events_tbl(.data) <- rbind(events_found, .data$.events, fill = TRUE)
  data.table::setorder(.data$.events, .id, .initial, .channel)
  .data
}

#' @rdname eeg_artif
#' @export
eeg_artif_amplitude <- function(.data,
                                ...,
                                .threshold = c(-200, 200),
                                .lim = c(-.2, .2),
                                .unit = "s",
                                .freq = NULL,
                                .config = list()) {
  UseMethod("eeg_artif_amplitude")
}

#' @export
eeg_artif_amplitude.eeg_lst <- function(.data,
                                        ...,
                                        .threshold = c(-200, 200),
                                        .lim = c(-.2, .2),
                                        .unit = "s",
                                        .freq = NULL,
                                        .config = list()) {
  if (length(.threshold) < 2) {
    stop("Two thresholds are needed", call. = FALSE)
  }

  .signal <- .data$.signal
  if (!is.null(.freq)) {
    h <- create_filter(
      l_freq = .freq[[1]],
      h_freq = .freq[[2]],
      sampling_rate = sampling_rate(.data),
      config = .config
    )
    .signal <- filt_eeg_lst(.signal, ..., h = h, na.rm = TRUE)
  }


  events_found <- events_artif_custom(
    .signal,
    ...,
    fun = detect_amplitude,
    args = list(
      threshold = c(min(.threshold), max(.threshold)),
      lim_samples = lim_samples(.lim, sampling_rate(.data), .unit)
    )
  )
  events_tbl(.data) <- rbind(events_found, .data$.events, fill = TRUE)
  data.table::setorder(.data$.events, .id, .initial, .channel)
  .data
}

#' @rdname eeg_artif
#' @export
eeg_artif_peak <- function(.data,
                           ...,
                           .threshold = 30,
                           .window = .2,
                           .lim = c(-.window, .window),
                           .unit = "s",
                           .freq = NULL,
                           .config = list()) {
  UseMethod("eeg_artif_peak")
}
#' @export
eeg_artif_peak.eeg_lst <- function(.data,
                                   ...,
                                   .threshold = 30,
                                   .window = .2,
                                   .lim = c(-.window, .window),
                                   .unit = "s",
                                   .freq = NULL,
                                   .config = list()) {
  if (!is.numeric(.window) || .window < 0) {
    stop("`.window` should be a positive number.", call. = FALSE)
  }
  .signal <- .data$.signal
  if (!is.null(.freq)) {
    h <- create_filter(
      l_freq = .freq[[1]],
      h_freq = .freq[[2]],
      sampling_rate = sampling_rate(.data),
      config = .config
    )
    .signal <- filt_eeg_lst(.signal, ..., h = h, na.rm = TRUE)
  }


  events_found <- events_artif_custom(
    .signal,
    ...,
    fun = detect_peak,
    args = list(
      threshold = .threshold,
      lim_samples = lim_samples(.lim, sampling_rate(.data), .unit),
      window_samples = window_samples(.window, sampling_rate(.data), .unit)
    )
  )

  events_tbl(.data) <- rbind(events_found, .data$.events, fill = TRUE)
  data.table::setorder(.data$.events, .id, .initial, .channel)
  .data
}


#' Remove (transform to NA) problematic events from the signal table of an eeg_lst.
#'
#' @param x An `eeg_lst` object.
#' @param ... Description of the problematic event.
#' @param .n_chs  If set to `NULL` (default), it will only set `NA` to the relevant channel.If set to a number `N`, it will set all the channels of the entire segment or interval to `NA`, if `N` or more channels have a certain event, and otherwise only the relevant channel.
#' @param .entire_seg If set to `FALSE`, it will consider only the marked part of the segment, otherwise it will consider the entire segment (Default: .entire_seg = TRUE).
#' @param .drop_events If set to `TRUE` (default), the events that were used for setting signals to NA, will be removed from the events table.
#' @param .all_chs Deprecated.
#'
#' @family events functions
#'
#' @examples
#' \dontrun{
#'
#' # Signals with artifacts are turned into NA values:
#' faces_clean <- faces_seg_artif %>%
#'   eeg_events_to_NA(.type == "artifact", .entire_seg = TRUE, .drop_events = TRUE)
#'
#'
#' # Specific segments are turned into NA values:
#' faces_clean <- faces_seg_artif %>%
#'   eeg_events_to_NA(.id %in% c(1:3), .entire_seg = TRUE, .drop_events = TRUE)
#' }
#'
#' @return An eeg_lst.
#' @export
eeg_events_to_NA <-
  function(x,
           ...,
           .n_chs = NULL,
           .entire_seg = TRUE,
           .drop_events = TRUE,
           .all_chs = FALSE) {
    UseMethod("eeg_events_to_NA")
  }

#' @export
eeg_events_to_NA.eeg_lst <-
  function(x,
           ...,
           .n_chs = NULL,
           .entire_seg = TRUE,
           .drop_events = TRUE,
           .all_chs = FALSE) {
    dots <- rlang::enquos(...)

    # dots <- rlang::quos(.type == "Bad Interval")
    #dots <- rlang::quos(.type == "Bad")
    #dots <- rlang::quos(.type == "artifact")
    
    signal <- data.table::copy(x$.signal)
     
    # Hack for match 2 columns with 2 columns, similar to semi_join but allowing
    # for assignment
    baddies <- filter.events_tbl(x$.events, !!!dots)
    if (!missing(".all_chs")){
      warning("The argument `.all_chs` is deprecated.\n",
              "Set `.n_chs = NULL` to reproduce the behavior of `.all_chs = FALSE`. ",
              "Set `.n_chs = 1` to reproduce the behavior of `.all_chs = TRUE`.")
      if(.all_chs) .n_chs <- 1 else .n_chs <- NULL
    }
    if (!is.null(.n_chs)) {
    ## this doesn't work
            # baddies <- baddies %>% mutate.(.channel = 
            #                 tidytable::case_when(n() > .n_chs ~ NA_character_,
            #                                       TRUE ~ .channel),
            #                                       .by = ".id")
      baddies[baddies[, .I[.N >= .n_chs], .id]$V1, .channel := NA_character_] 
      # it doesn't work:
      # baddies[, if(.N >= .n_chs) .channel := NA_character_, by =.id] 
      
      # baddies <- mutate.(baddies, 
      #                    .channel = ifelse (.N >= .n_chs, NA_character_, .channel),
      #                    .by = ".id")
    }

    # For the replacement in parts of the segments
    b_chans <- filter.events_tbl(baddies, !is.na(.channel)) %>%
      distinct.(.channel) %>%
      tidytable::pull()

    for (ch in b_chans) {
      b <- filter.events_tbl(baddies, .channel == ch, !is.na(.channel))
      if (!.entire_seg) {
        for (i in seq(1, nrow(b))) {
          data.table::set(
            signal,
            i = which(
              signal$.id %in% b$.id[i] & between(
                signal$.sample, b$.initial[i],
                b$.final[i]
              )
            ),
            j = ch,
            NA_real_
          )
        }
      } else {
        data.table::set(
          signal,
          i = which(signal$.id %in% b$.id),
          j = ch,
          value = NA_real_
        )
      }
    }
    # For the replacement in the complete of the segments
    b_all <-
      filter.events_tbl(baddies, is.na(.channel)) %>% distinct.()

    if (!.entire_seg & nrow(b_all) != 0) {
      for (i in seq(1, nrow(b_all))) {
        data.table::set(
          signal,
          which(
            signal$.id == b_all$.id[i] &
              between(
                signal$.sample, b_all$.initial[i],
                b_all$.final[i]
              )
          ),
          j = channel_names(x),
          value = NA_real_
        )
      }
    } else {
      data.table::set(signal,
        which(signal$.id %in% b_all$.id),
        j = channel_names(x),
        value = NA_real_
      )
    }


    if (.drop_events) {
      x$.events <- anti_join.(x$.events, filter.events_tbl(x$.events, !!!dots))
    }
    x$.signal <- signal

    validate_eeg_lst(x)
  }
