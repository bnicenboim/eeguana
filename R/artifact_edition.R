#' Detect artifacts and add them in the events table of an eeg_lst.
#'
#'   * `eeg_artif_step()`
#'   * `eeg_artif_minmax()`
#' 
#' `eeg_artif_minmax()`and `eeg_artif_step()` can be used to detect blinks and horizontal eye movements
#'  in the electro-oculographic (V/HEOG) channels or large voltage jumps in other channels. 
#'  For the EOG channels, a relatively low threshold (e.g., 30 µV) is recommended. For non EOG channels, a relatively high threshold (e.g., 100 µV) would be 
#'  more appropiate.
#'    
#' @param .data An `eeg_lst` object.
#' @param ... Channels to include. All the channels by default, but eye channels should be removed.
#' @param threshold Voltage threshold that indicates an artifact. This is between two consecutive data points for `eeg_artif_step`, and in a `window` for `eeg_artif_minmax`.
#' @param window Sliding window length for the artifact detection (same unit as `lim`).
#' @param lim Vector indicating the time before and after the artifact that will be included in events_tbl (by default the size the window before and afterwards).
#' @inheritParams as_time
#' @return An `eeg_lst`.
#'
#' @importFrom magrittr %>%
#'
#' @name eeg_artif
#' 
NULL
#' > NULL


#' @name eeg_artif
#' @export
eeg_artif_minmax <- function(.data,...,
                                     threshold = 100,
                                     window = .2,
                                     lim = c(-window, window),
                                     unit = "s"){
    UseMethod("eeg_artif_minmax")
}

#' @name eeg_artif
#' @export
eeg_artif_minmax.eeg_lst <- function(.data,
                                     ...,
                                     threshold = 100,
                                     window = .2,
                                     lim = c(-window, window),
                                     unit = "s"){
    if(!is.numeric(window) || window < 0) {
        stop("`window` should be a positive number.", call. = FALSE)
    }


    eeg_artif_custom(.data,...,
                     fun = detect_minmax,
                     threshold = threshold,
                     lim = lim,
                     window = window,
                     unit = unit)
}



#' @name eeg_artif
#' @export
eeg_artif_step <- function(.data,..., 
                           threshold = 50,
                                     window = .2,
                                     lim = c(-window, window),
                                     unit = "s"){
    UseMethod("eeg_artif_step")
}

#' @name eeg_artif
#' @export 
eeg_artif_step.eeg_lst <- function(.data,..., 
                           threshold = 50,
                                     window = .2,
                                     lim = c(-window, window),
                                     unit = "s"){
    if(!is.numeric(window) || window < 0) {
        stop("`window` should be a positive number.", call. = FALSE)
    }


    eeg_artif_custom(.data,...,
        fun = detect_step,
                threshold = threshold,
                lim = lim,
                window = window,
                unit = unit )
 }

#' @name eeg_artif
#' @export
eeg_artif_amplitude <- function(.data,..., 
                                threshold = c(-200,200),
                           lim = c(-.2, .2),
                           unit = "s"){
    UseMethod("eeg_artif_amplitude")
}

#' @name eeg_artif
#' @export 
eeg_artif_amplitude.eeg_lst <- function(.data,..., 
                                   threshold = c(-200,200),
                                   lim = c(-.2, .2),
                                   unit = "s"){
    if(length(threshold)<2) {
        stop("Two thresholds are needed", call. = FALSE)
    }
    
    eeg_artif_custom(.data,...,
                     fun = detect_amplitude,
                     threshold = c(min(threshold),max(threshold)),
                     window= NULL,
                     lim = lim,
                     unit = unit )
}




#' Remove (transform to NA) problematic events from an eeg_lst.
#'
#' @param x An `eeg_lst` object.
#' @param ... Description of the problematic event.
#'
#'
#' @return An eeg_lst.
#'
#' @importFrom magrittr %>%
#'
#' @export
eeg_events_to_NA <- function(x, ...) {
  UseMethod("eeg_events_to_NA")
}


#' @param all_chans If set to `TRUE`,
#'     it will consider samples from all channels (Default:  `all_chans = FALSE`).
#' @param entire_seg If set to `FALSE`, it will consider only the marked part of the segment,
#'     otherwise it will consider the entire segment (Default: entire_seg = TRUE). 
#' @param drop_events If set to `TRUE` (default), the events that were used for setting signals to NA, will be removed from the events table.
#' @rdname eeg_events_to_NA
#' @export 
eeg_events_to_NA.eeg_lst <- function(x, ..., all_chans = FALSE, entire_seg = TRUE,
                                   drop_events = TRUE) {
  dots <- rlang::enquos(...)

  #TODO in data.table
  ## signal <- as.data.frame(x$.signal)
  signal <- data.table::copy(x$.signal)
  ## x$.events <- dplyr::as_tibble(x$.events)

  # dots <- rlang::quos(.type == "Bad Interval")

  # Hack for match 2 columns with 2 columns, similar to semi_join but allowing
  # for assignment
  baddies <- dplyr::filter(x$.events, !!!dots)

  if (all_chans) baddies <- dplyr::mutate(baddies, .channel = NA)

  # For the replacement in parts of the segments
  b_chans <- dplyr::filter(baddies, !is.na(.channel)) %>%
      dplyr::distinct(.channel) %>%
      dplyr::pull()

  for (ch in b_chans) {
    b <- dplyr::filter(baddies, .channel == ch,!is.na(.channel))
    if (!entire_seg) {
        for (i in seq(1, nrow(b))) {
            data.table::set(signal,i = which(signal$.id %in% b$.id[i] & between(
                                                               signal$.sample, b$.initial[i],
                                                               b$.final[i]
                                                           )), j = ch, NA_real_)
       }
    } else {
     data.table::set(signal,i = which(signal$.id %in% b$.id), j = ch, value =  NA_real_)
    }
  }
  # For the replacement in the complete of the segments
  b_all <- dplyr::filter(baddies, is.na(.channel)) %>% dplyr::distinct()

  if (!entire_seg & nrow(b_all) != 0) {
    for (i in seq(1, nrow(b_all))) {
      data.table::set(signal, which(signal$.id == b_all$.id[i] &
        between(
          signal$.sample, b_all$.initial[i],
          b_all$.final[i]
        )),  j = channel_names(x),value = NA_real_)
    }
  } else {
      data.table::set(signal, which(signal$.id %in% b_all$.id), j = channel_names(x), value =  NA_real_)
  }
 

  if (drop_events) {
   x$.events<-  anti_join_dt(x$.events, filter_dt(x$.events, !!!dots) )
  }
x$.signal <- signal

validate_eeg_lst(x)

}
