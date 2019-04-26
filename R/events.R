#' Annotate artifacts in the  events table of an eeg_lst.
#'
#'   * `eeg_artif_step()`
#'   * `eeg_artif_minmax()`
#' 
#' @param .data An `eeg_lst` object.
#' @param ... Channels to include. All the channels by default, but eye channels should be removed.
#' @param difference Maximum permissible difference in voltage. This is between two consecutive data points for `eeg_artif_step`, and in a `window` for `eeg_artif_minmax`.
#' @param window Sliding window for min-max artifact detection (same unit as `lim`).
#' @param lim Vector indicating the time before and after the event.
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
                                     difference = 100,
                                     lim = c(-200, 200),
                                     window = (lim[2]-lim[1])/2,
                                     unit = "ms"){
    UseMethod("eeg_artif_minmax")
}

#' @name eeg_artif
#' @export
eeg_artif_minmax.eeg_lst <- function(.data,
                                     ...,
                                     difference = 100,
                                     lim = c(-200, 200),
                                     window = (lim[2]-lim[1])/2,
                                     unit = "ms"){
    sample_range <- as.integer(lim * scaling(sampling_rate = sampling_rate(.data),unit = unit))
    win_sample  <- as.integer(window * scaling(sampling_rate = sampling_rate(.data),unit = unit )) + 1L
    if(win_sample == 0) stop("The `window` needs to contain at least one sample.")
    if(win_sample >= (sample_range[2]-sample_range[1])){
        warning("The number of samples in `window` (",win_sample,
                ") should be smaller than half of the samples contained in  `lim` (",
                (sample_range[2]-sample_range[1])/2, ").")
    }
    artifacts_found <- search_artifacts(signal = .data$signal,fun = detect_minmax, ...,
                                        args = list(win_sample=win_sample,
                                                    difference=difference))
    events_tbl(.data) <- add_intervals_from_artifacts(old_events = events_tbl(.data), 
                                                  artifacts_found, 
                                                  sample_range,"minmax")
    validate_eeg_lst(.data)
}



#' @name eeg_artif
#' @export
eeg_artif_step <- function(.data,..., difference = 50 , lim = c(-200, 200), unit = "ms" ) {
    UseMethod("eeg_artif_step")
}

#' @name eeg_artif
#' @export 
eeg_artif_step.eeg_lst <- function(.data, ..., difference = 50 , lim = c(-200, 200), unit = "ms" ) {
    sample_range <- as.integer(lim * scaling(sampling_rate = sampling_rate(.data),unit = unit )) 

    artifacts_found <- search_artifacts(.data$signal,fun = detect_step, ...,
                                        args = list(difference = difference))

    events_tbl(.data) <- add_intervals_from_artifacts(old_events = events_tbl(.data), 
                                                  artifacts_found, 
                                                  sample_range,
                                                  type = "step")

    validate_eeg_lst(.data)
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
#'     otherwise it will consider the entire segment (Default: entire_seg = TRUE). Setting it to FALSE can make the function very slow.
#' @param drop_events If set to `TRUE` (default), the events that were using for setting signals to NA, will be removed from the events table.
#' @rdname eeg_events_to_NA
#' @export 
eeg_events_to_NA.eeg_lst <- function(x, ..., all_chans = FALSE, entire_seg = TRUE,
                                   drop_events = TRUE) {
  dots <- rlang::enquos(...)

  #TODO in data.table
  ## signal <- as.data.frame(x$signal)
  signal <- data.table::copy(x$signal)
  ## x$events <- dplyr::as_tibble(x$events)

  # dots <- rlang::quos(type == "Bad Interval")

  # Hack for match 2 columns with 2 columns, similar to semi_join but allowing
  # for assignment
  baddies <- dplyr::filter(x$events, !!!dots)

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
                                                               signal$.sample_id, b$.initial[i],
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
          signal$.sample_id, b_all$.initial[i],
          b_all$.final[i]
        )),  j = channel_names(x),value = NA_real_)
    }
  } else {
      data.table::set(signal, which(signal$.id %in% b_all$.id), j = channel_names(x), value =  NA_real_)
  }
 

  if (drop_events) {
   x$events<-  anti_join_dt(x$events, filter_dt(x$events, !!!dots) )
  }
x$signal <- signal

validate_eeg_lst(x)

}
