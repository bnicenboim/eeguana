#' Segment an eeg_lst.
#'
#' Subdivide of the EEG into different segments or epochs. When there is no
#' segmentation, the `eeg_lst` contain one segment. (Fieldtrip calls the
#' segment "trials".) The limits of `segment` are inclusive: If, for
#' example, `lim = c(0,0)`, the segment would contain only sample 1.
#'
#' @param x An `eeg_lst` object.
#' @param ... Description of the event.
#' @param unit Unit
#'
#' @return An `eeg_lst`.
#'
#' @importFrom magrittr %>%
#'
#' @export
segment <- function(x, ...) {
  UseMethod("segment")
}
#' @rdname segment
#' @param lim Vector indicating the time before and after the event. Or dataframe with two columns, with nrow=total number of segments
#' @param end Description of the event that indicates the end of the segment, if this is used, `lim` is ignored.
#' @param recording_col Column in the segments table indicating to which recording or file each segment belogs.
#' @inheritParams as_time
#' @export
segment.eeg_lst <- function(x, ..., lim = c(-.5, .5), end, unit = "seconds", recording_col = "recording") {
  dots <- rlang::enquos(...)
  end <- rlang::enquo(end)

  times0 <- dplyr::filter(x$events, !!!dots) %>%
    dplyr::select(-.channel, -.size) 
  
  if(!rlang::quo_is_missing(end)){
    times_end <- dplyr::filter(x$events, !!end) %>%
      dplyr::select(-.channel, -.size) 
  }

  if (rlang::quo_is_missing(end) && any(lim[[2]] < lim[[1]])) {
    stop("A segment needs to be of positive length and include at least 1 sample.")
  }
  
  
  if (rlang::quo_is_missing(end) && (length(lim) == 2) || ## two values or a dataframe
    (!is.null(nrow(lim)) && nrow(lim) == nrow(times0)) ) {
    scaling <- scaling(sampling_rate(x), unit = unit)
    sample_lim <- round(lim * scaling) 
    seg_names <- colnames(times0)[!startsWith(colnames(times0),".")]
    segmentation_info <- times0 %>% dplyr::mutate(.lower =.sample_0+ sample_lim[[1]] %>% as_integer(), 
                                                  .upper = .sample_0+ sample_lim[[2]] %>% as_integer(),
                                                  .new_id = seq_len(dplyr::n())) %>%
                                    dplyr::select(-dplyr::one_of(seg_names))
  } else if (rlang::quo_is_missing(end)) {
    stop("Wrong dimension of lim")
  } else if(!rlang::quo_is_missing(end) && (nrow(times0) == nrow(times_end)) ) {

    seg_names <- colnames(times0)[!startsWith(colnames(times0),".")]
    segmentation_info <- times0 %>% dplyr::mutate(.lower = .sample_0, 
                                                  .upper = times_end$.sample_0,
                                                  .new_id = seq_len(dplyr::n()))%>%
                                    dplyr::select(-dplyr::one_of(seg_names))
  } else {
    stop(sprintf("Number of initial markers (%d) doesn't match the number of final markers (%d)", nrow(times0), nrow(times_end)))
  }

  segmentation <- data.table::as.data.table(segmentation_info)

  # update the signal tbl:
  cols_signal <- colnames(x$signal)
  cols_signal_temp <- c(".new_id",".sample_0","x..sample_id",cols_signal[cols_signal!=".id"])

  new_signal <- x$signal[segmentation, on = .(.id, .sample_id >= .lower, .sample_id <= .upper), allow.cartesian=TRUE,
  ..cols_signal_temp]

  
  #.sample_id is now the lower bound
  #x..sample_id is the original columns
  new_signal[, .sample_id := x..sample_id - .sample_0 + 1L][,.sample_0 := NULL][, x..sample_id:=NULL ]
  data.table::setnames(new_signal, ".new_id",".id")
  attributes(new_signal$.sample_id) <- attributes(x$signal$.sample_id)
  data.table::setkey(new_signal, .id, .sample_id)
  x$signal <- new_signal

  #update events table
  cols_events <- colnames(x$events)
  cols_events_temp <- unique(c(cols_events, colnames(segmentation_info),"i..sample_0"))
  #i..sample_0 is the sample_0 of events
  new_events <- segmentation[x$events, on = .(.id), ..cols_events_temp, allow.cartesian=TRUE][
                                data.table::between(i..sample_0,.lower - .size + 1 , .upper)]

  new_events[,.size := dplyr::if_else(i..sample_0 < .lower,
                                      as.integer(.size - (.lower - i..sample_0) + 1L),
                                      # adjust the size so that it doesn't spillover after the segment
                                      .size) %>% 
                        pmin(., .upper + 1L - i..sample_0 )][,
               .sample_0 := dplyr::if_else(i..sample_0 < .lower, 
                                           .lower - .sample_0 + 1L,
                                           i..sample_0 - .sample_0 + 1L)  ][, 
               .id := .new_id]

  x$events <- new_events[,..cols_events] 


  message(paste0("# Total of ", max(x$signal$.id), " segments found."))

  x$segments <- dplyr::right_join(x$segments,
                dplyr::select(times0, -.sample_0), by = ".id") %>%
                dplyr::mutate(.id = 1:n()) 

  if(!is.null(recording_col) && !is.na(recording_col)){
  x$segments <- x$segments %>% dplyr::group_by_at(dplyr::vars(recording_col)) %>%
                    dplyr::mutate(segment = 1:n()) %>%
                    dplyr::ungroup()
  } 

  message(paste0(say_size(x), " after segmentation."))
  validate_eeg_lst(x)
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
event_to_ch_NA <- function(x, ...) {
  UseMethod("event_to_ch_NA")
}

#' @param all_chans If set to `TRUE`,
#'     it will consider samples from all channels (Default:  `all_chans = FALSE`).
#' @param entire_seg If set to `FALSE`, it will consider only the marked part of the segment,
#'     otherwise it will consider the entire segment (Default: entire_seg = TRUE). Setting it to FALSE can make the function very slow.
#' @param drop_events If set to `TRUE` (default), the events that were using for setting signals to NA, will be removed from the events table.
#' @rdname event_to_ch_NA
#' @export 
event_to_ch_NA.eeg_lst <- function(x, ..., all_chans = FALSE, entire_seg = TRUE,
                                   drop_events = TRUE) {
  dots <- rlang::enquos(...)

  #TODO in data.table
  x$signal <- as.data.frame(x$signal)
  x$events <- dplyr::as_tibble(x$events)

  # dots <- rlang::quos(type == "Bad Interval")

  # Hack for match 2 columns with 2 columns, similar to semi_join but allowing
  # for assignment
  baddies <- dplyr::filter(x$events, !!!dots)

  if (all_chans) baddies <- dplyr::mutate(baddies, .channel = NA)

  # For the replacement in parts of the segments
  b_chans <- dplyr::filter(baddies, !is.na(.channel)) %>%
    .$.channel %>%
    unique()

  for (c in b_chans) {
    b <- dplyr::filter(baddies, .channel == c & !is.na(.channel))
    if (!entire_seg) {
      for (i in seq(1, nrow(b))) {
        x$signal[[as.character(c)]][x$signal$.id %in% b$.id[i] &
          between(
            x$signal$.sample_id, b$.sample_0[i],
            b$.sample_0[i] + b$.size[i] - 1
          )  ] <- NA
      }
      # could try with na_if, maybe it's faster?
    } else {
      x$signal[[as.character(c)]][x$signal$.id %in% b$.id] <- NA
    }
  }
  # For the replacement in the complete of the segments
  b_all <- dplyr::filter(baddies, is.na(.channel)) %>% dplyr::distinct()

  if (!entire_seg & nrow(b_all) != 0) {
    for (i in seq(1, nrow(b_all))) {
      x$signal[, channel_names(x)][x$signal$.id == b_all$.id[i] &
        between(
          x$signal$.sample_id, b_all$.sample_0[i],
          b_all$.sample_0[i] + b_all$.size[i] - 1
        ), ] <- NA
    }
  } else {
    x$signal[, channel_names(x)][x$signal$.id %in% b_all$.id, ] <- NA
  }

  if (drop_events) {
    x$events <- suppressMessages(dplyr::anti_join(
      x$events,
      dplyr::filter(x$events, !!!dots)
    ))
  }

  #TODO fix this:
  x$signal <- data.table::data.table(x$signal)
  data.table::setattr(x$signal, "class", c("signal_tbl", class(x$signal))) 
  data.table::setkey(x$signal,.id,.sample_id)
  x$events <- data.table::data.table(x$events)
  validate_eeg_lst(x)
}
