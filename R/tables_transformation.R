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
eeg_segment <- function(x, ...) {
  UseMethod("eeg_segment")
}
#' @rdname eeg_segment
#' @param lim Vector indicating the time before and after the event. Or dataframe with two columns, with nrow=total number of segments
#' @param end Description of the event that indicates the end of the segment, if this is used, `lim` is ignored.
#' @param recording_col Column in the segments table indicating to which recording or file each segment belongs.
#' @inheritParams as_time
#' @export
eeg_segment.eeg_lst <- function(x, ..., lim = c(-.5, .5), end, unit = "seconds",
                                recording_col = "recording") {
  dots <- rlang::enquos(...)
  end <- rlang::enquo(end)

  times0 <- dplyr::filter(x$events, !!!dots) %>%
    dplyr::select(-.channel, -.final)

  if(!rlang::quo_is_missing(end)){
    times_end <- dplyr::filter(x$events, !!end) %>%
      dplyr::select(-.channel, -.final) 
  }

  if (rlang::quo_is_missing(end) && any(lim[[2]] < lim[[1]])) {
    stop("A segment needs to be of positive length and include at least 1 sample.")
  }

  if (rlang::quo_is_missing(end) && (length(lim) == 2) || ## two values or a dataframe
    (!is.null(nrow(lim)) && nrow(lim) == nrow(times0)) ) {
    scaling <- scaling(sampling_rate(x), unit = unit)
    sample_lim <- round(lim * scaling) 
    seg_names <- colnames(times0)[!startsWith(colnames(times0),".")]
    segmentation_info <- times0 %>% dplyr::mutate(.lower =.initial+ sample_lim[[1]] %>% as_integer(), 
                                                  .upper = .initial+ sample_lim[[2]] %>% as_integer(),
                                                  .new_id = seq_len(dplyr::n())) %>%
                                    dplyr::select(-dplyr::one_of(seg_names))
  } else if (rlang::quo_is_missing(end)) {
    stop("Wrong dimension of lim")
  } else if(!rlang::quo_is_missing(end) && (nrow(times0) == nrow(times_end)) ) {

    seg_names <- colnames(times0)[!startsWith(colnames(times0),".")]
    segmentation_info <- times0 %>% dplyr::mutate(.lower = .initial, 
                                                  .upper = times_end$.initial,
                                                  .new_id = seq_len(dplyr::n()))%>%
                                    dplyr::select(-dplyr::one_of(seg_names))
  } else {
    stop(sprintf("Number of initial markers (%d) doesn't match the number of final markers (%d)", nrow(times0), nrow(times_end)))
  }

  segmentation <- data.table::as.data.table(segmentation_info)
  segmentation[,.first_sample := sample_int(.initial, sampling_rate = sampling_rate(x))]
  # update the signal tbl:
  cols_signal <- colnames(x$signal)
  cols_signal_temp <- c(".new_id",".initial","x..sample_id",cols_signal[cols_signal!=".id"])

  new_signal <- x$signal[segmentation, on = .(.id, .sample_id >= .lower, .sample_id <= .upper), allow.cartesian=TRUE,
  ..cols_signal_temp]

  
  #.sample_id is now the lower bound
  #x..sample_id is the original columns
  new_signal[, .sample_id := x..sample_id - .initial + 1L][,.initial := NULL][, x..sample_id:=NULL ]
  data.table::setnames(new_signal, ".new_id",".id")
  attributes(new_signal$.sample_id) <- attributes(x$signal$.sample_id)
  data.table::setkey(new_signal, .id, .sample_id)
  x$signal <- new_signal

  

  x$events <- update_events(x$events, segmentation)
  ## data.table::setattr(x$events,"class",c("events_tbl",class(x$events)))


  message(paste0("# Total of ", max(x$signal$.id), " segments found."))

  x$segments <- dplyr::right_join(x$segments,
                dplyr::select(times0, -.initial), by = ".id") %>%
                dplyr::mutate(.id = 1:n()) 

  if(!is.null(recording_col) && !is.na(recording_col)){
  x$segments <- x$segments %>% dplyr::group_by_at(dplyr::vars(recording_col)) %>%
                    dplyr::mutate(segment = 1:n()) %>%
                    dplyr::ungroup()
  } 

  message(paste0(say_size(x), " after segmentation."))
  validate_eeg_lst(x)
}




#' update events table based on a segmentation table
#' segmentation is a data.table with the following columns:
#' * .id:.id of the events
#' * .first_sample:sample_id 1 of the new new segmentation, 1 if left empty
#' * .lower: lower boundary of the event (included)
#' * .upper: upper boundary of the event, (included)
#' * .new_id: new id for the event, current one if left empty
#' @noRd
update_events <- function(events_tbl, segmentation){
    segmentation[, .new_id := if(!".new_id" %in% colnames(segmentation)) .id else .new_id ]
    segmentation[, .first_sample := if(!".initial" %in% colnames(segmentation)) 1 else .first_sample]
    cols_events <- colnames(events_tbl)
    cols_events_temp <- unique(c(cols_events, colnames(segmentation),"i..initial"))
                                        #i..initial is the.initial of events
    new_events <- segmentation[events_tbl, on = .(.id), ..cols_events_temp, allow.cartesian=TRUE][
        .initial <= .upper & .lower <= .final]
    new_events[, .initial := pmax(i..initial, .lower)]
    new_events[, .final:= pmin(.final, .upper)]
    new_events[, .id := .new_id][,..cols_events] %>%
    as_events_tbl(., sampling_rate = sampling_rate(events_tbl))
}

