#' Segment an eeg_lst.
#'
#' Subdivide of the EEG into different segments or epochs. When there is no
#' segmentation, the `eeg_lst` contain one segment. (Fieldtrip calls the
#' segment "trials".) The limits of `segment` are inclusive: If, for
#' example, `lim = c(0,0)`, the segment would contain only sample 1.
#'
#' @param .data An `eeg_lst` object.
#' @param ... Description of the event.
#' @param unit Unit
#' @param lim Vector indicating the time before and after the event. Or dataframe with two columns, with nrow=total number of segments
#' @param end Description of the event that indicates the end of the segment, if this is used, `lim` is ignored.
#' @inheritParams as_time
#' @family preprocessing functions
#'
#' @return An `eeg_lst`.
#'
#'
#' @export
eeg_segment <- function(.data, ..., lim = c(-.5, .5), end, unit = "s") {
  UseMethod("eeg_segment")
}
#' @export
eeg_segment.eeg_lst <- function(.data, ..., lim = c(-.5, .5), end, unit = "s") {
  dots <- rlang::enquos(...)
  end <- rlang::enquo(end)


  ## .data$.events needs to stop being an events_tbl in order to remove stuff from it.
  ## if not, it calls to filter.events_tbl
  times0 <- dplyr::filter(dplyr::as_tibble(.data$.events), !!!dots) %>%
    dplyr::select(-.channel, -.final) %>%
    dplyr::rename(.first_sample = .initial) %>%
    dplyr::distinct()

  if (!rlang::quo_is_missing(end)) {
    times_end <- dplyr::filter(dplyr::as_tibble(.data$.events), !!end) %>%
      dplyr::select(-.channel, -.final) %>%
      dplyr::rename(.first_sample = .initial) %>%
    dplyr::distinct()
  }

  if (rlang::quo_is_missing(end) && any(lim[[2]] < lim[[1]])) {
    stop("A segment needs to be of positive length and include at least 1 sample.")
  }

  if (rlang::quo_is_missing(end) && (length(lim) == 2) || ## two values or a dataframe
    (!is.null(nrow(lim)) && nrow(lim) == nrow(times0))) {
    scaling <- scaling(sampling_rate(.data), unit = unit)
    sample_lim <- round(lim * scaling)
    seg_names <- colnames(times0)[!startsWith(colnames(times0), ".")]
    segmentation_info <- times0 %>%
      dplyr::mutate(
        .lower = .first_sample + sample_lim[[1]] %>% as_integer(),
        .upper = .first_sample + sample_lim[[2]] %>% as_integer(),
        .new_id = seq_len(dplyr::n())
      ) %>%
      dplyr::select(-dplyr::one_of(seg_names))
  } else if (rlang::quo_is_missing(end)) {
    stop("Wrong dimension of lim")
  } else if (!rlang::quo_is_missing(end)) {
    
#          warning(sprintf("Number of initial markers (%d) doesn't match the number of final markers (%d)", nrow(times0), nrow(times_end)))
          times0 <- dplyr::mutate(times0,.zero=TRUE)
          times_end <- dplyr::mutate(times_end,.zero=FALSE) 
          times <- dplyr::bind_rows(times0,times_end) %>%
              dplyr::arrange(.id, .first_sample) 
          unmatched_initial <- times %>%
              dplyr::filter(dplyr::lead(.zero, default = TRUE), .zero) %>%
              dplyr::select(-.zero)
          unmatched_final <- times %>% 
              dplyr::filter(!dplyr::lag(.zero,default = FALSE), !.zero)%>%
              dplyr::select(-.zero)
    if(nrow(unmatched_initial) >0){
      warning("Unmatched initial segments:\n\n",  paste0(
          utils::capture.output(unmatched_initial%>%
        dplyr::rename(.initial = .first_sample)),
          collapse = "\n"
        ))
      times_end <- dplyr::bind_rows(times_end, unmatched_initial) %>%
                dplyr::arrange(.id, .first_sample)
      times0 <- dplyr::anti_join(times0, unmatched_initial, by =c(".id",".type",".description",".first_sample")) %>%
                dplyr::bind_rows(unmatched_initial %>% dplyr::mutate(.type = "incorrect segment")) %>%
                dplyr::arrange(.id, .first_sample)
    } 
    if(nrow(unmatched_final) >0){
      warning("Unmatched final segments:\n\n",  paste0(
          utils::capture.output(unmatched_final%>%
        dplyr::rename(.initial = .first_sample)),
          collapse = "\n"
        ))
      times0 <- dplyr::bind_rows(times0, unmatched_final%>%
            dplyr::mutate(.type = "incorrect segment")) %>%
                dplyr::arrange(.id, .first_sample)
    }
    times0 <- times0 %>%  dplyr::select(-.zero)
    times_end <- times_end %>%  dplyr::select(-.zero)
    
    seg_names <- colnames(times0)[!startsWith(colnames(times0), ".")]
    segmentation_info <- times0 %>%
      dplyr::mutate(
        .lower = .first_sample,
        .upper = times_end$.first_sample,
        .new_id = seq_len(dplyr::n())
      ) %>%
      dplyr::select(-dplyr::one_of(seg_names))
  }       

  segmentation <- data.table::as.data.table(segmentation_info)
  segmentation[, .first_sample := sample_int(.first_sample, sampling_rate = sampling_rate(.data))]
  # update the signal tbl:
  cols_signal <- colnames(.data$.signal)
  cols_signal_temp <- c(".new_id", ".first_sample", "x..sample", cols_signal[cols_signal != ".id"])

  new_signal <- .data$.signal[segmentation,
    on = .(.id, .sample >= .lower, .sample <= .upper), allow.cartesian = TRUE,
    ..cols_signal_temp
  ]


  # .sample is now the lower bound
  # x..sample is the original columns
  new_signal[, .sample := x..sample - .first_sample + 1L][, .first_sample := NULL][, x..sample := NULL ]
  data.table::setnames(new_signal, ".new_id", ".id")
  attributes(new_signal$.sample) <- attributes(.data$.signal$.sample)
  data.table::setkey(new_signal, .id, .sample)
  .data$.signal <- new_signal



  .data$.events <- update_events(.data$.events, segmentation[, c(".type", ".description") := NULL][])
  ## data.table::setattr(.data$.events,"class",c("events_tbl",class(.data$.events)))


  message(paste0("# Total of ", max(.data$.signal$.id), " segments found."))

  # remove the . from the segments so that it's clear that it's not protected
  data.table::setnames(times0, -1, stringr::str_remove(colnames(times0)[-1], "^\\."))

  .data$.segments <- dplyr::right_join(.data$.segments,
    dplyr::select(times0, -first_sample),
    by = ".id"
  ) %>%
    dplyr::mutate(.id = 1:dplyr::n())


  if (!is.null(.data$.segments$.recording) && !anyNA(.data$.segments$.recording)) {
    .data$.segments <- .data$.segments %>%
      dplyr::group_by(.recording) %>%
      dplyr::mutate(segment = 1:dplyr::n()) %>%
      dplyr::ungroup()
  }

  message(paste0(say_size(.data), " after segmentation."))
  validate_eeg_lst(.data)
}




#' update events table based on a segmentation table
#' segmentation is a data.table with the following columns:
#' * .id:.id of the events
#' * .first_sample:sample_id 1 of the new new segmentation, 1 if left empty
#' * .lower: lower boundary of the event (included)
#' * .upper: upper boundary of the event, (included)
#' * .new_id: new id for the event, current one if left empty
#' @noRd
update_events <- function(events_tbl, segmentation) {
  segmentation <- data.table::as.data.table(segmentation)
  segmentation[, .new_id := if (!".new_id" %in% colnames(segmentation)) .id else .new_id ]
  segmentation[, .first_sample := if (!".first_sample" %in% colnames(segmentation)) 1 else .first_sample]
  cols_events <- colnames(events_tbl)
  cols_events_temp <- unique(c(cols_events, colnames(segmentation), "i..initial"))
  # i..initial is the.initial of events
  new_events <- segmentation[events_tbl, on = .(.id), ..cols_events_temp, allow.cartesian = TRUE][
    i..initial <= .upper & .lower <= .final
  ]
  new_events[, .initial := pmax(i..initial, .lower) - .first_sample + 1L]
  new_events[, .final := pmin(.final, .upper) - .first_sample + 1L]
  new_events[, .id := .new_id][, ..cols_events] %>%
    as_events_tbl(., sampling_rate = sampling_rate(events_tbl))
}
