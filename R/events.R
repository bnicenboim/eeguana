#' Annotate artifacts in the  events table of an eeg_lst.
#'
#' @param .data An `eeg_lst` object.
#'
#'
#' @return An `eeg_lst`.
#'
#' @importFrom magrittr %>%
#'
#' @export
eeg_artif_minmax <- function(.data,...){
    UseMethod("eeg_artif_minmax")
}

eeg_artif_minmax.eeg_lst <- function(.data, ..., difference = 100 , lim = c(-200, 200), window = (lim[2]-lim[1])/2, unit = "ms" ) {
    sample_range <- as.integer(lim * scaling(sampling_rate = sampling_rate(.data),unit = unit ))
    dots <- rlang::enquos(...)
    if(rlang::is_empty(dots)) {
        ch_sel <- channel_names(.data)
    } else {
        ch_sel <- tidyselect::vars_select(channel_names(.data), !!!dots)
    }

    win_sample  <- as.integer(window * scaling(sampling_rate = sampling_rate(.data),unit = unit ))
    if(window >= (lim[2]-lim[1])){
        warning("`window` should be smaller than the range of `lim`")
    }

    search_artifacts <- function(signal, fun){

    }

    artifact_found <- .data$signal[,c(list(.sample_id = .sample_id),
                                      c(rep(FALSE, win_sample - .N + 1) ,purrr::map(.SD, ~ {
                                          rmin <- RcppRoll::roll_min(x,n=win_sample, align = "left")
                                          rmax <- RcppRoll::roll_max(x,n=win_sample, align = "left")
                                      abs(rmin-rmax) > difference
                                      }))),
                                   .SDcols = (ch_sel), by = .id]

    
    events(.data) <- add_intervals_from_artifacts( events(.data), artifact_found, sample_range)
    validate_eeg_lst(.data)
}



#' @export
eeg_artif_step <- function(.data, ...) {
    UseMethod("eeg_artif_step")
}

#' @param ... Channels to include in the artifact detection. All the channels by default, but eye channels should be removed.
#' @param step Maximum permissible difference in voltage between two consecutive data points.
#' @param lim Vector indicating the time before and after the event.
#' @inheritParams as_time
#' @rdname eeg_events_to_NA
#' @export 
eeg_artif_step.eeg_lst <- function(.data, ..., step = 50 , lim = c(-200, 200), unit = "ms" ) {
    sample_range = as.integer(lim * scaling(sampling_rate = sampling_rate(.data),unit = unit ))
    dots <- rlang::enquos(...)
    if(rlang::is_empty(dots)) {
      ch_sel <- channel_names(.data)
    } else {
      ch_sel <- tidyselect::vars_select(channel_names(.data), !!!dots)
    }

    artifact_found <- .data$signal[,c(list(.sample_id = .sample_id),
                                      c(FALSE,purrr::map(.SD, ~ abs(diff(.x)) > step))),
                                   .SDcols = (ch_sel), by = .id]

       
    events(.data) <- add_intervals_from_artifacts( events(.data), artifact_found, sample_range)
    validate_eeg_lst(.data)
}

#' add events from a table similar to signal, but with TRUE/FALSE depending if an artifact was detected.
#' @noRd 
add_intervals_from_artifacts <- function(old_events, artifact_found, sample_range){
    artifact_found %>%
        split(.,.$.id) %>%
        map_dtr( function(.eeg)
            .eeg %>% dplyr::select_at(dplyr::vars(ch_sel)) %>%
            imap_dtr( ~{
                if(all(.x==FALSE)){
                    data.table::data.table()
                } else {
                    ## left and right values of the window of bad values (respecting the min max samples)
                    left <-  .eeg$.sample_id[.x] + sample_range[[1]]
                    ## the smallest sample is one less than the present one because diff() in artifact_found reduces the vector by 1
                    left[left < (min(.eeg$.sample_id)-1L)] <- (min(.eeg$.sample_id)) 
                    right <-  .eeg$.sample_id[.x] + sample_range[[2]]
                    right[right > max(.eeg$.sample_id)] <- max(.eeg$.sample_id)
                    ##merge if there are steps closer than the window for removal 
                    intervals <- data.table::data.table(start=left, stop=right) %>%
                      .[, .(start=min(start), stop=max(stop)),
                       by=.(group=cumsum(c(1, tail(start, -1) > head(stop, -1))))] 
                    data.table::data.table(type = "artifact",
                                           description="gradient",
                                           .sample_0 = intervals$start,
                                           .size = intervals$stop + 1L - intervals$start,
                                           .channel = .y)
                }
            }),.id = TRUE
            )
    events_found[,.id:= as.integer(.id)]
    new_events <- rbind(events_found, old_events, fill = TRUE)
    
    data.table::setorder(new_events,.id, .sample_0, .channel)
    new_events
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
  signal <- as.data.frame(x$signal)
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
        signal[[as.character(c)]][signal$.id %in% b$.id[i] &
          between(
            signal$.sample_id, b$.sample_0[i],
            b$.sample_0[i] + b$.size[i] - 1
          )  ] <- NA
      }
      # could try with na_if, maybe it's faster?
    } else {
      signal[[as.character(c)]][signal$.id %in% b$.id] <- NA
    }
  }
  # For the replacement in the complete of the segments
  b_all <- dplyr::filter(baddies, is.na(.channel)) %>% dplyr::distinct()

  if (!entire_seg & nrow(b_all) != 0) {
    for (i in seq(1, nrow(b_all))) {
      signal[, channel_names(x)][signal$.id == b_all$.id[i] &
        between(
          signal$.sample_id, b_all$.sample_0[i],
          b_all$.sample_0[i] + b_all$.size[i] - 1
        ), ] <- NA
    }
  } else {
    signal[, channel_names(x)][signal$.id %in% b_all$.id, ] <- NA
  }

  if (drop_events) {
    x$events <- suppressMessages(dplyr::anti_join(
      x$events,
      dplyr::filter(x$events, !!!dots)
    ))
  }

  #TODO fix this:
  x$signal <- data.table::data.table(signal)
  data.table::setattr(x$signal, "class", c("signal_tbl", class(x$signal))) 
  data.table::setkey(x$signal,.id,.sample_id)
  x$events <- as_events_tbl(x$events)
  validate_eeg_lst(x)
}
