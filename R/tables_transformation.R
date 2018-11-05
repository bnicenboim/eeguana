#' Segment an eeg_lst.
#'
#' Subdivide of the EEG into different segments or epochs. When there is no
#' segmentation, the `eeg_lst` contain one segment. (Fieldtrip calls the
#' segment "trials".) The limits of `segment` are inclusive: If, for
#' example, lim =c(0,0), the segment would contain only sample 1.
#'
#' @param x An `eeg_lst` object.
#' @param ... Description of the event.
#' @param lim Vector indicating the time before and after the event. Or dataframe with two columns, with nrow=total number of segments
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

#' @export
segment.eeg_lst <- function(x, ..., lim = c(-.5, .5), unit = "seconds") {
  dots <- rlang::enquos(...)

  times0 <- dplyr::filter(x$events, !!!dots) %>%
    dplyr::select(-.channel, -.size) 
    
  if (any(lim[[2]] < lim[[1]])) {
    stop("A segment needs to be of positive length and include at least 1 sample.")
  }
  
  
  if ((length(lim) == 2) || ## two values or a dataframe
    (!is.null(nrow(lim)) && nrow(lim) == nrow(times0)) ) {
    scaling <- scaling(sampling_rate(x), unit = unit)
    sample_lim <- round(lim * scaling) 
    seg_names <- colnames(times0)[!startsWith(colnames(times0),".")]
    segmentation_info <- times0 %>% dplyr::mutate(.lower =.sample_0+ sample_lim[[1]] %>% as_integer(), 
                                                  .upper = .sample_0+ sample_lim[[2]] %>% as_integer(),
                                                  .new_id = seq_len(dplyr::n())) %>%
                                    dplyr::select(-dplyr::one_of(seg_names))
  } else {
    stop("Wrong dimension of lim")
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
    dplyr::select(times0, -.sample_0),
    by = ".id"
  ) %>%
    dplyr::mutate(.id = 1:n()) %>%
    dplyr::group_by(recording) %>%
    dplyr::mutate(segment = 1:n())

  message(paste0(say_size(x), " after segmentation."))
  validate_eeg_lst(x)
}


#' Bind eeg_lst objects.
#'
#' Binds eeg_lst and throws a warning if there is a mismatch in the channel information.
#'
#' @param ... eeg_lst objects to combine.
#'
#' @return An `eeg_lst` object.
#'
#' @importFrom magrittr %>%
#'
#' @export
bind <- function(...) {
  eeg_lsts <- list(...)
  # hack to allow that "..." would already be a list
  if (class(eeg_lsts[[1]]) != "eeg_lst") {
    eeg_lsts <- list(...)[[1]]
  }

  # Checks:
  purrr::iwalk(
    eeg_lsts[seq(2, length(eeg_lsts))],
    ~if (!identical(channels_tbl(eeg_lsts[[1]]), channels_tbl(.x))) {
      warning("Objects with different channels information, see below\n\n", "File ",
        as.character(as.numeric(.y) + 1), " ... \n",
        paste0(
          capture.output(setdiff(channels_tbl(eeg_lsts[[1]]), channels_tbl(.x))),
          collapse = "\n"
        ),
        "\n\n ... in comparison with file 1 ...\n\n",
        paste0(
          capture.output(setdiff(channels_tbl(.x), channels_tbl(eeg_lsts[[1]]))),
          collapse = "\n"
        )
        ,
        call. = FALSE
      )
    }
  )

  # Binding
  # .id of the new eggbles needs to be adapted

  signal <- purrr::map(eeg_lsts, ~.x$signal) %>% data.table::rbindlist(idcol=".sid")
  signal[, .id := .GRP, by = .(.sid,.id)][,.sid := NULL]
  data.table::setkey(signal,.id,.sample_id)

  events <- purrr::map(eeg_lsts, ~.x$events) %>% data.table::rbindlist(idcol=".sid")
  events[, .id := .GRP, by = .(.sid,.id)][,.sid := NULL]


  segments <- purrr::map_dfr(eeg_lsts, ~
  dplyr::ungroup(.x$segments), .id = ".sid") %>%
    dplyr::mutate(.id = dplyr::group_indices(., .sid,.id)) %>%
    dplyr::select(-.sid)



  new_eeg_lst <- new_eeg_lst(
    signal = signal, events = events, segments = segments
  ) %>%
    validate_eeg_lst()
  message(say_size(new_eeg_lst))
  new_eeg_lst
}


#' Remove (transform to NA) problematic events from an eeg_lst.
#'
#' @param x An `eeg_lst` object.
#' @param ... Description of the problematic event.
#' @param all_chans If set to `TRUE`,
#'     it will consider samples from all channels (Default:  `all_chans = FALSE`).
#' @param entire_seg If set to `FALSE`, it will consider only the marked part of the segment,
#'     otherwise it will consider the entire segment (Default: entire_seg = TRUE). Setting it to FALSE can make the function very slow.
#' @param drop_events
#'
#'
#' @return An eeg_lst.
#'
#' @importFrom fastmatch %fin%
#' @importFrom magrittr %>%
#'
#' @export
event_to_ch_NA <- function(x, ...) {
  UseMethod("event_to_ch_NA")
}


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
