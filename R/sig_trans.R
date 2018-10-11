#' Remove (transform to NA) problematic events from an eegble.
#'
#' @param x An \code{eegble} object.
#' @param ... Description of the problematic event.
#' @param all_chans If set to TRUE,
#'     it will consider samples from all channels (Default:  all_chans = FALSE).
#' @param entire_seg If set to FALSE, it will consider only the marked part of the segment,
#'     otherwise it will consider the entire segment (Default: entire_seg = TRUE).
#' @param drop_events
#'
#' @examples
#'
#' @return An eegbl.
#'
#' @importFrom fastmatch %fin%
#' @importFrom magrittr %>%
#'
#' @export
event_to_NA <- function(x, ...) {
  UseMethod("event_to_NA")
}

#' @export
event_to_NA.eegble <- function(x, ..., all_chans = FALSE, entire_seg = TRUE,
                              drop_events = TRUE) {
  dots <- rlang::enquos(...)


  # dots <- rlang::quos(type == "Bad Interval")

  # Hack for match 2 columns with 2 columns, similar to semi_join but allowing 
  #for assignment
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
          dplyr::between(
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
        dplyr::between(
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
    )) %>%
      dplyr::mutate(.channel = forcats::lvls_expand(.channel,
        new_levels = channel_names(x)
      ))
  }
  validate_eegble(x)
}







