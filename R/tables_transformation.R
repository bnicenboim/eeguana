#' Segments an eegble.
#'
#' Subdivides of the EEG into different segments or epochs. When there is no
#' segmentation, the \code{eegble} contain one segment. (Fieldtrip calls the
#' segment "trials".) The limits of \code{segment} are inclusive: If, for
#' example, lim =c(0,0), the segment would contain only sample 1.
#'
#' @param x An \code{eegble} object.
#' @param ... Description of the event.
#' @param lim Vector indicating the time before and after the event. Or matrix with two columns, with nrow=total number of segments
#'
#' @return An \code{eegble}.
#'
#' @importFrom magrittr %>%
#'
#' @export
segment <- function(x, ...) {
  UseMethod("segment")
}


#' @export
segment.eegble <- function(x, ..., lim = c(-.5, .5), unit = "seconds") {
  dots <- rlang::enquos(...)
  # dots <- rlang::quos(description == "s121")
  # dots <- rlang::quos(description == "s70")
  # dots <- rlang::quos(description == "s13")
  # dots <- rlang::quos(description %in% c("s70",s71"))
  # dots <- rlang::quos(type == "New Segment")

  # the segmentation will ignore the groups:
  orig_groups <- list()
  orig_groups$signal <- dplyr::groups(x$signal)
  orig_groups$events <- dplyr::groups(x$events)
  orig_groups$segments <- dplyr::groups(x$segments)

  x$signal <- dplyr::ungroup(x$signal)
  x$events <- dplyr::ungroup(x$events)
  x$segments <- dplyr::ungroup(x$segments)

  times0 <- dplyr::filter(x$events, !!!dots) %>%
    dplyr::select(-.channel, -.size)

  scaling <- scaling(sampling_rate(x), unit)

  if (length(lim) == 2) {
    lim <- rep(list(lim), each = nrow(times0))
  }

  if (is.matrix(lim) && dim(lim)[2] == 2 && dim(lim)[1] == nrow(times0)) {
    lim <- purrr::array_branch(lim, 1)
  } else if (is.list(lim) && length(lim) == nrow(times0)) {
    # ok format
    NULL
  } else {
    stop("Wrong dimension of lim")
  }

  slim <- purrr::modify(lim, function(l) {
    if (l[2] < l[1]) {
      stop("A segment needs to be of positive length and include at least 1 sample.")
    }
    round(l * scaling) %>% as_integer()
  })

  # bind_rows looses the attributes
  # https://github.com/tidyverse/dplyr/issues/2457
  # pmap_sgr is pmap_dfr for signal_table
  x$signal <- pmap_sgr(list(times0$.id, times0$.sample_0, slim),
    function(i, s0, sl) x$signal %>%
        # filter the relevant samples
        dplyr::filter(
          .sample_id >= s0 + sl[1],
          .sample_id <= s0 + sl[2],
          .id == i
        ) %>%
        dplyr::mutate(.sample_id = .sample_id - s0 + 1L) %>%
        # order the signals df:
        dplyr::select(-.id)
  ,.id = ".id") %>%
    dplyr::mutate(.id = as.integer(.id)) 


  slim <- purrr::map2(slim, split(x$signal, x$signal$.id), function(sl, d) {
    sl <- c(min(d$.sample_id) - 1L, max(d$.sample_id) - 1L)
  })

  x$events <- purrr::pmap_dfr(list(times0$.id, times0$.sample_0, slim),
    .id = ".id",
    function(i, s0, sl) {
      # bound according to the segment
      x$events %>%
        # filter the relevant events
        # started after the segment (s0 + slim[1])
        # or span after the segment (s0 + slim[1])
        dplyr::filter(
          .sample_0 + .size - 1 >= s0 + sl[1],
          .sample_0 <= s0 + sl[2],
          .id == i
        ) %>%
        dplyr::mutate(
          .size = dplyr::if_else(
            .sample_0 < s0 + sl[1],
            as.integer(.size - (s0 + sl[1] - .sample_0) + 1L),
            # adjust the size so that it doesn't spillover after the segment
            .size
          ) %>% pmin(., sl[2] + 1L - .sample_0 + s0),
          .sample_0 = dplyr::if_else(
            .sample_0 < s0 + sl[1], sl[1] + 1L,
            .sample_0 - s0 + 1L
          )
        ) %>%
        dplyr::select(-.id)
    }
  ) %>%
    dplyr::mutate(.id = as.integer(.id))

  message(paste0("# Total of ", max(x$signal$.id), " segments found."))

  x$segments <- dplyr::right_join(x$segments, 
                        dplyr::select(times0, -.sample_0), by = ".id") %>%
                dplyr::mutate(.id = 1:n()) %>%
                dplyr::group_by(recording) %>%
                dplyr::mutate(segment = 1:n())

  x$signal <- dplyr::group_by(x$signal, !!!orig_groups$signal)
  x$events <- dplyr::group_by(x$events, !!!orig_groups$events)
  x$segments <- dplyr::group_by(x$segments, !!!orig_groups$segments)

  message(paste0(say_size(x), " after segmentation."))
  validate_eegble(x)
}


#' Bind eegble objects.
#'
#' Binds eegble and throws a warning if there is a mismatch in the channel information.
#' 
#' @param ... Eegble objects to combine.
#'
#' @return An \code{eegble} object.
#'
#' @importFrom magrittr %>%
#'
#' @export
bind <- function(...) {
  eegbles <- list(...)
  # hack to allow that "..." would already be a list
  if (class(eegbles[[1]]) != "eegble") {
    eegbles <- list(...)[[1]]
  }

  # Checks:
  purrr::iwalk(
    eegbles[seq(2, length(eegbles))],
    ~if (!identical(channels_tbl(eegbles[[1]]), channels_tbl(.x))) {
      warning(
        "Objects with different channels information",
        as.character(as.numeric(.y) + 1), "."
      )
    }
  )


  # Binding
  # .id of the new eggbles needs to be adapted
  add_ids <- purrr::map_int(eegbles, ~max(.x$signal$.id)) %>%
    cumsum() %>%
    dplyr::lag(default = 0) %>%
    as.integer()


  signal <- map2_sgr(eegbles, add_ids, ~
  dplyr::ungroup(.x$signal) %>%
    dplyr::mutate(.id = .id + .y))
  events <- purrr::map2_dfr(eegbles, add_ids, ~
  dplyr::ungroup(.x$events) %>%
    dplyr::mutate(.id = .id + .y))
  segments <- purrr::map2_dfr(eegbles, add_ids, ~
  dplyr::ungroup(.x$segments) %>%
    dplyr::mutate(.id = .id + .y))

  # If more segments of the same recording are added, these need to be adapted.
  segments <- segments %>%
    dplyr::group_by(recording) %>%
    dplyr::mutate(segment = 1:n()) %>%
    dplyr::ungroup()

  new_eegble <- new_eegble(
    signal = signal, events = events, segments = segments) %>% 
     validate_eegble()
  message(say_size(new_eegble))
  new_eegble
}


#' Remove (transform to NA) problematic events from an eegble.
#'
#' @param x An \code{eegble} object.
#' @param ... Description of the problematic event.
#' @param all_chans If set to TRUE,
#'     it will consider samples from all channels (Default:  all_chans = FALSE).
#' @param entire_seg If set to FALSE, it will consider only the marked part of the segment,
#'     otherwise it will consider the entire segment (Default: entire_seg = TRUE). Setting it to FALSE can make the function very slow.
#' @param drop_events
#'
#' @examples
#'
#' @return An eegble.
#'
#' @importFrom fastmatch %fin%
#' @importFrom magrittr %>%
#'
#' @export
event_to_ch_NA <- function(x, ...) {
  UseMethod("event_to_ch_NA")
}

#' @export
event_to_ch_NA.eegble <- function(x, ..., all_chans = FALSE, entire_seg = TRUE,
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


