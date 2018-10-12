#' Segments an eegble.
#'
#' EXPLAIN WHAT A SEGMENT IS/ DIFFERENCE WITH EPOCH (LINK?) + EXAMPLES OF SEGMENT WITH +Inf.
#' Fieldtrip calls the segment "trials". The limits are inclusive, the event is
#'  set as happening in the sample 1. If, for example, lim =c(0,0), the segment would contain only one sample.
#'
#' @param x An \code{eegble} object.
#' @param ... Description of the event.
#' @param lim Vector indicating the time before and after the event. Or matrix with two columns, with nrow=total number of segments
#'
#' @return An eegbl.
#'
#' @importFrom magrittr %>%
#'
#' @export
segment <- function(x, ...) {
  UseMethod("segment")
}

# , segmentation refers to the subdivision of the EEG into differ-
# ent segments (epochs).

#' @export
segment.eegble <- function(x, ..., lim = c(-.5, .5), unit = "seconds") {
  dots <- rlang::enquos(...)
  # dots <- rlang::quos(description == "s121")
  # dots <- rlang::quos(description == "s70")
  # dots <- rlang::quos(description == "s13")
  # dots <- rlang::quos(description %in% c("s70",s71"))

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

  scaling <- scaling(x, unit)

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


# pmap_dfr(list(times0$.id, times0$.sample_0, slim),
#     function(i, s0, sl) declass(xx$signal)$tbl %>%
#         # filter the relevant samples
#         dplyr::filter(
#           .sample_id >= s0 + sl[1],
#           .sample_id <= s0 + sl[2],
#           .id == i
#         ) %>%
#         dplyr::mutate(.sample_id = .sample_id - s0 + 1L) %>%
#         # order the signals df:
#         dplyr::select(-.id)
#   ,.id = ".id") %>% {unique(.$.id)}




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
