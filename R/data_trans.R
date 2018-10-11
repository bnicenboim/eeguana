#' Bind eegble objects.
#'
#' This function binds eegbles and throws a warning if there is a mismatch in the metadata.
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
  if (class(eegbles[[1]]) != "eegbl") {
    eegbles <- list(...)[[1]]
  }

  # Checks:
  purrr::iwalk(
    eegbles[seq(2, length(eegbles))],
    ~if (!identical(sampling_rate(eegbles[[1]]), sampling_rate(.x))) {
      stop(
        "Can't bind eegbles with different sampling rate, non identical 'sampling_rate'  in element ",
        as.character(as.numeric(.y) + 1), "."
      )
    }
  )

  purrr::iwalk(
    eegbles[seq(2, length(eegbles))],sa
    ~if (!identical(reference(eegbles[[1]]), reference(.x))) {
      stop("Non identical 'reference' in element ", as.character(as.numeric(.y) + 1), ".")
    }
  )

  purrr::iwalk(
    eegbles[seq(2, length(eegbles))],
    ~if (!identical(channels(eegbles[[1]]), channels(.x))) {
      warning("Non identical 'channels' in element ", as.character(as.numeric(.y) + 1), ".")
    }
  )


  # Binding
  # .id of the new eggbles needs to be adapted
  add_ids <- purrr::map_int(eegbles, ~max(.x$signal$.id)) %>%
    cumsum() %>%
    dplyr::lag(default = 0) %>%
    as.integer()


  signal <- purrr::map2_dfr(eegbles, add_ids, ~
  dplyr::ungroup(.x$signal) %>%
    dplyr::mutate(.id = .id + .y))
  events <- purrr::map2_dfr(eegbles, add_ids, ~
  dplyr::ungroup(.x$events) %>%
    dplyr::mutate(.id = .id + .y))
  segments <- purrr::map2_dfr(eegbles, add_ids, ~
  dplyr::ungroup(.x$segments) %>%
    dplyr::mutate(.id = .id + .y))

  channels <- purrr::map_dfr(eegbles[-1], ~dplyr::anti_join(channels(.x), channels(eegbles[[1]])), by = "labels") %>%
    dplyr::bind_rows(channels(eegbles[[1]]), .)


  # If more segments of the same recording are added, these need to be adapted.
  segments <- segments %>%
    dplyr::group_by(recording) %>%
    dplyr::mutate(segment = 1:n()) %>%
    dplyr::ungroup()

  new_eegble <- new_eegbl(
    signal = signal, events = events, segments = segments,
    channels = channels, info = list(sampling_rate = sampling_rate(eegbles[[1]]), reference = reference(eegbles[[1]]))
  )
  message(say_size(eegble))
  validate_eegbl(new_eegble)
}




#' Convert an eegble to a tibble.
#'
#' @param x An \code{eegble} object.
#' @param add_segments
#' @param thinning
#' @param ... Other arguments passed on to individual methods.
#'
#' `as_data_frame` and `as.tibble` are aliases.
#' @return A tibble.
#'
#' @importFrom magrittr %>%
#'
#' @export
as_tibble.eegble <- function(x, ..., add_segments = TRUE) {
  
  df <- x$signal %>%
    tidyr::gather(key = channel, value = amplitude, channel_names(x)) %>%
    {
      if (add_segments) {
        dplyr::left_join(., x$segments, by = ".id")
      } else {
        .
      }
    } %>%
    dplyr::group_by(.id, channel) %>%
    dplyr::mutate(time = (.sample_id - 1) / sampling_rate(x)) %>%
    dplyr::select(-.sample_id, time, dplyr::everything())
  df
}



#' Convert an eegble into a summary long-data frame based on a statistics.
#'
#' @param x An \code{eegble} object.
#' @param .funs A statistics to be used on every segment
#' @param ... Other arguments passed on to \code{.funs}. See \link{dplyr-package} help.
#'
#' @return A long tibble.
#'
#'
#' @export
summarize_id_as_tibble <- function(x, ...) {
  UseMethod("summarize_id_as_tibble")
}


#' @export
summarize_id_as_tibble.eegbl <- function(x, .funs = mean, ...) {
  funs_name <- rlang::enquo(.funs)

  # I need to define a name to unify the columns based on the function applied
  #  .funs = funs(nas = unique(is.na(.)))
  # [[1]]
  #  [1] "funs"   "nas"    "unique" "is.na"
  # .funs = mean
  # [[1]]
  # [1] "mean"
  fname <- rlang::quo_text(funs_name) %>%
    stringr::str_extract_all(stringr::boundary("word")) %>%
    {
      if (length(.[[1]]) == 1) .[[1]] else .[[1]][2]
    }

  x$signal %>%
    dplyr::group_by(.id) %>% # keep grouping for later
    dplyr::summarize_at(channel_names(x), .funs, ...) %>%
    # change the column back to channel names, when funs(?? = fun)
    # maybe there is a tidyverse solution
    {
      colnames(.) <- c(".id", channel_names(x))
      .
    } %>%
    # make it long format:
    tidyr::gather(key = channel, value = !!rlang::sym(fname), -.id) %>%
    # adds segment info
    dplyr::left_join(., x$segments, by = ".id") %>%
    dplyr::left_join(x$channels %>%
      dplyr::mutate(labels = as.character(labels)), by = c("channel" = "labels"))
}


#' @rdname as_tibble.eegbl
#' @export
as_data_frame.eegble <- as_tibble.eegble

#' @rdname as_tibble.eegbl
#' @export
as.tibble.eegble <- as_tibble.eegble


#' Convert an eegble to a (base) data frame.
#'
#' @param x An \code{eegble} object.
#' @param add_segments
#' @param thinning
#' @param ... Other arguments passed on to individual methods.
#'
#' `as_data_frame` and `as.tibble` are aliases.
#' @return A tibble.
#'
#' @importFrom magrittr %>%
#'
#' @export
as.data.frame.eegbl <- function(...) {
  as.data.frame(as_tibble.eegbl(...))
}
