#' Convert an eeg_lst to a tibble.
#' 
#' Convert the signal table from wide to long format, and optionally \code{left_join}s the segment table
#'
#' @param x An \code{eeg_lst} object.
#' @param add_segments Whether the segments table
#'
#' `as_data_frame` and `as.tibble` are aliases.
#' @return A tibble.
#'
#' @importFrom magrittr %>%
#'
#' @export
as_tibble.eeg_lst <- function(x, add_segments = TRUE) {
  
  df <- declass(x$signal)$tbl %>%
    tidyr::gather(key = "channel", value = "amplitude", channel_names(x)) %>%
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


#' @export
as_tibble.signal_tbl <- function(x) {
x <- declass(x)$tbl
NextMethod()
}

  


#' Convert an eeg_lst into a summary long-data frame based on a statistics.
#'
#' @param x An \code{eeg_lst} object.
#' @param .funs A statistics to be used on every segment
#' @param ... Other arguments passed on to \code{.funs}. See \link{dplyr-package} help.
#'
#' @return A long tibble.
#'
#'
#' @export
summarize_by_id_tbl <- function(x, ...) {
  UseMethod("summarize_by_id_tbl")
}


#' @export
summarize_by_id_tbl.eeg_lst <- function(x, .funs = mean, ...) {
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

  declass(x$signal)$tbl %>%
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
    dplyr::left_join(rename(channels_tbl(x), channel = .name), by = "channel")
}


#' @rdname as_tibble.eeg_lst
#' @export
as_data_frame.eeg_lst <- as_tibble.eeg_lst

#' @rdname as_tibble.eeg_lst
#' @export
as.tibble.eeg_lst <- as_tibble.eeg_lst


#' Convert an eeg_lst to a (base) data frame.
#'
#' @param x An \code{eeg_lst} object.
#' @param add_segments
#' @param ... Other arguments passed on to individual methods.
#'
#' @return A tibble.
#'
#' @importFrom magrittr %>%
#'
#' @export
as.data.frame.eeg_lst <- function(...) {
  as.data.frame(as_tibble.eeg_lst(...))
}
