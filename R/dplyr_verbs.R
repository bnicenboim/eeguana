#' Dplyr verbs overloaded for manipulating eeg_lst objects.
#'
#' Manipulate the signal table and the segments table of an eeg_lst.
#'
#' Wrappers for [dplyr][dplyr::dplyr]'s commands that act on different parts
#' `eeg_lst` objects.
#' The following wrappers have been implemented for `eeg_lst` objects:
#'
#' * `left_join()`: left-joins an external table to the segments table of the eeg_lst.
#' * `semi_join()`: semi-joins an external table to the segments table of the eeg_lst.
#' * `anti_join()`: anti-joins an external table to the segments table of the eeg_lst.
#' * `mutate()` adds new variables and preserves existing ones. Variables that are a function of a channel are added to the signal_tbl table, and other variables are added to the segments table.
#' * `transmute()` like `mutate` but drops non-used variables of the referred table, except for the obligatory columns starting with `.`.
#' * `filter()`: finds segments/samples where conditions are true. Segments/samples where the condition evaluates to NA are dropped.
#' * `summarize()` summarizes the channel of the signal_tbl table
#' * `group_by()`: allows that operations would be performed "by group".
#' * `ungroup()`: removes the grouping created by group_by.
#' * `select()` keeps only the mentioned variables from the referred table, except for the obligatory columns starting with `.`.
#' * `rename()`: keeps all variables.
#'
#' In addition, `_at()`, and `_if()` versions of these verbs work as well. Notice that  `_at()` versions are much faster than `_if()` versions of these commands.
#'
#' These functions always return the entire `eeg_lst` so that
#' they can be ' piped using [magrittr][magrittr::magrittr] 's pipe, %>%.
#'
#' @param .data An eeg_lst.
#' @param x An eeg_lst.
#' @param y A data frame, tibble, or data.table.
#' @inheritParams dplyr::join
#' @param ... Name-value pairs of expressions; see [dplyr][dplyr::dplyr] for more help.
#' @param .preserve Not in use, for compatibility reasons.
#' @param add Not in use, for compatibility reasons.
#' @param .drop When .drop = TRUE, empty groups are dropped. (FALSE by default.)
#' @importFrom dplyr filter select mutate transmute summarise rename
#' @importFrom dplyr group_by ungroup group_vars
#' @importFrom dplyr groups
#' @importFrom dplyr anti_join left_join right_join full_join semi_join inner_join
#' 
#' @return An eeg_lst object.
#'
#' @family dplyr functions
#'
#' @name dplyr_verbs
#'
#' @examples
#' library(dplyr)
#' # Create new channel in the signal table
#' data_faces_ERPs %>%
#'   mutate(tmp = Fz - Cz)
#' 
#' # Create a new condition in the segments table
#' data_faces_ERPs %>%
#'   mutate(code = ifelse(condition == "faces", 1, -1))
#' 
#' # Create a new channel and drop all others
#' data_faces_ERPs %>%
#'   transmute(Occipital = chs_mean(O1, O2, Oz,
#'     na.rm = TRUE
#'   ))
#' 
#' # Extract data associated with a condition
#' data_faces_ERPs %>%
#'   filter(condition == "faces")
#' 
#' # Group and summarize
#' data_faces_ERPs %>%
#'   # Convert samples to times, filter between timepoints
#'   filter(between(
#'     as_time(.sample, unit = "ms"),
#'     100, 200
#'   )) %>%
#'   # Find mean amplitude of Fz for each condition
#'   group_by(condition) %>%
#'   summarize(mean.amplitude = mean(Fz))
#' 
#' # Select specific electrodes
#' data_faces_ERPs %>%
#'   select(O1, O2, P7, P8)
#' 
#' # Rename a variable
#' data_faces_ERPs %>%
#'   rename(Predictor = condition)
NULL
# > NULL


#' @rdname dplyr_verbs
#' @export
mutate.eeg_lst <- function(.data, ...) {
  dots <- rlang::quos(...)
  mutate_eeg_lst(.data, dots, keep_cols = TRUE)
}

#' @rdname dplyr_verbs
#' @export
transmute.eeg_lst <- function(.data, ...) {
  dots <- rlang::quos(...)
  mutate_eeg_lst(.data, dots, keep_cols = FALSE)
}
#' @rdname dplyr_verbs
#' @export
filter.eeg_lst <- function(.data, ..., .preserve = FALSE) {
  if (.preserve == TRUE) {
    warning("Ignoring `.preserve` argument.")
  }
  dots <- rlang::quos(...)
  filter_eeg_lst(.data, dots = dots)
}
#' @export
filter.eeg_ica_lst <- function(.data, ..., .preserve = FALSE) {
  out <- NextMethod()
  recordings <- unique(out$.segments$.recording)
  out$ica <- out$ica[recordings]
  out
}
#' @rdname dplyr_verbs
#' @export
summarise.eeg_lst <- function(.data, ...) {
  dots <- rlang::quos(...)
  summarize_eeg_lst(.data, dots)
}
#' @rdname dplyr_verbs
#' @export
group_by.eeg_lst <- function(.data, ..., add = FALSE, .drop = FALSE) {
  dots <- rlang::quos(...)
  if (.drop == TRUE) {
    warning("Ignoring .drop argument. It hasn't been implemented yet.")
  }
  group_by_eeg_lst(.eeg_lst = .data, dots, .add = add)
}
#' @rdname dplyr_verbs
#' @export
ungroup.eeg_lst <- function(.data, ...) {
  attributes(.data)$vars <- character(0)
  validate_eeg_lst(.data)
}

#' @rdname dplyr_verbs
#' @export
select.eeg_lst <- function(.data, ...) {
  select_rename(.data, select = TRUE, ...)
}

#' @rdname dplyr_verbs
#' @export
rename.eeg_lst <- function(.data, ...) {
  select_rename(.data, select = FALSE, ...)
}


#' @rdname dplyr_verbs
#' @export
groups.eeg_lst <- function(x) {
  attributes(x)$vars %>% purrr::map(as.name)
}


#' @rdname dplyr_verbs
#' @export
group_vars.eeg_lst <- function(x) {
  attributes(x)$vars
}

#' @rdname dplyr_verbs
#' @export
anti_join.eeg_lst <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  if (!is.data.frame(y)) stop("y must be a data frame or tibble.")

  x$.segments <- dplyr::anti_join(x$.segments, y, by = NULL, suffix = c(".x", ".y"), ...)

  segments <- data.table::as.data.table(x$.segments)
  x$.signal <- semi_join_dt(x$.signal, segments, by = ".id")
  x$.events <- semi_join_dt(x$.events, segments, by = ".id")
  x %>% validate_eeg_lst()
}
#' @rdname dplyr_verbs
#' @export
left_join.eeg_lst <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  if (!is.data.frame(y)) stop("y must be a data frame or tibble.")

  x$.segments <- dplyr::left_join(x$.segments, y = y, by = by, copy = copy, suffix = c(".x", ".y"), ...)

  validate_eeg_lst(x)
}
#' @rdname dplyr_verbs
#' @export
semi_join.eeg_lst <- function(x, y, by = NULL, suffix = c(".x", ".y"), ...) {
  if (!is.data.frame(y)) stop("y must be a data frame or tibble.")

  x$.segments <- dplyr::semi_join(x$.segments, y, by = NULL, suffix = c(".x", ".y"), ...)

  segments <- data.table::as.data.table(x$.segments)
  x$.signal <- semi_join_dt(x$.signal, segments, by = ".id")
  x$.events <- semi_join_dt(x$.events, segments, by = ".id")
  x %>% validate_eeg_lst()
}
#' @export
tbl_vars.eeg_lst <- function(x) {
  setdiff(dplyr::tbl_vars(x$.signal), c(dplyr::tbl_vars(x$.segments), c(".id", ".sample")))
}
