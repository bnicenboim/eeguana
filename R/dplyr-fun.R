
#' Dplyr functions for eeg_lst objects.
#'
#' Manipulate the signal_tbl and the segments table of an eeg_lst.
#'
#' Wrappers for \link{dplyr}'s commands that act on different parts
#' `eggble` objects.
#' The following wrappers have been implemented for `eeg_lst` objects:
#' * `summarize()` summarizes the channel of the signal_tbl table
#' * `summarize_all()` summarizes all the channels of the signal_tbl table.
#' * `mutate()` adds new variables and preserves existing ones. Variables that are a function of a channel are added to the signal_tbl table, and other variables are added to the segments table.
#' * `mutate_all()` mutates all the channels of the signal_tbl table.
#' * `transmute()` like `mutate` but drops non-used variables of the referred table.
#' * `select()` keeps only the mentioned variables from the refered table, except for the obligatory columns starting with `.`.
#' * `rename()`: keeps all variables.
#' * `filter()`: finds segments/samples where conditions are true. Segments/samples where the condition evaluates to NA are dropped.
#' * `left_join()`: left-joins an external table to one of the segments of the eeg_lst.
#' * `semi_join()`: semi-joins an external table to one of the segments of the eeg_lst.
#' * `anti_join()`: anti-joins an external table to one of the segments of the eeg_lst.
#' *  `group_by()`: allows that operations would be performed "by group".
#' *  `ungroup()`: removes the grouping created by group_by.
#'
#' These commands always return the entire eeg_lst so that
#' they can be ' piped using \link{magrittr}'s pipe, %>%.
#'
#' @param .data An eeg_lst.
#' @param ... Name-value pairs of expressions; see  \link{dplyr-package} for more help.
#' @return An eeg_lst object.
#'
#'
#' @name dplyr
#' @family dplyr
#' 
#' @examples
#' \dontrun{
#'
#' faces_segs %>% select(O1, O2, P7, P8)
#' }
NULL
# > NULL

#' @rdname dplyr
#' @export
mutate.eeg_lst <- function(.data, ...) {
  dots <- rlang::quos(...)
  mutate_transmute(.data, mutate = TRUE, dots)
}
#' @export
mutate_.eeg_lst <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  mutate_transmute(.data, mutate = TRUE, dots)
}
#' @rdname dplyr
#' @export
transmute.eeg_lst <- function(.data, ...) {
  dots <- rlang::quos(...)
  mutate_transmute(.data, mutate = FALSE, dots)
}
#' @export
transmute_.eeg_lst <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  mutate_transmute(.data, mutate = FALSE, dots)
}
#' @export
summarise_.eeg_lst <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  summarize_eeg_lst(.data, dots)
}
#' @rdname dplyr
#' @export
summarise.eeg_lst <- function(.data, ...) {
  dots <- rlang::quos(...)
  summarize_eeg_lst(.data, dots)
}
#' @rdname dplyr
#' @export
tbl_vars.eeg_lst <- function(x) {
  setdiff(tbl_vars(x$signal_tbl), c(".id", ".sample_id"))
}
#' @rdname dplyr
#' @export
groups.eeg_lst <- function(x) {
  groups(x$segments)
}
#' @export
group_by_.eeg_lst <- function(.data, ..., .dots = list(), add = FALSE) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  group_by_eeg_lst(.eeg_lst = .data, .dots = dots, .add = add)
}
#' @rdname dplyr
#' @export
group_by.eeg_lst <- function(.data, ..., add = FALSE) {
  dots <- rlang::quos(...)
  group_by_eeg_lst(.eeg_lst = .data, .dots = dots, .add = add)
}
#' @rdname dplyr
#' @export
ungroup.eeg_lst <- function(.data, ..., add = add) {
  .data$signal_tbl <- dplyr::ungroup(.data$signal_tbl)
  .data$segments <- dplyr::ungroup(.data$segments)
  validate_eeg_lst(.data)
}
#' @rdname dplyr
#' @export
group_vars.eeg_lst <- function(x) {
  c(group_vars(x$signal_tbl), group_vars(x$segments))
}
#' @export
filter_.eeg_lst <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)

  filter_eeg_lst(.data,dots)
}
#' @rdname dplyr
#' @export
filter.eeg_lst <- function(.data, ...) {
  dots <- rlang::quos(...)
  filter_eeg_lst(.data,dots)
}
#' @rdname dplyr
#' @export
select.eeg_lst <- function(.data, ...) {
  select_rename(.data, select = TRUE, ...)
}
#' @rdname dplyr
#' @export
rename.eeg_lst <- function(.data, ...) {
  select_rename(.data, select = FALSE, ...)
}
#' @rdname dplyr
#' @export
left_join.eeg_lst <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  if (!is.data.frame(y)) stop("y must be a data frame or tibble.")

  x[["segments"]] <- dplyr::left_join(x[["segments"]], y = y, by = by, copy = copy, suffix = c(".x", ".y"), ...)

  validate_eeg_lst(x)
}
#' @rdname dplyr
#' @export
semi_join.eeg_lst <- function(x, y, by = NULL, suffix = c(".x", ".y"), ...) {
  if (!is.data.frame(y)) stop("y must be a data frame or tibble.")

  x[["segments"]] <- dplyr::semi_join(x[["segments"]], y, by = NULL, suffix = c(".x", ".y"), ...)
  x$signal_tbl <- dplyr::semi_join(x$signal_tbl, x[["segments"]], by = ".id")
  x$events <- dplyr::semi_join(x$events, x[["segments"]], by = ".id")

  redo_indices(x) %>% validate_eeg_lst()
}
#' @rdname dplyr
#' @export
anti_join.eeg_lst <- function(x, y, by = NULL, suffix = c(".x", ".y"), ...) {
  if (!is.data.frame(y)) stop("y must be a data frame or tibble.")

  x[["segments"]] <- dplyr::anti_join(x[["segments"]], y, by = NULL, suffix = c(".x", ".y"), ...)
  x[["signal_tbl"]] <- dplyr::semi_join(x[["signal_tbl"]], x$segments, by = ".id")
  x$events <- dplyr::semi_join(x$events, x$segments, by = ".id")

  redo_indices(x) %>% validate_eeg_lst()
}
