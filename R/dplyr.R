
#' Dplyr functions for eeg_lst objects.
#'
#' Manipulate the signal_tbl and the segments table of an eeg_lst.
#'
#' Wrappers for [dplyr][dplyr::dplyr]'s commands that act on different parts
#' `eggble` objects.
#' The following wrappers have been implemented for `eeg_lst` objects:
#' * `mutate()` adds new variables and preserves existing ones. Variables that are a function of a channel are added to the signal_tbl table, and other variables are added to the segments table.
#' * `transmute()` like `mutate` but drops non-used variables of the referred table, except for the obligatory columns starting with `.`.
#' * `filter()`: finds segments/samples where conditions are true. Segments/samples where the condition evaluates to NA are dropped.
#' * `summarize()` summarizes the channel of the signal_tbl table
#' *  `group_by()`: allows that operations would be performed "by group".
#' *  `ungroup()`: removes the grouping created by group_by.
#' * `select()` keeps only the mentioned variables from the refered table, except for the obligatory columns starting with `.`.
#' * `rename()`: keeps all variables.
#' * `left_join()`: left-joins an external table to the segments table of the eeg_lst.
#' * `semi_join()`: semi-joins an external table to the segments table of the eeg_lst.
#' * `anti_join()`: anti-joins an external table to the segments table of the eeg_lst.
#'
#' These commands always return the entire eeg_lst so that
#' they can be ' piped using [magrittr][magrittr::magrittr] 's pipe, [%>%][magrittr::`%>%`()].
#'
#' @param .data An eeg_lst.
#' @param ... Name-value pairs of expressions; see  [dplyr][dplyr::dplyr] for more help.
#' @return An eeg_lst object.
#'
#' @family dplyr functions
#' @seealso [summarize_at_ch], [summarize_all_ch], [bind] for the extended dplyr-like functions.
#'
#' @name dplyr
#' 
#' @examples
#' \dontrun{
#'
#' faces_segs %>% select(O1, O2, P7, P8)
#' }
NULL
# > NULL

#' @rdname dplyr
mutate.eeg_lst <- function(.data, ...) {
  dots <- rlang::quos(...)
  mutate_eeg_lst(.data, dots, keep_cols = TRUE)
}
mutate_.eeg_lst <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, rlang::caller_env(), ...)
  mutate_eeg_lst(.data, dots, keep_cols = TRUE)
}
#' @rdname dplyr
transmute.eeg_lst <- function(.data, ...) {
  dots <- rlang::quos(...)
  mutate_eeg_lst(.data, dots, keep_cols = FALSE)
}
transmute_.eeg_lst <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, rlang::caller_env(), ...)
  mutate_eeg_lst(.data, dots, keep_cols = FALSE)
}
filter_.eeg_lst <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, rlang::caller_env(), ...)
  filter_eeg_lst(.data, dots = dots)
}
#' @rdname dplyr
filter.eeg_lst <- function(.data, ...) {
  dots <- rlang::quos(...)
  filter_eeg_lst(.data, dots = dots)
}
#' @rdname dplyr
summarise.eeg_lst <- function(.data, ...) {
  dots <- rlang::quos(...)
 summarize_eeg_lst(.data, dots)
}
summarise_.eeg_lst <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, rlang::caller_env(), ...)
 summarize_eeg_lst(.data, dots)
}
group_by_.eeg_lst <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, rlang::caller_env(), ...)
  group_by_eeg_lst(.eeg_lst = .data, dots, .add = FALSE)
}
#' @rdname dplyr
group_by.eeg_lst <- function(.data, ...) {
  dots <- rlang::quos(...)
  group_by_eeg_lst(.eeg_lst = .data, dots, .add = FALSE)
}
#' @rdname dplyr
ungroup.eeg_lst <- function(.data, ...) {
  attributes(.data)$vars <- character(0)
  validate_eeg_lst(.data)
}
#' @rdname dplyr
groups.eeg_lst <- function(x) {
attributes(x)$vars %>% purrr::map(as.name)
}
#' @rdname dplyr
group_vars.eeg_lst <- function(x) {
  attributes(x)$vars
}
#' @rdname dplyr
select.eeg_lst <- function(.data, ...) {
  select_rename(.data, select = TRUE, ...)
}
#' @rdname dplyr
rename.eeg_lst <- function(.data, ...) {
  select_rename(.data, select = FALSE, ...)
}
#' @rdname dplyr
anti_join.eeg_lst <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  if (!is.data.frame(y)) stop("y must be a data frame or tibble.")

  x$segments <- dplyr::anti_join(x$segments, y, by = NULL, suffix = c(".x", ".y"), ...)

  segments <- data.table::as.data.table(x$segments)
  x$signal <- semi_join_dt(x$signal, segments, by = ".id")
  x$events <- semi_join_dt(x$events, segments, by = ".id")
  redo_indices(x) %>% validate_eeg_lst()
}
#' @rdname dplyr
left_join.eeg_lst <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  if (!is.data.frame(y)) stop("y must be a data frame or tibble.")

  x$segments <- dplyr::left_join(x$segments, y = y, by = by, copy = copy, suffix = c(".x", ".y"), ...)

  validate_eeg_lst(x)
}
#' @rdname dplyr
semi_join.eeg_lst <- function(x, y, by = NULL, suffix = c(".x", ".y"), ...) {
  if (!is.data.frame(y)) stop("y must be a data frame or tibble.")

  x$segments <- dplyr::semi_join(x$segments, y, by = NULL, suffix = c(".x", ".y"), ...)

  segments <- data.table::as.data.table(x$segments)
  x$signal <- semi_join_dt(x$signal, segments, by = ".id")
  x$events <- semi_join_dt(x$events, segments, by = ".id")
  redo_indices(x) %>% validate_eeg_lst()
  
}

#' @rdname dplyr
tbl_vars.eeg_lst <- function(x) {
  setdiff(dplyr::tbl_vars(x$signal),dplyr::tbl_vars(x$segments), c(".id", ".sample_id"))
}