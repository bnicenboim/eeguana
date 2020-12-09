#' Dplyr verbs overloaded for manipulating eeg_lst objects.
#'
#' Manipulate the signal table and the segments table of an eeg_lst.
#'
#' Wrappers for [`dplyr`][dplyr::dplyr]'s commands that act on different parts
#' `eeg_lst` objects. Functions that drop or rename column won't remove columns starting with a dot.
#' The following wrappers act in a special way for `eeg_lst` objects:
#'
#' * `*_join()`: joins an external table to the *segments* table of the eeg_lst.
#' * `mutate()` and `transmute()` Variables that are a function of a channel are added to the signal_tbl table, and other variables are added to the segments table.
#' * `summarize()` summarizes the channel of the signal_tbl table.
#' * `pull()` only pulls columns of the signal table
#'
#' In addition, `_at()`, and `_if()` versions of the functions should work as well. Notice that  the `_at()` versions are 
#' much faster than the `_if()` versions of these commands.
#'
#' ## Gotchas
#'
#' These functions emulate [dplyr] functionality but they are actually powered by [data.table], and some times they might be behave a bit differently than the dplyr counterpart.
#'
#' - The default values of the arguments might be different, and some arguments might not exist for the eeguana dplyr-like functions.
#' - grouped mutations behave slightly different than ungrouped ones: Channel properties are removed if the data is ungrouped and one does `mutate(data, channel = 0)`, but not if the data is grouped.
#' - eeguana's [mutate] doesn't allow to refer back to a recently created channel: `data_eeg %>% mutate(X = F1 *2, Y = X)` is not valid. One needs to do `data_eeg %>% mutate(X = F1) %>% mutate(Y = X)`.
#' - eeguana's [mutate] doesn't use the most updated value of a column from the same call. If X is a channel, then `data_eeg %>% mutate(X = X *2, Y = X+1)` will add `1` to the original value of `X`, and not to the latest one.
#' - `n()` doesn't work, instead `length(.sample)` will give the same answer.
#' - `across()` and `where()` cannot be used.
#'
#'
#' ## Pitfalls
#'
#' These functions not only edit the eeg_lst objects but they also do book-keeping: They remove unnecessary channels, or update their information and they ensure that three tables (signal, segments, and events) match. It's then not recommended to edit the signal and segments table directly. (The events and channels table can be edited directly by doing `events_tbl(data_eeg) <- ...` or `channels_tbl(data_eeg) <- ...`).
#'
#'
#' @param .data An eeg_lst.
#' @param x An eeg_lst.
#' @param y A data frame, tibble, or data.table.
#' @inheritParams dplyr::join
#' @inheritParams dplyr::pull
#' @param ... Name-value pairs of expressions; see [dplyr][dplyr::dplyr] for more help.
#' @param .preserve Not in use, for compatibility reasons.
#' @param .add When FALSE, the default, group_by() will override existing groups. To add to the existing groups, use .add = TRUE.
#' @param .drop Only .drop = FALSE is avaialble, empty groups are never dropped.
#' @param .groups Only .groups = "keep" is avaialble.  Same grouping structure as .data.
#' @importFrom dplyr  select mutate transmute summarise rename
#' @importFrom dplyr group_by ungroup group_vars
#' @importFrom dplyr groups
#' @importFrom dplyr anti_join left_join right_join full_join semi_join inner_join
#' 
#' @return An eeg_lst object.
#'
#' @family tidyverse-like functions
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
  # dots <- rlang::quos(...)
  mutate_eeg_lst(.data, ..., keep_cols = TRUE) %>% 
    validate_eeg_lst()
}

#' @rdname dplyr_verbs
#' @export
transmute.eeg_lst <- function(.data, ...) {
  # dots <- rlang::quos(...)
  mutate_eeg_lst(.data, ..., keep_cols = FALSE)
}
#' @rdname dplyr_verbs
filter.eeg_lst <- function(.data, ..., .preserve = FALSE) {
  if (.preserve == TRUE) {
    warning("Ignoring `.preserve` argument.")
  }
  filter_eeg_lst(.data, ...)
}
#' @rdname dplyr_verbs
filter.eeg_ica_lst <- function(.data, ..., .preserve = FALSE) {
  out <- NextMethod()
  recordings <- unique(out$.segments$.recording)
  out$.ica <- out$.ica[recordings]
  out
}
#' @rdname dplyr_verbs
#' @export
summarise.eeg_lst <- function(.data, ..., .groups = "keep") {
  dots <- rlang::quos(...)
  if(.groups != "keep") {
    warning("Only  'keep' option is available")
  }
  summarize_eeg_lst(.data, dots, .groups = "keep")
}
#' @rdname dplyr_verbs
#' @export
group_by.eeg_lst <- function(.data, ..., .add = FALSE, .drop = FALSE) {
  dots <- rlang::quos(...)
  if (.drop == TRUE) {
    warning("Ignoring .drop argument. It can only be set to FALSE.")
  }
  group_by_eeg_lst(.eeg_lst = .data, dots, .add = .add)
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

#' @rdname dplyr_verbs
#' @export
tbl_vars.eeg_lst <- function(x) {
  setdiff(dplyr::tbl_vars(x$.signal), c(dplyr::tbl_vars(x$.segments), c(".id", ".sample")))
}

#' @rdname dplyr_verbs
#' @export
pull.eeg_lst <- function(.data, var = -1, name = NULL, ...) {
  var <- tidyselect::vars_pull(names(.data$.signal), !!rlang::enquo(var))
  name <- rlang::enquo(name)
  if (rlang::quo_is_null(name)) {
    return(.data$.signal[[var]])
  }
  name <- tidyselect::vars_pull(names(.data$.signal), !!name)
  rlang::set_names(.data[[var]], nm = .data[[name]])
}


