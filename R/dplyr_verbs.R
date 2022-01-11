#' Dplyr-like functions for manipulating eeg_lst objects.
#'
#' Manipulate the signal table and the segments table of an eeg_lst.
#'
#' Wrappers for [`dplyr`][dplyr::dplyr]'s commands that act on different parts
#' `eeg_lst` objects. Functions that drop or rename columns won't remove columns starting with a dot. These functions are powered by [`data.table`][data.table::data.table], and inspired by the way [`tidytable`][tidytable::tidytable] works.
#'
#' The following wrappers act in a special way for `eeg_lst` objects:
#'
#' * `*_join()`: joins an external table to the *segments* table of the eeg_lst.
#' * `eeg_mutate()` and `eeg_transmute()`: mutates the signal_tbl table when is applied to a channel (or a column of the signal table), and mutates the segments table otherwise. It can also mutates by reference.
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
#' - [eeg_filter] behaves similarly to dplyr's [`filter`][dplyr::filter]. If you want to filter the signal using IIR or FIR filters use [eeg_filt*][eeguana::filt] functions.
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
#' @param .by_reference Acts in place rewriting the eeg_lst object.
#' @inheritParams dplyr::join
#' @inheritParams dplyr::pull
#' @param ... Name-value pairs of expressions; see [dplyr][dplyr::dplyr] for more help.
#' @param .preserve Not in use, for compatibility reasons.
#' @param .add When FALSE, the default, group_by() will override existing groups. To add to the existing groups, use .add = TRUE.
#' @param .drop Only .drop = FALSE is available, empty groups are never dropped.
#' @param .groups Only .groups = "keep" is available.  Same grouping structure as .data.
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
#'     as_time(.sample, .unit = "ms"),
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
eeg_mutate <- function(.data, ..., .by_reference = FALSE) {
  UseMethod("eeg_mutate")
}

#' @export
eeg_mutate.eeg_lst <- function(.data, ...) {
  .data <- update_eeg_lst(.data)
  mutate_eeg_lst(.data, ..., keep_cols = TRUE) %>%
    validate_eeg_lst()
}

#' @export
mutate.eeg_lst <- eeg_mutate.eeg_lst


#' @rdname dplyr_verbs
#' @export
eeg_transmute <- function(.data, ..., .by_reference = FALSE) {
  UseMethod("eeg_transmute")
}


#' @export
eeg_transmute.eeg_lst <- function(.data, ..., .by_reference = FALSE) {
  .data <- update_eeg_lst(.data)
  mutate_eeg_lst(.data, ..., keep_cols = FALSE, .by_reference = .by_reference)
}

#' @noRd
#' @export
transmute.eeg_lst <- eeg_transmute.eeg_lst


#' @rdname dplyr_verbs
#' @export
eeg_filter <- function(.data, ..., .preserve = FALSE) {
  UseMethod("eeg_filter")
}

#' @export
eeg_filter.eeg_lst <- function(.data, ..., .preserve = FALSE) {
  if (.preserve == TRUE) {
    warning("Ignoring `.preserve` argument.")
  }
  .data <- update_eeg_lst(.data)
  filter_eeg_lst(.data, ...)
}

#' @export
eeg_filter.eeg_ica_lst <- function(.data, ..., .preserve = FALSE) {
  out <- NextMethod()
  recordings <- unique(out$.segments$.recording)
  out$.ica <- out$.ica[recordings]
  out
}

#' @noRd
#' @export
filter.eeg_lst <- eeg_filter.eeg_lst
#' @noRd
#' @export
filter.eeg_ica_lst <- eeg_filter.eeg_ica_lst

#' @rdname dplyr_verbs
#' @export
eeg_summarize <- function(.data, ..., .groups = "keep") {
  UseMethod("eeg_summarize")
}

#' @export
eeg_summarize.eeg_lst <- function(.data, ..., .groups = "keep") {
  dots <- rlang::quos(...)
  .data <- update_eeg_lst(.data)
  if(.groups != "keep") {
    warning("Only  'keep' option is available")
  }
  summarize_eeg_lst(.data, dots, .groups = "keep")
}

#' @noRd
#' @export
summarize.eeg_lst <- eeg_summarize.eeg_lst
#' @noRd
#' @export
summarise.eeg_lst <- eeg_summarize.eeg_lst
#' @noRd
#' @export
eeg_summarise.eeg_lst <- eeg_summarize.eeg_lst


#' @rdname dplyr_verbs
#' @export
eeg_group_by <- function(.data, ..., .add = FALSE, .drop = FALSE) {
  UseMethod("eeg_group_by")
}

#' @rdname dplyr_verbs
#' @export
eeg_ungroup <- function(.data, ...) {
  UseMethod("eeg_ungroup")
}

#' @export
eeg_group_by.eeg_lst <- function(.data, ..., .add = FALSE, .drop = FALSE) {
  dots <- rlang::quos(...)
  if (.drop == TRUE) {
    warning("Ignoring .drop argument. It can only be set to FALSE.")
  }
  group_by_eeg_lst(.eeg_lst = .data, dots, .add = .add)
}

#' @export
eeg_ungroup.eeg_lst <- function(.data, ...) {
  attributes(.data)$vars <- character(0)
  validate_eeg_lst(.data)
}

#' @noRd
#' @export
group_by.eeg_lst <- eeg_group_by.eeg_lst
#' @noRd
#' @export
ungroup.eeg_lst <- eeg_ungroup.eeg_lst

#' @rdname dplyr_verbs
#' @export
eeg_select <- function(.data, ...) {
  UseMethod("eeg_select")
}

#' @export
eeg_select.eeg_lst <- function(.data, ...) {
  .data <- update_eeg_lst(.data)
  select_rename(.data, select = TRUE, ...)
}

#' @noRd
#' @export
select.eeg_lst <- eeg_select.eeg_lst


#' @rdname dplyr_verbs
#' @export
eeg_rename <- function(.data, ...) {
  UseMethod("eeg_rename")
}

#' @export
eeg_rename.eeg_lst <- function(.data, ...) {
  .data <- update_eeg_lst(.data)
  #TODO: simplify and use parts of eeg_rename_with
  select_rename(.data, select = FALSE, ...)
}

#' @rdname dplyr_verbs
#' @export
eeg_rename_with <- function(.data, .fn, .cols = where(is_channel_dbl), ...) {
  UseMethod("eeg_rename_with")
}

#' @export
eeg_rename_with.eeg_lst <- function(.data, .fn, .cols = where(is_channel_dbl), ...) {
  .data <- update_eeg_lst(.data)
  .cols <- rlang::enquo(.cols)
  .fn <- rlang::as_function(.fn)

    vars_signal <- tidyselect::eval_select(expr = rlang::expr(!!.cols),
                            data = .data$.signal[0],
                            strict = FALSE
      ) %>%  names()
    vars_segments <- tidyselect::eval_select(expr = rlang::expr(!!.cols),
                            data = .data$.segments[0],
                            strict = FALSE
      ) %>%  names()
    
    if(length(c(vars_signal, vars_segments)) == 0){
      stop("Can't subset columns that don't exist.\n ",
           "Can't find columns that match ",rlang::as_label(.cols),
           call. = FALSE)
    }
    
    renamed_obligatory <- intersect(c(rel_signal, rel_segments),
                                    c(obligatory_cols$.signal, 
                                      obligatory_cols$.segments))
    if(length(renamed_obligatory)>0){
      stop("Trying to rename obligatory column(s): ", renamed_obligatory,
           call. = FALSE)
    }
    
    new_segments <- NULL
    new_signal <- NULL
     if(length(vars_signal) > 0){ 

      .data$.signal <- shallow(.data$.signal)
      
      new_signal <- .fn(vars_signal,...) 
      names(new_signal) <- vars_signal
      data.table::setnames(.data$.signal, vars_signal, new_signal)
      
      # update channels in events
      .data$.events <- .data$.events %>% 
        mutate.(.channel = ifelse(.channel %in% vars_signal,
                                                  new_signal[.channel],
                                                  .channel))
          }
    if(length(vars_segments) > 0){ 
      .data$.segments <- shallow(.data$.segments)
      new_segments <- .fn(vars_segments,...)
      names(new_segments) <- vars_segments
      
      data.table::setnames(.data$.segments, vars_segments, new_segments)
      }
    
    # Update groups
    g_vars <- eeg_group_vars(.data)
    if(length(g_vars) >0 ) {
      rel_vars <- c(vars_signal, vars_segments)
      new_names <- c(new_signal, new_segments)
      g_vars[eeg_group_vars(.data) %in% rel_vars] <- new_names[names(new_names) %in% eeg_group_vars(.data)]
      #TODO: fix so this works:
        #.data <- eeg_group_by(.data, across(g_vars))
        #alternative
        attributes(.data)$vars <- g_vars
    }  

  data.table::setkey(.data$.signal, .id, .sample)
  data.table::setkey(.data$.segments, .id)
  
  .data %>%
    validate_eeg_lst()
}

#' @noRd
#' @export
rename_with.eeg_lst <- eeg_rename_with.eeg_lst


#' @noRd
#' @export
rename.eeg_lst <- eeg_rename.eeg_lst


#' @rdname dplyr_verbs
#' @export
eeg_groups <- function(x) {
  UseMethod("eeg_groups")
}
#' @rdname dplyr_verbs
#' @export
eeg_groups.eeg_lst <- function(x) {
  attributes(x)$vars %>% purrr::map(as.name)
}

#' @noRd
#' @export
groups.eeg_lst <- eeg_groups.eeg_lst


#' @rdname dplyr_verbs
#' @export
eeg_group_vars <- function(x) {
  UseMethod("eeg_group_vars")
}
#' @export
eeg_group_vars.eeg_lst <- function(x) {
  attributes(x)$vars
}

#' @noRd
#' @export
group_vars.eeg_lst <- eeg_group_vars.eeg_lst

#' @rdname dplyr_verbs
#' @export
eeg_anti_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  UseMethod("eeg_anti_join")
}

#' @export
eeg_anti_join.eeg_lst <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  if (!is.data.frame(y)) stop("y must be a data frame, a data table or tibble.")

  x$.segments <- dplyr::anti_join(x$.segments, y, by = NULL, suffix = c(".x", ".y"), ...)

  segments <- data.table::as.data.table(x$.segments)
  x$.signal <- semi_join_dt(x$.signal, segments, by = ".id")
  x$.events <- semi_join_dt(x$.events, segments, by = ".id")
  x %>% validate_eeg_lst()
}

#' @noRd
#' @export
anti_join.eeg_lst <- eeg_anti_join.eeg_lst

#' @rdname dplyr_verbs
#' @export
eeg_left_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  UseMethod("eeg_left_join")
}

#' @export
eeg_left_join.eeg_lst <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  if (!is.data.frame(y)) stop("y must be a data frame, a data table or tibble.")

  x$.segments <- dplyr::left_join(x$.segments, y = y, by = by, copy = copy, suffix = c(".x", ".y"), ...)

  validate_eeg_lst(x)
}
#' @noRd
#' @export
left_join.eeg_lst <- eeg_left_join.eeg_lst

#' @rdname dplyr_verbs
#' @export
eeg_semi_join <- function(x, y, by = NULL, suffix = c(".x", ".y"), ...) {
  UseMethod("eeg_semi_join")
}

#' @export
eeg_semi_join.eeg_lst <- function(x, y, by = NULL, suffix = c(".x", ".y"), ...) {
  if (!is.data.frame(y)) stop("y must be a data frame, a data table or tibble.")

  x$.segments <- dplyr::semi_join(x$.segments, y, by = NULL, suffix = c(".x", ".y"), ...)

  segments <- data.table::as.data.table(x$.segments)
  x$.signal <- semi_join_dt(x$.signal, segments, by = ".id")
  x$.events <- semi_join_dt(x$.events, segments, by = ".id")
  x %>% validate_eeg_lst()
}

#' @noRd
#' @export
semi_join.eeg_lst <- eeg_semi_join.eeg_lst

#' @rdname dplyr_verbs
#' @export
eeg_vars <- function(x) {
  UseMethod("eeg_vars")
}
#' @export
eeg_vars.eeg_lst <- function(x) {
  setdiff(dplyr::tbl_vars(x$.signal), c(dplyr::tbl_vars(x$.segments), c(".id", ".sample")))
}

#' @noRd
#' @export
tbl_vars.eeg_lst <- eeg_vars.eeg_lst


#' @rdname dplyr_verbs
#' @export
eeg_pull <- function(.data, var = -1, name = NULL, ...) {
  UseMethod("eeg_pull")
}

#' @export
eeg_pull.eeg_lst <- function(.data, var = -1, name = NULL, ...) {
  var <- tidyselect::vars_pull(names(.data$.signal), !!rlang::enquo(var))
  name <- rlang::enquo(name)
  if (rlang::quo_is_null(name)) {
    return(.data$.signal[[var]])
  }
  name <- tidyselect::vars_pull(names(.data$.signal), !!name)
  rlang::set_names(.data[[var]], nm = .data[[name]])
}

#' @noRd
#' @export
pull.eeg_lst <- eeg_pull.eeg_lst

