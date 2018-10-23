
#' Mutate/transmute/select/rename channels with signals,  segments.
#'
#' Manipulate the signal and the segments table of an eeg_lst.
#'
#' Wrappers for \link{dplyr}'s commands that act on different parts
#' `eggble` objects.
#' The following wrappers have been implemented for `eeg_lst` objects:
#' * `summarize()` summarizes the channel of the signal table
#' * `summarize_all()` summarizes all the channels of the signal table.
#' * `mutate()` adds new variables and preserves existing ones. Variables that are a function of a channel are added to the signal table, and other variables are added to the segments table.
#' * `mutate_all()` mutates all the channels of the signal table.
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

#' @rdname dplyr
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

#' @rdname dplyr
#' @export
transmute_.eeg_lst <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  mutate_transmute(.data, mutate = FALSE, dots)
}


mutate_transmute <- function(.eeg_lst, mutate = TRUE, .dots) {
  if (mutate) {
    dplyr_fun <- dplyr::mutate
  } else {
    dplyr_fun <- dplyr::transmute
  }
  # For testing:
  # .dots <- rlang::quos(Occipital = (O1 + O2 + Oz)/3)
  new_dots <- dots_by_df(.dots, .eeg_lst)

  if (length(new_dots$signal) > 0) {

    # I add the missing variables in case one uses transmute,
    # it doesn't hurt to mutate. This prevents from deleting sample or .id
    missing_vars <- dplyr::setdiff(
      obligatory_cols$signal,
      dplyr::group_vars(.eeg_lst$signal)
    )
    .dots <- c(rlang::syms(missing_vars), new_dots$signal)
    .eeg_lst$signal <- do_based_on_grps(
      .df = .eeg_lst$signal,
      ext_grouping_df = .eeg_lst$segments,
      dplyr_fun = dplyr_fun,
      dots = .dots
    )
  }

  if (length(new_dots$segments) > 0) {
    missing_vars <- dplyr::setdiff(
      obligatory_cols$segments,
      dplyr::group_vars(.eeg_lst$segments)
    ) %>%
      rlang::syms(.)

    .eeg_lst$segments <- dplyr_fun(
      .eeg_lst$segments, !!!missing_vars,
      !!!new_dots$segments
    )
  }
  update_events_channels(.eeg_lst) %>% validate_eeg_lst()
}

#' @rdname dplyr
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


summarize_eeg_lst <- function(.eeg_lst, .dots){
    segments_groups <- dplyr::groups(.eeg_lst$segments)
  signal_groups <- dplyr::groups(.eeg_lst$signal)


  # if there is something conditional on segments (O[condition == "faces"],
  # I should add them to the signal df temporarily
  add_cols <- names_segments_col(.eeg_lst, .dots)
  if (length(add_cols) > 0) {
    .eeg_lst$signal <- dplyr::left_join(.eeg_lst$signal,
      dplyr::select(dplyr::ungroup(.eeg_lst$segments), .id, add_cols),
      by = ".id"
    )
  }

  .eeg_lst$signal <- do_based_on_grps(
    .df = .eeg_lst$signal,
    ext_grouping_df = .eeg_lst$segments,
    dplyr_fun = dplyr::summarize,
    dots = .dots
  ) %>%
    # dplyr::ungroup() %>%
    # dplyr::mutate(.sample_id = if (".sample_id" %in% tbl_vars(.)) {
    #   .sample_id
    # } else {
    #   NA_integer_
    # }) %>%
    dplyr::group_by(.sample_id) %>%
    dplyr::mutate(.id = seq_len(dplyr::n()) %>% as.integer()) %>%
    dplyr::group_by(!!!signal_groups)

  if (nrow(.eeg_lst$signal) != 0) {
    last_id <- max(.eeg_lst$signal$.id)
  } else {
    last_id <- integer(0)
  }

  .eeg_lst$segments <- dplyr::summarize(.eeg_lst$segments) %>%
    dplyr::ungroup() %>%
    {
      if (!".id" %in% dplyr::tbl_vars(.)) {
        hd_add_column(., .id = seq_len(last_id) %>% as.integer())
      } else {
        .
      }
    } %>%
    # dplyr::mutate(recording = if ("recording" %in% tbl_vars(.)) {
    #   recording
    # } else {
    #   NA_character_
    # }) %>%
    dplyr::select(.id, dplyr::everything()) %>%
    # dplyr::group_by(recording) %>%
    # dplyr::mutate(segment = seq_len(dplyr::n())) %>%
    dplyr::group_by(!!!segments_groups)

  # TODO maybe I can do some type of summary of the events table, instead
  .eeg_lst$events <- .eeg_lst$events %>% filter(FALSE)

  update_events_channels(.eeg_lst) %>% validate_eeg_lst()
}

#' @export
tbl_vars.eeg_lst <- function(x) {
  setdiff(tbl_vars(x$signal), c(".id", ".sample_id"))
}

#' @rdname dplyr
#' @export
groups.eeg_lst <- function(x) {
  groups(x$segments)
}

#' @rdname dplyr
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

group_by_eeg_lst <- function(.eeg_lst, .dots, .add = FALSE){

  # dots <- rlang::quos(segment)
  # dots <- rlang::quos(.sample_id)
  # divide dots according to if they belong to $signal or segments
  new_dots <- dots_by_df(.dots, .eeg_lst)

  .eeg_lst$signal <- dplyr::group_by(.eeg_lst$signal, !!!new_dots$signal, add = .add)
  .eeg_lst$segments <- dplyr::group_by(.eeg_lst$segments, !!!new_dots$segments, add = .add)

  if (".id" %in% dplyr::group_vars(.eeg_lst$signal)) {
    .eeg_lst$segments <- dplyr::group_by(.eeg_lst$segments, .id, add = TRUE)
  }

  validate_eeg_lst(.eeg_lst)
}

#' @rdname dplyr
#' @export
ungroup.eeg_lst <- function(.data, ..., add = add) {
  .data$signal <- dplyr::ungroup(.data$signal)
  .data$segments <- dplyr::ungroup(.data$segments)
  validate_eeg_lst(.data)
}

#' @rdname dplyr
#' @export
group_vars.eeg_lst <- function(x) {
  c(group_vars(x$signal), group_vars(x$segments))
}

#' @rdname dplyr
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


filter_eeg_lst <- function(.eeg_lst, .dots){  

  # .dots <- rlang::quos(recording == "0")
    new_dots <- dots_by_df(.dots, .eeg_lst)
  
    # filter the signal and update the segments, in case an entire id drops
    if (length(new_dots$signal) > 0) {
      .eeg_lst$signal <- do_based_on_grps(.eeg_lst$signal,
        ext_grouping_df = .eeg_lst$segments,
        dplyr_fun = dplyr::filter, new_dots$signal
      )
  
      .eeg_lst$segments <- dplyr::semi_join(.eeg_lst$segments, .eeg_lst$signal, by = ".id")
      .eeg_lst$events <- dplyr::semi_join(.eeg_lst$events, .eeg_lst$segments, by = ".id")
    }
    # filter the segments and update the signal
    if (length(new_dots$segments) > 0) {
      .eeg_lst$segments <- dplyr::filter(.eeg_lst$segments, !!!new_.dots$segments)
      .eeg_lst$signal <- dplyr::semi_join(.eeg_lst$signal, .eeg_lst$segments, by = ".id")
      .eeg_lst$events <- dplyr::semi_join(.eeg_lst$events, .eeg_lst$segments, by = ".id")
    }
  
    # Fix the indices in case some of them drop out
    redo_indices(.eeg_lst) %>% update_events_channels() %>% validate_eeg_lst()
  }  

#' @rdname dplyr
#' @export
#'
select.eeg_lst <- function(.data, ...) {
  select_rename(.data, select = TRUE, ...)
}

#' @rdname dplyr
#' @export
#'
rename.eeg_lst <- function(.data, ...) {
  select_rename(.data, select = FALSE, ...)
}


select_rename <- function(.eeg_lst, select = TRUE, ...) {
  if (select) {
    vars_fun <- tidyselect::vars_select
    # dplyr_fun <- dplyr::select
  } else {
    vars_fun <- tidyselect::vars_rename
    # dplyr_fun <- dplyr::rename
  }
  dots <- rlang::enquos(...)
  # dots <- rlang::quos(xx =Fp1, yy = Cz)
  # dots <- rlang::quos(O1, O2, P7, P8)
  # dots <- rlang::quos(-Fp1)
  all_vars <- vars_fun(unique(c(
    names(.eeg_lst$signal),
    names(.eeg_lst$segments)
  )), !!!dots)

  select_in_df <- c("signal", "segments")
  if (length(intersect(all_vars, names(.eeg_lst$segments))) == 0) {
    select_in_df <- select_in_df[select_in_df != "segments"]
  }
  if (length(intersect(all_vars, names(.eeg_lst$signal))) == 0) {
    select_in_df <- select_in_df[select_in_df != "signal"]
  }

  # Divide the variables into the relevant columns
  for (dfs in select_in_df) {
    vars_dfs <- all_vars[all_vars %in% colnames(.eeg_lst[[dfs]])]
    # by adding these groups, select won't remove the obligatory columns
    vars_dfs <- c(obligatory_cols[[dfs]], vars_dfs)

    if (length(vars_dfs) > 0) {
      orig_groups <- dplyr::groups(.eeg_lst[[dfs]])
      .eeg_lst[[dfs]] <- .eeg_lst[[dfs]] %>%
        dplyr::select(vars_dfs) %>%
        dplyr::group_by(!!!orig_groups)
    }
  }

  update_events_channels(.eeg_lst) %>% validate_eeg_lst()
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
  x$signal <- dplyr::semi_join(x$signal, x[["segments"]], by = ".id")
  x$events <- dplyr::semi_join(x$events, x[["segments"]], by = ".id")

  redo_indices(x) %>% validate_eeg_lst()
}

#' @rdname dplyr
#' @export
anti_join.eeg_lst <- function(x, y, by = NULL, suffix = c(".x", ".y"), ...) {
  if (!is.data.frame(y)) stop("y must be a data frame or tibble.")

  x[["segments"]] <- dplyr::anti_join(x[["segments"]], y, by = NULL, suffix = c(".x", ".y"), ...)
  x[["signal"]] <- dplyr::semi_join(x[["signal"]], x$segments, by = ".id")
  x$events <- dplyr::semi_join(x$events, x$segments, by = ".id")

  redo_indices(x) %>% validate_eeg_lst()
}
