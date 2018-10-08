
#' Mutate/transmute/select/rename channels with signals,  segments.
#'
#' Manipulate the signal, events and the segments of an eegble.
#'
#' Wrappers for \link{dplyr}'s commands that act on different parts
#' \code{eggble} objects.
#' The following wrappers have been implemented for \code{eegble} objects:
#' \itemize{
#' \item \code{summarize()} summarizes the channel of the signal dataframe.
#' \item \code{summarize_all()} summarizes all the channels of the signal dataframe.
#' \item \code{mutate()} adds new variables and preserves existing ones.
#' \item \code{mutate_all()} mutates all the channels of the signal dataframe.
#' \item \code{transmute()} drops existing variables.
#' \item \code{select()} keeps only the mentioned variables.
#' \item \code{rename()}: keeps all variables.
#' \item \code{filter()}: finds segments/samples where conditions are true. Segments/samples where the condition evaluates to NA are dropped.
#' \item \code{left_join()}: left-joins an external dataframe to one of the segments of the eegble.
#' \item \code{semi_join()}: semi-joins an external dataframe to one of the segments of the eegble.
#' \item \code{anti_join()}: anti-joins an external dataframe to one of the segments of the eegble.
#' \item  \code{group_by()}: allows that operations would be performed "by group".
#' \item  \code{ungroup()}: removes the grouping created by group_by.
#' }
#'
#' These commands always return the entire eegble so that
#' they can be ' piped using \link{magrittr}'s pipe, %>%.
#'
#' @param .data An eegbl.
#' @param ... \code{signal}, \code{events} or \code{segments} for \code{act_on}, and see  \link{dplyr-package} for the different dplyr "verbs".
#' @return An eegble object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' faces_segs %>% select(O1, O2, P7, P8)
#' }

#' @export
mutate_.eegbl <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  mutate_transmute(.data, mutate = TRUE, dots)
}

#' @export
transmute_.eegbl <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  mutate_transmute(.data, mutate = FALSE, dots)
}


mutate_transmute <- function(.data, mutate = TRUE, dots) {
  if (mutate) {
    dplyr_fun <- dplyr::mutate
  } else {
    dplyr_fun <- dplyr::transmute
  }
  # dots <- rlang::quos(Occipital = (O1 + O2 + Oz)/3)
  new_dots <- dots_by_df(dots, .data)


  if (length(new_dots$signal) > 0) {

    # I add the missing variables in case one uses transmute,
    # it doesn't hurt to mutate. This prevents from deleting sample or .id
    missing_vars <- dplyr::setdiff(
      obligatory_cols$signal,
      dplyr::group_vars(.data$signal)
    )
    dots <- c(rlang::syms(missing_vars), new_dots$signal)
    .data$signal <- do_based_on_grps(
      .df = .data$signal,
      ext_grouping_df = .data$segments,
      dplyr_fun = dplyr_fun,
      dots = dots
    )
  }

  if (length(new_dots$segments) > 0) {
    missing_vars <- dplyr::setdiff(
      obligatory_cols$segments,
      dplyr::group_vars(.data$segments)
    ) %>%
      rlang::syms(.)

    .data$segments <- dplyr_fun(
      .data$segments, !!!missing_vars,
      !!!new_dots$segments
    )
  }

  update_chans(.data) %>% validate_eegbl()
}

#' @export
summarise_.eegbl <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  segments_groups <- dplyr::groups(.data$segments)
  signal_groups <- dplyr::groups(.data$signal)


  # if there is something conditional on segments (O[condition == "faces"],
  # I should add them to the signal df temporarily
  add_cols <- names_segments_col(.data, dots)
  if (length(add_cols) > 0) {
    .data$signal <- dplyr::left_join(.data$signal,
      dplyr::select(dplyr::ungroup(.data$segments), .id, add_cols),
      by = ".id"
    )
  }

  .data$signal <- do_based_on_grps(
    .df = .data$signal,
    ext_grouping_df = .data$segments,
    dplyr_fun = dplyr::summarize,
    dots = dots
  ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(sample = if ("sample" %in% tbl_vars(.)) {
      sample
    } else {
      NA_integer_
    }) %>%
    dplyr::group_by(sample) %>%
    dplyr::mutate(.id = seq(dplyr::n()) %>% as.integer()) %>%
    dplyr::group_by(!!!signal_groups)

  last_id <- max(.data$signal$.id)

  .data$segments <- dplyr::summarize(.data$segments) %>%
    dplyr::ungroup() %>%
    hd_add_column(.id = seq(last_id) %>% as.integer()) %>%
    dplyr::mutate(recording = if ("recording" %in% tbl_vars(.)) {
      recording
    } else {
      NA_character_
    }) %>%
    dplyr::select(.id, dplyr::everything()) %>%
    dplyr::group_by(recording) %>%
    dplyr::mutate(segment = seq(dplyr::n())) %>%
    dplyr::group_by(!!!segments_groups)
  # dplyr::group_by_at(dplyr::vars(segments_groups)) #for chars

  update_chans(.data) %>% validate_eegbl()
}

#' @export
tbl_vars.eegbl <- function(x) {
  setdiff(tbl_vars(x$signal), c(".id", "sample"))
}

#' @export
groups.eegbl <- function(x) {
  groups(x$segments)
}

#' @export
group_by_.eegbl <- function(.data, ..., .dots = list(), add = add) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  # dots <- rlang::quos(segment)
  # divide dots according to if they belong to $signal or segments
  new_dots <- dots_by_df(dots, .data)

  .data$segments <- dplyr::group_by(.data$segments, !!!new_dots$segments, add = add)
  .data$signal <- dplyr::group_by(.data$signal, !!!new_dots$signal, add = add)
  update_chans(.data) %>% validate_eegbl()
}

#' @export
ungroup.eegbl <- function(.data, ..., add = add) {
  .data$segments <- dplyr::ungroup(.data$segments)
  update_chans(.data) %>% validate_eegbl()
}



#' @export
filter_.eegbl <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  # dots <- rlang::quo(recording == "0")
  new_dots <- dots_by_df(dots, .data)

  # filter the signal and update the segments, in case an entire id drops
  if (length(new_dots$signal) > 0) {
    .data$signal <- do_based_on_grps(.data$signal,
      ext_grouping_df = .data$segments,
      dplyr_fun = dplyr::filter, new_dots$signal
    )

    .data$segments <- dplyr::semi_join(.data$segments, .data$signal, by = ".id")
  }
  # filter the segments and update the signal
  if (length(new_dots$segments) > 0) {
    .data$segments <- dplyr::filter(.data$segments, !!!new_dots$segments)
    .data$signal <- dplyr::semi_join(.data$signal, .data$segments, by = ".id")
  }

  # Fix the indices in case some of them drop out
  redo_indices(.data) %>%
    update_chans() %>%
    validate_eegbl()
}


#' @export
#'
select.eegbl <- function(.data, ...) {
  select_rename(.data, select = TRUE, ...)
}

#' @export
#'
rename.eegbl <- function(.data, ...) {
  select_rename(.data, select = FALSE, ...)
}


select_rename <- function(.data, select = TRUE, ...) {
  if (select) {
    vars_fun <- tidyselect::vars_select
    # dplyr_fun <- dplyr::select
  } else {
    vars_fun <- tidyselect::vars_rename
    # dplyr_fun <- dplyr::rename
  }
  dots <- rlang::enquos(...)
  # dots <- rlang::quos(xx =Fp1, yy = Cz)
  all_vars <- vars_fun(unique(c(
    names(.data$signal),
    names(.data$segments)
  )), !!!dots)

  # Divide the variables into the relevant columns

  for (dfs in c("signal", "segments")) {
    vars_dfs <- all_vars[all_vars %in% colnames(.data[[dfs]])]
    # intersect(all_vars, colnames(.data$signal))
    if (length(vars_dfs) > 0) {
      orig_groups <- dplyr::groups(.data[[dfs]])
      .data[[dfs]] <- .data[[dfs]] %>%
        # by adding these groups, select won't remove the obligatory columns
        dplyr::group_by_at(vars(obligatory_cols[[dfs]]), add = TRUE) %>%
        # silences the message: Adding missing grouping variables: `.id`, `sample`
        dplyr::select(vars_dfs) %>%
        dplyr::group_by(!!!orig_groups)
    }
  }

  update_chans(.data) %>% validate_eegbl()
}



#' @export
left_join.eegbl <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  # if(!is_eegble(x)) stop("x must be an eegble.")
  # df <-  attr(x, "act_on")
  # if(!df %in% c("segments","events")) stop("x must be act_on segments or events.")

  x[["segments"]] <- dplyr::left_join(x[["segments"]], y = y, by = by, copy = copy, suffix = c(".x", ".y"), ...)

  validate_eegbl(x)
}



#' @export
semi_join.eegbl <- function(x, y, by = NULL, suffix = c(".x", ".y"), ...) {
  # if(!is_eegble(x)) stop("x must be an eegble.")
  # df <-  attr(x, "act_on")
  # if(!df %in% c("segments","events")) stop("x must be act_on segments or events.")

  x[["segments"]] <- dplyr::semi_join(x[["segments"]], y, by = NULL, suffix = c(".x", ".y"), ...)
  x$signal <- dplyr::semi_join(x$signal, x[["segments"]], by = ".id")

  # if(df %in% "events"){
  #   x$segments <- dplyr::semi_join(x$segments, x[[df]], by = ".id")
  # }

  # if(df %in% "segments"){
  # x$events <- dplyr::semi_join(x$events, x[[df]], by = ".id")
  # }

  validate_eegbl(x)
}


#' @export
anti_join.eegbl <- function(x, y, by = NULL, suffix = c(".x", ".y"), ...) {
  # if(!is_eegble(x)) stop("x must be an eegble.")
  # df <-  attr(x, "act_on")
  # if(!df %in% c("segments","events")) stop("x must be act_on segments or events.")

  x[["segments"]] <- dplyr::anti_join(x[["segments"]], y, by = NULL, suffix = c(".x", ".y"), ...)
  x[["signal"]] <- dplyr::semi_join(x[["signal"]], x[["segments"]], by = ".id")

  # if(df %in% "events"){
  #   x$segments <- dplyr::semi_join(x$segments, x[[df]], by = ".id")
  # }

  # if(df %in% "segments"){
  #   x$events <- dplyr::semi_join(x$events, x[[df]], by = ".id")
  # }

  validate_eegbl(x)
}
