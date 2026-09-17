#' @noRd
## dplyr and tidytable each carry their own grouping and neither reads the
## other's. tidytable::group_vars() on a dplyr grouped_df does not error, it
## silently returns the contents of dplyr's .groups attribute instead of the
## group names. The public data.frame methods take whatever the user grouped,
## so go by the class rather than trusting either one.
tbl_group_vars <- function(x) {
  if (inherits(x, "grouped_df")) {
    ## only reachable when the caller built a grouped_df, so dplyr is present
    dplyr::group_vars(x)
  } else if (inherits(x, "grouped_tt")) {
    tidytable::group_vars(x)
  } else {
    character(0)
  }
}

#' @noRd
tbl_ungroup <- function(x) {
  if (inherits(x, "grouped_df")) {
    dplyr::ungroup(x)
  } else if (inherits(x, "grouped_tt")) {
    tidytable::ungroup(x)
  } else {
    x
  }
}

#' @noRd
## tidytable verbs hand back a plain tidytable, dropping the class they were
## given. Where the result goes straight back into an eeg_lst, the class has to
## be put back. eeguana no longer sets data.table keys, so there is no key to
## restore.
keep_dt_attrs <- function(new, old) {
  class(new) <- class(old)
  new
}

#' @noRd
lapply_dtc <- function(X, FUN, ...) {
  lapply(X, FUN, ...) %>%
    data.table::setDT()
}

#' @noRd
vec_index <- function (x) names(x) %||% seq_along(x)

#' @noRd
imap_chr <- function (.x, .f, ...) {
  .f <- rlang::as_function(.f)
  tidytable::map2_chr(.x, vec_index(.x), .f, ...)
}

#' @noRd
imap <- function (.x, .f, ...) 
{
  .f <- rlang::as_function(.f, ...)
  tidytable::map2(.x, vec_index(.x), .f, ...)
}

#' @noRd
imap_lgl <- function(.x, .f, ...) {
  .f <- rlang::as_function(.f)
  tidytable::map2_lgl(.x, vec_index(.x), .f, ...)
}

#' @noRd
iwalk <- function(.x, .f, ...) {
  imap(.x, .f, ...)
  invisible(.x)
}

#' Apply .f only to the elements where .p holds, leaving the rest alone
#' @noRd
map_if <- function(.x, .p, .f, ...) {
  .p <- rlang::as_function(.p)
  .f <- rlang::as_function(.f)
  ## as.list() keeps a quosure's class, and subsetting a quosure with `[` is
  ## deprecated in rlang, so work on a plain list
  .x <- as.list(unclass(.x))
  sel <- vapply(.x, .p, logical(1))
  ## a fresh list, so that the attributes of a quosure do not travel with it
  out <- vector("list", length(.x))
  out[sel] <- lapply(.x[sel], .f, ...)
  out[!sel] <- .x[!sel]
  names(out) <- names(.x)
  out
}

#' The rows of a table as a list, one named list per row
#'
#' What purrr::transpose() did to a table: take the columns apart and put
#' them back together row by row.
#' @noRd
rows_as_list <- function(tbl) {
  lapply(seq_len(nrow(tbl)), function(i) as.list(tbl[i, ]))
}

#' @noRd
map_dtr <- function(.x, .f, ..., .id = NULL) {
  res <- tidytable::map(.x, .f, ...)
  data.table::rbindlist(res, fill = TRUE, idcol = .id)
}

#' @noRd
imap_dtr <- function(.x, .f, ..., .id = NULL) {
  map2_dtr(.x, names(.x), .f, ..., .id = .id)
}


#' @noRd
map2_dtr <- function(.x, .y, .f, ..., .id = NULL) {
  res <- tidytable::map2(.x, .y, .f, ...)
  ## rbindlist() turns these names into the .id column
  names(res) <- names(.x)
  data.table::rbindlist(res, fill = TRUE, idcol = .id)
}

#' @noRd
map2_dtc <- function(.x, .y, .f, ...) {
    data.table::as.data.table(tidytable::map2_dfc(.x=.x, .y = .y, .f =.f, ...) )
}

#' @noRd
left_join_dt <- function(x, y, by = NULL) {
  if (is.null(by)) {
    by <- intersect(colnames(x), colnames(y))
  }
  ## need to be reversed:
  if (!is.null(names(by))) {
    by_names <- names(by)
    by_content <- unname(by)
    by <- by_names
    names(by) <- by_content
    ## message_verbose('Joining, by = "',by,'"')
  } else {
    names(by) <- by
  }
  out <- y[x, on = by]

  # should I set allow.cartesian = TRUE?
  data.table::setnames(out, names(by), by)[]
}

#' @noRd
filter_dt <- function(.data, ..., group_by_ = character(0)) {
  dots <- rlang::quos(...)
  newdots <- Reduce(x = dots, f = function(x, y) rlang::quo(!!x & !!y))
  if (length(group_by_) == 0) {
    # TODO: this might be dangerous
    .data[rlang::eval_tidy(newdots, data = rlang::as_data_mask(.data))]
  } else {
    col_order <- names(.data)
    .data <- .data[, .SD[rlang::eval_tidy(newdots, data = cbind(.SD, data.table::as.data.table(.BY)))], by = c(group_by_)]
    data.table::setcolorder(.data, col_order)
    .data
  }
}



#' @noRd
unnest_dt <- function(.data, col) {
  # https://www.johannesbgruber.eu/post/a-faster-unnest/#fn1
  col <- rlang::ensyms(col)
  clnms <- rlang::syms(setdiff(colnames(.data), as.character(col)))
  tbl <- eval(
    rlang::expr(.data[, as.character(unlist(!!!col)), by = list(!!!clnms)])
  )
  colnames(.data) <- c(as.character(clnms), as.character(col))
  .data
}

#' Converts a struct from matlab into a data table
#' @noRd
struct_to_dt <- function(struct, .id = NULL) {
  if (length(struct) == 0) {
    data.table::data.table()
  } else {
    list_str <- apply(
      struct, 3,
      function(x) {
        lapply(
          x[, 1],
          function(x) {
            x <- x %||% NA
            # unmatrix
            #                         if(all((dim(x) %||% 1) ==c(1,1)))
            c(unlist(x)) %||% rep(NA, length(x))
          }
        )
      }
    )
    map_dtr(list_str, data.table::setDT, .id = .id)
  }
}

#' @noRd
tt_distinct <- function(.df, ..., .keep_all = FALSE) {
  oldclass <- class(.df)
  .df <- tidytable::distinct(.df = .df, ..., .keep_all = .keep_all)
  class(.df) <- oldclass
  .df
}

#' not in use yet
#' #' @noRd
#' rename. <- function(.df, ...) {
#'   oldclass <- class(.df)
#'   .df <- tidytable::rename(.df = .df, ...)
#'   class(.df) <- oldclass
#'   .df
#' }

#' @noRd
tt_select <- function(.df, ...) {
  oldclass <- class(.df)
  .df <- tidytable::select(.df = .df, ...)
  class(.df) <- oldclass
  .df
}

#' @noRd
tt_transmute <- function(.df, ..., .by = NULL){
  oldclass <- class(.df)
  if (length(.by) > 0) {
    .df <- tidytable::transmute(
      .df = .df, ...,
      .by = any_of(.by))
  } else {
    # much faster to remove the by=character(0) when not needed
    .df <- tidytable::transmute(
      .df = .df, ...)
  }
  
  class(.df) <- oldclass
  .df
}

#' @noRd
tt_bind_cols <- function(...){
  oldclass <- class(list(...)[[1]])
  .df <- tidytable::bind_cols(...)
  class(.df) <- oldclass
  .df
}


#' @noRd
tt_mutate <- function(.df, ...,
                    .by = NULL,
                    .keep = c("all", "used", "unused", "none")) {
  oldclass <- class(.df)
  if (length(.by) > 0) {
    .df <- tidytable::mutate(
      .df = .df, ...,
      .by = any_of(.by),
      .keep = .keep
    )
  } else {
    # much faster to remove the by=character(0) when not needed
    .df <- tidytable::mutate(
      .df = .df, ...,
      .keep = .keep
    )
  }

  class(.df) <- oldclass
  .df
}

#' @noRd
tt_filter <- function(.df, ...,
                    .by = NULL) {
  oldclass <- class(.df)
  .df <- tidytable::filter(
    .df = .df, ...,
    .by = any_of(.by)
  )
  class(.df) <- oldclass
  .df
}

#' @noRd
tt_summarize <- function(.df, ..., .by = NULL, .sort = FALSE) {
  oldclass <- class(.df)
  .df <- tidytable::summarize(.df = .df, ..., .by = any_of(.by), .sort = .sort)
  class(.df) <- oldclass
  .df
}

#' @noRd
tt_anti_join <- function(x, y, by = NULL) {
  oldclass <- class(x)
  .df <- tidytable::anti_join(x = x, y = y, by = by)
  class(.df) <- oldclass
  .df
}
#' @noRd
tt_semi_join <- function(x, y, by = NULL) {
  oldclass <- class(x)
  .df <- tidytable::semi_join(x = x, y = y, by = by)
  class(.df) <- oldclass
  .df
}


#' @noRd
tt_full_join <- function(x, y, by = NULL, suffix = c(".x", ".y"), ..., keep = FALSE) {
  oldclass <- class(x)
  .df <- tidytable::full_join(x = x, y = y, by = by, suffix = suffix, ..., keep = keep)
  class(.df) <- oldclass
  .df
}

#' @noRd
tt_left_join <- function(x, y, by = NULL, suffix = c(".x", ".y"), ..., keep = FALSE) {
  oldclass <- class(x)
  .df <- tidytable::left_join(x = x, y = y, by = by, suffix = suffix, ..., keep = keep)
  class(.df) <- oldclass
  .df
}

# Flatten lists
#' @noRd
list_flatten <- function(x, recursive = FALSE) {
  is_list <- tidytable::map_lgl(x, is.list)# obj_is_list)
  any_list <- any(is_list)
  if (any_list) {
    is_not_list <- !is_list
    x[is_not_list] <- lapply(x[is_not_list], list)
    out <- list_unchop(x, ptype = list())
  } else {
    out <- x
  }
  
  if (recursive && any_list) {
    out <- list_flatten(out, recursive)
  }
  
  out
}
#' @noRd
tt_rename_with <- function(.df, .fn = NULL, .cols = everything(), ...) {
  oldclass <- class(.df)
  .df <- tidytable::rename_with(.df = .df, .fn = .fn, 
                                .cols = tidyselect::all_of(.cols), ...)
  class(.df) <- oldclass
  .df
}
