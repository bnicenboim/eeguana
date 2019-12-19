#' @noRd
map_dtr <- function(.x, .f, ..., .id = NULL) {
  .f <- purrr::as_mapper(.f, ...)
  res <- purrr::map(.x, .f, ...)
  data.table::rbindlist(res, fill = TRUE, idcol = .id)
}

#' @noRd
map2_dtr <- function(.x, .f, ..., .id = NULL) {
  .f <- purrr::as_mapper(.f, ...)
  res <- purrr::map2(.x, .y, .f, ...)
  data.table::rbindlist(res, fill = TRUE, idcol = .id)
}


#' @noRd
imap_dtr <- function(.x, .f, ..., .id = NULL) {
  .f <- purrr::as_mapper(.f, ...)
  map2_dtr(.x, names(.x), .f, ..., .id = .id)
}



#' @noRd
map2_dtr <- function(.x, .y, .f, ..., .id = NULL) {
  .f <- purrr::as_mapper(.f, ...)
  res <- purrr::map2(.x, .y, .f, ...)
  data.table::rbindlist(res, fill = TRUE, idcol = .id)
}

#' @noRd
## https://github.com/mllg/batchtools/blob/master/R/Joins.R
semi_join_dt <- function(x, y, by = NULL) {
  if (is.null(by)) {
    by <- intersect(colnames(x), colnames(y))
  }
  w <- unique(x[y, on = by, nomatch = 0L, which = TRUE, allow.cartesian = TRUE])
  x[w]
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
  } else {
    names(by) <- by
  }
  out <- y[x, on = by]
  data.table::setnames(out, names(by), by)[]
}

anti_join_dt <- function(x, y, by = NULL) {
  if (is.null(by)) {
    by <- intersect(colnames(x), colnames(y))
  }

  x[!y, on = by]
}

#' @noRd
filter_dt <- function(.data, ..., group_by_ = character(0)) {
  dots <- rlang::quos(...)
  # if(rlang::is_quosures(dots))

  # TODO check parse_quo(), as_label reduces the quo if it's too long
  cnds <- lapply(dots, rlang::quo_text) %>% paste0(collapse = " & ")
  env <- lapply(dots, rlang::quo_get_env) %>% unique()
  if (length(env) != 1) stop("Need to fix filter_dt; env", env)
  ## TODO: check why this happens: for some reason if I don't do that, I modify the index of .data
  ## .data <- data.table::copy(.data)
  ## .data[eval(parse(text = cnds), envir =envs[[1]]),]
  ## TODO eval_tidy
  .data[.data[, .I[eval(parse(text = cnds), envir = env)], by = c(group_by_)]$V1]
}
#' binds cols of dt and adds the class of the first object
#' @noRd
bind_cols_dt <- function(...) {
  new_dt <- cbind(...)
  class(new_dt) <- class(list(...)[[1]])
  new_dt
}


#' @noRd
unnest_dt <- function(.data, col) {
    #https://www.johannesbgruber.eu/post/a-faster-unnest/#fn1
    col <- rlang::ensyms(col)
    clnms <- rlang::syms(setdiff(colnames(.data), as.character(col)))
    tbl <- eval(
        rlang::expr(.data[, as.character(unlist(!!!col)), by = list(!!!clnms)])
    )
    colnames(.data) <- c(as.character(clnms), as.character(col))
    .data
}

