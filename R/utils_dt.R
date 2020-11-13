#' @noRd
lapply_dtc <- function(X, FUN, ...){
  lapply(X, FUN, ...) %>%
    data.table::setDT()
}

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
  newdots <- Reduce(x =dots,  f= function(x,y) rlang::quo(!!x & !!y))
 if(length(group_by_)==0) {
   #TODO: optimize this to remove the by
  .data[.data[, .I[
    rlang::eval_tidy(newdots, data =
                                rlang::as_data_mask(.SD))],by = c(group_by_)
    ]$V1]
  } else {
  .data[.data[, .I[
    rlang::eval_tidy(newdots, data =
                                rlang::as_data_mask(cbind(.SD,data.table::as.data.table(.BY))))],
              by = c(group_by_)]$V1]
  }
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


#' @noRd
recycle <- function(x, size) {
  x_length <- length(x)

  if (x_length != 1 && x_length != size)
    stop(paste0("x must have length 1 or length ", size))

  if (x_length == 1) x <- rep(x, size)

  x
}


