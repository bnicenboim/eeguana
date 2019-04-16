#' @noRd
map_dtr <- function(.x,.f,..., .id = NULL){
    .f <- purrr::as_mapper(.f, ...)
    res <- purrr::map(.x, .f, ...)
    data.table::rbindlist(res, fill = TRUE, idcol = .id)
}

#' @noRd
map2_dtr <- function(.x,.f,..., .id = NULL){
    .f <- purrr::as_mapper(.f, ...)
    res <- purrr::map2(.x, .y, .f, ...)
    data.table::rbindlist(res, fill = TRUE, idcol = .id)
}


#' @noRd
imap_dtr <- function(.x,.f,..., .id = NULL){
    .f <- purrr::as_mapper(.f, ...)
    map2_dtr(.x, names(.x), .f,  ...)
}


 
#' @noRd
map2_dtr <- function(.x,.y, .f,..., .id = NULL){
    .f <- purrr::as_mapper(.f, ...)
    res <- purrr::map2(.x, .y, .f, ...)
    data.table::rbindlist(res, fill = TRUE, idcol = .id)
}

#' @noRd
                                        # https://github.com/mllg/batchtools/blob/master/R/Joins.R
semi_join_dt <- function(x, y, by = NULL) {
    w <- unique(x[y, on = by, nomatch = 0L, which = TRUE, allow.cartesian = TRUE])
    x[w]
}

#' @noRd
left_join_dt <- function(x, y, by = NULL) {
    y[x, on = by]
}

#' binds cols of dt and adds the class of the first object
#' @noRd
bind_cols_dt<- function(...){
    new_dt <- cbind(...)
    class(new_dt) <- class(list(...)[[1]])
new_dt
}
