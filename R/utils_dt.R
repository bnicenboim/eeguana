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
    map2_dtr(.x, names(.x), .f,  ..., .id=.id)
}


 
#' @noRd
map2_dtr <- function(.x,.y, .f,..., .id = NULL){
    .f <- purrr::as_mapper(.f, ...)
    res <- purrr::map2(.x, .y, .f, ...)
    data.table::rbindlist(res, fill = TRUE, idcol = .id)
}

#' @noRd
## https://github.com/mllg/batchtools/blob/master/R/Joins.R
semi_join_dt <- function(x, y, by = NULL) {
    if(is.null(by)){
        by <- intersect(colnames(x), colnames(y))
    }
    w <- unique(x[y, on = by, nomatch = 0L, which = TRUE, allow.cartesian = TRUE])
    x[w]
}

#' @noRd
left_join_dt <- function(x, y, by = NULL) {
    if(is.null(by)){
        by <- intersect(colnames(x), colnames(y))
    }
   ##need to be reversed:
    if(!is.null(names(by))) {
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

anti_join_dt <- function(x,y,by = NULL){
    if(is.null(by)){
        by <- intersect(colnames(x), colnames(y))
    }

    x[!y, on = by]
}


#' binds cols of dt and adds the class of the first object
#' @noRd
bind_cols_dt<- function(...){
    new_dt <- cbind(...)
    class(new_dt) <- class(list(...)[[1]])
    new_dt
}

#' @noRd
filter_dt <- function(.data, ... ){
    dots <- rlang::enquos(...)
    cnds <- lapply(dots, rlang::quo_name) %>% paste0(collapse = " & ")
    envs <- lapply(dots, rlang::quo_get_env) %>% unique()
    if(length(envs)!=1) stop("Need to fix filter_dt")
    .data[eval(parse(text = cnds), envir =envs[[1]]),]
}
 
