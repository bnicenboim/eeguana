#' @noRd
shallow <- function(x){
  x[TRUE]
}

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
    ## message_verbose('Joining, by = "',by,'"')
  } else {
    names(by) <- by
  }
  out <- y[x, on = by]

  #should I set allow.cartesian = TRUE?
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
  newdots <- Reduce(x = dots,  f = function(x,y) rlang::quo(!!x & !!y))
 if(length(group_by_) == 0) {
   #TODO: this might be dangerous
   .data[rlang::eval_tidy(newdots, data = rlang::as_data_mask(.data))]
  } else {
    col_order <- names(.data)
  .data <- .data[, .SD[rlang::eval_tidy(newdots, data = cbind(.SD,data.table::as.data.table(.BY)))], by = c(group_by_)]
    data.table::setcolorder(.data, col_order)
    .data
  }
}


#' @noRd
mutate_dt <- function(.data, ..., group_by_ = character(0), .by_reference = FALSE, omit_shallow = FALSE){

  dots <- rlang::quos(...)
  dots <- rlang::quos_auto_name(dots)
  col_names <- names(dots)
  if(!omit_shallow & !.by_reference){ #it might be done before, or it might be by reference
    .data <- data.table:::shallow(.data)
  }
  if(length(group_by_) == 0) {
      # From: https://github.com/markfairbanks/tidytable/blob/549f330837be5adb510b4599142cc5f4a615a4be/R/mutate.R
      # Prevent modify-by-reference if the column already exists in the data.table
      # Fixes cases when user supplies a single value ex. 1, -1, "a"
      .data[, `:=`((col_names),
                                   lapply(dots, recycle_eval,
                                                     data = rlang::as_data_mask(
                                                       .SD),size = .N))]
    } else {

      if (length(intersect(col_names, colnames(.data)))>0 & .by_reference==FALSE) {
        #needs a real copy
        .data <- data.table::copy(.data)
        }
        .data[, `:=`((col_names),
                                  lapply(dots, rlang::eval_tidy,
                                                 data= rlang::as_data_mask(
                                                   cbind(.SD,data.table::as.data.table(.BY))
                                                  ))),
                               by = c(group_by_)]
    }

}

#' @noRd
summarize_dt <- function(.data, ..., group_by_ = character(0)){
  dots <- rlang::quos(...)
  if(length(group_by_) > 0) {
    .data <- .data[,
                                             lapply(dots, rlang::eval_tidy,
                                               data =
                                                 rlang::as_data_mask(cbind(.SD,data.table::as.data.table(.BY)))),
                                      keyby = c(group_by_)]
  } else {
    .data <- .data[, lapply(dots, rlang::eval_tidy,
                                               data= rlang::as_data_mask(.SD))]
  }
  .data[]
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

#' Converts a struct from matlab into a data table
#' @noRd
struct_to_dt <- function(struct, .id = NULL) {
  if(length(struct)==0) {
    data.table::data.table()
  } else {
    list_str <-  apply(struct,3,
                       function(x) lapply(x[,1],
                                          function(x){
                         x <- x %||% NA
                         #unmatrix
#                         if(all((dim(x) %||% 1) ==c(1,1)))
                           c(unlist(x)) %||% rep(NA, length(x))

                       }  ))
    map_dtr(list_str, data.table::setDT, .id =.id)

  }
}



#' @noRd
recycle <- function(x, size) {

  if(is.null(x)) return(NULL)

  x_length <- length(x)

  if (x_length != 1 && x_length != size)
    stop(paste0("x must have length 1 or length ", size))

  if (x_length == 1) x <- rep(x, size)

  x
}


#' @noRd
recycle_eval <- function(expr, data = NULL, env= rlang::caller_env(), size){
  recycle(rlang::eval_tidy(expr, data = data, env = env),size = size)
}
#' @noRd
changed_objects <- function(obj){
  ## name <- rlang::eval_tidy(rlang::as_name(rlang::enquo(obj)))
  oo <- ls(envir=.GlobalEnv)
  mem <- data.table::data.table(mem = lapply(oo, function(x)  do.call(data.table::address,list(rlang::sym(x))) ) %>% unlist(), names = oo)

  loc <- data.table::address(force(obj))
  changed <- mem[mem ==loc,]$names
  if(length(changed)>1){
    message_verbose("The following objects have been changed in place: ", paste0(changed,sep =", "))
  } else {
    message_verbose(changed, " has been changed in place.")
  }
}

#' @noRd
distinct. <- function(.df, ..., .keep_all = FALSE) {
  oldclass <- class(.df)
  .df <- tidytable::distinct.(.df = .df, ..., .keep_all = .keep_all) 
  class(.df) <- oldclass
  .df
}

#' @noRd
rename. <- function(.df, ...) {
  oldclass <- class(.df)
  .df <- tidytable::rename.(.df = .df, ...) 
  class(.df) <- oldclass
  .df
}

#' @noRd
select. <- function(.df, ...) {
  oldclass <- class(.df)
    .df <- tidytable::select.(.df = .df, ...)  
  class(.df) <- oldclass
  .df
}


#' @noRd
mutate. <- function(.df, ..., 
                    .by = NULL, 
                    .keep = c("all", "used", "unused","none") ) {
  oldclass <- class(.df)
  if(length(.by)>0) {
    .df <- tidytable::mutate.(.df = .df, ...,
                              .by = .by, 
                              .keep = .keep)  
  } else {
    .df <- tidytable::mutate.(.df = .df, ...,
                              .keep = .keep)  
  }
  
  class(.df) <- oldclass
  .df
}

#' @noRd
filter. <- function(.df, ..., 
                    .by = NULL) {
  oldclass <- class(.df)
  .df <- tidytable::filter.(.df = .df, ...,
                            .by = .by)
  class(.df) <- oldclass
  .df
}

#' @noRd
summarize. <- function(.df, ..., .by= NULL, .sort = FALSE) {
  oldclass <- class(.df)
  .df <- tidytable::summarize.(.df = .df, ...,.by = .by, .sort = .sort)
  class(.df) <- oldclass
  .df
}
