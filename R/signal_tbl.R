# #' @export
# # TODO: not working key and values cannot be changed
# gather.signal_tbl <- function(data, key = "key", value = "value", ..., na.rm = FALSE, 
#     convert = FALSE, factor_key = FALSE) {
#   data <- declass(data)$tbl
#   NextMethod()  
# }

#' @export
filter_.signal_tbl <- function(.data, ..., .dots = list()) {
  declassed <- declass(.data)
 .data <- declassed$tbl
  reclass(NextMethod(), declassed$attr)  
}

#' @export
mutate_.signal_tbl <- function(.data, ..., .dots = list()) {
  declassed <- declass(.data)
 .data <- declassed$tbl
  reclass(NextMethod(), declassed$attr)  
}

#' @export
mutate.signal_tbl <- function(.data, ...) {
  declassed <- declass(.data)
 .data <- declassed$tbl
  reclass(NextMethod(), declassed$attr)  
}

#' @export
transmute_.signal_tbl <- function(.data, ..., .dots = list()) {
  declassed <- declass(.data)
 .data <- declassed$tbl
  reclass(NextMethod(), declassed$attr)  
}

#' @export
summarise_.signal_tbl <- function(.data, ..., .dots = list()) {
 declassed <- declass(.data)
 .data <- declassed$tbl
  reclass(NextMethod(), declassed$attr)  
}

#' @export
summarise.signal_tbl <- function(.data, ...) {
 declassed <- declass(.data)
 .data <- declassed$tbl
 reclass(NextMethod(), declassed$attr)  
}

#' @export
group_by_.signal_tbl <- function(.data, ..., .dots = list(), add = add) {
  declassed <- declass(.data)
 .data <- declassed$tbl
  reclass(NextMethod(), declassed$attr)  
}

#' @export
group_by.signal_tbl <- function(.data, ..., add = add) {
   declassed <- declass(.data)
  .data <- declassed$tbl
  result <-  reclass(NextMethod(), declassed$attr)  
   reclass(NextMethod(), declassed$attr)  
}

#' @export
ungroup.signal_tbl <- function(.data, ..., add = add) {
  declassed <- declass(.data)
 .data <- declassed$tbl
  reclass(NextMethod(), declassed$attr)  
}

#' @export
select.signal_tbl <- function(.data, ...) {
  declassed <- declass(.data)
 .data <- declassed$tbl
  reclass(NextMethod(), declassed$attr)  
}

#' @export
rename.signal_tbl <- function(.data, ...) {
  declassed <- declass(.data)
 .data <- declassed$tbl
  reclass(NextMethod(), declassed$attr)  
}

#' @export
left_join.signal_tbl <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  declassed <- declass(x)
  x <- declassed$tbl
  reclass(NextMethod(), declassed$attr)  
}

#' @export
semi_join.signal_tbl <- function(x, y, by = NULL, suffix = c(".x", ".y"), ...) {
  declassed <- declass(x)
  x <- declassed$tbl
  reclass(NextMethod(), declassed$attr)  
}

#' @export
anti_join.signal_tbl <- function(x, y, by = NULL, suffix = c(".x", ".y"), ...) {
  declassed <- declass(x)
  x <- declassed$tbl
  reclass(NextMethod(), declassed$attr)  
}
