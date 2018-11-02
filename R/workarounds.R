#' bind_rows looses the attributes, TODO: look for solutions
#' https://github.com/tidyverse/dplyr/issues/2457
#' This is a workaround to create signal_tbl tables
#' @noRd
pmap_sgr <- function(.l, .f, ..., .id = NULL) {
  .f <- purrr::as_mapper(.f, ...)
  res <- purrr::pmap(.l, .f, ...)
  as_signal_tbl(data.table::rbindlist(res, idcol=.id)) 
}

#' @noRd
map2_sgr <- function(.x, .y, .f, ..., .id = NULL) {
  # if (!is_installed("dplyr")) {
  #     abort("`map2_dfr()` requires dplyr")
  # }
  .f <- purrr::as_mapper(.f, ...)
  res <- purrr::map2(.x, .y, .f, ...) 
  as_signal_tbl(data.table::rbindlist(res, idcol=.id))
}

#' @export
between <- data.table::between
