
#' bind_rows looses the attributes
#' https://github.com/tidyverse/dplyr/issues/2457
#' This is a workaround to create signal tables
#' @noRd
pmap_sgr <- 
function (.l, .f, ..., .id = NULL) {
    .f <- purrr::as_mapper(.f, ...)
    res <- purrr::pmap(.l, .f, ...) %>% 
    purrr::map(declass)
    res_tbl <- purrr::map(res, ~ .x$tbl)
    res_attr <- purrr::flatten(purrr::map(res, ~ .x$attr))
    dplyr::bind_rows(res_tbl, .id = .id) %>% 
    reclass(res_attr)
}

#' @noRd
map2_sgr <- function (.x, .y, .f, ..., .id = NULL) {
    # if (!is_installed("dplyr")) {
    #     abort("`map2_dfr()` requires dplyr")
    # }
    .f <- purrr::as_mapper(.f, ...)
    res <- purrr::map2(.x, .y, .f, ...) %>%
        purrr::map(declass)
    res_tbl <- purrr::map(res, ~ .x$tbl)
    res_attr <- purrr::flatten(purrr::map(res, ~ .x$attr))
    dplyr::bind_rows(res_tbl, .id = .id) %>% 
    reclass(res_attr)
}