#' Bind eegble objects.
#'
#' This function binds eegbles and throws a warning if there is a mismatch in the metadata.
#' @param ... Eegble objects to combine.
#' 
#' @return An \code{eegble} object.
#' 
#' @importFrom magrittr %>%
#' 
#' @export
bind <- function(...){
  eegbles <- list(...)
  # hack to allow that ... is already a list
  if(class(eegbles[[1]]) != "eegbl") {
    eegbles <- list(...)[[1]]
  }
  new_eegble <- eegbles[[1]]
  
  purrr::walk(eegbles[seq(2,length(eegbles))], 
        ~ if(!identical(eegbles[[1]]$gral_info, .x$gral_info)) 
        warning("'gral_info' fields are not identical."))

  purrr::walk(eegbles[seq(2,length(eegbles))], 
        ~ if(!identical(eegbles[[1]]$chan_info, .x$chan_info)) 
        warning("'chan_info' fields are not identical."))
  
  new_eegble$data <- purrr::map(eegbles, "data") %>% 
                  unlist(recursive = FALSE) %>% c()

  validate_eegbl(new_eegble)
 }


as_tibble <-function (x, ...) {
    UseMethod("as_tibble")
}


#' Convert an eegble to a tibble.
#'
#' @param x An \code{eegble} object.
#' @param chans Channel names to be used; all of them by default.
#' @param .id Name of the column that identifies the data.
#' @param ... Other arguments passed on to individual methods.
#' 
#' `as_data_frame` and `as.tibble` are aliases.
#' @return A tibble.
#' 
#' @importFrom magrittr %>%
#' 
#' @export 
 as_tibble.eegbl <- function(x, ..., chans = NULL, .id = "id") {
  if(is.null(chans)) {chans = chan_names(x)}
  chan_rv <- setdiff(chan_names(x),chans)
  
  purrr::map(x$data, ~ 
    purrr::map_dfr(.x$signals , .id = "segment", ~ .x) %>% 
    dplyr::select(-dplyr::one_of(chan_rv)) %>%
    tidyr::gather(key = channel, value = amplitude, dplyr::one_of(chans)) ) %>%

  purrr::map_dfr(.id = .id, ~ .x)
}


#' @rdname as_tibble.eegbl
#' @export
as_data_frame.eegbl <- as_tibble.eegbl

#' @rdname as_tibble.eegbl
#' @export
as.tibble.eegbl <- as_tibble.eegbl


#' Convert an eegble to a (base) data frame.
#'
#' @param x An \code{eegble} object.
#' @param chans Channel names to be used.
#' @param .id Name of the column that identifies the data.
#' @param ... Other arguments passed on to individual methods.
#' 
#' `as_data_frame` and `as.tibble` are aliases.
#' @return A tibble.
#' 
#' @importFrom magrittr %>%
#' 
#' @export
as.data.frame.eegbl <- function(x, ..., chans, .id = "id") {
as_tibble.eegbl(x, ..., chans = chans, .id = .id)
}