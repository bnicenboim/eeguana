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
  new_eegble <- eegbles[[1]]
  
  purrr::walk(eegbles[seq(2,length(eegbles))], 
        ~ if(!identical(eegbles[[1]]$gral_info, .x$gral_info)) 
        warning("'gral_info' fields are not identical."))

  purrr::walk(eegbles[seq(2,length(eegbles))], 
        ~ if(!identical(eegbles[[1]]$chan_info, .x$chan_info)) 
        warning("'chan_info' fields are not identical."))
  
  new_eegble$data <- purrr::map(eegbles, "data") %>% 
                  unlist(recursive = FALSE) %>% c()

  new_eegble
 }


#' Convert an eegble to a tibble.
#'
#' @param x An \code{eegble} object.
#' @param chans Channell names to be used.
#' @param .id Name of the column that identifies the data.
#' @param ... Other arguments passed on to individual methods.
#' 
#' @return A tibble.
#' 
#' @importFrom magrittr %>%
#' 
#' @export

 as_tibble.eegbl <- function(x, ..., chans, .id = "id") {
  chan_rv <- setdiff(chan_names(x),chans)
  
  purrr::map(x$data, ~ 
    purrr::map_dfr(.x$signals , .id = "segment", ~ .x) %>% 
    dplyr::select(-dplyr::one_of(chan_rv)) %>%
    tidyr::gather(key = channel, value = amplitude, dplyr::one_of(chans)) ) %>%

  purrr::map_dfr(.id = .id, ~ .x)
}
