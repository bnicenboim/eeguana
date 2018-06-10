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
#' @param add_seg_info 
#' @param thinning 
#' @param ... Other arguments passed on to individual methods.
#' 
#' `as_data_frame` and `as.tibble` are aliases.
#' @return A tibble.
#' 
#' @importFrom magrittr %>%
#' 
#' @export 
 as_tibble.eegbl <- function(x, ..., thinning = NULL, add_seg_info = TRUE) {
  
  if(is.null(thinning)){ 
    by <- 1
  } else if(thinning == "auto") {
    by <- length(chan_names(x)) * 2
  } else {
    by <- thinning
  }

  x$data <- x$data %>% tidyr::gather(key = channel, value = amplitude, chan_names(x)) %>%
  {if(add_seg_info) { dplyr::left_join(., x$seg_info, by =".id")} else {.}} %>% 
    dplyr::group_by(.id, channel) %>% 
        dplyr::filter(sample %in%  sample[seq(1,length(sample), by = by)]) %>%
      dplyr::mutate(time = (sample - 1)/ srate(x)) %>% dplyr::select(-sample, time, dplyr::everything())
 x$data
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
#' @param add_seg_info 
#' @param thinning 
#' @param ... Other arguments passed on to individual methods.
#' 
#' `as_data_frame` and `as.tibble` are aliases.
#' @return A tibble.
#' 
#' @importFrom magrittr %>%
#' 
#' @export
as.data.frame.eegbl <- function(x, ..., thinning = "auto", add_seg_info = TRUE) {
as.data.frame(as_tibble.eegbl(x, ..., thinning = "auto", add_seg_info = TRUE))
}