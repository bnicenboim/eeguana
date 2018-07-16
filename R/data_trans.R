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
  # hack to allow that "..." would already be a list
  if(class(eegbles[[1]]) != "eegbl") {
    eegbles <- list(...)[[1]]
  }
  
  # Checks:           
  purrr::iwalk(eegbles[seq(2,length(eegbles))], 
        ~ if(!identical(eegbles[[1]]$info, .x$info)) 
        warning("Non identical 'info' field in element ", as.character(as.numeric(.y)+1), "."))

  purrr::iwalk(eegbles[seq(2,length(eegbles))], 
        ~ if(!identical(eegbles[[1]]$channels, .x$channels)) 
        warning("Non identical 'channels' in element ",as.character(as.numeric(.y)+1), "."))
  

  # Binding
  #.id of the new eggbles needs to be adapted
  add_ids <- purrr::map_int(eegbles, ~ max(.x$signal$.id)) %>%
            cumsum() %>% dplyr::lag(default=0) %>% as.integer()
  
  
  signal <- purrr::map2_dfr(eegbles, add_ids, ~
                                    dplyr::ungroup(.x$signal) %>% 
                                    dplyr::mutate(.id = .id + .y))
  events <- purrr::map2_dfr(eegbles, add_ids, ~ 
                                      dplyr::ungroup(.x$events) %>%
                                      dplyr::mutate(.id = .id + .y))
  segments <- purrr::map2_dfr(eegbles, add_ids, ~ 
                                    dplyr::ungroup(.x$segments) %>% 
                                    dplyr::mutate(.id = .id + .y))
  
  # If more segments of the same recording are added, these need to be adapted.
  segments <- segments %>% 
                         dplyr::group_by(recording) %>% 
                         dplyr::mutate(segment = 1:n())
  
  new_eegble <- new_eegbl(signal = signal, events = events, segments = segments, 
                    channels = eegbles[[1]]$channels, info = eegbles[[1]]$info)
  validate_eegbl(new_eegble)
 }




#' Convert an eegble to a tibble.
#'
#' @param x An \code{eegble} object.
#' @param add_segments 
#' @param thinning 
#' @param ... Other arguments passed on to individual methods.
#' 
#' `as_data_frame` and `as.tibble` are aliases.
#' @return A tibble.
#' 
#' @importFrom magrittr %>%
#' 
#' @export 
as_tibble.eegbl <- function(x, ..., thinning = NULL, add_segments = TRUE) {
  
  if(is.null(thinning)){ 
    by <- 1
  } else if(thinning == "auto") {
    by <- round(pmax(length(channel_names(x)) * 2 * max(duration(x)/srate(x)) , 1))
    message(paste("# Thinning by", by))
  } else {
    by <- thinning
  }

  df <- x$signal %>% tidyr::gather(key = channel, value = amplitude, channel_names(x)) %>%
  {if(add_segments) { dplyr::left_join(., x$segments, by =".id")} else {.}} %>% 
    dplyr::group_by(.id, channel) %>% 
        dplyr::filter(sample %in%  sample[seq(1,length(sample), by = by)]) %>%
      dplyr::mutate(time = (sample - 1)/ srate(x)) %>% dplyr::select(-sample, time, dplyr::everything())
 df
}



#' Convert an eegble into a summary long-data frame based on a statistics.
#'
#' @param x An \code{eegble} object.
#' @param .funs A statistics to be used on every segment
#' @param ... Other arguments passed on to \code{.funs}. See \link{dplyr-package} help.
#' 
#' @return A long tibble.
#' 
#' 
#' @export 
summarize_id_as_tibble <- function(x, ...) {
UseMethod("summarize_id_as_tibble")
}


#' @export 
summarize_id_as_tibble.eegbl <- function(x, .funs = mean, ...) {
  funs_name <- rlang::enquo(.funs)
  x$signal %>% dplyr::group_by(.id) %>% #keep grouping for later 
              dplyr::summarize_at(channel_names(x), .funs, ...)  %>%
              # make it long format:
              tidyr::gather(key = channel, value = !!funs_name, channel_names(x)) %>%
              # adds segment info
              dplyr::left_join(., x$segments, by =".id") %>%
            left_join(x$channels %>% mutate(labels = as.character(labels)), by = c("channel" = "labels"))
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
#' @param add_segments 
#' @param thinning 
#' @param ... Other arguments passed on to individual methods.
#' 
#' `as_data_frame` and `as.tibble` are aliases.
#' @return A tibble.
#' 
#' @importFrom magrittr %>%
#' 
#' @export
as.data.frame.eegbl <- function(...) {
as.data.frame(as_tibble.eegbl(...))
}