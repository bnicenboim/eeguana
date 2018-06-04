#' Remove (transform to NA) problematic events from an eegble.
#'
#' @param x An \code{eegble} object.
#' @param ... Description of the problematic event.
#' 
#' @return An eegbl. 
#' 
#' @importFrom magrittr %>%
#' 
#' @export

event_to_NA <- function(x, ..., ind_channel = TRUE, entire_seg = FALSE) {
  dots <- rlang::enquos(...)
	baddies <- purrr::map(x$data, ~ dplyr::filter(.x$events, !!!dots) %>% 
       dplyr::mutate( sample = purrr::map2(sample, sample + size - 1,seq)) %>% 
       unnest %>%
       dplyr::select(channel, sample))
  x$data <- purrr::map2(x$data, baddies, function(data_file, bad_file) {

              list(signals = purrr::modify(data_file$signals, 
                function(seg){
                  if(ind_channel){
                    for(c in chan_names(x)){
                        baddies_chan <- bad_file %>% filter(channel==c)
                        seg <- dplyr::mutate_at(seg, c,
                       funs(dplyr::if_else(sample %in% baddies_chan$sample, NA_real_,. )) )
                        if(!entire_seg){ warning("Not implemented")}
                    }
                  } else {
                      baddies_all <- bad_file %>% dplyr::select(sample) %>% dplyr::distinct()
                    if(!entire_seg){ 
                      seg <- dplyr::mutate_at(seg, chan_names(x),
                       funs(dplyr::if_else(sample %in% baddies_all$sample, NA_real_,. )) )
                    } else {
                      if(length(intersect(seg$sample, baddies_all$sample)) != 0){
                        seg <- NULL  
                      }
                    }  
                  }
                  seg
                }), events = data_file$events  )}
              )
  x
}


#' Segments an eegble.
#'
#' @param x An \code{eegble} object.
#' @param ... Description of the problematic event.
#' 
#' @return An eegbl. 
#' 
#' @importFrom magrittr %>%
#' 
#' @export

segment <- function(x, ..., lim = c(-1,1)){

  dots <- rlang::enquos(...)
  #dots <- rlang::quos(description == "s70")
  # creates a list of vector with the times zero for each "file"
  times0 <- purrr::map(x$data, ~.x$events %>% 
                dplyr::filter(!!!dots) %>% 
                dplyr::select(segment = sample) %>% 
                unlist)
   
  # for each vector of times zero (t0) associated with the data of each file   
  x$data <- purrr::map2(.x= times0, .y= x$data,
  # iterate over the vector of times zeros (in sample)
  function(t0 , l) purrr::map(.x=t0, ~ 
     map_dfr(l$signals, function(df) df %>%
    # filter the relevant samples
    dplyr::filter(sample >= .x + lim[1] * srate(x), 
           sample <= .x + lim[2] * srate(x)) %>%
    # add a time column
    dplyr::mutate(time = round(sample/srate(x) -.x/srate(x), 
      decimals(1/srate(x)))) %>% 
    # order the signals df:
    dplyr::select(sample, time, everything())
    # reconstruct
    )) %>% {list(signals = .,  events = l$events)} )

x
}


