#' Remove (transform to NA) problematic events from an eegble.
#'
#' @param x An \code{eegble} object.
#' @param ... Description of the problematic event.
#' @param all_chans If set to TRUE, 
#'     it will remove samples from all channels (Default:  all_chans = FALSE).
#' @param entire_seg If set to TRUE, it will remove the entire segment 
#'    (Default: entire_seg = FALSE).
#' @param drop_events 
#'
#' @examples
#'
#' @return An eegbl. 
#' 
#' @importFrom fastmatch %fin%
#' @importFrom magrittr %>%
#' 
#' @export
event_to_NA <- function(x, ..., all_chans = FALSE, entire_seg = FALSE, 
 drop_events = TRUE) {
  dots <- rlang::enquos(...)
  

  # dots <- rlang::quos(type == "Bad Interval")

  # Hack for match 2 columns with 2 columns, similar to semi_join but allowing for assignment
  baddies <- dplyr::filter(x$events, !!!dots) %>%  
      dplyr::mutate( sample = purrr::map2(sample, sample + size - 1, seq)) %>% 
       tidyr::unnest() %>% dplyr::mutate(.id, .bid = paste(.id, sample), channel) %>%
       dplyr::select(-size)

  if(all_chans) baddies <- dplyr::mutate(baddies, channel = NA) 
 
  # For the replacement in parts of the segments
  b_chans <- dplyr::filter(baddies, !is.na(channel))  %>% .$channel %>% unique()
  for(c in b_chans){
    b <- dplyr::filter(baddies, channel==c & !is.na(channel)) 
    if(!entire_seg){
      x$data[[as.character(c)]][paste(x$data$.id, x$data$sample) %in% b$.bid] <- NA
      #could try with na_if, maybe it's faster?
    } else {
      x$data[[as.character(c)]][x$data$.id %in% b$.id] <- NA
    }
  }
  # For the replacement in the complete of the segments
  b_all <- dplyr::filter(baddies, is.na(channel)) %>% distinct()

  if(!entire_seg){
      x$data[, chan_names(x)][paste(x$data$.id, x$data$sample) %in% b_all$.bid, ] <- NA
    } else {
      x$data[, chan_names(x)][x$data$.id %in% b_all$.id, ] <- NA
    }
 
  if(drop_events) {
    x$events <- suppressMessages(dplyr::anti_join(x$events, dplyr::filter(x$events, !!!dots)))
  }
  x
}



#' Segments an eegble.
#'
#' @param x An \code{eegble} object.
#' @param ... Description of the event.
#' @param lim Vector indicating the time before and after the event.
#' 
#' @return An eegbl. 
#' 
#' @importFrom magrittr %>%
#' 
#' @export
segment <- function(x, ..., lim = c(-.5,.5)){
  
  dots <- rlang::enquos(...)
  #dots <- rlang::quos(description == "s111")
  #dots <- rlang::quos(description == "s70")
  #dots <- rlang::quos(description %in% c("s70",s71"))
  
 times0 <- dplyr::filter(x$events, !!!dots) %>% 
                  dplyr::select(-channel, -size) 

  slim <- lim * srate(x) + 1 
  x$data <- purrr::pmap_dfr(list(times0$.id,  times0$sample), .id = ".id",
                                    function(i, s0) x$data %>%
                                          # filter the relevant samples
                                          dplyr::filter(sample >= s0 + slim[1], 
                                                         sample < s0 + slim[2],
                                                        .id == i) %>%
                                          # add a time column
                                          dplyr::mutate(sample = sample - s0) %>%
                                          # order the signals df:
                                          dplyr::select(-.id)) %>%
                                          dplyr::mutate(.id = as.integer(.id))

 x$events <- purrr::pmap_dfr(list(times0$.id, times0$sample), .id = ".id",
                    function(i, s0){ 
                      b <-   as.integer(s0 + slim[1])

                      x$events %>%
                      # filter the relevant events
                      # started after the segment (b) 
                      # or span after the segment (b)  
                      dplyr::filter(sample >=  s0 + slim[1] - size + 1 ,
                                    sample <  s0 + slim[2],
                                    .id == i) %>%
                      dplyr::mutate(size = dplyr::if_else(sample < b, 
                                                 as.integer(size - (b - sample) + 1), size), 
                                  sample = dplyr::if_else(sample < b, b - s0,
                                                             sample - s0)) %>%
                                                dplyr::select(-.id)
                                              }) %>%
                     dplyr::mutate(.id = as.integer(.id)) 




  message(paste0("# Total of ", max(x$data$.id)," segments found."))
 

 
  x$seg_info <- dplyr::right_join(dplyr::select(x$seg_info,-type), dplyr::select(times0,-sample), by =".id") %>%
                dplyr::mutate(.id = 1:n()) %>% 
                dplyr::group_by(recording) %>% 
                dplyr::mutate(segment = 1:n())


  message(paste0(say_size(x)," after segmentation."))
  # validate_eegbl(x)
  x
}


#' Baseline an eegble.
#'
#' @param x An \code{eegble} object.
#' @param t A negative number indicating from when to baseline 
#'    (TO COMPLETE). The default is to use all the negative times.
#' 
#' @examples
#' @return An eegbl. 
#' 
#' @importFrom magrittr %>%
#' 
#' @export

baseline <- function(x, t = -Inf) {
  s <- t * srate(x)
  x$data <- dplyr::group_by(x$data, .id) %>% 
            dplyr::mutate_at(chan_names(x), 
                    dplyr::funs( . - mean(.[dplyr::between(sample, s, 0  )])))

  x

}            
