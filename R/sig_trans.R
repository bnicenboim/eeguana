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
  baddies <- dplyr::filter(x$events, !!!dots) 

  if(all_chans) baddies <- dplyr::mutate(baddies, channel = NA) 
 
  # For the replacement in parts of the segments
  b_chans <- dplyr::filter(baddies, !is.na(channel))  %>% .$channel %>% unique()
 for(c in b_chans){
    b <- dplyr::filter(baddies, channel==c & !is.na(channel)) 
    if(!entire_seg){
      for(i in seq(1,nrow(b))){
         x$data[[as.character(c)]][x$data$.id %in% b$.id[i] & 
                                  dplyr::between(x$data$sample, b$sample[i], 
                                    b$sample[i] + b$size[i] - 1)  ] <- NA
       }
      #could try with na_if, maybe it's faster?
    } else {
      x$data[[as.character(c)]][x$data$.id %in% b$.id] <- NA
    }
  }
  # For the replacement in the complete of the segments
  b_all <- dplyr::filter(baddies, is.na(channel)) %>% dplyr::distinct()

  if(!entire_seg & nrow(b_all) != 0){
      for(i in seq(1,nrow(b_all))){
       x$data[, chan_names(x)][x$data$.id == b_all$.id[i] & 
                    dplyr::between(x$data$sample, b_all$sample[i], 
                      b_all$sample[i] + b_all$size[i] -1)  , ] <- NA
      }
    } else {
      x$data[, chan_names(x)][x$data$.id %in% b_all$.id, ] <- NA
    }
 
  if(drop_events) {
    x$events <- suppressMessages(dplyr::anti_join(x$events, 
              dplyr::filter(x$events, !!!dots))) %>%  
              dplyr::mutate(channel = forcats::lvls_expand(channel, 
                                      new_levels = chan_names(x))) 





  }
  validate_eegbl(x)
}



#' Segments an eegble.
#'
#' EXPLAIN WHAT A SEGMENT IS/ DIFFERENCE WITH EPOCH (LINK?) + EXAMPLES OF SEGMENT WITH +Inf.
#' Fieldtrip calls the segment "trials". The limits are inclusive, the event is
#'  set as happening in the sample 1. If, for example, lim =c(0,0), the segment would contain only one sample.
#' 
#' @param x An \code{eegble} object.
#' @param ... Description of the event.
#' @param lim Vector indicating the time before and after the event. Or matrix with two columns, with nrow=total number of segments
#' 
#' @return An eegbl. 
#' 
#' @importFrom magrittr %>%
#' 
#' @export
segment <- function(x, ..., lim = c(-.5,.5), unit = "seconds"){
  
  dots <- rlang::enquos(...)
  #dots <- rlang::quos(description == "s121")
  #dots <- rlang::quos(description == "s70")
  #dots <- rlang::quos(description %in% c("s70",s71"))
  
 times0 <- dplyr::filter(x$events, !!!dots) %>% 
                  dplyr::select(-channel, -size) 

  if(stringr::str_to_lower(unit) %in% c("s","sec","second","seconds","secs")) {
    scaling <- srate(x)
  } else if(stringr::str_to_lower(unit) %in% c("ms","msec", "millisecond",
                                               "milli second", "milli-second", 
                                               "milliseconds", "milli seconds", 
                                               "msecs")) {
    scaling <- srate(x)/1000
  } else if(stringr::str_to_lower(unit) %in% c("sam","sample","samples")){
    scaling <- 1
  } else {
    stop("Incorrect unit. Please use 'ms', 's', or 'sample'")
  }

  
  if(length(lim) == 2) {
   lim <- rep(list(lim),each=nrow(times0))  
  } 

  if(is.matrix(lim) && dim(lim)[2]==2 && dim(lim)[1] == nrow(times0)){
    lim <- purrr::array_branch(lim,1)
  } else if(is.list(lim) && length(lim) ==nrow(times0) ){
   #ok format 
    NULL
  } else {
    stop("Wrong dimension of lim")
  }
  
  slim <- purrr::modify(lim, function(l) { 
                            if(l[2] < l[1]){
                              stop("A segment needs to be of positive length and include at least 1 sample.")
                            }
                              round(l * scaling) %>% as_integer } 
                            )

  x$data <- purrr::pmap_dfr(list(times0$.id,  times0$sample, slim), .id = ".id",

                                    function(i, s0, sl) x$data %>%
                                          # filter the relevant samples
                                          dplyr::filter(sample >= s0 + sl[1], 
                                                         sample <= s0 + sl[2],
                                                        .id == i) %>%
                                          dplyr::mutate(sample = sample - s0 + 1L) %>%
                                          # order the signals df:
                                          dplyr::select(-.id)) %>%
                                          dplyr::mutate(.id = as.integer(.id))

  slim <- purrr::map2(slim, split(x$data, x$data$.id), function(sl,d) { 
                              sl <- c(min(d$sample) - 1L, max(d$sample) - 1L) 
                            })

 x$events <- purrr::pmap_dfr(list(times0$.id, times0$sample, slim), .id = ".id",
                    function(i, s0, sl){ 
                      #bound according to the segment
                      x$events %>%
                      # filter the relevant events
                      # started after the segment (s0 + slim[1]) 
                      # or span after the segment (s0 + slim[1])  
                      dplyr::filter(sample + size - 1 >=  s0 + sl[1],
                                    sample <=  s0 + sl[2],
                                    .id == i) %>%
                      dplyr::mutate(size = dplyr::if_else(sample < s0 + sl[1], 
                                    as.integer(size - (s0 + sl[1] - sample) + 1L), 
                    # adjust the size so that it doesn't spillover after the segment
                                    size) %>% pmin(., sl[2] + 1L - sample + s0   ), 
                                  sample = dplyr::if_else(sample < s0 + sl[1], sl[1] + 1L,
                                                             sample - s0 + 1L)) %>%
                                                dplyr::select(-.id)
                                              }) %>%
                     dplyr::mutate(.id = as.integer(.id)) 

  message(paste0("# Total of ", max(x$data$.id)," segments found."))
 
  x$seg_info <- dplyr::right_join(x$seg_info, dplyr::select(times0,-sample), by =".id") %>%
                dplyr::ungroup() %>% dplyr::mutate(.id = 1:n()) %>% 
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
