#' Remove (transform to NA) problematic events from an eegble.
#'
#' @param x An \code{eegble} object.
#' @param ... Description of the problematic event.
#' @param ind_channel If set to FALSE, 
#'     it will remove samples from all channels (Default:  ind_channel = TRUE).
#' @param entire_seg If set to TRUE, it will remove the entire segment 
#'    (Default: entire_seg = FALSE).
#' @param parallel If set to TRUE, it will use all cores. 
#'    (Default: parallel = FALSE). See details.
#' 
#' The parallel option runs the process in several cores using \code{furrr} 
#' with \code{future} as the backends. To use the parallel option these 
#' packages need to be installed, the library \code{future} needs to be 
#' attached, and a "plan" needs to be specified (e.g., 
#' \code{plan(multiprocess)})
#' 
#' @examples
#' \dontrun{
#' library(future)
#' plan(multiprocess)
#' clean_eggble <- event_to_NA(eegble, parallel = TRUE)
#' }
#' @return An eegbl. 
#' 
#' @importFrom fastmatch %fin%
#' @importFrom magrittr %>%
#' 
#' @export
event_to_NA <- function(x, ..., ind_channel = TRUE, entire_seg = FALSE, 
  parallel = FALSE) {
  dots <- rlang::enquos(...)
  if(parallel){
    # the progress bar are not very useful
    modify <- function(...) furrr::future_modify(..., .progress = FALSE)
  } else {
    modify <- purrr::modify
  }

  # dots <- rlang::quos(type == "Bad Interval")
  baddies <- purrr::map(x$data, ~ dplyr::filter(.x$events, !!!dots) %>% 
       dplyr::mutate( sample = purrr::map2(sample, sample + size - 1,seq)) %>% 
       tidyr::unnest() %>%
       dplyr::select(channel, sample))
  x$data <- purrr::map2(x$data, baddies, function(data_file, bad_file) {
              message("# Starting new file")
              modify(data_file$signals, 
                function(seg){
                  if(ind_channel & !all(is.na(bad_file$channel))){
                   
                    bad_file_rel <- bad_file %>% 
                                      dplyr::filter(sample >= min(seg$sample),
                                             sample <= max(seg$sample))
                    if(!entire_seg){
                      for(c in unique(bad_file$channel)){
                        baddies_chan <- bad_file_rel %>% 
                                          dplyr::filter(channel==c)
                        seg <- dplyr::mutate_at(seg, c,
                          dplyr::funs(dplyr::if_else(sample %fin% baddies_chan$sample, 
                            NA_real_, .)) )
                      }
                    } else {
                      warning("entire_seg = FALSE is not implemented")
                    }
                  } else { 
                    # 1. NA in all channels if there's something bad on one of them
                    # or 2. NA in all channels if the channel is not specified
                      baddies_all <- bad_file %>% 
                                     dplyr::select(sample) %>% 
                                     dplyr::distinct()
                    if(!entire_seg){ 
                      seg <- dplyr::mutate_at(seg, chan_names(x),
                       dplyr::funs(dplyr::if_else(sample %fin% baddies_all$sample,
                        NA_real_,. )) )
                    } else {
                      if(length(intersect(seg$sample, baddies_all$sample)) != 0){
                        seg <- NULL  
                      }
                    }  
                  }
                  seg
                }) %>% list(signals = ., events = data_file$events  )}
              )
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
segment <- function(x, ..., lim = c(-1,1)){
  
  dots <- rlang::enquos(...)
  #dots <- rlang::quos(description == "s70")
  #dots <- rlang::quos(description %in% c("s70",s71"))
  # creates a list of vector with the times zero for each "file"
  times0 <- purrr::map(x$data, function(l) {
                event <- l$events %>% 
                  dplyr::filter(!!!dots)

                t0s <- event %>% 
                  dplyr::select(-channel, -size) 

                #   # # hack to extract the first column name as the name of the segment
                #   # descr <- dots %>% 
                #   #          as.character() %>% 
                #   #          stringr::str_match("~([A-Za-z]*) ") %>% 
                #   #          .[,2]
                # names(t0s) <- purrr::map2_chr(event[[descr]], 
                #               seq(1,length(t0s)), paste, sep="_")
                # t0s
              })
   
  # for each vector of times zero (t0) associated with the data of each file   
  x$data <- purrr::map2(.x= times0, .y= x$data,
  # iterate over the vector of times zeros (in sample)
  function(t0, l){ 
    segs <- purrr::map(.x=t0$sample, ~ 
       purrr::map_dfr(l$signals, function(df) df %>%
      # filter the relevant samples
      dplyr::filter(sample >= .x + lim[1] * srate(x), 
             sample <= .x + lim[2] * srate(x)) %>%
      # add a time column
      dplyr::mutate(time = round(sample/srate(x) -.x/srate(x), 
        decimals(1/srate(x)))) %>% 
      # order the signals df:
      dplyr::select(sample, time, dplyr::everything())
      # reconstruct
      )) 
       message(paste0("# ", length(segs)," segments found."))
        list(signals = segs,  events = l$events)

  } )
  x$seg_info <- purrr::map_dfr(times0, .id = "id", ~ .x) %>% 
                dplyr::select(-sample) %>% group_by(id) %>% 
                mutate(seg =seq(1,n()))
  message(paste0(say_size(x)," after segmentation."))
  validate_eegbl(x)
}


#' Baseline an eegble.
#'
#' @param x An \code{eegble} object.
#' @param t A negative number indicating from when to baseline 
#'    (TO COMPLETE). The default is to use all the negative times.
#' @param parallel If set to TRUE, it will use all cores. 
#'    (Default: parallel = FALSE).
#' 
#' The parallel option runs the process in several cores using \code{furrr} 
#' with \code{future} as the backends. To use the parallel option these 
#' packages need to be installed, the library \code{future} needs to be 
#' attached, and a "plan" needs to be specified (e.g., 
#' \code{plan(multiprocess)})
#' 
#' @examples
#' \dontrun{
#' library(future)
#' plan(multiprocess)
#' segmented_eggble <- segment(clean_eggble, description== "s13", lim = c(-.2,.5)
#' segmented_bl_eggble <- baseline(segmented_eggble, parallel = TRUE)
#' }
#' @return An eegbl. 
#' 
#' @importFrom magrittr %>%
#' 
#' @export

baseline <- function(x, t = -Inf, parallel = FALSE) {

  # function to apply to every segment
  bl_seg <- function(seg){
                  bl <- dplyr::filter(seg, time < 0, time > t) %>% 
                              dplyr::summarize_at(chan_names(x), mean, 
                                        na.rm = TRUE)
                  # can it be done with tidyverse?
                  # will it be faster?            
                  seg[,chan_names(x)] <- seg[,chan_names(x)] - as.list(bl)
                  seg
                }

  segmap_eggbl(x, f_seg = bl_seg, parallel = parallel)
}            
