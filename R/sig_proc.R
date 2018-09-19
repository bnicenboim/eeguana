#' Rereference channel.
#' 
#' This function is meant to be used together with \code{mutate} or \code{mutate_all}. See the example
#' 
#' @param x A channel.
#' @param ... Channels that will be averaged as the reference.
#' @return A rereferenced channel.
#' @export
#' 
#' @examples
#' \dontrun{
#' # Rereference all channels used the linked mastoids (average of the two mastoids)
#' 
#' faces_segs %>% act_on(signal) %>%
#'                  mutate_all(funs(rereference(., M1, M2)))
#' }#' 
rereference <- function(x, ..., na.rm = FALSE) {
   x - vec_mean(..., na.rm = na.rm)
}            


#' Downsampling EEG data
#'
#' Downsample a signal by a factor \code{q}, using an FIR or IIR filter.
#' This is a wrapper for \code{decimate} from the
#' \link{signal} package, see its documentation for details.
#'
#' A factor q larger than 13 can result in NAs. To avoid this,
#' the downsampling can be done in steps. For example, instead of setting 
#' \code{q = 20}, it is possible to set \code{q = c(2,10)}.
#' 
#' @param x An eegble object.
#' @param q integer factor(s) to downsample by. 
#' @param max_sample Optionally, the (approximated) maximum sample number can be defined here.
#' @param ... Other arguments passed to decimate.
#' @export
#' 
#' 
#' 
#' 
#' 
downsample <- function(x, q = 2, max_sample = NULL, ...) {
  UseMethod("downsample")
}

#' @export 
downsample.eegbl <- function(x, q = 2, max_sample = NULL, 
  n = if (ftype == "iir") 8 else 30, 
  ftype = "iir") {
  


 # if(stringr::str_to_lower(q) == "min") {
 #   q <- mindiv(srate(x), start = 2) 
 #  message(paste0("Using q = ", q))
 # }

  if(any(q < 2)) {
    stop("The factor q must be 2 or more.")
  }
  
  if(any(round(q) != q)) {
    q <- round(q) 
    warning(paste0("The factor q needs to be round number, using q = ", q))
  }

  if(!is.null(max_sample)){
       approx_q <- max(duration(x)) * srate(x) / max_sample
       q <- factors(round(approx_q))
  }

  # if(srate(x) %% q != 0){
  #     q <- mindiv(srate(x), start = q)
  #     warning(paste0("The signal rate needs to be divisable by q, using q = ", q))
  # }
  # if(q >= srate(x)){
  #   stop("The factor q is too large.")
  # }

  q <- as.integer(q)
  factor <- prod(q)
  new_srate <- srate(x) / factor
  message(paste0("# Downsampling from ", srate(x), "Hz to ",
                 new_srate, "Hz."))

  x$signal <- x$signal %>% 
      dplyr::select(-sample) %>% 
      split(x$signal$.id) %>%  
      purrr::map_dfr(
        function(signal_id) purrr::map_dfc(signal_id[-1],  
          # reduce the channel info by applying decimate to the elements of q
          function(channel) purrr::reduce(c(list(channel), as.list(q)), ~ 
            signal::decimate(x = .x, q = .y, n = n, ftype = ftype)))
        , .id =".id" ) %>%
      dplyr::mutate(.id = as.integer(.id)) %>% 
      dplyr::group_by(.id) %>%
      dplyr::mutate(sample = seq.int(1,dplyr::n())) %>%
      dplyr::select(.id, sample, dplyr::everything()) %>%
      dplyr::ungroup()

  x$info$srate <- new_srate

  # even table needs to be adapted, starts from 1, 
  # and the size is divided by two with a min of 1
  x$events <- x$events %>% 
              dplyr::mutate(sample = as.integer(round(sample / factor)) + 1L,
                            size = round(size / factor) %>%  
                                  as.integer %>% purrr::map_int(~max(.x, 1L)) )

  # just in case I update the .id from segments table
  x$segments <- dplyr::mutate(x$segments, .id = seq.int(1L, dplyr::n()))
  
  message(say_size(x))
  validate_eegbl(x)
}

#' Get integers so that their prod is approx N
factors <- function(N){
  out <- c()
  while(N != 1){
    for(i in 10:1){
      if(i == 1) {
       N <- N - 1
      } else if(N %% i == 0) {
        out <- c(out, i) 
        N <- N/i
        break
      }
    }}
    out
}

