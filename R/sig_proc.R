#' Butterworth IIR filter
#'
#' Apply a Butterworth IIR filter using
#' \code{signal::filt_filt}, which filters the signal twice to - once forwards,
#' then again backwards). Based on Matt Craddock's code of \code{eegUtils} \email{matt@@mattcraddock.com}.
#'
#' low and high are passband edges. Pass low freq or high freq alone
#' to perform high-pass or low-pass filtering respectively. For band-pass or
#' band-stop filters, pass both low and high.
#'
#' If low < high, bandpass filtering is performed.
#'
#' If low > high, bandstop filtering is performed.
#'
#' @author 
#' @param x A channel.
#' @param low Low frequency passband edge.
#' @param high High frequency passband edge.
#' @param order Filter order.
#' @param ... Other parameters passed.
#' 
#' 
#'
#' 

iir_filt <- function(x,
                                low = NULL,
                                high = NULL,
                                order = 4,
                                srate) {
  srate <- srate
  get('sample', envir = parent.frame(), inherit = TRUE)


  if(is.null(low) & is.numeric(high)){  # Low pass filter
    type <- "low"
    # message(sprintf("Low-pass IIR filter at %.4g Hz", high))
    W <- high / (srate / 2)
  } else if (is.null(high) & is.numeric(low)){# High pass filter
    type <- "high"
    # message("High-pass IIR filter at ", low," Hz")
    W <- low / (srate / 2)
    x <- x - mean(x)
  } else if(low < high){ # Band-pass filter
    # message(sprintf("Band-pass IIR filter from %.4g-%.4g Hz",
              # low, high))
    type <- "pass"
    W <- c(low / (srate / 2), high / (srate / 2))
    x <- x - mean(x)
  } else if(low > high) { # Band-stop filter
    # message(sprintf("Band-stop IIR filter from %.4g-%.4g Hz",
    #           high, low))
    type <- "stop"
    W <- c(high / (srate / 2), low / (srate / 2))
    x <- x - mean(x)
  } else {
    stop("Incorrect low/high frequencies")
  }

        #filtfilt filters twice, so effectively doubles filter_order - we half it here
      #so that it corresponds to the expectation of the user
      order <- round(order / 2)
      filt <- signal::butter(order, W, type = type)
      signal::filtfilt(filt, x)
}


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


#' Downsample EEG data
#'
#' Downsample a signal by a factor \code{q}, using an FIR or IIR filter.
#' This is a wrapper for \link{decimate} from the
#' \link{signal} package, see its documentation for details. Notice that 
#' the code of the \link{signal} package might be outdated.
#'
#' A factor q larger than 13 can result in NAs. To avoid this,
#' the downsampling can be done in steps. For example, instead of setting 
#' \code{q = 20}, it is possible to set \code{q = c(2,10)}.
#' 
#' @param x An eegble object.
#' @param q integer factor(s) to downsample by. 
#' @param max_sample Optionally, the (approximated) maximum sample number can be defined here, which is at least half of the total numbe of samples.
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
    stop("# The factor q must be 2 or more.")
  }
  
  if(any(round(q) != q)) {
    q <- round(q) 
    warning(paste0("# The factor q needs to be round number, using q = ", q))
  }

  if(!is.null(max_sample)){
       len_samples <- max(nsamples(x))
       if(max_sample > len_samples/2) {
        stop("The maximum value for max_sample allowed is half the number of samples.")
       }
       approx_q <- len_samples / max_sample
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
  warning("# Use with caution, downsampling is based on decimate from the signal package, which might be outdated")
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


#' Resample EEG data
#'
#' Resample a signal by setting new sampling rate or a maximum number of samples.
#' This is a wrapper for \link{interp1} from the
#' \link{signal} package, see its documentation for details. Notice that 
#' the code of the \link{signal} package might be outdated.
#'
#' 
#' @param x An eegble object.
#' @param srate New sampling rate. 
#' @param max_sample Optionally, the (approximated) maximum sample number can be defined here.
#' @param method Method passed to interp1, "pchip" by default.
#' @param ... Other arguments passed to interp1.
#' 
#' 
#' 
#' 
#' 
#' 
resample <- function(x, srate = NULL, max_sample = NULL, method = "pchip", ...) {
  UseMethod("resample")
}

#' 
resample <- function(x, srate = NULL, max_sample = NULL, method = "pchip", ...) {

  nsamples <- 1000
  1:nsamples

  warning("# Use with caution, downsampling is based on decimate from the signal package, which might be outdated")
  message(paste0("# Resampling from ", srate(x), "Hz to ",
                 new_srate, "Hz."))



}