#' Butterworth IIR filters.
#'
#' Apply a Butterworth IIR filter using \code{signal::filt_filt}, which
#' filters the signal twice (once forwards, then again backwards). Based on
#' Matt Craddock's code of \code{eegUtils} \email{matt@mattcraddock.com}.
#'
#' \itemize{
#' \item \code{filt_low_pass_ch()} Low-pass or high-cut filter.
#' \item \code{filt_high_pass_ch()} High-pass or low-cut filter.
#' \item \code{filt_band_pass_ch()} Band-pass filter.
#' \item \code{filt_stop_pass_ch()} Stop-pass filter.
#' }
#'
#' @param channel A channel.
#' @param band_edge A single cut frequency for \code{filt_low_pass_ch} and \code{filt_high_pass_ch}, two edges for  \code{filt_band_pass_ch} and \code{filt_stop_pass_ch}.
#' @param order Filter order.
#' @param ... The sample rate can be included as \code{srate}, when the function is called outside mutate/summarize. 
#'
#' @return A channel.
#'
#'
#' @examples
#' \dontrun{
#'
#' faces_segs %>% mutate_all(filt_low_pass_ch, band_edge = 5, order = 2)
#' }
#' @name filt
NULL
#> NULL

#' @rdname filt
#' @export
filt_low_pass_ch <- function(channel, band_edge = NULL, order = 4, ...) {
  if(length(band_edge)>1) stop("band_edge should contain only one frequency.")
  type <- "low"
  filt_ch(channel = channel, band_edge = band_edge, order = order, type = type,...)
}

#' @rdname filt
#' @export
filt_high_pass_ch <- function(channel, band_edge = NULL, order = 4, ...) {
  if(length(band_edge)>1) stop("band_edge should contain only one frequency.")
  type <- "high"
  centered_channel <- channel - mean(channel)
  filt_ch(channel = centered_channel, band_edge = band_edge, order = order, type = type,...)
}

#' @rdname filt
#' @export
filt_band_pass_ch <- function(channel, band_edge = NULL, order = 4, ...) {
  if(length(band_edge) != 2) stop("band_edge should contain two frequency.")
  if(band_edge[1] >= band_edge[2]) {
    stop("The first argument of band_edge should be larger than the second one.")
  }
  type <- "pass"
  centered_channel <- channel - mean(channel)
  filt_ch(channel = centered_channel, band_edge = band_edge, order = order, type = type,...)
}

#' @rdname filt
#' @export
filt_stop_pass_ch <- function(channel, band_edge = NULL, order = 4, ...) {
  if(length(band_edge) != 2) stop("band_edge should contain two frequency.")
  if(band_edge[1] <= band_edge[2]) {
    stop("The second argument of band_edge should be larger than the first one.")  }
  type <- "stop"
  centered_channel <- channel - mean(channel)
  filt_ch(channel = centered_channel, band_edge = band_edge, order = order, type = type,...)
}


filt_ch <- function(channel, band_edge = NULL, order = 4, type, ...){
  args <- list(...)
  if(!"srate" %in% names(args)){ #if it's empty it will be taken from the attributes of sample
      #Black magic to extract sample columns and its attribute srate that has the srate of the eegble object:  
      signal_env <- rlang::env_get(env = parent.frame(2), '.top_env', inherit = TRUE)
      sample <- rlang::env_get(signal_env, "sample")

      srate <- attributes(sample)$srate
      print(srate)
    } else {
      srate <- args$srate
    }
  W <- band_edge / (srate / 2)

  # filtfilt filters twice, so effectively doubles filt_order - we half it here
  # so that it corresponds to the expectation of the user
  order <- round(order / 2)
  filt <- signal::butter(n = order, W = W, type = type)
  signal::filtfilt(filt, channel)
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

  if (any(q < 2)) {
    stop("# The factor q must be 2 or more.")
  }

  if (any(round(q) != q)) {
    q <- round(q)
    warning(paste0("# The factor q needs to be round number, using q = ", q))
  }

  if (!is.null(max_sample)) {
    len_samples <- max(nsamples(x))
    if (max_sample > len_samples / 2) {
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
  message(paste0(
    "# Downsampling from ", srate(x), "Hz to ",
    new_srate, "Hz."
  ))

  x$signal <- x$signal %>%
    dplyr::select(-sample) %>%
    split(x$signal$.id) %>%
    purrr::map_dfr(
      function(signal_id) purrr::map_dfc(
          signal_id[-1],
          # reduce the channel info by applying decimate to the elements of q
          function(channel) purrr::reduce(c(list(channel), as.list(q)), ~
            signal::decimate(x = .x, q = .y, n = n, ftype = ftype))
        )
      ,
      .id = ".id"
    ) %>%
    dplyr::mutate(.id = as.integer(.id)) %>%
    dplyr::group_by(.id) %>%
    dplyr::mutate(sample = seq.int(1, dplyr::n())) %>%
    dplyr::select(.id, sample, dplyr::everything()) %>%
    dplyr::ungroup()

  x$info$srate <- new_srate

  # even table needs to be adapted, starts from 1,
  # and the size is divided by two with a min of 1
  x$events <- x$events %>%
    dplyr::mutate(
      sample = as.integer(round(sample / factor)) + 1L,
      size = round(size / factor) %>%
        as.integer() %>%
        purrr::map_int(~max(.x, 1L))
    )

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
resample.eegbl <- function(x, srate = NULL, max_sample = NULL, method = "", ...) {
  nsamples <- 1000
  1:nsamples

  warning("# Use with caution, resampling is based on decimate from the signal package, which might be outdated")
  message(paste0(
    "# Resampling from ", srate(x), "Hz to ",
    new_srate, "Hz."
  ))
}
