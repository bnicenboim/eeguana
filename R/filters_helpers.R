## This functions are adapted from https://github.com/mne-tools/mne-python/blob/master/mne/filter.py and https://github.com/mne-tools/mne-python/blob/master/mne/cuda.py ; latest commit c30e70c
check_zero_phase_length <- function(N, phase, gain_nyq = 0) {
  N <- as.integer(N)
  if (N %% 2 == 0) {
    if (phase == "zero") {
      stop(
        "filter_length must be odd if phase=\"zero\"",
        "got ", N
      )
    }
    else if (phase == "zero-double" && gain_nyq == 1) {
      N <- N + 1
    }
  }
  return(N)
}

filter_attenuation <- function(h, freq, gain) {
  ##        """Compute minimum attenuation at stop frequency."""
  filt_resp <- signal::freqz(h, n = pi * freq)[[1]] %>%
    ## use amplitude respons
    abs()
  filt_resp[gain == 1] <- 0
  idx <- which.max(filt_resp)
  att_db <- -20 * log10(max(filt_resp[idx], 1e-20))
  att_freq <- freq[idx]
  return(list(db = att_db, freq = att_freq))
}


firwin_design <- function(N, freq, gain, window = "hamming", sampling_rate) {
  ##    """Construct a FIR filter using firwin."""
  if (freq[1] != 0) stop("First element of 'freq' should be 0")
  if (length(freq) <= 1) stop("Length of frequency should be larger than 1.")
  if (length(freq) != length(gain)) stop("'freq' and 'gain' should have the same length.")
  h <- rep(0, N)
  prev_freq <- freq[length(freq)]
  prev_gain <- gain[length(gain)]
  if (prev_gain == 1) h[(N %/% 2) + 1] <- 1 # start with "all up"
  if (!prev_gain %in% c(0, 1)) stop("Gain should end in 1 or 0")
  length_factors <- dplyr::case_when(
    window == "hann" ~ 3.1,
    window == "hamming" ~ 3.3,
    window == "blackman" ~ 5.0
  )
  for (i in seq_len(length(freq) - 1)) {
    this_freq <- rev(freq)[-1][i]
    this_gain <- rev(gain)[-1][i]
    if (!this_gain %in% c(0, 1)) stop()
    if (this_gain != prev_gain) {
      transition <- (prev_freq - this_freq) / 2
      this_N <- as.integer(round(length_factors / transition)) %>% {
        . + (1 - . %% 2)
      } ## make it odd
      if (this_N > N) {
        stop(
          "The requested filter length ", N, " is too short ",
          "for the requested ", transition * sampling_rate / 2, " Hz transition band, ",
          "which requires ", this_N, "  samples"
        )
      }
      this_h <- firwin(this_N, (prev_freq + this_freq) / 2,
        window = window, pass_zero = TRUE, fs = freq[length(freq)] * 2
      )
      offset <- (N - this_N) %/% 2
      if (this_gain == 0) {
        h[seq(offset + 1, N - offset, by = 1)] <- h[seq(offset + 1, N - offset, by = 1)] - this_h
      } else {
        h[seq(offset + 1, N - offset, by = 1)] <- h[seq(offset + 1, N - offset, by = 1)] + this_h
      }
    }
    prev_gain <- this_gain
    prev_freq <- this_freq
  }
  return(h)
}


#' Use IIR parameters to get filtering coefficients.
#'
#'
#' This function works like a wrapper for iirdesign and iirfilter in
#' scipy.signal to make filter coefficients for IIR filtering. It also
#' estimates the number of padding samples based on the filter ringing.
#' It creates a new iir_params dict (or updates the one passed to the
#' function) with the filter coefficients ('b' and 'a') and an estimate
#' of the padding necessary ('padlen') so IIR filtering can be performed.
#' Parameters
#' ----------
#' iir_params : dict
#'     Dictionary of parameters to use for IIR filtering.
#'         * If ``iir_params['sos']`` exists, it will be used as
#'           second-order sections to perform IIR filtering.
#'           .. versionadded:: 0.13
#'         * Otherwise, if ``iir_params['b']`` and ``iir_params['a']``
#'           exist, these will be used as coefficients to perform IIR
#'           filtering.
#'         * Otherwise, if ``iir_params['order']`` and
#'           ``iir_params['ftype']`` exist, these will be used with
#'           `scipy.signal.iirfilter` to make a filter.
#'           You should also supply ``iir_params['rs']`` and
#'           ``iir_params['rp']`` if using elliptic or Chebychev filters.
#'         * Otherwise, if ``iir_params['gpass']`` and
#'           ``iir_params['gstop']`` exist, these will be used with
#'           `scipy.signal.iirdesign` to design a filter.
#'         * ``iir_params['padlen']`` defines the number of samples to pad
#'           (and an estimate will be calculated if it is not given).
#'           See Notes for more details.
#'         * ``iir_params['output']`` defines the system output kind when
#'           designing filters, either "sos" or "ba". For 0.13 the
#'           default is 'ba' but will change to 'sos' in 0.14.
#' f_pass : float or list of float
#'     Frequency for the pass-band. Low-pass and high-pass filters should
#'     be a float, band-pass should be a 2-element list of float.
#' f_stop : float or list of float
#'     Stop-band frequency (same size as f_pass). Not used if 'order' is
#'     specified in iir_params.
#' sfreq : float | None
#'     The sample rate.
#' btype : str
#'     Type of filter. Should be 'lowpass', 'highpass', or 'bandpass'
#'     (or analogous string representations known to
#'     :func:`scipy.signal.iirfilter`).
#' return_copy : bool
#'     If False, the 'sos', 'b', 'a', and 'padlen' entries in
#'     ``iir_params`` will be set inplace (if they weren't already).
#'     Otherwise, a new ``iir_params`` instance will be created and
#'     returned with these entries.
#' %(verbose)s
#' Returns
#' -------
#' iir_params : dict
#'     Updated iir_params dict, with the entries (set only if they didn't
#'     exist before) for 'sos' (or 'b', 'a'), and 'padlen' for
#'     IIR filtering.
#' See Also
#' --------
#' mne.filter.filter_data
#' mne.io.Raw.filter
#' Notes
#' -----
#' This function triages calls to :func:`scipy.signal.iirfilter` and
#' :func:`scipy.signal.iirdesign` based on the input arguments (see
#' linked functions for more details).
#' .. versionchanged:: 0.14
#'    Second-order sections are used in filter design by default (replacing
#'    ``output='ba'`` by ``output='sos'``) to help ensure filter stability
#'    and reduce numerical error.
#' @examples
#'
#'  ##iir_params can have several forms. Consider constructing a low-pass
#'   ## filter at 40 Hz with 1000 Hz sampling rate. ## In the most basic (2-parameter) form of iir_params, the order of the ## filter 'N' and the type of filtering 'ftype' are specified. To get ## coefficients for a 4th-order Butterworth filter, this would be:
#'
#' # In Python:
#'
#' library(reticulate)
#' py_run_string("import mne")
#' py_run_string("iir_params = dict(order=4, ftype='butter', output='ba')")
#' py_run_string("iir_params = mne.filter.construct_iir_filter(iir_params, 40, None, 1000, 'low', return_copy=False)")
#' py_run_string("print(iir_params)")
#'
#' iir_params = list(order =4, type ="butter", output="ba")
#' construct_iir_filter(iir_params, f_pass = 40, f_stop = NULL, sfreq = 1000, btype = "low")
#' # Filters can also be constructed using filter design methods. To get a
#' # 40 Hz Chebyshev type 1 lowpass with specific gain characteristics in the
#' # pass and stop bands (assuming the desired stop band is at 45 Hz), this
#' # would be a filter with much longer ringing:
#' py_run_string("iir_params = dict(ftype='cheby1', gpass=3, gstop=20, output='ba')")
#' py_run_string("iir_params = mne.filter.construct_iir_filter(iir_params, 40, 50, 1000, 'low')")
#' >>> print((2 * len(iir_params['sos']), iir_params['padlen']))  # doctest:+SKIP
#' (6, 439)
#' Padding and/or filter coefficients can also be manually specified. For
#' a 10-sample moving window with no padding during filtering, for example,
#' one can just do:
#' >>> iir_params = dict(b=np.ones((10)), a=[1, 0], padlen=0)  # doctest:+SKIP
#' >>> iir_params = construct_iir_filter(iir_params, return_copy=False)  # doctest:+SKIP
#' >>> print((iir_params['b'], iir_params['a'], iir_params['padlen']))  # doctest:+SKIP
#' (array([1., 1., 1., 1., 1., 1., 1., 1., 1., 1.]), [1, 0], 0)
#' @noRd

construct_iir_filter <- function(iir_params, f_pass=NULL, f_stop=NULL, sfreq=NULL,
                         btype=NULL){

  known_filters = c("butter", "cheby1", "cheby2", "ellip")
  ## c('bessel', 'butter', 'butterworth', 'cauer', 'cheby1',
  ##                    'cheby2', 'chebyshev1', 'chebyshev2', 'chebyshevi',
  ##                    'chebyshevii', 'ellip', 'elliptic')
    # if the filter has been designed, we're good to go
    Wp = NULL
    if ('sos' %in% names(iir_params)){
        system = iir_params[['sos']]
        output = 'sos'
    } else if ('a' %in% names(iir_params) & 'b' %in% names(iir_params)){
        system = list(b =iir_params[['b']], a = iir_params[['a']])
        output = 'ba'
    } else {
        output = ifelse(is.null(iir_params[["output"]]), 'ba', iir_params[["output"]])
        # ensure we have a valid type
        if (!'type'  %in% names(iir_params))
            stop("type must be an entry in iir_params if ''b'' ",
                               "and ''a'' are not specified", call. = FALSE)
        type = iir_params[['type']] %||% ""
        if (!type  %in% known_filters)
            stop("type must one of ", paste0(known_filters, sep = ", "))

        # use order-based design
        ## f_pass = np.atleast_1d(f_pass)
        ## if f_pass.ndim > 1:
            ## raise ValueError('frequencies must be 1D, got %dD' % f_pass.ndim)
        edge_freqs <- paste0(f_pass, collapse = ", ")
        Wp = f_pass / (sfreq / 2)
        # IT will de designed
        ## ftype_nice = _ftype_dict.get(type, type)
        if(options()$eeguana.verbose){
          message("IIR filter parameters\n",
                  "---------------------\n",
                  type," ", btype,
                  " zero-phase (two-pass forward and reverse) \n",
                    " non-causal filter:\n" )
        }
# SciPy designs for -3dB but we do forward-backward, so this is -6dB
        if ('order' %in% names(iir_params)){

           system <- iirfilter(iir_params[["order"]],
                               rp = iir_params[["rp"]],
                               rs = iir_params[["rs"]],
                               Wn = Wp,
                               btype = btype,
                               type = type,
                               output = output)
          forder <-  (2 * iir_params[['order']] * length(Wp))
            if(options()$eeguana.verbose)
              message("- Filter order " , forder,"  (effective, after forward-backward)")

        } else {
            ## # use gpass / gstop design
            Ws = f_stop / (sfreq / 2)
            ## Ws = np.asanyarray(f_stop) / (float(sfreq) / 2)
            ## if 'gpass' not in iir_params or 'gstop' not in iir_params:
            ##     raise ValueError('iir_params must have at least ''gstop'' and'
            ##                      ' ''gpass'' (or ''N'') entries')
            system <- iirdesign(wp = Wp, ws = Ws, gpass = iir_params[['gpass']],
                                gstop = iir_params[['gstop']], type=type, output=output)
        }
    }

    if (is.null(system))
        stop('coefficients could not be created from iir_params')
    # do some sanity checks
    ## _check_coefficients(system)

    # get the gains at the cutoff frequencies
    if (!is.null(Wp)){
        if (output == 'sos'){
          #originally with sosfreqz, TODO check if it works
          #cutoffs = gsignal::freqz(system, n=Wp * pi)$h
        } else {
          #CAN'T make this work in R
        ## cuttoff <- s$signal$freqz(system$b, system$a, worN=Wp * pi)[[2]]

        ## signal::buttord(Wp =40,Ws=40)
        ## gsignal::freqz(system$b, a = system$a)$h
##         s$signal$freqz(system$b, system$a, worN = .2)[[2]]
## np <- reticulate::import("numpy")
##         npp <- np$polynomial$polynomial
##         zm1 = exp(-1i * Wp * pi)
##         h = (signal::polyval(zm1, system$b) /
##              pracma::polyval(zm1, system$a))

##              pracma::polyval(zm1, 1)
##         npp$polyval(zm1, 1)
##         np$polyval(zm1, 1)
##         (npp$polyval(zm1, system$b, tensor=FALSE) / npp$polyval(zm1, system$a, tensor=FALSE))
##         (npp$polyval(zm1, system$b, tensor=TRUE) / npp$polyval(zm1, system$a, tensor=TRUE))

##         n <- 2*pi*Wp  * pi/(2*pi)
##         cutoffs <-
##           signal::freqz(system$b, a = system$a,  n=Wp * pi)$h
##           signal::freqz(system$b, a = system$a, n = .9)$h
##           COUDN't DO it yet
 cutoffs <- NA
        }

        ## # 2 * 20 here because we do forward-backward filtering
        cutoffs <- 40 * log10(abs(cutoffs))
  ## if (options()$eeguana.verbose)
    ## message("Cutoff(s) at ", edge_freqs, " Hz: ", cutoffs, "dB")
    }
# now deal with padding
    if (!'padlen' %in% names(iir_params)){
        padlen = estimate_ringing_samples(system)
    } else{
        padlen = iir_params[['padlen']]
    }

    iir_params[["padlen"]] <- padlen
    if (output == 'sos'){
        iir_params[["sos"]] <- system$sos
    } else {
        iir_params[["b"]] <- system$b
        iir_params[["a"]] <- system$a
    }
    iir_params
}

#' Estimate filter ringing
#'
#' @param system  list(b, a)
#' @param max_try  Approximate maximum number of samples to try. This will be changed to a multiple of 1000.
#'
#' @return integer with the approximate ringing.
#'
#' @noRd
estimate_ringing_samples <- function(system, max_try=100000){
  if(all(c("a","b") %in% names(system))){
      kind = 'ba'
      b = system$b
      a = system$a
      zi=  rep(0, length(a) - 1)
    }   else {
      kind = 'sos'
      sos = system$sos
    }

    n_per_chunk = 1000

    n_chunks_max = ceiling(max_try/n_per_chunk)
    x = rep(0, n_per_chunk)
    x[1] = 1
    last_good = n_per_chunk
    thresh_val = 0

    for (ii in seq_len(n_chunks_max)){
        if (kind == 'ba'){
            h = signal::filter(b, a, x)
            # s$signal$lfilter(b, a, x, zi = zi)
        } else {
          stop("sos output for filters not implemented")
            ## h = gsignal::sosfilt(sos, x)
        }
        x[1] = 0  # for subsequent iterations we want zero input
        h = abs(h)
        #h is too high

        thresh_val = max(0.001 * max(h), thresh_val)
         ## np$where(np$abs(h) > thresh_val)[[1]]
        idx = which(abs(h) > thresh_val)
        # it should be vetweeb 1 and 83 for the example
        if (length(idx) > 1){
            last_good = idx[length(idx)] - 1
        } else{  # this iteration had no sufficiently large values
            idx = (ii - 2) * n_per_chunk + last_good
            return(idx) #stops the for loop and return this
        }
    }
  warning('Could not properly estimate ringing for the filter')
  return(n_per_chunk * n_chunks_max)
}


construct_fir_filter <- function(sampling_rate, freq, gain, filter_length, phase = "zero",
                                 fir_window = "hamming", fir_design = "firwin") {
  ## """Filter signal using gain control points in the frequency domain.
  ## The filter impulse response is constructed from a Hann window (window
  ## used in "firwin2" function) to avoid ripples in the frequency response
  ## (windowing is a smoothing in frequency domain).
  ## If x is multi-dimensional, this operates along the last dimension.
  ## Parameters
  ## ----------
  ## Fs : float
  ##     Sampling rate in Hz.
  ## freq : 1d array
  ##     Frequency sampling points in Hz.
  ## gain : 1d array
  ##     Filter gain at frequency sampling points.
  ##     Must be all 0 and 1 for fir_design=="firwin".
  ## filter_length : int
  ##     Length of the filter to use. Must be odd length if phase == "zero".
  ## phase : str
  ##     If "zero", the delay for the filter is compensated (and it must be
  ##     an odd-length symmetric filter). If "linear", the response is
  ##     uncompensated. If "zero-double", the filter is applied in the
  ##     forward and reverse directions. If "minimum", a minimum-phase
  ##     filter will be used.
  ## fir_window : str
  ##     The window to use in FIR design, can be "hamming" (default),
  ##     "hann", or "blackman".
  ## fir_design : str
  ##     Can be "firwin2" or "firwin".
  ## Returns
  ## -------
  ## h : array
  ##     Filter coefficients.
  ## """
  if (freq[1] != 0) stop("Error in freq argument, first element must be 0")
  if (fir_design == "firwin") {
    fir_fun <- purrr::partial(firwin_design, sampling_rate = sampling_rate)
  } else {
    stop("Unsupported filter_design", call. = FALSE)
  }

  # issue a warning if attenuation is less than this
  min_att_db <- if (phase == "minimum") 12 else 20

  # normalize frequencies
  n_freq <- freq / (sampling_rate / 2.)
  if (freq[1] != 0 || n_freq[length(freq)] != 1) {
    stop("freq must start at 0 and end an Nyquist,", sampling_rate / 2, ", got ", freq[length(freq)])
  }

  # Use overlap-add filter with a fixed length
  N <- check_zero_phase_length(filter_length, phase, gain[length(gain)])
  # construct symmetric (linear phase) filter
  if (phase == "minimum") {
    h <- fir_fun(N * 2 - 1, n_freq, gain, window = fir_window)
    ##        h = minimum_phase(h)
    stop("Not supported")
  } else {
    h <- fir_fun(N, n_freq, gain, window = fir_window)
  }
  if (length(h) != N) {
    stop("Wrong size of h")
  }
  att <- filter_attenuation(h, n_freq, gain)

  if (phase == "zero-double") {
    att$db <- att$db + 6
  }
  if (att$db < min_att_db) {
    att$freq <- att$freq * sampling_rate / 2.
    warning(
      "Attenuation at stop frequency ", att$freq, "Hz is only ", att$db, " dB. ",
      "Increase filter_length for higher attenuation."
    )
  }
  return(h)
}


#' Create a FIR or IIR filter.
#'
#' `l_freq` and `h_freq` are the frequencies below which and above
#' which, respectively, to filter out of the data. Thus the uses are:
#'
#' - `l_freq < h_freq`: band-pass filter
#' - `l_freq > h_freq`: band-stop filter
#' - `!is.null(l_freq) & is.null(h_freq)`: high-pass filter
#' - `is.null(l_freq) & !is.null(h_freq)`: low-pass filter
#' @param data  `n_times` length or NULL. The data that will be filtered. This is used for sanity checking only. If None, no sanity checking related to the length of the signal relative to the filter order will be performed.
#' @param sampling_rate  The sample frequency in Hz.
#' @param l_freq Low cut-off frequency in Hz. If None the data are only low-passed.
#' @param h_freq High cut-off frequency in Hz. If None the data are only high-passed.
#' @param filter_length Length of the FIR filter to use (if applicable):
#'     * "auto" (default): the filter length is chosen based
#'       on the size of the transition regions (6.6 times the reciprocal
#'       of the shortest transition band for fir_window="hamming"
#'       and fir_design="firwin2", and half that for "firwin").
#'     * str: a human-readable time in
#'       units of "s" or "ms" (e.g., "10s" or "5500ms") will be
#'       converted to that number of samples if ``phase="zero"``, or
#'       the shortest power-of-two length at least that duration for
#'       ``phase="zero-double"``.
#'     * int: specified length in samples. For fir_design="firwin",
#'       this should not be used.
#' @param l_trans_bandwidth Width of the transition band at the low cut-off frequency in Hz (high pass or cutoff 1 in bandpass). Can be "auto" (default) to use a multiple of `l_freq`, min(max(l_freq * 0.25, 2), l_freq). Only used for `method="fir"`.
#' @param h_trans_bandwidth Width of the transition band at the high cut-off frequency in Hz (low pass or cutoff 2 in bandpass). Can be "auto" (default in 0.14) to use a multiple of `h_freq`,  min(max(h_freq * 0.25, 2.), info["sampling_rate"] / 2. - h_freq) Only used for `method="fir"`.
#' @param method "fir" will use overlap-add FIR filtering, "iir" will use IIR forward-backward filtering (via filtfilt).
#' @param iir_params Dictionary of parameters to use for IIR filtering. See mne.filter.construct_iir_filter for details. If iir_params is None and method="iir", 4th order Butterworth will be used.
#' @param phase Phase of the filter, only used if ``method="fir"``. By default, a symmetric linear-phase FIR filter is constructed. If ``phase="zero"`` (default), the delay of this filter is compensated for. If ``phase=="zero-double"``, then this filter is applied twice, once forward, and once backward. If "minimum", then a minimum-phase, causal filter will be used.
#' @param fir_window The window to use in FIR design, can be "hamming" (default), "hann", or "blackman".
#' @param fir_design Can be "firwin" (default) to use :func:`scipy.signal.firwin`, or "firwin2" to use :func:`scipy.signal.firwin2`. "firwin" uses a time-domain design technique that generally gives improved attenuation using fewer samples than "firwin2".
#' @return Will be an array of FIR coefficients for method="fir", and dict
#'     with IIR parameters for method="iir".
#'
#'
#' The -6 dB point for all filters is in the middle of the transition band.
#' **Band-pass filter**
#' The frequency response is (approximately) given by::
#'    1-|               ----------
#'      |             /|         | \
#'  |H| |            / |         |  \
#'      |           /  |         |   \
#'      |          /   |         |    \
#'    0-|----------    |         |     --------------
#'      |         |    |         |     |            |
#'      0        Fs1  Fp1       Fp2   Fs2          Nyq
#' Where:
#'     * Fs1 = Fp1 - l_trans_bandwidth in Hz
#'     * Fs2 = Fp2 + h_trans_bandwidth in Hz
#' **Band-stop filter**
#' The frequency response is (approximately) given by::
#'     1-|---------                   ----------
#'       |         \                 /
#'   |H| |          \               /
#'       |           \             /
#'       |            \           /
#'     0-|             -----------
#'       |        |    |         |    |        |
#'       0       Fp1  Fs1       Fs2  Fp2      Nyq
#' Where ``Fs1 = Fp1 + l_trans_bandwidth`` and
#' ``Fs2 = Fp2 - h_trans_bandwidth``.
#' Multiple stop bands can be specified using arrays.
#' **Low-pass filter**
#' The frequency response is (approximately) given by::
#'     1-|------------------------
#'       |                        \
#'   |H| |                         \
#'       |                          \
#'       |                           \
#'     0-|                            ----------------
#'       |                       |    |              |
#'       0                      Fp  Fstop           Nyq
#' Where ``Fstop = Fp + trans_bandwidth``.
#' **High-pass filter**
#' The frequency response is (approximately) given by::
#'     1-|             -----------------------
#'       |            /
#'   |H| |           /
#'       |          /
#'       |         /
#'     0-|---------
#'       |        |    |                     |
#'       0      Fstop  Fp                   Nyq
#' Where ``Fstop = Fp - trans_bandwidth``.
#' @noRd
create_filter <- function(data,
                          sampling_rate = NULL,
                          l_freq = NULL,
                          h_freq = NULL,
                          method = "fir",
                          config = list()) {



  config_names <- names(config)
  srate <- sampling_rate
  #srate <- 500
  if(!is.null(h_freq) && is.na(h_freq)) h_freq <- NULL
  if(!is.null(l_freq) && is.na(l_freq)) l_freq <- NULL
  if (is.null(srate) || srate < 0) stop("sampling_rate must be positive")
  # If no data specified, sanity checking will be skipped
  if (!is.null(h_freq) & any(h_freq > srate / 2.)) {
    stop(
      "h_freq ", h_freq, " must be less than the Nyquist ",
      "frequency ", srate / 2.
    )
  }
  if (!is.null(l_freq) & all(l_freq == 0)) l_freq <- NULL


  ## Defaults
  if (!is.null(l_freq) && !is.null(h_freq) && l_freq > h_freq) {
    type <- "stop"
    h_temp <- l_freq
    l_freq <- h_freq
    h_freq <- h_temp

    if(options()$eeguana.verbose)
      message("Setting up band-stop filter from ", h_freq, " - ", l_freq, " Hz")
  } else if (!is.null(l_freq) && !is.null(h_freq) && l_freq < h_freq) {
    type <- "pass"
    if(options()$eeguana.verbose)
      message("Setting up band-pass filter from ", l_freq, " - ", h_freq, " Hz")
  } else if (!is.null(l_freq)) {
    type <- "high" # pass
    if(options()$eeguana.verbose)
      message("Setting up high-pass filter at ", l_freq, " Hz")
  } else if (!is.null(h_freq)) {
    type <- "low" # pass
    if(options()$eeguana.verbose)
      message("Setting up low-pass filter at ", h_freq, " Hz")
  } else {
    stop("Both freq can't be NULL")
  }

  if(method == "fir"){

    phase <- "zero"
    filter_length <- "auto"
    fir_window <- "hamming"
    fir_design <- "firwin"
    is_arg_recognizable(config_names, c("l_trans_bandwidth", "h_trans_bandwidth"),   pre_msg = "passing unknown arguments for fir method: ", call. = FALSE)

    if(is.null(config$l_trans_bandwidth)){
      if(!is.null(l_freq)) stop("Missing `l_trans_bandwidth`", call. = FALSE)
      l_trans_bandwidth <- Inf
  } else if (config$l_trans_bandwidth == "auto") {
      l_trans_bandwidth <- min(max(l_freq * 0.25, 2), l_freq)
      if(options()$eeguana.verbose)
        message("Width of the transition band at the low cut-off frequency is ",
                l_trans_bandwidth, " Hz" )
    } else {
    l_trans_bandwidth <- config$l_trans_bandwidth
    }

    if(is.null(config$h_trans_bandwidth)){
      if(!is.null(h_freq)) stop("Missing `h_trans_bandwidth`", call. = FALSE)
      h_trans_bandwidth <- Inf
    } else if (config$h_trans_bandwidth == "auto") {
      h_trans_bandwidth <- min(max(0.25 * h_freq, 2.), srate / 2. - h_freq)
      if(options()$eeguana.verbose)
        message("Width of the transition band at the high cut-off frequency is ",h_trans_bandwidth, " Hz" )
    } else {
      h_trans_bandwidth <- config$h_trans_bandwidth
    }

    mult_fact <- if (fir_design == "firwin2") 2 else 1
    length_factor <- list(hann = 3.1, hamming = 3.3, blackman = 5)
    filter_length <- max(as.integer(round(length_factor[[fir_window]] * srate * mult_fact /
                                            min(h_trans_bandwidth, l_trans_bandwidth))), 1)
    if (fir_design == "firwin") {
      filter_length <- filter_length + (filter_length - 1) %% 2
    }
    l_stop <- l_freq - l_trans_bandwidth
    h_stop <- h_freq + h_trans_bandwidth


    ## LOW PASS:
    if (type == "low") {

      f_pass <- f_stop <- h_freq # iir
      freq <- c(0, h_freq, h_stop) # 0, f_p, f_s
      gain <- c(1, 1, 0)
      if (h_stop != srate / 2) {
        freq <- c(freq, srate / 2)
        gain <- c(gain, 0)
      }
      ## HIGH PASS
    } else if (type == "high") {

      f_pass <- f_stop <- l_freq # iir
      freq <- c(l_stop, l_freq, srate / 2) # stop, pass,.._
      gain <- c(0, 1, 1)
      if (l_stop != 0) {
        freq <- c(0, freq)
        gain <- c(0, gain)
      }
    } else if (type == "pass") {
      f_pass <- f_stop <- c(l_freq, h_freq) # iir
      freq <- c(l_stop, l_freq, h_freq, h_stop) # f_s1, f_p1, f_p2, f_s2
      gain <- c(0, 1, 1, 0)
      if (h_stop != srate / 2) {
        freq <- c(freq, srate / 2)
        gain <- c(gain, 0)
      }
      if (l_stop != 0) {
        freq <- c(0, freq)
        gain <- c(0, gain)
      }
    } else if (type == "stop") {
      if (length(l_freq) != length(h_freq)) {
        stop("l_freq and h_freq must be the same length")
      }


      ## Note: order of outputs is intentionally switched here!
      ## data, srate, f_s1, f_s2, f_p1, f_p2, filter_length, phase, \
      ## fir_window, fir_design = _triage_filter_params(
      ##                 data, srate, h_freq, l_freq, h_trans_bandwidth,
      ##                 l_trans_bandwidth, filter_length, method, phase,
      ##                 fir_window, fir_design, bands="arr", reverse=True)
      l_stop <- l_stop + l_trans_bandwidth
      l_freq <- l_freq + l_trans_bandwidth
      h_stop <- h_stop - h_trans_bandwidth
      h_freq <- h_freq - h_trans_bandwidth
      f_pass <- f_stop <- c(l_freq, h_freq) ## iir

      freq <- c(l_stop, l_freq, h_freq, h_stop)
      gain <- c(
        rep(1, length(l_stop)),
        rep(0, length(l_freq)),
        rep(0, length(h_freq)),
        rep(1, length(h_stop))
      )
      order <- order(freq)
      freq <- freq[order]
      gain <- gain[order]
      if (freq[1] != 0) {
        freq <- c(0., freq)
        gain <- c(1, gain)
      }
      if (freq[length(freq)] != srate / 2.) {
        freq <- c(freq, srate / 2.)
        gain <- c(gain, 1)
      }
      if (any(abs(diff(gain, differences = 2)) > 1)) {
        stop(
          "Stop bands are not sufficiently ",
          "separated."
        )
      }
    }

    construct_fir_filter(
      srate, freq, gain, filter_length, phase,
      fir_window, fir_design
    )
  } else if(method == "iir"){
    iir_params_names <- c("type", "b", "a", "sos", "output", "order", "gpass", "gstop", "rp", "rs", "padlen")
    is_arg_recognizable(config_names,iir_params_names,   pre_msg = "passing unknown arguments for fir method: ", call. = FALSE)
    construct_iir_filter(config[names(config) %in% iir_params_names], f_pass = c(l_freq, h_freq), f_stop = c(l_freq, h_freq) , srate, type)
  } else {
    stop("Invalid method, only iir and fir are possible.", call. = FALSE)
  }
}

next_fast_len <- function(target) {
  ## """
  ## Find the next fast size of input data to `fft`, for zero-padding, etc.
  ## SciPy's FFTPACK has efficient functions for radix {2, 3, 4, 5}, so this
  ## returns the next composite of the prime factors 2, 3, and 5 which is
  ## greater than or equal to `target`. (These are also known as 5-smooth
  ## numbers, regular numbers, or Hamming numbers.)
  ## Parameters
  ## ----------
  ## target : int
  ##     Length to start searching from.  Must be a positive integer.
  ## Returns
  ## -------
  ## out : int
  ##     The first 5-smooth number greater than or equal to `target`.
  ## Notes
  ## -----
  ## Copied from SciPy with minor modifications.
  ## """
  hams <- c(
    8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36, 40, 45, 48,
    50, 54, 60, 64, 72, 75, 80, 81, 90, 96, 100, 108, 120, 125, 128,
    135, 144, 150, 160, 162, 180, 192, 200, 216, 225, 240, 243, 250,
    256, 270, 288, 300, 320, 324, 360, 375, 384, 400, 405, 432, 450,
    480, 486, 500, 512, 540, 576, 600, 625, 640, 648, 675, 720, 729,
    750, 768, 800, 810, 864, 900, 960, 972, 1000, 1024, 1080, 1125,
    1152, 1200, 1215, 1250, 1280, 1296, 1350, 1440, 1458, 1500, 1536,
    1600, 1620, 1728, 1800, 1875, 1920, 1944, 2000, 2025, 2048, 2160,
    2187, 2250, 2304, 2400, 2430, 2500, 2560, 2592, 2700, 2880, 2916,
    3000, 3072, 3125, 3200, 3240, 3375, 3456, 3600, 3645, 3750, 3840,
    3888, 4000, 4050, 4096, 4320, 4374, 4500, 4608, 4800, 4860, 5000,
    5120, 5184, 5400, 5625, 5760, 5832, 6000, 6075, 6144, 6250, 6400,
    6480, 6561, 6750, 6912, 7200, 7290, 7500, 7680, 7776, 8000, 8100,
    8192, 8640, 8748, 9000, 9216, 9375, 9600, 9720, 10000
  )

  if (target <= 6) return(target)

  # Quickly check if it's already a power of 2
  if (!(bitwAnd(target, target - 1))) return(target)

  # Get result quickly for small sizes, since FFT itself is similarly fast.
  if (target <= max(hams)) {
    return(hams[findInterval(target - 1, hams) + 1])
  }
  match <- Inf # Anything found will be smaller
  p5 <- 1

  bit_length <- function(x) {
    bits <- intToBits(x) %>% as.numeric()
    length(bits) - which.max(rev(bits)) + 1
  }
  ## message("starts loop")
  while (p5 < target) {
    p35 <- p5
    while (p35 < target) {
      # Ceiling integer division, avoiding conversion to float
      # (quotient = ceil(target / p35))
      quotient <- -(-target %/% p35)
      p2 <- 2^bit_length(as.integer(quotient - 1))

      N <- p2 * p35
      if (N == target) {
        message("ends 1")
        return(N)
      } else if (N < match) {
        match <- N
      }
      p35 <- p35 * 3
      if (p35 == target) {
        message("ends 2")
        return(p35)
      }
    }
    if (p35 < match) match <- p35
    p5 <- p5 * 5
    if (p5 == target) {
      message("ends 3")
      return(p5)
    }
  }

  if (p5 < match) match <- p5
  return(match)
}

fft_multiply_repeated <- function(x, h, n_fft = NULL) {
  ## """Do FFT multiplication by a filter function (possibly using CUDA).
  ## Parameters
  ## ----------
  ## h_fft : 1-d array or gpuarray
  ##     The filtering array to apply.
  ## x : 1-d array
  ##     The array to filter.
  ## n_fft : int
  ##     The number of points in the FFT.
  ## cuda_dict : dict
  ##     Dictionary constructed using setup_cuda_multiply_repeated().
  ## Returns
  ## -------
  ## x : 1-d array
  ##     Filtered version of x.
  ## """
  x_fft <- rfft(x, n_fft) * rfft(h, n_fft)
  irfft(x_fft, n_fft)
}

smart_pad <- function(x, n_pad, pad = "reflect_limited") {
  ## """Pad vector x."""
  if (length(n_pad) != 2) {
    stop("n_pad should have two dimensions", call. = FALSE)
  } else if (all(n_pad == 0)) {
    return(x)
  } else if (any(n_pad < 0)) {
    stop("n_pad must be non-negative", call. = FALSE)
  }
  if (pad == "reflect_limited") {
    ## need to pad with zeros if len(x) <= npad
    l_z_pad <- rep(0, max(n_pad[1] - length(x) + 1, 0))
    r_z_pad <- rep(0, max(n_pad[2] - length(x) + 1, 0))

    return(c(
      l_z_pad,
      2 * x[1] - x[seq.int(min(length(x), n_pad[1] + 1), to = 2, by = -1)],
      x,
      2 * x[length(x)] - x[seq.int(length(x) - 1, to = max(1, length(x) - n_pad[1]), by = -1)],
      r_z_pad
    ))
  } else {
    stop("Other paddings are not implemented", call. = FALSE)
    ## return np.pad(x, (tuple(n_pad),), pad)
  }
}

#' Filter the signal x using h with overlap-add FFTs.
#'
#' @param x 1 dimension, Signals to filter.
#' @param h  1d array.   Filter impulse response (FIR filter coefficients).
#'  Must be odd length     if phase == "linear".
#' @param n_fft int,  Length of the FFT. If None, the best size is determined automatically.
#' @param phase  str. If "zero", the delay for the filter is compensated (and it must be
#'   an odd-length symmetric filter). If "linear", the response is
#'   uncompensated. If "zero-double", the filter is applied in the
#'   forward and reverse directions. If "minimum", a minimum-phase
#'   filter will be used.
#' @param pad str. Padding type for ``_smart_pad``.
#'
#' @return array, shape (n_signals, n_times)
#' @noRd
#'
overlap_add_filter <- function(x, h, n_fft = NULL, phase = "zero",
                               pad = "reflect_limited") {
  ## """
  ## Parameters
  ## ----------
  ## picks : list | None
  ##     See calling functions.
  ## n_jobs : int | str
  ##     Number of jobs to run in parallel. Can be "cuda" if ``cupy``
  ##     is installed properly.
  ## copy : bool
  ##     If True, a copy of x, filtered, is returned. Otherwise, it operates
  ##     on x in place.
  ## Returns
  ## -------
  ## """
  # Extend the signal by mirroring the edges to reduce transient filter
  # response

  # TODO?
  ## _check_zero_phase_length(len(h), phase)
  if (length(h) == 1) {
    if (phase == "zero-double") {
      return(x * h^2)
    } else {
      return(x * h)
    }
  }
  n_edge <- max(min(length(h), length(x)) - 1, 0)
  ## message("Smart-padding with,",n_edge," samples")
  n_x <- length(x) + 2 * n_edge

  if (phase == "zero-double") {
    ## h = np.convolve(h, h[::-1])
    ## h <- linear_convolve(h, rev(h)) #TODO: see how to implement in R
    stop("phase zero-double not implemented")
  }

  ## Determine FFT length to use
  min_fft <- 2 * length(h) - 1
  if (is.null(n_fft)) {
    max_fft <- n_x
    if (max_fft >= min_fft) {
      ## cost function based on number of multiplications
      N <- 2^seq.int(
        ceiling(log2(min_fft)),
        ceiling(log2(max_fft))
      )
      cost <- (ceiling(n_x / (N - length(h) + 1)) *
        N * (log2(N) + 1)) +
        ## add a heuristic term to prevent too-long FFT"s which are slow
        ## (not predicted by mult. cost alone, 4e-5 exp. determined)
        4e-5 * N * n_x

      n_fft <- N[which.min(cost)]
    } else {
      ## Use only a single block
      n_fft <- next_fast_len(as.integer(min_fft))
    }
  }
  ## message("FFT block length :", n_fft)
  if (n_fft < min_fft) {
    stop(
      "n_fft is too short, has to be at least ",
      "2 * len(h) - 1,", min_fft, ", got", n_fft
    )
  }

  ## x = _1d_overlap_filter(x, len(h), n_edge, phase,
  ## cuda_dict, pad, n_fft)
  ## pad to reduce ringing
  x_ext <- smart_pad(x, c(n_edge, n_edge), pad)
  ## works x2 = mne$cuda$`_smart_pad`(np$array(x), c(n_edge,n_edge) %>% as.integer, pad)
  n_x <- length(x_ext)
  x_filtered <- rep(0, n_x)
  n_h <- length(h)
  n_seg <- n_fft - n_h + 1
  n_segments <- as.integer(ceiling(n_x / n_seg))
  if (phase %in% c("zero", "zero-double")) {
    shift <- (n_h - 1) %/% 2 + n_edge
  } else {
    shift <- n_edge
  }
  ## warning("remove dict")
  ## cuda_dict <- dict(n_fft=n_fft,
  ##                   rfft=np$fft$rfft,
  ##                   irfft=np$fft$irfft,
  ##                   h_fft=np$fft$rfft(h, n=n_fft))

  for (seg_idx in (seq_len(n_segments) - 1)) {
    start <- seg_idx * n_seg
    stop <- (seg_idx + 1) * n_seg
    seg <- x_ext[seq.int(start + 1, min(stop, n_x))]
    seg <- c(seg, rep(0, n_fft - length(seg)))
    prod <- fft_multiply_repeated(seg, h, n_fft) #
    ## good, prod2= mne$cuda$`_fft_multiply_repeated`(seg, cuda_dict2) %>% c()
    start_filt <- max(0, start - shift)
    stop_filt <- min(start - shift + n_fft, n_x)
    start_prod <- max(0, shift - start)
    stop_prod <- start_prod + stop_filt - start_filt
    x_filtered[seq.int(start_filt + 1, stop_filt)] <- x_filtered[seq.int(start_filt + 1, stop_filt)] + prod[seq.int(start_prod + 1, stop_prod)]
  }
  ## Remove mirrored edges that we added and cast (n_edge can be zero)
  x_filtered[seq.int(from = 1, to = length(x))]
}
