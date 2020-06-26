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


create_filter <- function(data,
                          sampling_rate = NULL,
                          l_freq = NULL,
                          h_freq = NULL,
                          config = list()) {
  #if (length(config) == 0) {
    filter_length <- "auto"
    l_trans_bandwidth <- "auto"
    if(!is.null(config$l_trans_bandwidth)) l_trans_bandwidth <- config$l_trans_bandwidth
    h_trans_bandwidth <- "auto"
    if(!is.null(config$h_trans_bandwidth)) h_trans_bandwidth <- config$h_trans_bandwidth
    method <- "fir"
    iir_params <- NULL
    phase <- "zero"
    fir_window <- "hamming"
    fir_design <- "firwin"
  #} else {
  #  stop("`config` parameter of the filters is not yet implemented", call. = FALSE)
  #}
  ## """Create a FIR or IIR filter.
  ## ``l_freq`` and ``h_freq`` are the frequencies below which and above
  ## which, respectively, to filter out of the data. Thus the uses are:
  ##     * ``l_freq < h_freq``: band-pass filter
  ##     * ``l_freq > h_freq``: band-stop filter
  ##     * ``l_freq is not None and h_freq is None``: high-pass filter
  ##     * ``l_freq is None and h_freq is not None``: low-pass filter
  ## Parameters
  ## ----------
  ## data : ndarray, shape (..., n_times) | None
  ##     The data that will be filtered. This is used for sanity checking
  ##     only. If None, no sanity checking related to the length of the signal
  ##     relative to the filter order will be performed.
  ## sampling_rate : float
  ##     The sample frequency in Hz.
  ## l_freq : float | None
  ##     Low cut-off frequency in Hz. If None the data are only low-passed.
  ## h_freq : float | None
  ##     High cut-off frequency in Hz. If None the data are only
  ##     high-passed.
  ## filter_length : str | int
  ##     Length of the FIR filter to use (if applicable):
  ##     * "auto" (default): the filter length is chosen based
  ##       on the size of the transition regions (6.6 times the reciprocal
  ##       of the shortest transition band for fir_window="hamming"
  ##       and fir_design="firwin2", and half that for "firwin").
  ##     * str: a human-readable time in
  ##       units of "s" or "ms" (e.g., "10s" or "5500ms") will be
  ##       converted to that number of samples if ``phase="zero"``, or
  ##       the shortest power-of-two length at least that duration for
  ##       ``phase="zero-double"``.
  ##     * int: specified length in samples. For fir_design="firwin",
  ##       this should not be used.
  ## l_trans_bandwidth : float | str
  ##     Width of the transition band at the low cut-off frequency in Hz
  ##     (high pass or cutoff 1 in bandpass). Can be "auto"
  ##     (default) to use a multiple of ``l_freq``::
  ##         min(max(l_freq * 0.25, 2), l_freq)
  ##     Only used for ``method="fir"``.
  ## h_trans_bandwidth : float | str
  ##     Width of the transition band at the high cut-off frequency in Hz
  ##     (low pass or cutoff 2 in bandpass). Can be "auto"
  ##     (default in 0.14) to use a multiple of ``h_freq``::
  ##         min(max(h_freq * 0.25, 2.), info["sampling_rate"] / 2. - h_freq)
  ##     Only used for ``method="fir"``.
  ## method : str
  ##     "fir" will use overlap-add FIR filtering, "iir" will use IIR
  ##     forward-backward filtering (via filtfilt).
  ## iir_params : dict | None
  ##     Dictionary of parameters to use for IIR filtering.
  ##     See mne.filter.construct_iir_filter for details. If iir_params
  ##     is None and method="iir", 4th order Butterworth will be used.
  ## phase : str
  ##     Phase of the filter, only used if ``method="fir"``.
  ##     By default, a symmetric linear-phase FIR filter is constructed.
  ##     If ``phase="zero"`` (default), the delay of this filter
  ##     is compensated for. If ``phase=="zero-double"``, then this filter
  ##     is applied twice, once forward, and once backward. If "minimum",
  ##     then a minimum-phase, causal filter will be used.
  ##     .. versionadded:: 0.13
  ## fir_window : str
  ##     The window to use in FIR design, can be "hamming" (default),
  ##     "hann", or "blackman".
  ##     .. versionadded:: 0.13
  ## fir_design : str
  ##     Can be "firwin" (default) to use :func:`scipy.signal.firwin`,
  ##     or "firwin2" to use :func:`scipy.signal.firwin2`. "firwin" uses
  ##     a time-domain design technique that generally gives improved
  ##     attenuation using fewer samples than "firwin2".
  ##     ..versionadded:: 0.15
  ## %(verbose)s
  ## Returns
  ## -------
  ## filt : array or dict
  ##     Will be an array of FIR coefficients for method="fir", and dict
  ##     with IIR parameters for method="iir".
  ## See Also
  ## --------
  ## filter_data
  ## Notes
  ## -----
  ## The -6 dB point for all filters is in the middle of the transition band.
  ## **Band-pass filter**
  ## The frequency response is (approximately) given by::
  ##    1-|               ----------
  ##      |             /|         | \
  ##  |H| |            / |         |  \
  ##      |           /  |         |   \
  ##      |          /   |         |    \
  ##    0-|----------    |         |     --------------
  ##      |         |    |         |     |            |
  ##      0        Fs1  Fp1       Fp2   Fs2          Nyq
  ## Where:
  ##     * Fs1 = Fp1 - l_trans_bandwidth in Hz
  ##     * Fs2 = Fp2 + h_trans_bandwidth in Hz
  ## **Band-stop filter**
  ## The frequency response is (approximately) given by::
  ##     1-|---------                   ----------
  ##       |         \                 /
  ##   |H| |          \               /
  ##       |           \             /
  ##       |            \           /
  ##     0-|             -----------
  ##       |        |    |         |    |        |
  ##       0       Fp1  Fs1       Fs2  Fp2      Nyq
  ## Where ``Fs1 = Fp1 + l_trans_bandwidth`` and
  ## ``Fs2 = Fp2 - h_trans_bandwidth``.
  ## Multiple stop bands can be specified using arrays.
  ## **Low-pass filter**
  ## The frequency response is (approximately) given by::
  ##     1-|------------------------
  ##       |                        \
  ##   |H| |                         \
  ##       |                          \
  ##       |                           \
  ##     0-|                            ----------------
  ##       |                       |    |              |
  ##       0                      Fp  Fstop           Nyq
  ## Where ``Fstop = Fp + trans_bandwidth``.
  ## **High-pass filter**
  ## The frequency response is (approximately) given by::
  ##     1-|             -----------------------
  ##       |            /
  ##   |H| |           /
  ##       |          /
  ##       |         /
  ##     0-|---------
  ##       |        |    |                     |
  ##       0      Fstop  Fp                   Nyq
  ## Where ``Fstop = Fp - trans_bandwidth``.
  ## .. versionadded:: 0.14
  ## """
  if(!is.null(h_freq) && is.na(h_freq)) h_freq <- NULL
  if(!is.null(l_freq) && is.na(l_freq)) l_freq <- NULL
  if (is.null(sampling_rate) || sampling_rate < 0) stop("sampling_rate must be positive")
  # If no data specified, sanity checking will be skipped
  if (!is.null(h_freq) & any(h_freq > sampling_rate / 2.)) {
    stop(
      "h_freq ", h_freq, " must be less than the Nyquist ",
      "frequency ", sampling_rate / 2.
    )
  }
  if (!is.null(l_freq) & all(l_freq == 0)) l_freq <- NULL

  ## iir_params, method = _check_method(method, iir_params)

  ## No idea when this is needed:
  ## if (is.null(l_freq) & is.null(h_freq)){
  ##     ## data, sampling_rate, _, _, _, _, filter_length, phase, fir_window, \
  ##     ##     fir_design = _triage_filter_params(
  ##     ##         data, sampling_rate, None, None, None, None,
  ##     ##         filter_length
  ##           ## , method, phase, fir_window, fir_design)
  ##     if method == "iir":
  ##         out = dict() if iir_params is None else deepcopy(iir_params)
  ##         out.update(b=np.array([1.]), a=np.array([1.]))
  ##     else:
  ##         freq = [0, sampling_rate / 2.]
  ##     gain = [1., 1.]
  ##  }

  ## Defaults
  if (!is.null(l_freq) && !is.null(h_freq) && l_freq > h_freq) {
    type <- "bandstop"
    h_temp <- l_freq
    l_freq <- h_freq
    h_freq <- h_temp
  } else if (!is.null(l_freq) && !is.null(h_freq) && l_freq < h_freq) {
    type <- "bandpass"
  } else if (!is.null(l_freq)) {
    type <- "high" # pass
  } else if (!is.null(h_freq)) {
    type <- "low" # pass
  } else {
    stop("Both freq can't be NULL")
  }

  if (l_trans_bandwidth == "auto") {
    l_trans_bandwidth <- min(max(l_freq * 0.25, 2), l_freq)
    if(options()$eeguana.verbose) 
      message("Width of the transition band at the low cut-off frequency is ", 
              l_trans_bandwidth, " Hz" )
  }
  if (h_trans_bandwidth == "auto") {
    h_trans_bandwidth <- min(max(0.25 * h_freq, 2.), sampling_rate / 2. - h_freq)
    if(options()$eeguana.verbose) 
      message("Width of the transition band at the high cut-off frequency is ",h_trans_bandwidth, " Hz" )
  }
  ## if(!is.null(l_trans_bandwidth)) 
  ## if(!is.null(h_trans_bandwidth)) message("'h_trans_bandwidth' chosen to be ",h_trans_bandwidth, " Hz" )
  h_check <- if (!is.null(h_freq)) h_trans_bandwidth else Inf
  l_check <- if (!is.null(l_freq)) l_trans_bandwidth else Inf

  mult_fact <- if (fir_design == "firwin2") 2 else 1
  length_factor <- list(hann = 3.1, hamming = 3.3, blackman = 5)
  filter_length <- max(as.integer(round(length_factor[[fir_window]] * sampling_rate * mult_fact /
    min(h_check, l_check))), 1)
  if (fir_design == "firwin") {
    filter_length <- filter_length + (filter_length - 1) %% 2
  }
  l_stop <- l_freq - l_trans_bandwidth
  h_stop <- h_freq + h_trans_bandwidth


  ## LOW PASS:
  if (type == "low") {
    message("Setting up low-pass filter at ", h_freq, " Hz")
    f_pass <- f_stop <- h_freq # iir
    freq <- c(0, h_freq, h_stop) # 0, f_p, f_s
    gain <- c(1, 1, 0)
    if (h_stop != sampling_rate / 2) {
      freq <- c(freq, sampling_rate / 2)
      gain <- c(gain, 0)
    }
    ## HIGH PASS
  } else if (type == "high") {
    message("Setting up high-pass filter at ", l_freq, " Hz")
    f_pass <- f_stop <- l_freq # iir
    freq <- c(l_stop, l_freq, sampling_rate / 2) # stop, pass,.._
    gain <- c(0, 1, 1)
    if (l_stop != 0) {
      freq <- c(0, freq)
      gain <- c(0, gain)
    }
  } else if (type == "bandpass") {
    message("Setting up band-pass filter from ", l_freq, " - ", h_freq, " Hz")
    f_pass <- f_stop <- c(l_freq, h_freq) # iir
    freq <- c(l_stop, l_freq, h_freq, h_stop) # f_s1, f_p1, f_p2, f_s2
    gain <- c(0, 1, 1, 0)
    if (h_stop != sampling_rate / 2) {
      freq <- c(freq, sampling_rate / 2)
      gain <- c(gain, 0)
    }
    if (l_stop != 0) {
      freq <- c(0, freq)
      gain <- c(0, gain)
    }
  } else if (type == "bandstop") {
    ## This could possibly be removed after 0.14 release, but might
    ## as well leave it in to sanity check notch_filter
    if (length(l_freq) != length(h_freq)) {
      stop("l_freq and h_freq must be the same length")
    }
    message("Setting up band-stop filter from ", h_freq, " - ", l_freq, " Hz")

    ## Note: order of outputs is intentionally switched here!
    ## data, sampling_rate, f_s1, f_s2, f_p1, f_p2, filter_length, phase, \
    ## fir_window, fir_design = _triage_filter_params(
    ##                 data, sampling_rate, h_freq, l_freq, h_trans_bandwidth,
    ##                 l_trans_bandwidth, filter_length, method, phase,
    ##                 fir_window, fir_design, bands="arr", reverse=True)
    l_stop <- l_stop + l_trans_bandwidth
    l_freq <- l_freq + l_trans_bandwidth
    h_stop <- h_stop - h_trans_bandwidth
    h_freq <- h_freq - h_trans_bandwidth
    f_pass <- f_stop <- c(l_freq[0], h_freq[0]) ## iir

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
    if (freq[length(freq)] != sampling_rate / 2.) {
      freq <- c(freq, sampling_rate / 2.)
      gain <- c(gain, 1)
    }
    if (any(abs(diff(gain, differences = 2)) > 1)) {
      stop(
        "Stop bands are not sufficiently ",
        "separated."
      )
    }
  }
  ## if method == "iir":
  ## construct_iir_filter(iir_params, f_pass, f_stop, sampling_rate, type)
  ## if method == "fir":
  construct_fir_filter(
    sampling_rate, freq, gain, filter_length, phase,
    fir_window, fir_design
  )
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
