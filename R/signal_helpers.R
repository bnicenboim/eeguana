## These functions are adapted from python scipy
sinc <- function(x) {
  ifelse(x == 0, 1, sin(pi * x) / (pi * x))
}

ifft <- function(x, N = NULL) {
  if (is.null(N) || N == length(x)) {
    x
  } else if (N < length(x)) {
    x <- x[seq.int(1, N)]
  } else {
    x <- c(x, rep(0, N - length(x)))
  }
  stats::fft(x, inverse = TRUE) / length(x)
}


rfft <- function(x, N = NULL) {
  if (is.null(N) || N == length(x)) {
    x
  } else if (N < length(x)) {
    x <- x[seq.int(1, N)]
  } else {
    x <- c(x, rep(0, N - length(x)))
  }

  stats::fft(x)[1:(floor(length(x) / 2) + 1)]
}

irfft <- function(x, N = NULL) {
  n <- 2 * (length(x) - 1)
  if (is.null(N)) {
    N <- n
  } else if (N < n) {
    lenx <- N / 2 + 1
    x <- x[seq_len(lenx)]
  }
  xn <- vector("numeric", N)
  xn[seq_len(length(x))] <- x
  s <- N - length(x) + 1
  xn[seq.int(length(x) + 1, N, by = 1)] <- Conj(xn[seq(s, to = 2, by = -1)])
  Re(ifft(xn))
}


hamming <- function(M, sym = TRUE) {
  ## """Return a Hamming window.
  ## The Hamming window is a taper formed by using a raised cosine with
  ## non-zero endpoints, optimized to minimize the nearest side lobe.
  ## Parameters
  ## ----------
  ## M : int
  ##     Number of points in the output window. If zero or less, an empty
  ##     array is returned.
  ## sym : bool, optional
  ##     When True (default), generates a symmetric window, for use in filter
  ##     design.
  ##     When False, generates a periodic window, for use in spectral analysis.
  ## Returns
  ## -------
  ## w : ndarray
  ##     The window, with the maximum value normalized to 1 (though the value 1
  ##     does not appear if `M` is even and `sym` is True).
  ## Notes
  ## -----
  ## The Hamming window is defined as
  ## .. math::  w(n) = 0.54 - 0.46 \cos\left(\frac{2\pi{n}}{M-1}\right)
  ##            \qquad 0 \leq n \leq M-1
  ## The Hamming was named for R. W. Hamming, an associate of J. W. Tukey and
  ## is described in Blackman and Tukey. It was recommended for smoothing the
  ## truncated autocovariance function in the time domain.
  ## Most references to the Hamming window come from the signal processing
  ## literature, where it is used as one of many windowing functions for
  ## smoothing values.  It is also known as an apodization (which means
  ## "removing the foot", i.e. smoothing discontinuities at the beginning
  ## and end of the sampled signal) or tapering function.
  ## References
  ## ----------
  ## .. [1] Blackman, R.B. and Tukey, J.W., (1958) The measurement of power
  ##        spectra, Dover Publications, New York.
  ## .. [2] E.R. Kanasewich, "Time Sequence Analysis in Geophysics", The
  ##        University of Alberta Press, 1975, pp. 109-110.
  ## .. [3] Wikipedia, "Window function",
  ##        http://en.wikipedia.org/wiki/Window_function
  ## .. [4] W.H. Press,  B.P. Flannery, S.A. Teukolsky, and W.T. Vetterling,
  ##        "Numerical Recipes", Cambridge University Press, 1986, page 425.
  ## Examples
  ## --------
  ## Plot the window and its frequency response:
  ## >>> from scipy import signal
  ## >>> from scipy.fftpack import fft, fftshift
  ## >>> import matplotlib.pyplot as plt
  ## >>> window = signal.hamming(51)
  ## >>> plt.plot(window)
  ## >>> plt.title("Hamming window")
  ## >>> plt.ylabel("Amplitude")
  ## >>> plt.xlabel("Sample")
  ## >>> plt.figure()
  ## >>> A = stats::fft(window, 2048) / (len(window)/2.0)
  ## >>> freq = np.linspace(-0.5, 0.5, len(A))
  ## >>> response = 20 * np.log10(np.abs(fftshift(A / abs(A).max())))
  ## >>> plt.plot(freq, response)
  ## >>> plt.axis([-0.5, 0.5, -120, 0])
  ## >>> plt.title("Frequency response of the Hamming window")
  ## >>> plt.ylabel("Normalized magnitude [dB]")
  ## >>> plt.xlabel("Normalized frequency [cycles per sample]")
  ## """
  ## # Docstring adapted from NumPy's hamming function

  if (M < 1) return(numeric(0))
  if (M == 1) return(1)
  odd <- M %% 2
  if (!sym && !odd) M <- M + 1

  n <- seq.int(0, M - 1)
  w <- 0.54 - 0.46 * cos(2.0 * pi * n / (M - 1))
  if (!sym && !odd) w <- w[-length(w)]
  return(w)
}

get_window <- function(N, window = "hamming", fftbins = TRUE) {
  ## Return a window.
  ## Parameters
  ## ----------
  ## window : string, float, or tuple
  ##     The type of window to create. See below for more details.
  ## Nx : int
  ##     The number of samples in the window.
  ## fftbins : bool, optional
  ##     If True, create a "periodic" window ready to use with ifftshift
  ##     and be multiplied by the result of an fft (SEE ALSO fftfreq).
  sym <- !fftbins
  if (window %in% c("hamming", "hamm", "ham")) {
    winfunc <- hamming
  } else {
    stop("Unrecognized window")
  }
  ## TODO add more windows
  winfunc(N, sym)
}


kaiser_beta <- function(a) {
  ##  """Compute the Kaiser parameter `beta`, given the attenuation `a`.
  ## Parameters
  ## ----------
  ## a : float
  ##     The desired attenuation in the stopband and maximum ripple in
  ##     the passband, in dB.  This should be a *positive* number.
  ## Returns
  ## -------
  ## beta : float
  ##     The `beta` parameter to be used in the formula for a Kaiser window.
  ## References
  ## ----------
  ## Oppenheim, Schafer, "Discrete-Time Signal Processing", p.475-476.
  ## Examples
  ## --------
  ## Suppose we want to design a lowpass filter, with 65 dB attenuation
  ## in the stop band.  The Kaiser window parameter to be used in the
  ## window method is computed by `kaiser_beta(65)`:
  ## >>> from scipy.signal import kaiser_beta
  ## >>> kaiser_beta(65)
  ## 6.20426
  ## """
  if (a > 50) {
    0.1102 * (a - 8.7)
  } else if (a > 21) {
    0.5842 * (a - 21)^0.4 + 0.07886 * (a - 21)
  } else {
    0.0
  }
}

kaiser_atten <- function(N, width) {
  ## """Compute the attenuation of a Kaiser FIR filter.
  ## Given the number of taps `N` and the transition width `width`, compute the
  ## attenuation `a` in dB, given by Kaiser's formula:
  ##     a = 2.285 * (N - 1) * pi * width + 7.95
  ## Parameters
  ## ----------
  ## numtaps : int
  ##     The number of taps in the FIR filter.
  ## width : float
  ##     The desired width of the transition region between passband and
  ##     stopband (or, in general, at any discontinuity) for the filter,
  ##     expressed as a fraction of the Nyquist frequency.
  ## Returns
  ## -------
  ## a : float
  ##     The attenuation of the ripple, in dB.
  ## See Also
  ## --------
  ## kaiserord, kaiser_beta
  ## Examples
  ## --------
  ## Suppose we want to design a FIR filter using the Kaiser window method
  ## that will have 211 taps and a transition width of 9 Hz for a signal that
  ## is sampled at 480 Hz.  Expressed as a fraction of the Nyquist frequency,
  ## the width is 9/(0.5*480) = 0.0375.  The approximate attenuation (in dB)
  ## is computed as follows:
  ## >>> from scipy.signal import kaiser_atten
  ## >>> kaiser_atten(211, 0.0375)
  ## 64.48099630593983
  ## """
  2.285 * (N - 1) * pi * width + 7.95
}

firwin <- function(N = NULL, cutoff = NULL, width = NULL, window = "hamming", pass_zero = TRUE,
                   scale = TRUE, fs = 2) {
  ## """
  ## FIR filter design using the window method.
  ## This function computes the coefficients of a finite impulse response
  ## filter.  The filter will have linear phase; it will be Type I if
  ## `numtaps` is odd and Type II if `numtaps` is even.
  ## Type II filters always have zero response at the Nyquist frequency, so a
  ## ValueError exception is raised if firwin is called with `numtaps` even and
  ## having a passband whose right end is at the Nyquist frequency.
  ## Parameters
  ## ----------
  ## numtaps : int
  ##     Length of the filter (number of coefficients, i.e. the filter
  ##     order + 1).  `numtaps` must be odd if a passband includes the
  ##     Nyquist frequency.
  ## cutoff : float or 1D array_like
  ##     Cutoff frequency of filter (expressed in the same units as `fs`)
  ##     OR an array of cutoff frequencies (that is, band edges). In the
  ##     latter case, the frequencies in `cutoff` should be positive and
  ##     monotonically increasing between 0 and `fs/2`.  The values 0 and
  ##     `fs/2` must not be included in `cutoff`.
  ## width : float or None, optional
  ##     If `width` is not None, then assume it is the approximate width
  ##     of the transition region (expressed in the same units as `fs`)
  ##     for use in Kaiser FIR filter design.  In this case, the `window`
  ##     argument is ignored.
  ## window : string or tuple of string and parameter values, optional
  ##     Desired window to use. See `scipy.signal.get_window` for a list
  ##     of windows and required parameters.
  ## pass_zero : bool, optional
  ##     If True, the gain at the frequency 0 (i.e. the "DC gain") is 1.
  ##     Otherwise the DC gain is 0.
  ## scale : bool, optional
  ##     Set to True to scale the coefficients so that the frequency
  ##     response is exactly unity at a certain frequency.
  ##     That frequency is either:
  ##     - 0 (DC) if the first passband starts at 0 (i.e. pass_zero
  ##       is True)
  ##     - `fs/2` (the Nyquist frequency) if the first passband ends at
  ##       `fs/2` (i.e the filter is a single band highpass filter);
  ##       center of first passband otherwise
  ## fs : float, optional
  ##     The sampling frequency of the signal.  Each frequency in `cutoff`
  ##     must be between 0 and ``fs/2``.  Default is 2.
  ## Returns
  ## -------
  ## h : (numtaps,) ndarray
  ##     Coefficients of length `numtaps` FIR filter.
  ## Raises
  ## ------
  ## ValueError
  ##     If any value in `cutoff` is less than or equal to 0 or greater
  ##     than or equal to ``fs/2``, if the values in `cutoff` are not strictly
  ##     monotonically increasing, or if `numtaps` is even but a passband
  ##     includes the Nyquist frequency.
  ## See Also
  ## --------
  ## firwin2
  ## firls
  ## minimum_phase
  ## remez
  ## Examples
  ## --------
  ## Low-pass from 0 to f:
  ## >>> from scipy import signal
  ## >>> numtaps = 3
  ## >>> f = 0.1
  ## >>> signal.firwin(numtaps, f)
  ## array([ 0.06799017,  0.86401967,  0.06799017])
  ## Use a specific window function:
  ## >>> signal.firwin(numtaps, f, window='nuttall')
  ## array([  3.56607041e-04,   9.99286786e-01,   3.56607041e-04])
  ## High-pass ('stop' from 0 to f):
  ## >>> signal.firwin(numtaps, f, pass_zero=False)
  ## array([-0.00859313,  0.98281375, -0.00859313])
  ## Band-pass:
  ## >>> f1, f2 = 0.1, 0.2
  ## >>> signal.firwin(numtaps, [f1, f2], pass_zero=False)
  ## array([ 0.06301614,  0.88770441,  0.06301614])
  ## Band-stop:
  ## >>> signal.firwin(numtaps, [f1, f2])
  ## array([-0.00801395,  1.0160279 , -0.00801395])
  ## Multi-band (passbands are [0, f1], [f2, f3] and [f4, 1]):
  ## >>> f3, f4 = 0.3, 0.4
  ## >>> signal.firwin(numtaps, [f1, f2, f3, f4])
  ## array([-0.01376344,  1.02752689, -0.01376344])
  ## Multi-band (passbands are [f1, f2] and [f3,f4]):
  ## >>> signal.firwin(numtaps, [f1, f2, f3, f4], pass_zero=False)
  ## array([ 0.04890915,  0.91284326,  0.04890915])
  ## """
  # The major enhancements to this function added in November 2010 were
  # developed by Tom Krauss (see ticket #902).

  nyq <- fs / 2


  # Check for invalid input.
  ## if cutoff.ndim > 1:
  ##     raise ValueError("The cutoff argument must be at most "
  ##                      "one-dimensional.")
  if (is.null(cutoff)) stop("At least one cutoff frequency must be given.", call. = FALSE)
  if (any(min(cutoff) <= 0) || any(max(cutoff) >= fs / 2)) stop("Invalid cutoff frequency: frequencies must be greater than 0 and less than fs/2.", call. = FALSE)
  ## if np.any(np.diff(cutoff) <= 0):
  ##     raise ValueError("Invalid cutoff frequencies: the frequencies "
  ##                      "must be strictly increasing.")

  if (!is.null(width)) {
    # A width was given.  Find the beta parameter of the Kaiser window
    # and set `window`.  This overrides the value of `window` passed in.
    atten <- kaiser_atten(N, width / nyq)
    beta <- kaiser_beta(atten)
    window <- list("kaiser", beta)
  }

  ### pass_nyquist = bool(cutoff.size & 1) ^ pass_zero

  pass_nyquist <- xor(length(cutoff) == 1, pass_zero)
  if (pass_nyquist && N %% 2 == 0) {
    stop("A filter with an even number of coefficients must ",
      "have zero response at the Nyquist frequency.",
      call. = FALSE
    )
  }


  bands <- (cutoff / nyq) %>%
    # Insert 0 and/or 1 at the ends of cutoff so that the length of cutoff
    # is even, and each pair in cutoff corresponds to passband.
    ## cutoff = np.hstack(([0.0] * pass_zero, cutoff, [1.0] * pass_nyquist))
    c(rep(0, pass_zero), ., rep(1, pass_nyquist)) %>%
    ## `bands` is a 2D array; each row gives the left and right edges of
    # a passband.
    matrix(ncol = 2, byrow = TRUE)

  win <- get_window(N, window = "hamming", fftbins = FALSE)
  ## Build up the coefficients.
  alpha <- 0.5 * (N - 1)
  m <- seq.int(0, N - 1) - alpha
  h <- {
    (bands[, 2] %>% map_matr(~ .x * sinc(.x %*% m)) %>% colSums()) -
      (bands[, 1] %>% map_matr(~ .x * sinc(.x %*% m)) %>% colSums())
  } %>%
    # apply the window function.
    {
      . * win
    }



  ## Now handle scaling if desired.
  if (scale) {
    if (bands[1, 1] == 0) {
      scale_frequency <- 0
    } else if (bands[1, 2] == 1) {
      scale_frequency <- 1
    } else {
      scale_frequency <- .5 * (bands[1, 1] + bands[1, 2])
    }
    c <- cos(pi * m * scale_frequency)
    s <- sum(h * c)
    h <- h / s
  }
  return(h)
}
