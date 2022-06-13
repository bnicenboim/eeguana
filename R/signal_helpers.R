## These functions are adapted from python scipy



#' @noRd
sinc <- function(x) {
  ifelse(x == 0, 1, sin(pi * x) / (pi * x))
}

#' Compute the one-dimensional inverse discrete Fourier Transform.
#'
#' This function computes the inverse of the one-dimensional n-point discrete Fourier
#'  transform computed by fft using a wrapper of `fft(z, inverse= TRUE)/length(z)` with
#'   an extra argument making it similar to   [numpy.fft.ifft](https://docs.scipy.org/doc/numpy-1.17.0/reference/generated/numpy.fft.ifft.html#numpy.fft.ifft).
#'  For a general description of the algorithm and definitions, see [fft](stats::fft) and [numpy.fft](https://docs.scipy.org/doc/numpy-1.17.0/reference/generated/numpy.fft.ifft.html#numpy.fft.ifft).
#'
#' The input should be ordered in the same way as is returned by fft, i.e.,
#' (*if I converted the indexes from python correctly*)
#'
#' * `a[1]` should contain the zero frequency term,
#' * `a[2:ceiling(n/2)]` should contain the positive-frequency terms,
#' * `a[ceiling(n/2) + 2:length(a)]` should contain the negative-frequency terms,
#' in increasing order starting from the most negative frequency.
#' * For an even number of input points, `a[ceiling(n/2)]` represents
#' the sum of the values at the positive and negative Nyquist frequencies,
#' as the two are aliased together. See see [numpy.fft](https://docs.scipy.org/doc/numpy-1.17.0/reference/generated/numpy.fft.ifft.html#numpy.fft.ifft) for details.
#'
#' **Notes**
#'
#' If the input parameter `n` is larger than the size of the input,
#' the input is padded by appending zeros at the end. Even though this is
#' the common approach, it might lead to surprising results. If a different padding
#' is desired, it must be performed before calling ifft.
#'
#' @inheritParams sig_fft
#' @return A vector
#' @export
#'
#' @examples
#'
#' a <- c(0, 4, 0, 0)
#' sig_ifft(a)
#' sig_ifft(sig_fft(a)) == a
sig_ifft <- function(x, n = NULL) {
  if (is.null(n) || n == length(x)) {
    x
  } else if (n < length(x)) {
    x <- x[seq.int(1, n)]
  } else {
    x <- c(x, rep(0, n - length(x)))
  }
  stats::fft(x, inverse = TRUE) / length(x)
}

#' Compute the one-dimensional discrete Fourier Transform.
#'
#' Computes the Discrete Fourier Transform (DFT) of an array
#' with a fast algorithm, the “Fast Fourier Transform” (FFT). Wrapper
#' of [fft](stats::fft) with an extra argument similar to python's [numpy.fft.fft](https://docs.scipy.org/doc/numpy/reference/generated/numpy.fft.fft.html#numpy.fft.fft).
#'
#' @param x A vector
#' @param n Length of the transformed axis of the output. If `n` is smaller
#'  than the length of the input, the input is cropped. If it is larger, the input
#'  is padded with zeros. If n is not given, the length of the input along
#'  the axis specified by axis is used.
#'
#'
#' @examples
#' a <- c(0, 4, 0, 0)
#' sig_fft(a)
#' @export
sig_fft <- function(x, n = NULL) {
  if (is.null(n) || n == length(x)) {
    x
  } else if (n < length(x)) {
    x <- x[seq.int(1, n)]
  } else {
    x <- c(x, rep(0, n - length(x)))
  }
  stats::fft(x) # / length(x)
}

#' Apply a digital filter forward and backward to a signal.
#'
#' Alternative to `gsignal::filtfilt` based on scipy implementation. 
#' 
#' @param x the input signal to be filtered, specified as a numeric or complex vector or matrix. If x is a matrix, each column is filtered.
#' @param b The numerator coefficient vector of the filter.
#' @param a The denominator coefficient vector of the filter. (If ``a[0]`` is not 1, then both `a` and `b` are normalized by ``a[0]``.??).
#' @param padlen The number of elements by which to extend `x` at both ends of `axis` before applying the filter.  This value must be less than than the length (or the number of rows if x is a matrix).  `padlen=0` implies no padding. The default value is ``3 * max(len(a), len(b))``. (The type of padding is always odd, as the default of scipy.signal.filtfilt)
#'
#' @noRd
sig_filtfilt <- function(x, b, a, padlen = 3 * max(length(a), length(b))) {
  #https://github.com/scipy/scipy/blob/v1.8.1/scipy/signal/_signaltools.py#L3886-L4084
  
  atx <- attributes(x)
  if(is.null(dim(x))){
    x <- matrix(x, ncol = 1)
  }

  ext <- do_padding(x, padlen)
  
  # Get the steady state of the filter's step response.
  zi = gsignal::filter_zi(filt = b,a = a) 
  
  # forward filter
  zix0 <- matrix(zi, ncol = 1) %*% ext[1, ,drop = FALSE] 
  y <- gsignal::filter(filt = b, a = a, x = ext,  zi = zix0)
  # backward filter
  ziy0 = zi %*% y$y[1, ,drop = FALSE] 
  y <- gsignal::filter(filt = b, a = a, x = apply(y$y,2, rev), zi = ziy0) # 
  y <- apply(y$y, 2 ,rev)
  y <- y[(padlen + 1):(padlen + nrow(x)),, drop= FALSE] 
  attributes(y) <- atx
  y
}


#' Apply a digital filter forward and backward to a signal using cascaded second-order sections.
#'
#' Alternative to `gsignal::filtfilt` based on scipy implementation. 
#' 
#' @param x the input signal to be filtered, specified as a numeric or complex vector or matrix. If x is a matrix, each column is filtered.
#' @param sos Matrix or array of second-order filter coefficients, must have dimensions n_sections x 6. Each row corresponds to a second-order section, with the first three columns providing the numerator coefficients and the last three providing the denominator coefficients.
#' @param padlen The number of elements by which to extend `x` at both ends before applying the filter.  This value must be less than than the length (or the number of rows if x is a matrix).  `padlen=0` implies no padding. The default value is ``3 * max(len(a), len(b))``. (The type of padding is always odd, as the default of scipy.signal.filtfilt)
#'
#' @noRd
sig_sosfiltfilt <- function(x,sos,g=1,  padlen=NULL){
# # These steps follow the same form as filtfilt with modifications
  atx <- attributes(x)
  if(is.null(dim(x))){
    x <- matrix(x, ncol = 1)
  }
  
  ext <- do_padding(x, padlen)
  Sos <- gsignal::Sos(sos,g) 
  zi <- gsignal::filter_zi(Sos)  

  #zi_a <- array(zi, dim = c(nrow(zi),ncol(x),1)) 
   # forward filter
  zix0 <- lapply(ext[1, ,drop = FALSE], function(.x) .x * zi) %>%
    unlist() %>% array( dim = c(nrow(zi),ncol(zi),ncol(x))) 
  y <- gsignal::filter(filt = Sos, x = ext,  zi = zix0)
  # backward filter
  ziy0 = lapply(y$y[1, ,drop = FALSE], function(.x) .x * zi) %>%
    unlist() %>% array( dim = c(nrow(zi),ncol(zi),ncol(x))) 
  y <- gsignal::filter(filt = Sos, x = apply(y$y,2, rev), zi = ziy0) # 
  y <- apply(y$y, 2 ,rev)
  y <- y[(padlen + 1):(padlen + nrow(x)),, drop= FALSE] 
  attributes(y) <- atx
  y
}

#' @noRd
do_padding <- function(x, padlen){
  if (padlen == 0) {
    ext <- x
  } else {
    #actually top and bottom end
    # it applies scipy.signal._arraytools.odd_ext
    left_end <- x[1,, drop = FALSE]
    left_ext <- x[seq.int(from = padlen + 1, to = 2, by = -1),, drop = FALSE]
    right_end <- x[nrow(x),, drop = FALSE]
    right_ext <- x[seq.int(from = nrow(x)-1 , to = nrow(x) - padlen, by = -1),, drop = FALSE]
    ext <- rbind(
      matrix(rep(2 * left_end,each=nrow(left_ext)),ncol=ncol(left_ext)) - left_ext,
      x,
      matrix(rep(2 * right_end,each=nrow(right_ext)),ncol=ncol(right_ext)) - right_ext
    )
  }
  ext
}

#' @noRd
#'
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

#' @noRd
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
  Re(sig_ifft(xn))
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

  if (M < 1) {
    return(numeric(0))
  }
  if (M == 1) {
    return(1)
  }
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
  h <-
    {
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

#' Emulates scipi.signal.iirfilter using signal (and gsignal package).
#'
#' @param n filter order or generic filter model
#' @param Wn critical frequencies of the filter. ‘W’ must be a scalar for low-pass and high-pass filters, and ‘W’ must be a two-element vector ‘c(low, high)’ specifying the lower and upper bands. For digital filters, ‘W’ must be between 0 and 1 where 1 is the Nyquist frequency.
#' @param rp For `cheby1`, `cheby2`, and `ellip` filters the dB of pass band ripple
#' @param rs For `ellip` filters the dB of stop band ripple
#' @param btype Filter type, one of ‘"low"’ for a low-pass filter, ‘"high"’ for a high-pass filter, ‘"stop"’ for a stop-band (band-reject) filter, or ‘"pass"’ for a pass-band filter.
#' @noRd
#'
iirfilter <- function(n, Wn, rp, rs, btype, type = c("butter", "cheby1", "cheby2", "ellip"), output = c("ba", "zpk", "sos")) {
  type <- match.arg(type)
  
  output <- match.arg(output)
  out_type <- switch(output, 
                     ba = "Arma",
                     zpk = "Zpg",
                     sos = "Sos")
  out <- switch(type,
    butter = gsignal::butter(n, w = Wn, type = btype, output = out_type),
    cheby1 = gsignal::cheby1(n, Rp = rp, w = Wn, type = btype, output = out_type),
    cheby2 = gsignal::cheby2(n, Rs = rp, w = Wn, type = btype, output = out_type),
    ellip = gsignal::ellip(n, Rp = rp, Rs = rs, w = Wn, type = btype, output = out_type),
  )

  if (output == "ba") {
    list(b = out$b, a = out$a)
  } else if (output == "zpk") {
    list(z = out$z, p = out$p, k = out$g)
  } else if (output == "sos") {
    #scipy.signal incorporates the system gain in the first section, while gsignal (and Matlab/Octave) do not.
    # https://github.com/gjmvanboxtel/gsignal/issues/1
    sos <- out$sos 
    sos[1,1:3] <- out$sos[1, 1:3] * out$g
    list(sos = sos, g = 1)
  }
}


#' Emulates scipi.signal.iirdesign using signal.
#'
#'
#' Complete IIR digital and analog filter design.
#' Given passband and stopband frequencies and gains, construct an analog or
#' digital IIR filter of minimum order for a given basic type. Return the
#' output in numerator, denominator ('ba'), pole-zero ('zpk') or second order
#' sections ('sos') form.
#' Parameters
#' ----------
#' wp, ws : float or array like, shape (2,)
#'     Passband and stopband edge frequencies. Possible values are scalars
#'     (for lowpass and highpass filters) or ranges (for bandpass and bandstop
#'     filters).
#'     For digital filters, these are in the same units as `fs`. By default,
#'     `fs` is 2 half-cycles/sample, so these are normalized from 0 to 1,
#'     where 1 is the Nyquist frequency. For example:
#'         - Lowpass:   wp = 0.2,          ws = 0.3
#'         - Highpass:  wp = 0.3,          ws = 0.2
#'         - Bandpass:  wp = [0.2, 0.5],   ws = [0.1, 0.6]
#'         - Bandstop:  wp = [0.1, 0.6],   ws = [0.2, 0.5]
#'     For analog filters, `wp` and `ws` are angular frequencies (e.g., rad/s).
#'     Note, that for bandpass and bandstop filters passband must lie strictly
#'     inside stopband or vice versa.
#' gpass : float
#'     The maximum loss in the passband (dB).
#' gstop : float
#'     The minimum attenuation in the stopband (dB).
#' analog : bool, optional
#'     When True, return an analog filter, otherwise a digital filter is
#'     returned.
#' type : str, optional
#'     The type of IIR filter to design:
#'         - Butterworth   : 'butter'
#'         - Chebyshev I   : 'cheby1'
#'         - Chebyshev II  : 'cheby2'
#'         - Cauer/elliptic: 'ellip'
#'         - Bessel/Thomson: 'bessel'
#' output : {'ba', 'zpk', 'sos'}, optional
#'     Filter form of the output:
#'         - second-order sections (recommended): 'sos'
#'         - numerator/denominator (default)    : 'ba'
#'         - pole-zero                          : 'zpk'
#'     In general the second-order sections ('sos') form  is
#'     recommended because inferring the coefficients for the
#'     numerator/denominator form ('ba') suffers from numerical
#'     instabilities. For reasons of backward compatibility the default
#'     form is the numerator/denominator form ('ba'), where the 'b'
#'     and the 'a' in 'ba' refer to the commonly used names of the
#'     coefficients used.
#'     Note: Using the second-order sections form ('sos') is sometimes
#'     associated with additional computational costs: for
#'     data-intense use cases it is therefore recommended to also
#'     investigate the numerator/denominator form ('ba').
#' fs : float, optional
#'     The sampling frequency of the digital system.
#'     .. versionadded:: 1.2.0
#' Returns
#' -------
#' b, a : ndarray, ndarray
#'     Numerator (`b`) and denominator (`a`) polynomials of the IIR filter.
#'     Only returned if ``output='ba'``.
#' z, p, k : ndarray, ndarray, float
#'     Zeros, poles, and system gain of the IIR filter transfer
#'     function.  Only returned if ``output='zpk'``.
#' sos : ndarray
#'     Second-order sections representation of the IIR filter.
#'     Only returned if ``output=='sos'``.
#' See Also
#' --------
#' butter : Filter design using order and critical points
#' cheby1, cheby2, ellip, bessel
#' buttord : Find order and critical points from passband and stopband spec
#' cheb1ord, cheb2ord, ellipord
#' iirfilter : General filter design using order and critical frequencies
#' Notes
#' -----
#' The ``'sos'`` output parameter was added in 0.16.0.
#' Examples
#' --------

#' @noRd
#'
iirdesign <- function(wp, ws, gpass, gstop, type = "ellip", output = "ba") {

  ## except KeyError as e:
  ##     raise ValueError("Invalid IIR filter type: %s" % type) from e
  ## except IndexError as e:
  ##     raise ValueError(("%s does not have order selection. Use "
  ## "iirfilter function.") % type) from e
  ## wp = atleast_1d(wp)
  ## ws = atleast_1d(ws)

  ## if wp.shape[0] != ws.shape[0] or wp.shape not in [(1,), (2,)]:
  ##     raise ValueError("wp and ws must have one or two elements each, and"
  ##                      "the same shape, got %s and %s"
  ##                      % (wp.shape, ws.shape))
  ## if wp.shape[0] == 2:
  ##     if wp[0] < 0 or ws[0] < 0:
  ##         raise ValueError("Values for wp, ws can't be negative")
  ##     elif 1 < wp[1] or 1 < ws[1]:
  ##         raise ValueError("Values for wp, ws can't be larger than 1")
  ##     elif not((ws[0] < wp[0] and wp[1] < ws[1]) or
  ##         (wp[0] < ws[0] and ws[1] < wp[1])):
  ##         raise ValueError("Passband must lie strictly inside stopband"
  ##                      " or vice versa")

  band_type <- 2 * (length(wp) - 1) + 1
  if (wp[1] >= ws[1]) {
    band_type <- band_type + 1
  }


  btype <- switch(band_type,
    "low",
    "high",
    "stop",
    "pass"
  )


  out <- switch(type,
    butter = gsignal::buttord(Wp = wp, Ws = ws, Rp = gpass, Rs = gstop),
    ellip =  gsignal::ellipord(Wp = wp, Ws = ws, Rp = gpass, Rs = gstop),
    cheby1 = gsignal::cheb1ord(Wp = wp, Ws = ws, Rp = gpass, Rs = gstop),
    stop("type can be butter, ellip or cheby1")
  )

  iirfilter(out$n, out$Wc,
    rp = gpass, rs = gstop, btype = btype,
    type = type, output = output
  )
}
