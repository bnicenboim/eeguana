check_zero_phase_length <- function(N, phase, gain_nyq=0){
        N <- as.integer(N)
        if (N %% 2 == 0){
            if (phase == "zero"){
            stop("filter_length must be odd if phase=\"zero\"", 
                 "got ", N)
            }
            else if (phase == "zero-double" && gain_nyq == 1){
                N = N+1
            }
        }
    return (N)
}

filter_attenuation <- function(h, freq, gain){
##        """Compute minimum attenuation at stop frequency."""
    filt_resp = signal::freqz(h, n=pi * freq)[[1]] %>%
        ## use amplitude respons
        abs()
    filt_resp[gain==1] <- 0
    idx <- which.max(filt_resp)
    att_db = -20 * log10(max(filt_resp[idx], 1e-20))
    att_freq = freq[idx]
    return (list(db = att_db, freq = att_freq))
}


firwin_design <- function(N, freq, gain, window = "hamming", sfreq){
     ##    """Construct a FIR filter using firwin."""
    if (freq[1] != 0) stop("First element of "freq" should be 0")
    if (length(freq) <= 1) stop("Length of frequency should be larger than 1.")
    if(length(freq) != length(gain)) stop(""freq" and "gain" should have the same length.")
    h <- rep(0,N)
    prev_freq <- freq[length(freq)]
    prev_gain <- gain[length(gain)]
    if (prev_gain == 1)  h[as.integer(N / 2)+1] <- 1  # start with "all up"
    if (!prev_gain %in% c(0, 1)) stop("Gain should end in 1 or 0")
    length_factors = dplyr::case_when(window == "hann"~3.1,
                                       window == "hamming" ~3.3,
                                       window == "blackman" ~5.0)
     for(i in seq_len(length(freq) -1) ){
            this_freq <-rev(freq)[-1][i]
            this_gain <-rev(gain)[-1][i]
            if(!this_gain %in% c(0,1)) stop()
            if(this_gain != prev_gain) {
                transition = (prev_freq - this_freq) / 2
                this_N = as.integer(round(length_factors/ transition)) %>%
                    {. + (1 - . %% 2)} ## make it odd
                if(this_N > N){
                    stop("The requested filter length ", N ," is too short ",
                         "for the requested ", transition * sfreq / 2, " Hz transition band, ",
                         "which requires ", this_N ,"  samples")
                }
                this_h <- firwin(this_N, (prev_freq + this_freq) / 2,
                                 window=window, pass_zero=TRUE, fs = freq[length(freq)]*2)
                offset <- as.integer((N - this_N) / 2)
                if(this_gain ==0){
                    h[seq(offset +1,N - offset, by=1)] = h[seq(offset +1,N - offset, by=1)]  - this_h
                } else{
                    h[seq(offset +1,N - offset, by=1)] = h[seq(offset +1,N - offset, by=1)]  + this_h
                }
            }    
            prev_gain = this_gain
            prev_freq = this_freq
     }
return(h)
}


construct_fir_filter <- function(sfreq, freq, gain, filter_length, phase="zero",
                                 fir_window = "hamming", fir_design = "firwin"){
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
    if (fir_design == "firwin"){
        fir_fun <-  purrr::partial(firwin_design,sfreq=sfreq)
    } else {
        stop("Unsupported filter_design", call. = FALSE)
    }

    # issue a warning if attenuation is less than this
    min_att_db <-if(phase == "minimum") 12 else 20

    # normalize frequencies
    n_freq = freq / (sfreq / 2.)
    if (freq[1] != 0 || n_freq[length(freq)] != 1)
        stop("freq must start at 0 and end an Nyquist,", sfreq / 2,", got ", freq[length(freq)])

    # Use overlap-add filter with a fixed length
    N <- check_zero_phase_length(filter_length, phase, gain[length(gain)])
    # construct symmetric (linear phase) filter
    if (phase == "minimum"){
        h = fir_fun(N * 2 - 1, n_freq, gain, window=fir_window)
        ##        h = minimum_phase(h)
        stop("Not supported")
    }else{
        h = fir_fun(N, n_freq, gain, window=fir_window)
    }
    if(length(h) != N){stop("Wrong size of h")}
    att <- filter_attenuation(h, n_freq, gain)

    if (phase == "zero-double"){
        att$db <- att$db  + 6
    }
    if (att$db < min_att_db){
           att$freq = att$freq *  sfreq / 2.
           warning("Attenuation at stop frequency ", att$freq, "Hz is only ", att$db, " dB. ",
             "Increase filter_length for higher attenuation.")
    }
    return (h)
}


create_filter <- function(data, sfreq=NULL, l_freq=NULL, h_freq=NULL, filter_length="auto",
                  l_trans_bandwidth="auto", h_trans_bandwidth="auto",
                  method="fir", iir_params=NULL, phase="zero",
                  fir_window="hamming", fir_design="firwin"){
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
    ## sfreq : float
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
    ##         min(max(h_freq * 0.25, 2.), info["sfreq"] / 2. - h_freq)
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
    if (is.null(sfreq) || sfreq < 0) stop("sfreq must be positive")
    # If no data specified, sanity checking will be skipped
    if (!is.null(h_freq) & any(h_freq > sfreq / 2.)){
            stop("h_freq ",h_freq, " must be less than the Nyquist ",
                 "frequency ", sfreq / 2.)
    }
    if (!is.null(l_freq) & all(l_freq == 0)) l_freq <- NULL
    
    ## iir_params, method = _check_method(method, iir_params)

    ## No idea when this is needed:
    ## if (is.null(l_freq) & is.null(h_freq)){
    ##     ## data, sfreq, _, _, _, _, filter_length, phase, fir_window, \
    ##     ##     fir_design = _triage_filter_params(
    ##     ##         data, sfreq, None, None, None, None,
    ##     ##         filter_length
    ##           ## , method, phase, fir_window, fir_design) 
    ##     if method == "iir":
    ##         out = dict() if iir_params is None else deepcopy(iir_params)
    ##         out.update(b=np.array([1.]), a=np.array([1.]))
    ##     else:
    ##         freq = [0, sfreq / 2.]
    ##     gain = [1., 1.]
    ##  }

    ## Defaults
    if(!is.null(l_freq) && !is.null(h_freq) && l_freq > h_freq) {
        type <- "bandstop"
        h_temp <- l_freq
        l_freq <- h_freq
        h_freq <- h_temp
    } else if(!is.null(l_freq) && !is.null(h_freq) && l_freq < h_freq){
        type <- "bandpass"
    } else if(!is.null(l_freq)){
        type <- "high" #pass
    } else if(!is.null(h_freq)){
        type <- "low" #pass
    } else {
        stop("Both freq can't be NULL")
    }

    if(l_trans_bandwidth == "auto"){
        l_trans_bandwidth  <- min(max(l_freq * 0.25, 2), l_freq)
    }
    if(h_trans_bandwidth == "auto"){
        h_trans_bandwidth = min(max(0.25 * h_freq, 2.),sfreq / 2. - h_freq)
    }
    if(!is.null(l_trans_bandwidth)) message("'l_trans_bandwidth' chosen to be ",l_trans_bandwidth, " Hz" )
    if(!is.null(h_trans_bandwidth)) message("'h_trans_bandwidth' chosen to be ",h_trans_bandwidth, " Hz" )
    h_check = if(!is.null(h_freq)) h_trans_bandwidth  else Inf
    l_check =if(!is.null(l_freq)) l_trans_bandwidth  else Inf

    mult_fact = if(fir_design == "firwin2") 2 else 1
    length_factor <- list(hann =3.1, hamming = 3.3, blackman = 5)
    filter_length = max(as.integer(round(length_factor[[window]] * sfreq * mult_fact /
                                         min(h_check, l_check))), 1)
    if(fir_design == "firwin"){
        filter_length <- filter_length  + (filter_length - 1) %% 2
        }
    l_stop = l_freq - l_trans_bandwidth
    h_stop = h_freq + h_trans_bandwidth


    ##LOW PASS:
    if (type =="low"){
        message("Setting up low-pass filter at ",h_freq," Hz")
        f_pass <- f_stop<- h_freq # iir
            freq = c(0, h_freq, h_stop)  #0, f_p, f_s
            gain = c(1, 1, 0)
        if (h_stop != sfreq / 2){
            freq <- c(freq, sfreq / 2)
            gain <- c(gain,0)
        }
    ##HIGH PASS    
    } else if (type =="high"){
        message("Setting up high-pass filter at ",l_freq," Hz")
        f_pass <- f_stop<- l_freq # iir
        freq = c(l_stop, l_freq, sfreq / 2) # stop, pass,.._
        gain = c(0, 1, 1)
        if (l_stop != 0){
            freq = c(0, freq)
            gain = c(0, gain)
        }
   } else if (type =="bandpass"){
        if (any(l_freq < h_freq)){
            message("Setting up band-pass filter from ",l_freq," - " , h_freq ," Hz")
            f_pass <- f_stop <- c(l_freq,h_freq)  #iir
            freq = c(l_stop, l_freq, h_freq, h_stop) #f_s1, f_p1, f_p2, f_s2
            gain = c(0, 1, 1, 0)
            if (h_stop != sfreq / 2){
                freq <- c(freq, sfreq / 2)
                gain <- c(gain,0)
            }
            if (l_stop != 0){
                    freq = c(0, freq)
                    gain = c(0, gain)
            }
        }   else if(type == "bandstop") {
            ## This could possibly be removed after 0.14 release, but might
            ## as well leave it in to sanity check notch_filter
            if (length(l_freq) != length(h_freq)){
                stop("l_freq and h_freq must be the same length")
            }
         message("Setting up band-stop filter from ",h_freq," - " , l_freq ," Hz")
         
         ## Note: order of outputs is intentionally switched here!
         ## data, sfreq, f_s1, f_s2, f_p1, f_p2, filter_length, phase, \
         ## fir_window, fir_design = _triage_filter_params(
         ##                 data, sfreq, h_freq, l_freq, h_trans_bandwidth,
         ##                 l_trans_bandwidth, filter_length, method, phase,
            ##                 fir_window, fir_design, bands="arr", reverse=True)
            l_stop <- l_stop + l_trans_bandwidth
            l_freq <- l_freq + l_trans_bandwidth
            h_stop <- h_stop - h_trans_bandwidth
            h_freq <- h_freq -  h_trans_bandwidth
            f_pass <- f_stop <- c(l_freq[0], h_freq[0]) ##iir

            freq = c(l_stop,l_freq,h_freq, h_stop)
            gain = c(rep(1,length(l_stop)),
                     rep(0,length(l_freq)),
                     rep(0, length(h_freq)),
                     rep(1, length(h_stop)))
            order <- order(freq)
            freq <- freq[order]
            gain <- gain[order]
            if (freq[1] != 0){
                freq <- c( 0., freq)
                gain <- c(1, gain)
            }
            if( freq[length(freq)] != sfreq / 2.){
                freq <- c(freq, sfreq / 2.)
                gain <- c(gain, 1)
            }
            if(any(abs(diff(gain,differences= 2)) > 1)){
                stop("Stop bands are not sufficiently ",
                     "separated.")
            }
        }
   }   
    ## if method == "iir":
    ## construct_iir_filter(iir_params, f_pass, f_stop, sfreq, type)
    ## if method == "fir":
    construct_fir_filter(sfreq, freq, gain, filter_length, phase,
                             fir_window, fir_design)
    

}
