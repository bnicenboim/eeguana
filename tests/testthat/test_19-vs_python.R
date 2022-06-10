library(eeguana)
options(eeguana.verbose = FALSE)
set.seed(123)
suppress_python_output <- function(x) {
  invisible(reticulate::py_capture_output(x))
}

test_that("compare construct iir filter with python", {
  eeguana:::skip_on_actions()
  skip_on_ci()
  reticulate::conda_list()

  # reticulate::py_install("mne")
  reticulate::py_run_string("import mne")
  reticulate::py_run_string("import numpy")
  reticulate::py_run_string("iir_params_0 = dict(order=4, ftype='butter', output='ba')")
  suppress_python_output(reticulate::py_run_string("iir_params = mne.filter.construct_iir_filter(iir_params_0, 40, None, 1000, 'low', return_copy=False)"))
  
  ##   IIR filter parameters
  ## ---------------------
  ## Butterworth low zero-phase (two-pass forward and reverse) non-causal filter:
  ## - Filter order 8 (effective, after forward-backward)
  ## - Cutoff at 40.00 Hz: -6.02 dB
  iir_params_0 <- list(order = 4, type = "butter", output = "ba")
  options(eeguana.verbose = TRUE)
  output <- capture.output(iir_params <- eeguana:::construct_iir_filter(iir_params_0, f_pass = 40, f_stop = NULL, sfreq = 1000, btype = "low"),type = "message")[1:7]
  py_iir_params <- reticulate::py$iir_params %>% lapply(c)
  names(py_iir_params)[[2]] <- "type"
  expect_equal(py_iir_params, iir_params)
expect_equal(output, c("IIR filter parameters", "---------------------", "butter low zero-phase (two-pass forward and reverse) ", 
                       " non-causal filter:", "", "- Filter order 8  (effective, after forward-backward)", 
                       "- Cutoff(s) at 40 Hz: -6.02 dB"))
  
reticulate::py_run_string("iir_params_0 = dict(order=4, ftype='butter', output='ba')")
  suppress_python_output(reticulate::py_run_string("iir_params_b = mne.filter.construct_iir_filter(iir_params_0, [40,100], None, 1000, 'bandpass', return_copy=False)"))
  options(eeguana.verbose = TRUE)
  output_b <- capture.output(iir_params_b <- eeguana:::construct_iir_filter(iir_params = iir_params_0, f_pass = c(40,100), f_stop = NULL, sfreq = 1000, btype = "pass"),type = "message")
  
  py_iir_params_b <- reticulate::py$iir_params_b %>% lapply(c)
  names(py_iir_params_b)[[2]] <- "type"
  expect_equal(py_iir_params_b, iir_params_b)
  expect_equal(output_b, c("IIR filter parameters", "---------------------", "butter pass zero-phase (two-pass forward and reverse) ", 
                         " non-causal filter:", "", "- Filter order 16  (effective, after forward-backward)", 
                         "- Cutoff(s) at 40, 100 Hz: -6.02, -6.02 dB"))
  
  ### problem with the precision of gsignal with ba filters
 #  iir_params_0 <- list(order = 4, type = "butter", output = "ba")
 #  h <- eeguana:::construct_iir_filter(iir_params = iir_params_0, f_pass = c(0.1, 30), f_stop = NULL, sfreq = 512, btype = "pass")
 # mne <- reticulate::import("mne")
 #  h2 <- mne$filter$construct_iir_filter(list(order = 4, ftype = "butter", output = "ba"), f_pass = c(0.1, 30), f_stop = NULL, sfreq = 512, btype = "pass")
 #  any(abs(gsignal::tf2zp(b = h$b, a = h$a)$p)> 1) 
 #  any(abs(gsignal::tf2zp(b = as.numeric(h2$b), a =as.numeric(h2$a))$p)> 1) 
  
  ## sos not working?
  reticulate::py_run_string("import mne")
  reticulate::py_run_string("iir_params = dict(order=4, ftype='butter', output='sos')")
  reticulate::py_run_string("iir_params = mne.filter.construct_iir_filter(iir_params, 40, None, 1000, 'low', return_copy=False)")
  iir_params = list(order =4, type ="butter", output="sos")
  iir_params <- eeguana:::construct_iir_filter(iir_params, f_pass = 40, f_stop = NULL, sfreq = 1000, btype = "low")
  py_iir_paramssos <- reticulate::py$iir_params[[5]] %>% as.matrix()
  expect_equal(py_iir_paramssos, iir_params[[5]], tolerance = .00001)

  #suppress_python_output(reticulate::py_run_string("iir_params = dict(ftype='cheby1', gpass=3, gstop=20, output='ba')"))
  ##   IIR filter parameters
  ## ---------------------
  ## Chebyshev I low zero-phase (two-pass forward and reverse) non-causal filter:
  ## - Cutoff at 40.00 Hz: -6.00 dB

  reticulate::py_run_string("iir_params = mne.filter.construct_iir_filter(iir_params, 40, 50, 1000, 'low')")
  iir_params <- list(type = "cheby1", gpass = 3, gstop = 20, output = "ba")
  iir_params <- eeguana:::construct_iir_filter(iir_params, f_pass = 40, f_stop = 50, sfreq = 1000, btype = "low")
  py_iir_params <- reticulate::py$iir_params %>% lapply(c)
  names(py_iir_params)[[1]] <- "type"
  expect_equal(py_iir_params, iir_params)

  reticulate::py_run_string("iir_params = dict(b=numpy.ones((10)), a=[1, 0], padlen=0)")
  reticulate::py_run_string("iir_params = mne.filter.construct_iir_filter(iir_params, return_copy=False)")
  iir_params <- list(b = rep(1, 10), a = c(1, 0), padlen = 0)
  iir_params <- eeguana:::construct_iir_filter(iir_params)
  py_iir_params <- reticulate::py$iir_params %>% lapply(c)
  expect_equal(py_iir_params, iir_params)

  # ## TO TEST:
  # mne <- reticulate::import("mne")
  # mne$filter$`_filter_attenuation`(h, freq, gain)
  # eeguana:::filter_attenuation(h, freq, gain)
  # 
  wp <- 0.2
  ws <- 0.3
  gpass <- 1
  gstop <- 40
  s <- reticulate::import("scipy")
  iirdesign_py <- s$signal$iirdesign(wp, ws, gpass, gstop)
  names(iirdesign_py) <- c("b", "a")
  expect_equal(eeguana:::iirdesign(wp, ws, gpass, gstop),
    iirdesign_py %>% lapply(c),
    tolerance = .001
  )


  cf <- eeguana:::construct_iir_filter(iir_params = list(type = "butter", output = "ba", order = 4), f_pass = 50, f_stop = 50, sfreq = 500, btype = "high")
  suppress_python_output(reticulate::py_run_string("cf = mne.filter.construct_iir_filter(sfreq=500, f_pass = 50, f_stop = 50, iir_params = dict(ftype = 'butter', order = 4 , output= 'ba'), btype = 'high')"))
  ##   IIR filter parameters
  ## ---------------------
  ## Butterworth high zero-phase (two-pass forward and reverse) non-causal filter:
  ## - Filter order 8 (effective, after forward-backward)
  ## - Cutoff at 50.00 Hz: -6.02 dB

  py_cf <- reticulate::py$cf %>% lapply(c)
  names(py_cf)[[1]] <- "type"
  expect_equal(cf, py_cf[names(cf)])
})

test_that("create filter", {
  eeguana:::skip_on_actions()
  skip_on_ci()
  reticulate::py_run_string("import mne")

  f <- eeguana:::create_filter(sampling_rate = 500, l_freq = 50)
  suppress_python_output(reticulate::py_run_string("f = mne.filter.create_filter(data = None, sfreq=500, l_freq = 50, h_freq = None)"))

  ## No data specified. Sanity checks related to the length of the signal relative to the filter order will be skipped.
  ## Setting up high-pass filter at 50 Hz

  ## FIR filter parameters
  ## ---------------------
  ## Designing a one-pass, zero-phase, non-causal highpass filter:
  ## - Windowed time-domain design (firwin) method
  ## - Hamming window with 0.0194 passband ripple and 53 dB stopband attenuation
  ## - Lower passband edge: 50.00
  ## - Lower transition bandwidth: 12.50 Hz (-6 dB cutoff frequency: 43.75 Hz)
  ## - Filter length: 133 samples (0.266 sec)

  expect_equal(f, c(reticulate::py$f), tolerance = .0000000001)

  f <- eeguana:::create_filter(sampling_rate = 500, l_freq = 50, config = list(method = "iir", type = "butter", order = 6, output = "ba"))
  suppress_python_output(reticulate::py_run_string("f = mne.filter.create_filter(data = None, sfreq=500, l_freq = 50, h_freq = None, method = 'iir', iir_params = dict(ftype = 'butter',output = 'ba', order = 6))"))

  ##   No data specified. Sanity checks related to the length of the signal relative to the filter order will be skipped.
  ## Setting up high-pass filter at 50 Hz

  ## IIR filter parameters
  ## ---------------------
  ## Butterworth highpass zero-phase (two-pass forward and reverse) non-causal filter:
  ## - Filter order 12 (effective, after forward-backward)
  ## - Cutoff at 50.00 Hz: -6.02 dB

  py_f <- reticulate::py$f %>% lapply(c)
  names(py_f)[[1]] <- "type"
  expect_equal(f, py_f[names(f)], tolerance = .0000000001)
})


test_that("lfilter", {
  t <- seq(-1, 1, length.out = 201)

  x <- (sin(2 * pi * 0.75 * t * (1 - t) + 2.1) +
    0.1 * sin(2 * pi * 1.25 * t + 1) +
    0.18 * cos(2 * pi * 3.85 * t))

 
})



if (0) {
  mne <- reticulate::import("mne")

  X1_firmne <- mne$filter$filter_data(signal$X1, sfreq = 500, h_freq = 500 * 1.5 / (2 * pi), l_freq = NULL)
  X1_iirmne <- mne$filter$filter_data(signal$X1, sfreq = 500, h_freq = 500 * 1.5 / (2 * pi), l_freq = NULL, method = "iir", iir_params = list(ftype = "butter", order = 6, output = "ba"))

  data_s <- bind(
    data_sin_X1 %>% mutate(cond = "fir"),
    data_sin_X1_iir %>% mutate(cond = "iir"),
    data_sin %>% mutate(cond = "raw")
  )

  ggplot(data_s, aes(x = .time, y = .value, color = cond)) +
    facet_grid(. ~ .key) +
    geom_line(alpha = .5)

  X1s <- tibble(X1_firmne, X1_iirmne, X1_iir = c(data_sin_X1_iir$.signal$X1), X1_fir = c(data_sin_X1$.signal$X1), t = 1:1000) %>% tidyr::pivot_longer(cols = -t)
  ggplot(X1s %>% filter(name %in% c("X1_iirmne", "X1_firmne")), aes(x = t, y = value, color = name)) +
    geom_line(alpha = .5)
  ggplot(X1s %>% filter(name %in% c("X1_iirmne", "X1_iir")), aes(x = t, y = value, color = name)) +
    geom_line(alpha = .5)
  ggplot(X1s %>% filter(name %in% c("X1_firmne", "X1_fir")), aes(x = t, y = value, color = name)) +
    geom_line(alpha = .5)
}

message("to test using python fast ica")
if (0) {
  test_that("can use other (python) functions", {
    skip_on_ci()
    eeguana:::skip_on_actions()
    sk <- reticulate::import("sklearn.decomposition")
    py_fica <- function(x) {
      x <- as.matrix(x)
      ica <- sk$FastICA(whiten = TRUE, random_state = 23L)
      X <- scale(x, scale = FALSE) %>%
        as.matrix(x)
      S <- ica$fit_transform(X)
      W <- t(ica$components_)
      list(W = W)
    }
    data_blinks <- eeguana:::data_blinks
    data_no_blinks <- eeguana:::data_no_blinks

    data_ica_py <- eeg_ica(data_blinks, .method = py_fica)
    ## data_ica_py %>% eeg_ica_show(ICA1, ICA2, ICA3) %>%
    ##   dplyr::select(ICA1,ICA2, ICA3) %>%
    ##   plot()
    data_no_blinks_py <- data_ica_py %>% eeg_ica_keep(-ICA1)
    expect_equal(data_no_blinks$.signal, data_no_blinks_py$.signal, tolerance = .4)
  })
}

test_that("raw brainvision read and converted from MNE match", {
  # it needs MNE installed
  ## skip_on_cran()
  eeguana:::skip_on_actions()
  skip_on_ci()
  ## reference
  bvfile <- system.file("testdata", "bv_export_bv_txt_bin_vector.vhdr", package = "eeguana")
  eeg_read <- read_vhdr(file = bvfile, .recording = "r1")
  ## bvfile_pkl <- paste0(bvfile,".pkl")

  mne_io <- reticulate::import("mne.io")
  suppress_python_output(eeg_mne_obj <- mne_io$read_raw_brainvision(bvfile,
    preload = TRUE,
    eog = c("VEOG", "HEOG"),
    misc = c("M1", "M2")
  ))
  eeg_mne <- as_eeg_lst(.data = eeg_mne_obj) %>%
    dplyr::mutate(.recording = "r1")
  channels_tbl(eeg_read) <- channels_tbl(eeg_read) %>%
    dplyr::select(.channel, .x, .y, .z, unit, .reference)
  expect_equal(eeg_read, eeg_mne)
})
