context("test in comparison with python")
library(eeguana)
set.seed(123)

test_that("compare construct iir filter with python", {
  eeguana:::skip_on_actions()
  skip_on_ci()

  reticulate::use_condaenv("anaconda3")
  reticulate::py_run_string("import mne")
  reticulate::py_run_string("import numpy")
  reticulate::py_run_string("iir_params = dict(order=4, ftype='butter', output='ba')")
  reticulate::py_run_string("iir_params = mne.filter.construct_iir_filter(iir_params, 40, None, 1000, 'low', return_copy=False)")
  iir_params = list(order =4, type ="butter", output="ba")
  iir_params <- eeguana:::construct_iir_filter(iir_params, f_pass = 40, f_stop = NULL, sfreq = 1000, btype = "low")
  py_iir_params <- reticulate::py$iir_params %>% lapply(c)
  names(py_iir_params)[[2]] <- "type"
  expect_equal(py_iir_params, iir_params)

  ## sos not working
  ## reticulate::py_run_string("import mne")
  ## reticulate::py_run_string("iir_params = dict(order=4, ftype='butter', output='sos')")
  ## reticulate::py_run_string("iir_params = mne.filter.construct_iir_filter(iir_params, 40, None, 1000, 'low', return_copy=False)")
  ## iir_params = list(order =4, ftype ="butter", output="sos")
  ## iir_params <- eeguana:::construct_iir_filter(iir_params, f_pass = 40, f_stop = NULL, sfreq = 1000, btype = "low")
  ## py_iir_params <- reticulate::py$iir_params %>% lapply(c)
  ## expect_equal(py_iir_params, iir_params)

  reticulate::py_run_string("iir_params = dict(ftype='cheby1', gpass=3, gstop=20, output='ba')")
  reticulate::py_run_string("iir_params = mne.filter.construct_iir_filter(iir_params, 40, 50, 1000, 'low')")
  iir_params = list(type='cheby1', gpass=3, gstop=20, output='ba')
  iir_params <- eeguana:::construct_iir_filter(iir_params, f_pass = 40, f_stop = 50, sfreq = 1000, btype = "low")
  py_iir_params <- reticulate::py$iir_params %>% lapply(c)
  names(py_iir_params)[[1]] <- "type"
  expect_equal(py_iir_params, iir_params)

  reticulate::py_run_string("iir_params = dict(b=numpy.ones((10)), a=[1, 0], padlen=0)")
  reticulate::py_run_string("iir_params = mne.filter.construct_iir_filter(iir_params, return_copy=False)")
  iir_params = list(b = rep(1,10), a= c(1,0),padlen = 0)
  iir_params <- eeguana:::construct_iir_filter(iir_params)
  py_iir_params <- reticulate::py$iir_params %>% lapply(c)
  expect_equal(py_iir_params, iir_params)


  wp = 0.2
  ws = 0.3
  gpass = 1
  gstop = 40
  s <- reticulate::import("scipy")
  iirdesign_py <- s$signal$iirdesign(wp, ws, gpass,gstop)
  names(iirdesign_py) <- c("b","a")
  expect_equal(eeguana:::iirdesign(wp, ws, gpass, gstop),
               iirdesign_py %>% lapply(c), tolerance = .001)


  cf <- eeguana:::construct_iir_filter(iir_params = list(type = "butter", output = "ba", order =4), f_pass = 50, f_stop = 50 ,sfreq = 500, btype= "high")
  reticulate::py_run_string("cf = mne.filter.construct_iir_filter(sfreq=500, f_pass = 50, f_stop = 50, iir_params = dict(ftype = 'butter', order = 4 , output= 'ba'), btype = 'high')")
  py_cf <- reticulate::py$cf  %>% lapply(c)
  names(py_cf)[[1]] <- "type"
  expect_equal(cf, py_cf[names(cf)])
})

test_that("create filter", {
  eeguana:::skip_on_actions()
  skip_on_ci()
  reticulate::use_condaenv("anaconda3")
  reticulate::py_run_string("import mne")

  f <- eeguana:::create_filter(sampling_rate = 500, l_freq = 50)
  reticulate::py_run_string("f = mne.filter.create_filter(data = None, sfreq=500, l_freq = 50, h_freq = None)")
  expect_equal(f, c(reticulate::py$f), tolerance = .0000000001)

  f <- eeguana:::create_filter(sampling_rate = 500, l_freq = 50,  config = list(method ="iir",type ="butter", order =6, output = "ba"))
  reticulate::py_run_string("f = mne.filter.create_filter(data = None, sfreq=500, l_freq = 50, h_freq = None, method = 'iir', iir_params = dict(ftype = 'butter',output = 'ba', order = 6))")
  py_f <- reticulate::py$f %>%lapply(c)
  names(py_f)[[1]] <- "type"
  expect_equal(f, py_f[names(f)] , tolerance = .0000000001)
})


test_that("lfilter",{
  t <- seq(-1, 1, length.out =  201)

  x <- (sin(2*pi*0.75*t*(1-t) + 2.1) +
          0.1*sin(2*pi*1.25*t + 1) +
          0.18*cos(2*pi*3.85*t))

  # Create an order 3 lowpass butterworth filter:
  h = signal::butter(3, 0.05)
  b <- h$b
  a <- h$a
  zi <- eeguana:::sig_lfilter_zi(b =b, a = a)
  eeguana:::skip_on_actions()
  skip_on_ci()
  reticulate::use_condaenv("anaconda3")
  sp <- reticulate::import("scipy")
  expect_equal(zi, sp$signal$lfilter_zi(b, a) %>% c())
})



if(0){
  mne <- reticulate::import("mne")

  X1_firmne <-  mne$filter$filter_data(signal$X1, sfreq =500, h_freq = 500 * 1.5 / (2 * pi), l_freq = NULL)
  X1_iirmne <-  mne$filter$filter_data(signal$X1, sfreq =500, h_freq = 500 * 1.5 / (2 * pi), l_freq = NULL, method='iir', iir_params = list(ftype = "butter", order = 6, output ="ba"))

  data_s <- bind(data_sin_X1 %>% mutate(cond = "fir"),
                 data_sin_X1_iir %>% mutate(cond = "iir"),
                 data_sin %>% mutate(cond ="raw")
                 )

  ggplot(data_s, aes(x = .time, y = .value, color = cond)) + facet_grid(.~.key) + geom_line( alpha = .5)

  X1s <- tibble(X1_firmne, X1_iirmne, X1_iir = c(data_sin_X1_iir$.signal$X1), X1_fir = c(data_sin_X1$.signal$X1), t = 1:1000) %>% tidyr::pivot_longer(cols = -t)
  ggplot(X1s %>% filter(name %in% c("X1_iirmne", "X1_firmne")), aes(x=t, y = value, color = name)) +geom_line(alpha = .5)
  ggplot(X1s %>% filter(name %in% c("X1_iirmne", "X1_iir")), aes(x=t, y = value, color = name)) +geom_line(alpha = .5)
  ggplot(X1s %>% filter(name %in% c("X1_firmne", "X1_fir")), aes(x=t, y = value, color = name)) +geom_line(alpha = .5)
}

test_that("can use other (python) functions", {
  skip_on_ci()
 eeguana:::skip_on_actions()
  py_fica <- function(x) {
    x <- as.matrix(x)
    reticulate::use_condaenv("anaconda3")
    sk <- reticulate::import("sklearn.decomposition")
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
  expect_equal(data_no_blinks$.signal, data_no_blinks_py$.signal, tolerance = .14)
})


test_that("raw brainvision read and converted from MNE match", {
  # it needs MNE installed
  ## skip_on_cran()
eeguana:::skip_on_actions()
skip_on_ci()
  ## reference
  bvfile <- system.file("testdata", "bv_export_bv_txt_bin_vector.vhdr", package = "eeguana")
  eeg_read <- read_vhdr(file = bvfile, .recording = "r1")
  ## bvfile_pkl <- paste0(bvfile,".pkl")

  reticulate::use_condaenv("anaconda3")
  mne_io <- reticulate::import("mne.io")
  eeg_mne_obj <- mne_io$read_raw_brainvision(bvfile,
                                             preload = TRUE,
                                             eog = c("VEOG","HEOG"),
                                             misc = c("M1","M2"))


  eeg_mne <- as_eeg_lst(.data = eeg_mne_obj) %>%
    dplyr::mutate(.recording = "r1")


  channels_tbl(eeg_read) <- channels_tbl(eeg_read) %>%
    dplyr::select(.channel, .x, .y, .z, unit, .reference)
  ## channels_tbl(eeg_mne)
  #TODO, to remove later:
  ## eeg_read <- as_eeg_lst(eeg_read)
   expect_equal(eeg_read, eeg_mne)
})
