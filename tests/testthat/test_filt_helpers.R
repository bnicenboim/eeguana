context("test dplyr::filters helpers")
library(eeguana)
set.seed(123)


test_that("construct iir filter", {
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
