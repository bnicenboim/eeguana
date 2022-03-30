library(eeguana)

data_sleep_F3 <- eeguana:::data_sleep_F3
data_sleep_F3_2 <- bind(data_sleep_F3 %>% eeg_mutate(.recording = "rec1"),
     data_sleep_F3 %>% eeg_mutate(.recording = "rec2"))
data_sleep_psd_default <- eeg_psd(data_sleep_F3)
data_sleep_psd <- eeg_psd(data_sleep_F3,.config = list(window = 400))
data_sleep_psd_2 <- eeg_psd(data_sleep_F3_2,.config = list(window = 400))

gsig_default <- gsignal::pwelch(c(data_sleep_F3[["F3"]]),fs = 100)
gsig <- gsignal::pwelch(c(data_sleep_F3[["F3"]]), window = 400, fs= 100)

test_that("eeg_psd returns output of pwelch ", {
  expect_equal(gsig_default$freq, data_sleep_psd_default$.psd$.freq)
  expect_equal(gsig$freq, data_sleep_psd$.psd$.freq)
  expect_equal(gsig_default$spec, data_sleep_psd_default$.psd$F3, ignore_attr = TRUE)
  expect_equal(gsig$spec, data_sleep_psd$.psd$F3, ignore_attr = TRUE)
  expect_equal(data_sleep_psd_2$.psd[.id==1, F3], 
               data_sleep_psd_2$.psd[.id==2, F3])
  expect_equal(data_sleep_psd_2$.psd[.id==1, F3], 
               data_sleep_psd$.psd$F3)
  expect_equal(data_sleep_psd_2$.psd[.id==1, .freq], 
               data_sleep_psd_2$.psd[.id==2, .freq])
  expect_equal(data_sleep_psd_2$.psd[.id==1, .freq], 
               data_sleep_psd$.psd$.freq)
  
  })

test_that("eeg_power_band returns output of pwelch ", {
  
pband <- data_sleep_psd %>% eeg_power_band()
pband2 <-  data_sleep_F3 %>% eeg_power_band(.config = list(window = 400))
pband_2 <- data_sleep_psd_2 %>% eeg_power_band()

pband_default <- data_sleep_F3 %>% eeg_power_band()

expect_equal(pband, pband2)  
expect_equal(pband_2$.psd[.id==1, F3], 
             pband_2$.psd[.id==2, F3])
expect_equal(pband_2$.psd[.id==1, F3], 
             pband$.psd$F3)
expect_equal(pband_2$.psd[.id==1, .freq], 
             pband_2$.psd[.id==2, .freq])
expect_equal(pband_2$.psd[.id==1, .freq], 
             pband$.psd$.freq)
expect_equal(pband_default, eeg_power_band(data_sleep_psd_default))
})

test_that("eeg_psd returns output of pwelch ", {
expect_gg(plot(data_sleep_psd))
})


test_that("basic test tidyverse works with psd_lst ", {
  data_sleep_psd_2 %>% eeg_select(f3 = F3)
  data_sleep_psd_2 %>% eeg_rename(f3 = F3)
  data_sleep_psd_2 %>% eeg_filter(.id==1)
  data_sleep_psd_2 %>% eeg_summarize(mean(F3))
  data_sleep_psd_2 %>% eeg_group_by(.id) %>% eeg_summarize(mean(F3))
  data_sleep_psd_2 %>% eeg_mutate(F3 =F3 * 0)
  data_sleep_psd_2 %>% eeg_transmute(F3 =F3 * 100)
})
