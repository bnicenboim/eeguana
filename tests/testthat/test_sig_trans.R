context("Signal manipulation")
library(eegble)


e_binary_noseg <- read_vhdr("binary-faces-noseg.vhdr")
 channels <- c("Fp1", "Fpz", "Fp2", "F7", "F3", "Fz", "F4", "F8", "FC5", "FC1",
 "FC2", "FC6", "M1", "T7", "C3", "Cz", "C4", "T8", "M2", "CP5",
 "CP1", "CP2", "CP6", "P7", "P3", "Pz", "P4", "P8", "POz", "O1",
 "Oz", "O2", "EOGV", "EOGH")

test_that("can clean files", {
  e_binary_noseg_clean <- event_to_NA(e_binary_noseg, 
    type == "Bad Interval", parallel = FALSE)
  Nas <- e_binary_noseg_clean$data[[1]]$signals[[1]][ 
  e_binary_noseg_clean$data[[1]]$signals[[1]]$sample >= 2158 &
  e_binary_noseg_clean$data[[1]]$signals[[1]]$sample <= 2158 + 738 -1,]$Fp1
  expect_equal(is.na(unique(Nas)), TRUE)
  Nas_channels <- e_binary_noseg_clean$data[[1]]$signals[[1]][ 
  e_binary_noseg_clean$data[[1]]$signals[[1]]$sample >= 2158 &
  e_binary_noseg_clean$data[[1]]$signals[[1]]$sample <= 2158 + 738 -1,
  channels]
  expect_equal(all(is.na(unlist(Nas_channels))), FALSE) # it shouldn't remove from all channels
  expect_known_output(e_binary_noseg_clean,file = "e_binary_noseg_clean.Rda" )
})

test_that("can clean whole channels in files", {
   e_binary_noseg_clean_ch <- event_to_NA(e_binary_noseg,  
    type == "Bad Interval", ind_channel = FALSE,  parallel = FALSE)
  Nas_channels <- e_binary_noseg_clean_ch$data[[1]]$signals[[1]][ 
  e_binary_noseg_clean_ch$data[[1]]$signals[[1]]$sample >= 2158 &
  e_binary_noseg_clean_ch$data[[1]]$signals[[1]]$sample <= 2158 + 738 -1,
  channels]
  expect_equal(all(is.na(unlist(Nas_channels))), TRUE)
  expect_known_output(e_binary_noseg_clean_ch, file = "e_binary_noseg_clean_ch.Rda" )
})

test_that("can remove whole segments in files", {
   e_binary_noseg_clean_seg <- event_to_NA(e_binary_noseg,  
    type == "Bad Interval", ind_channel = FALSE, entire_seg = TRUE ,
        parallel = FALSE)
  expect_equal(is.null(e_binary_noseg_clean_seg$data[[1]]$signals[[1]]), TRUE)
  expect_known_output(e_binary_noseg_clean_seg, 
    file = "e_binary_noseg_clean_seg.Rda" )
})

test_that("can clean files in parallel", {
  library(future)
  plan(multiprocess)
  e_binary_noseg_clean <- event_to_NA(e_binary_noseg, 
  type == "Bad Interval", parallel = TRUE)

  Nas <- e_binary_noseg_clean$data[[1]]$signals[[1]][ 
  e_binary_noseg_clean$data[[1]]$signals[[1]]$sample >= 2158 &
  e_binary_noseg_clean$data[[1]]$signals[[1]]$sample <= 2158 + 738 -1,]$Fp1
  expect_equal(is.na(unique(Nas)), TRUE)
  expect_known_output(e_binary_noseg_clean,file = "e_binary_noseg_clean.Rda" )
})

 s <- segment(e_binary_noseg, description == "s70" | 
                  description == "s71", 
                            lim=c(-.2,.25))

test_that("can segment", {
  expect_length(s$data[[1]]$signals, 200)
  seg1 <- s$data[[1]]$signals[[1]]
  expect_equal(nrow(seg1), 226)
  expect_equal(max(seg1$time), .25)
  expect_equal(min(seg1$time), -.2)
})

#manually baseline
sdf <- as.data.frame(s, chans = "Fp1")
av <- mean(sdf[sdf$time < 0 & sdf$segment == "s70_1",]$amplitude)
Fp1_def_bl <- sdf[sdf$segment == "s70_1",]$amplitude - av

av_1 <- mean(sdf[sdf$time < 0 & sdf$time > -0.1 & sdf$segment == "s70_1",]$amplitude)
Fp1_def_bl_1 <- sdf[sdf$segment == "s70_1",]$amplitude - av_1

test_that("can baseline", {
  bl_default <- baseline(s)
  expect_equal(bl_default$data[[1]]$signals[[1]]$Fp1, Fp1_def_bl)
  bl_1 <- baseline(s, t = -.1)
  expect_equal(bl_1$data[[1]]$signals[[1]]$Fp1, Fp1_def_bl_1)
})
