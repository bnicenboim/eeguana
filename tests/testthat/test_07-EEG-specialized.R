library(eeguana)
options(eeguana.verbose = FALSE)

# Datasets to test:
data_sincos2id <- eeguana:::data_sincos2id
data_sincos2id_b <- data_sincos2id
events_tbl(data_sincos2id_b)$.channel <- NA
data_sincos2id_c <- data_sincos2id
events_tbl(data_sincos2id_c)$.channel <- NA
data_sincos2id_2 <- eeguana:::data_sincos2id_2

data_sincos2id_3 <- data_sincos2id_2 %>% eeg_mutate(Z = X)

test_that("can clean files with entire_seg = FALSE", {
  clean_data <- eeg_events_to_NA(data_sincos2id, .type == "Bad", .entire_seg = FALSE)
  clean_data_sincos2id_2 <- eeg_events_to_NA(data_sincos2id_2, .type == "Bad", .entire_seg = FALSE)
  expect_equal(clean_data, clean_data_sincos2id_2)
  expect_equal(nrow(clean_data$.events), 4)
  expect_equal(all(is.na(clean_data$.signal[clean_data$.signal$.sample %in% seq(-2, -3 + 3 - 1) &
    clean_data$.signal$.id == 1, c("X", "Y")])), TRUE)
  expect_equal(all(is.na(clean_data$.signal[clean_data$.signal$.sample %in% seq(2, 2 + 2 - 1) &
    clean_data$.signal$.id == 1, c("X")])), TRUE)
  expect_equal(all(is.na(clean_data$.signal[clean_data$.signal$.sample %in% seq(2, 2 + 2 - 1) &
    clean_data$.signal$.id == 1, c("Y")])), FALSE)
  expect_equal(all(is.na(clean_data$.signal[clean_data$.signal$.sample %in% seq(2, 2 + 1 - 1) &
    clean_data$.signal$.id == 2, c("Y")])), TRUE)
  expect_equal(all(is.na(clean_data$.signal[clean_data$.signal$.sample %in% seq(2, 2 + 1 - 1) &
    clean_data$.signal$.id == 2, c("X")])), FALSE)
  expect_equal(all(is.na(clean_data$.signal[clean_data$.signal$.id == 1, c("X", "Y")])), FALSE)
  expect_equal(all(is.na(clean_data$.signal[clean_data$.signal$.id == 2, c("Y")])), FALSE)
  expect_equal(all(is.na(clean_data$.signal[clean_data$.signal$.id == 2, c("X")])), FALSE)
})

test_that("can clean whole channels in files", {
  clean_data_chan <- eeg_events_to_NA(data_sincos2id, .type == "Bad", .n_chs = 1, .entire_seg = FALSE)
  clean_data_chan2 <- eeg_events_to_NA(data_sincos2id_b, .type == "Bad", .entire_seg = FALSE)
  clean_data_chan3 <- eeg_events_to_NA(data_sincos2id_b, .type == "Bad", .n_chs = 1, .entire_seg = FALSE)
  clean_data_sincos2id_22 <- eeg_events_to_NA(data_sincos2id_2, .type == "Bad", .n_chs = 1, .entire_seg = FALSE)
  clean_data_sincos2id_3 <- eeg_events_to_NA(data_sincos2id_3, .type == "Bad", .n_chs = 2, .entire_seg = FALSE)
  data_sincos2id_3b <- data_sincos2id_3
  tmp <- events_tbl(data_sincos2id_3b)[3]
  tmp$.channel <- "Z"
  events_tbl(data_sincos2id_3b) <- rbind(events_tbl(data_sincos2id_3b), tmp)
  clean_data_sincos2id_3_b <- eeg_events_to_NA(data_sincos2id_3b, .type == "Bad", .n_chs = 1, .entire_seg = FALSE)
  expect_equal(clean_data_sincos2id_3 %>% eeg_filter(.id==1), clean_data_sincos2id_3_b %>% eeg_filter(.id==1))
    expect_equal(clean_data_chan, clean_data_chan2)
  expect_equal(clean_data_chan, clean_data_chan3)
  expect_equal(clean_data_chan, clean_data_sincos2id_22)
  expect_equal(nrow(clean_data_chan$.events), 4)
  expect_equal(all(is.na(clean_data_chan$.signal[clean_data_chan$.signal$.sample %in% seq(-2, -3 + 3 - 1) &
    clean_data_chan$.signal$.id == 1, c("X", "Y")])), TRUE)
  expect_equal(all(is.na(clean_data_chan$.signal[clean_data_chan$.signal$.sample %in% seq(2, 2 + 2 - 1) &
    clean_data_chan$.signal$.id == 1, c("X")])), TRUE)
  expect_equal(all(is.na(clean_data_chan$.signal[clean_data_chan$.signal$.sample %in% seq(2, 2 + 2 - 1) &
    clean_data_chan$.signal$.id == 1, c("Y")])), TRUE)
  expect_equal(all(is.na(clean_data_chan$.signal[clean_data_chan$.signal$.sample %in% seq(2, 2 + 1 - 1) &
    clean_data_chan$.signal$.id == 2, c("Y")])), TRUE)
  expect_equal(all(is.na(clean_data_chan$.signal[clean_data_chan$.signal$.sample %in% seq(2, 2 + 1 - 1) &
    clean_data_chan$.signal$.id == 2, c("X")])), TRUE)
  expect_equal(all(is.na(clean_data_chan$.signal[clean_data_chan$.signal$.id == 1, c("X", "Y")])), FALSE)
  expect_equal(all(is.na(clean_data_chan$.signal[clean_data_chan$.signal$.id == 2, c("Y")])), FALSE)
  expect_equal(all(is.na(clean_data_chan$.signal[clean_data_chan$.signal$.id == 2, c("X")])), FALSE)
})




test_that("can clean whole segments in files", {
  clean_data_seg <- eeg_events_to_NA(data_sincos2id, .type == "Bad", .entire_seg = TRUE)
  expect_equal(nrow(clean_data_seg$.events), 4)
  expect_equal(all(is.na(clean_data_seg$.signal[clean_data_seg$.signal$.id == 1, c("X", "Y")])), TRUE)
  expect_equal(all(is.na(clean_data_seg$.signal[clean_data_seg$.signal$.id == 2, c("Y")])), TRUE)
  expect_equal(all(is.na(clean_data_seg$.signal[clean_data_seg$.signal$.id == 2, c("X")])), FALSE)
})




data_sincos2id_1000 <- eeguana:::data_sincos2id_1000
N <- 1000 # number of samples

data_d <- eeg_downsample(.data = data_sincos2id_1000, .q = 2)

##  bind_rows( dplyr::tibble(x=seq_along(data_eeg$.signal$X),y= data_eeg$.signal$X %>% as.numeric, .type= "original"),
##                   dplyr::tibble(x=seq(from = 1, to = N, by= 2),y= data_d$.signal$X %>% as.numeric, .type= "downsampled")) %>%
## ggplot2::ggplot(aes(x=x,y=y, color = .type)) + geom_point()

if (0) {
  test_that("the signal after downsampling remains similar; sample0 = -100 ", {
    expect_equal(as.numeric(data_d$.signal$X), as.numeric(data_sincos2id_1000$.signal$X)[seq(1, N, by = 2)], tolerance = .011)
  })


  test_that("times remain similar; sample0 = -100", {
    expect_equal(as_time(data_d$.signal$.sample), as_time(data_sincos2id_1000$.signal$.sample)[seq(1, N, by = 2)], tolerance = 1 / 500 + .00001)
    expect_equal(as.numeric(events_tbl(data_d)$.initial / 250), as.numeric(events_tbl(data_sincos2id_1000)$.initial / 500), tolerance = 1 / 500 + .00001)
    expect_equal(as.numeric(events_tbl(data_d)$.final / 250), as.numeric(events_tbl(data_sincos2id_1000)$.final / 500), tolerance = 1 / 500 + .00001)
  })


  data_eegm1 <- data_sincos2id_1000
  data_eegm1$.signal$.sample <- data_eegm1$.signal$.sample + 1
  data_eegm1$.events$.initial <- data_eegm1$.events$.initial + 1
  data_eegm1$.events$.final <- data_eegm1$.events$.final + 1


  data_dm1 <- eeg_downsample(data_eegm1, .q = 2)

  test_that("the signal after downsampling remains similar; sample0 = 0 ", {
    expect_equal(as.numeric(data_dm1$.signal$X), as.numeric(data_eegm1$.signal$X)[seq(1, N, by = 2)], tolerance = .011)
  })


  test_that("times remain similar; sample0 = 0", {
    expect_equal(as_time(data_dm1$.signal$.sample), as_time(data_eegm1$.signal$.sample)[seq(1, N, by = 2)], tolerance = 1 / 500 + .00001)
    expect_equal(as.numeric(events_tbl(data_dm1)$.initial / 250), as.numeric(events_tbl(data_eegm1)$.initial / 500), tolerance = 1 / 500 + .00001)
    expect_equal(as.numeric(events_tbl(data_dm1)$.final / 250), as.numeric(events_tbl(data_eegm1)$.final / 500), tolerance = 1 / 500 + .00001)
  })



  data_eeg0 <- data_sincos2id_1000
  data_eeg0$.signal$.sample <- data_eeg0$.signal$.sample + 100
  data_eeg0$.events$.initial <- data_eeg0$.events$.initial + 100
  data_eeg0$.events$.final <- data_eeg0$.events$.final + 100


  data_d0 <- eeg_downsample(data_eeg0, .q = 2)

  test_that("the signal after downsampling remains similar; sample0 = 0 ", {
    expect_equal(as.numeric(data_d0$.signal$X), as.numeric(data_eeg0$.signal$X)[seq(1, N, by = 2)], tolerance = .011)
  })


  test_that("times remain similar; sample0 = 0", {
    expect_equal(as_time(data_d0$.signal$.sample), as_time(data_eeg0$.signal$.sample)[seq(1, N, by = 2)], tolerance = 1 / 500 + .00001)
    expect_equal(as.numeric(events_tbl(data_d0)$.initial / 250), as.numeric(events_tbl(data_eeg0)$.initial / 500), tolerance = 1 / 500 + .00001)
    expect_equal(as.numeric(events_tbl(data_d0)$.final / 250), as.numeric(events_tbl(data_eeg0)$.final / 500), tolerance = 1 / 500 + .00001)
  })


  data_eeg1 <- data_sincos2id_1000
  data_eeg1$.signal$.sample <- data_eeg1$.signal$.sample + 101
  data_eeg1$.events$.initial <- data_eeg1$.events$.initial + 101
  data_eeg1$.events$.final <- data_eeg1$.events$.final + 101


  data_d1 <- eeg_downsample(data_eeg1, .q = 2)

  test_that("the signal after downsampling remains similar ; sample0 = 1", {
    expect_equal(as.numeric(data_d1$.signal$X), as.numeric(data_eeg1$.signal$X)[seq(1, N, by = 2)], tolerance = .011)
  })


  test_that("times remain similar; sample0 = 1", {
    expect_equal(as_time(data_d1$.signal$.sample), as_time(data_eeg1$.signal$.sample)[seq(1, N, by = 2)], tolerance = 1 / 500 + .00001)
    expect_equal(as.numeric(events_tbl(data_d1)$.initial / 250), as.numeric(events_tbl(data_eeg1)$.initial / 500), tolerance = 1 / 500 + .00001)
    expect_equal(as.numeric(events_tbl(data_d1)$.final / 250), as.numeric(events_tbl(data_eeg1)$.final / 500), tolerance = 1 / 500 + .00001)
  })

  data_eeg2 <- data_sincos2id_1000
  data_eeg2$.signal$.sample <- data_eeg2$.signal$.sample + 102
  data_eeg2$.events$.final <- data_eeg2$.events$.final + 102


  data_d2 <- eeg_downsample(data_eeg2, .q = 2)

  test_that("the signal after downsampling remains similar ; sample0 = 2", {
    expect_equal(as.numeric(data_d2$.signal$X), as.numeric(data_eeg2$.signal$X)[seq(1, N, by = 2)], tolerance = .011)
  })


  test_that("times remain similar; sample0 = 2", {
    expect_equal(as_time(data_d2$.signal$.sample), as_time(data_eeg2$.signal$.sample)[seq(1, N, by = 2)], tolerance = 1 / 500 + .00001)
    expect_equal(as.numeric(events_tbl(data_d2)$.initial / 250), as.numeric(events_tbl(data_eeg2)$.initial / 500), tolerance = 1 / 500 + .00001)
    expect_equal(as.numeric(events_tbl(data_d2)$.final / 250), as.numeric(events_tbl(data_eeg2)$.final / 500), tolerance = 1 / 500 + .00001)
  })



  data_eeg100 <- data_sincos2id_1000
  data_eeg100$.signal$.sample <- data_eeg100$.signal$.sample + 201
  data_eeg100$.events$.initial <- data_eeg100$.events$.initial + 201
  data_eeg100$.events$.final <- data_eeg100$.events$.final + 201


  data_d100 <- eeg_downsample(data_eeg100, .q = 2)

  test_that("the signal after downsampling remains similar ; sample0 = 101", {
    expect_equal(as.numeric(data_d100$.signal$X), as.numeric(data_eeg100$.signal$X)[seq(1, N, by = 2)], tolerance = .011)
  })


  test_that("times remain similar; sample0 = 101", {
    expect_equal(as_time(data_d100$.signal$.sample), as_time(data_eeg100$.signal$.sample)[seq(1, N, by = 2)], tolerance = 1 / 500 + .00001)
    expect_equal(as.numeric(events_tbl(data_d100)$.initial / 250), as.numeric(events_tbl(data_eeg100)$.initial / 500), tolerance = 1 / 500 + .00001)
    expect_equal(as.numeric(events_tbl(data_d100)$.final / 250), as.numeric(events_tbl(data_eeg100)$.final / 500), tolerance = 1 / 500 + .00001)
  })


  #### OTHER Q:

  data_d <- eeg_downsample(data_sincos2id_1000, .q = 20, .multiple_times = TRUE)

  ##  bind_rows( dplyr::tibble(x=seq_along(data_eeg$.signal$X),y= data_eeg$.signal$X %>% as.numeric, .type= "original"),
  ##                   dplyr::tibble(x=seq(from = 1, to = N, by= 20),y= data_d$.signal$X %>% as.numeric, .type= "downsampled")) %>%
  ## ggplot2::ggplot(aes(x=x,y=y, color = .type)) + geom_point()


  test_that("the signal after downsampling remains similar;q=20 ", {
    expect_equal(as.numeric(data_d$.signal$X), as.numeric(data_sincos2id_1000$.signal$X)[seq(1, N, by = 20)], tolerance = .04)
  })

  test_that("times remain similar; q=20", {
    expect_equal(as_time(data_d$.signal$.sample), as_time(data_sincos2id_1000$.signal$.sample)[seq(1, N, by = 20)], tolerance = 1 / 20)
    expect_equal(as.numeric(events_tbl(data_d)$.initial / 25), as.numeric(events_tbl(data_sincos2id_1000)$.initial / 500), tolerance = 1 / 20)
    expect_equal(as.numeric(events_tbl(data_d)$.final / 25), as.numeric(events_tbl(data_sincos2id_1000)$.final / 500), tolerance = 1 / 20)
  })
  #### OTHER Q:

  data_dmax <- eeg_downsample(data_sincos2id_1000, .max_sample = 100)

  ##  bind_rows( dplyr::tibble(x=seq_along(data_eeg$.signal$X),y= data_eeg$.signal$X %>% as.numeric, .type= "original"),
  ##                   dplyr::tibble(x=seq(from = 1, to = N, by= 2),y= data_d$.signal$X %>% as.numeric, .type= "downsampled")) %>%
  ## ggplot2::ggplot(aes(x=x,y=y, color = .type)) + geom_point()


  test_that("the signal after downsampling remains similar; .max_sample =100 ", {
    expect_equal(as.numeric(data_dmax$.signal$X), as.numeric(data_sincos2id_1000$.signal$X)[seq(1, N, by = 5)], tolerance = .011)
  })

  test_that("times remain similar; .max_sample=100", {
    expect_equal(as_time(data_dmax$.signal$.sample), as_time(data_sincos2id_1000$.signal$.sample)[seq(1, N, by = 5)], tolerance = 1 / 100)
    expect_equal(as.numeric(events_tbl(data_dmax)$.initial / 100), as.numeric(events_tbl(data_sincos2id_1000)$.initial / 500), tolerance = 1 / 20)
    expect_equal(as.numeric(events_tbl(data_dmax)$.final / 100), as.numeric(events_tbl(data_sincos2id_1000)$.final / 500), tolerance = 1 / 20)
  })
}

message("\n*******")
message("Downsampling test omited, weird stuff here!!!!")
message("*******\n")

