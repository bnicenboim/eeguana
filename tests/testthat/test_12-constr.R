library(eeguana)
options(eeguana.verbose = FALSE)

new_obj <- eeg_lst(signal_tbl = data.table::data.table(.id = 1, .sample = sample_int(1:10, 246), channel = channel_dbl(1:10)))

test_that("can build eeg_lst", {
  expect_true(is_eeg_lst(eeg_lst()))
  expect_true(is_eeg_lst(new_obj))
})

test_that("[[ works", {
  expect_equal(
    channel_dbl(1:10, x = 1, y = 2, z = 3)[[1]],
    channel_dbl(1, x = 1, y = 2, z = 3)
  )
  expect_equal(
    subset(channel_dbl(1:10, x = 1, y = 2, z = 3), c(TRUE, rep(FALSE, 9))),
    channel_dbl(1, x = 1, y = 2, z = 3)
  )
  expect_equal(
    component_dbl(1:10)[[1]],
    component_dbl(1)
  )
  expect_equal(
    subset(component_dbl(1:10), c(TRUE, rep(FALSE, 9))),
    component_dbl(1)
  )
  expect_true(is.na(new_obj[[".recording"]]))
  expect_equal(new_obj[["channel"]], signal_tbl(new_obj)$channel)
})


test_that("eeg_lst", {
  
N<- 1000
df0 <- eeg_lst(signal_tbl = data.frame(.id=1L, .sample = sample_int(1:N, .sampling_rate = 500),
                                      X = channel_dbl(sin(seq(0, 8*pi, length.out = N)))))
df1 <- eeg_lst(signal_tbl = data.frame(.sample = sample_int(1:N, .sampling_rate = 500),
                                       X = channel_dbl(sin(seq(0, 8*pi, length.out = N)))))
df2 <- eeg_lst(signal_tbl = data.frame(.id=1L, X = channel_dbl(sin(seq(0, 8*pi, length.out = N)))),
              .sampling_rate = 500)

df3 <- eeg_lst(signal_tbl = data.frame(X = channel_dbl(sin(seq(0, 8*pi, length.out = N)))),
      .sampling_rate = 500)
df4 <- eeg_lst(signal_tbl = data.table::data.table(X = channel_dbl(sin(seq(0, 8*pi, length.out = N)))),
               .sampling_rate = 500)
df5 <- eeg_lst(signal_tbl = data.table::data.table(.id=1L, .sample = sample_int(1:N, .sampling_rate = 500),
                                       X = channel_dbl(sin(seq(0, 8*pi, length.out = N)))))

df6 <- eeg_lst(signal_tbl = dplyr::tibble(X = channel_dbl(sin(seq(0, 8*pi, length.out = N)))),
               .sampling_rate = 500)
df7 <- eeg_lst(signal_tbl =dplyr::tibble(.id=1L, .sample = sample_int(1:N, .sampling_rate = 500),
                                                   X = channel_dbl(sin(seq(0, 8*pi, length.out = N)))))
expect_equal(df0,df1)
expect_equal(df0,df2)
expect_equal(df0,df3)
expect_equal(df0,df4)
expect_equal(df0,df5)
expect_equal(df0,df6)
expect_equal(df0,df7)

})

test_that("Subsetting returns correct single element", {
  x <- channel_dbl(1:4)
  expect_equal(x[2], channel_dbl(2))
  expect_s3_class(x[2], "channel_dbl")
})

test_that("Subsetting with recycling works correctly", {
  x <- channel_dbl(1:4)
  # Assuming your recycling logic is corrected
  expect_equal(x[c(1,2)], channel_dbl(c(1,2)))
  expect_length(x[c(1,2)], channel_dbl(2))
})

test_that("concatenation works", {
  x <- channel_dbl(1:4)
  y <- channel_dbl(4)
  z <- channel_dbl(c(1,2,3,4,4))
  y2 <- channel_dbl(4,.x=1)

  expect_equal(c(x,y),z)
  expect_warning(z2 <- c(x,y2))
  expect_equal(z2,z)
})


test_that("Subsetting returns correct single element", {
  x <- sample_int(1:4,500)
  expect_equal(x[2], sample_int(2,500))
  expect_s3_class(x[2], "sample_int")
})

test_that("Subsetting with recycling works correctly", {
  x <- sample_int(1:4,500)
  # Assuming your recycling logic is corrected
  expect_equal(x[c(1,2)], sample_int(c(1,2),500))
  expect_length(x[c(1,2)], sample_int(2, 500))
})

test_that("concatenation works", {
  x <- sample_int(1:4,500)
  y <- sample_int(4,500)
  z <- sample_int(c(1,2,3,4,4),500)
  y2 <- sample_int(4,600)

  expect_equal(c(x,y),z)
  expect_error(z2 <- c(x,y2))
})
