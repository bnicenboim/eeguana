library(eeguana)
options(eeguana.verbose = FALSE)

data_sincos2id <- eeguana:::data_sincos2id
# for checks later
reference_data <- data.table::copy(data_sincos2id)

#### eeg_baseline
test_that("baseline works", {
  baselines <- dplyr::summarize(
    dplyr::group_by(
      dplyr::filter(dplyr::as_tibble(data_sincos2id$.signal), .sample <= 0),
      .id
    ),
    bX = mean(X), bY = mean(Y)
  )
  signal_with_baselines <- dplyr::left_join(dplyr::as_tibble(data_sincos2id$.signal), baselines, by = ".id")
  signal_with_baselines$X <- signal_with_baselines$X - signal_with_baselines$bX
  signal_with_baselines$Y <- signal_with_baselines$Y - signal_with_baselines$bY
  signal_with_baselines <- signal_with_baselines[, c(".id", ".sample", "X", "Y")]
  baselined <- eeg_baseline(data_sincos2id)

  expect_equal_plain_df(signal_tbl(baselined), signal_with_baselines)
})


### chs_mean
data_M <- eeg_transmute(data_sincos2id, mean = chs_mean(X, Y))
data_NAsincos2id <- data_sincos2id
data_NAsincos2id$.signal[1, "X"] <- NA
data_MNA <- eeg_transmute(data_NAsincos2id, mean = chs_mean(X, Y, na.rm = TRUE))
data_MNA2 <- eeg_transmute(data_NAsincos2id, mean = chs_mean(across(c("X", "Y")), na.rm = TRUE))
# data_MNA2 <- eeg_transmute(data_NAsincos2id, mean = eeguana:::chs_mean.list(across(c(X, Y)), na.rm = TRUE))
# data_MNA2 <- eeg_transmute(data_NAsincos2id, mean = eeguana:::chs_mean.list(across(starts_with("X")), na.rm = TRUE))
# eeg_transmute(data_NAsincos2id, mean = rowMeans(across(c(X, Y)), na.rm = TRUE))
#
# dots <- rlang::quos(mean = rowMeans(across(c(X, Y)), na.rm = TRUE))
# .eeg_lst <- data_sincos2id
# eeg_transmute(data_NAsincos2id, mean = chs_mean(across("X", "Y"), na.rm = TRUE))
# eeg_transmute(data_NAsincos2id, mean = chs_mean.default(c(X,Y), na.rm = TRUE))
# eeg_transmute(data_NAsincos2id, mean = chs_mean(across(.cols = where(is.numeric()) & c("X", "Y")), na.rm = TRUE))
#
#
# data_M <- eeg_transmute(data_NAsincos2id, mean = eeguana:::rowMeans_ch(cbind(X, Y), na.rm = TRUE))
#  eeg_transmute(data_NAsincos2id, mean = eeguana:::rowMeans_ch(across(c(X, Y)), na.rm = TRUE))
# tidytable::transmute.(data_NAsincos2id$.signal, mean = eeguana:::rowMeans_ch(across.(c(X, Y)), na.rm = TRUE))
# data_M <- eeg_transmute(data_NAsincos2id, mean = rowMeans(cbind(X, Y), na.rm = TRUE))
# tidytable::transmute.(data_NAsincos2id$.signal,
#                       mean = rowMeans(across.(.cols = where(is.numeric) & c( Y), na.rm = TRUE)))
#
# tidytable::transmute.(data_NAsincos2id$.signal,
#                       mean = rowMeans_ch(across.(where(is_channel_dbl) & c(X, Y)) ))
# # dplyr::transmute(data_NAsincos2id$.signal,
# #                       mean = rowSums(across(c(X, Y)) ))
# dots <- rlang::quos(mean = rowMeans(cbind(X, Y), na.rm = TRUE))
# .eeg_lst <- data_NAsincos2id


test_that("can take the mean of the channels", {
  expect_equal(data_M$.signal$mean %>% as.numeric(), rowMeans(data_sincos2id$.signal[, .(X, Y)]))
  expect_equal(data_MNA$.signal$mean %>% as.numeric(), rowMeans(data_NAsincos2id$.signal[, .(X, Y)], na.rm = TRUE))
  expect_equal(data_MNA, data_MNA2)
  # TODO try to tfix
  # expect_equal(data_M_q$.signal$mean %>% as.numeric(), rowMeans(data_sincos2id$.signal[, .(X, Y)]))
})

test_that("both .eeg_lst and .channel_dbl give the same output for chs_mean", {
  data_M2 <- chs_mean(data_sincos2id)
  expect_equal(data_M, data_M2)
})

test_that("both chs_fun and chs_mean give the same output", {
  data_M_f <- eeg_transmute(data_sincos2id, mean = chs_fun(X, Y, .funs = mean))
  data_M_fa <- chs_fun(data_sincos2id, "mean")
  data_M_fa2 <- chs_fun(data_sincos2id, mean)
  data_M_fa3 <- chs_fun(data_sincos2id, list(mean = ~ mean(.)))
  data_M_fa4 <- chs_fun(data_sincos2id, ~ mean(., na.rm = TRUE)) %>%
    dplyr::rename(mean = X___mean____na_rm___TRUE_)
  data_sincos2id_NA <- data_sincos2id %>% eeg_mutate(X = ifelse(X > .98, NA, X))
  data_M_fa_NA1 <- chs_fun(data_sincos2id_NA, list(mean = ~ mean(., na.rm = TRUE)))
  data_M_fa_NA2 <- chs_fun(data_sincos2id_NA, mean, list(na.rm = TRUE))
  expect_equal(data_M_f, data_M_fa)
  expect_equal(data_M_f, data_M_fa2)
  expect_equal(data_M_f, data_M_fa3)
  expect_equal(data_M_f, data_M_fa4)
  expect_equal(data_M_fa_NA1, data_M_fa_NA2)
  expect_equal(data_M, data_M_f)
})


### rereference

## data_reref <- dplyr::mutate(data_sincos2id, X = ch_rereference(X, X, Y))
data_sincos2id_Z <- data_sincos2id %>% dplyr::mutate(Z = channel_dbl(0))
reference_data_Z <- data.table::copy(data_sincos2id_Z)

X_reref <- data_sincos2id_Z$.signal$X - (data_sincos2id$.signal$X + data_sincos2id$.signal$Y) / 2
Y_reref <- data_sincos2id_Z$.signal$Y - (data_sincos2id$.signal$X + data_sincos2id$.signal$Y) / 2
Z_reref <- data_sincos2id_Z$.signal$Z - (data_sincos2id$.signal$X + data_sincos2id$.signal$Y) / 2
attributes(X_reref)$.reference <- "X, Y"
attributes(Y_reref)$.reference <- "X, Y"
attributes(Z_reref)$.reference <- "X, Y"

## test_that("can reref the mean of the channels", {
##   expect_equal(data_reref$.signal$X, X_reref)
## })

data_reref_all_chs <- eeg_rereference(data_sincos2id_Z, .ref = c("X", "Y"))
data_reref_all_chs2  <- eeg_rereference(data_sincos2id_Z, .ref = -Z)
test_that(".reference changes", {
  expect_equal(data_reref_all_chs, data_reref_all_chs2)
  expect_equal(unique(channels_tbl(data_reref_all_chs)$.reference), "X, Y")
  expect_equal(data_reref_all_chs$.signal$X %>% as.numeric(), X_reref %>% as.numeric())
  expect_equal(data_reref_all_chs$.signal$Y %>% as.numeric(), Y_reref %>% as.numeric())
  expect_equal(data_reref_all_chs$.signal$Z %>% as.numeric(), Z_reref %>% as.numeric())
})


## data_reref_all <- dplyr::transmute(data_sincos2id, X_ref = ch_rereference(X, X, Y), Y_ref = ch_rereference(Y, X, Y))  %>%
##                     dplyr::rename(X = X_ref, Y = Y_ref)


## test_that("both .eeg_lst and .channel_dbl give the same values for ch_rereference (it's ok to loose the events and attributes", {
##   expect_equal(data_reref_all$.signal$X %>% as.numeric, data_reref_all_chs$.signal$X %>% as.numeric)
##   expect_equal(data_reref_all$.signal$Y %>% as.numeric, data_reref_all_chs$.signal$Y %>% as.numeric)
## })
test_that("data didn't change after grouping and mutate functions", {
  expect_equal_eeg_lst(reference_data, data_sincos2id)
  expect_equal_eeg_lst(reference_data_Z, data_sincos2id_Z)
})
