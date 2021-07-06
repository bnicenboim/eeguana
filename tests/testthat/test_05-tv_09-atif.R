library(eeguana)
options(eeguana.verbose = FALSE)

chr_remove <- eeguana:::chr_remove
data("data_faces_10_trials")

data_grouped_descr <- data_faces_10_trials %>%
  eeg_segment(.description %in% c("s70", "s71"), .lim = c(-1, 1)) %>%
  eeg_events_to_NA(.description == "Bad Min-Max") %>%
  dplyr::group_by(description)

segment_summ <- data.table::data.table(data_grouped_descr$.segments) %>%
  .[, .(.recording = unique(.recording)), by = "description"] %>%
  dplyr::bind_cols(dplyr::tibble(.id = c(1L, 2L)), .) %>%
  dplyr::select(eeguana:::obligatory_cols[[".segments"]], dplyr::everything()) %>%
  data.table::as.data.table()
data.table::setkey(segment_summ, .id)

test_that("summarize ats (and rename) no extra args", {
  data_mean <- data_grouped_descr %>%
    dplyr::summarize_at(channel_names(.), mean)

  signal_means <- eeguana:::left_join_dt(
    data_grouped_descr$.signal,
    data.table::data.table(data_grouped_descr$.segments)
  ) %>%
    .[, lapply(.SD, mean), .SDcols = channel_names(data_grouped_descr), by = "description"] %>%
    dplyr::select(-description) %>%
    cbind(data.table::data.table(.id = c(1L, 2L), .sample = sample_int(c(NA, NA), 500)), .)

  data.table::setkey(signal_means, ".id", ".sample")
  data_mean_var <- data_grouped_descr %>%
    dplyr::summarize_at(channel_names(.), list(~ mean(.), ~ var(.)))

  data_var <- data_grouped_descr %>%
    dplyr::summarize_at(channel_names(.), var)

  expect_equal(data_mean, data_grouped_descr %>%
    dplyr::summarize_at(channel_names(.), "mean"))
  expect_equal(data_mean, data_grouped_descr %>%
    dplyr::summarize_at(channel_names(.), ~ mean(.)))
  expect_equal(data_mean, data_grouped_descr %>%
    dplyr::summarize_at(channel_names(.), list(~ mean(.))))
  expect_equal(
    data_mean %>%
      dplyr::rename_at(channel_names(.), ~ paste0(., "_M")),
    data_grouped_descr %>%
      dplyr::summarize_at(channel_names(.), list(M = ~ mean(.)))
  )
  expect_equal(data_mean, data_grouped_descr %>%
    dplyr::summarize_at(channel_names(.), list(M = ~ mean(.))) %>%
    dplyr::rename_at(channel_names(.), ~ chr_remove(., "_M")))
  expect_equal(data_mean, data_mean_var %>%
    dplyr::select(dplyr::ends_with("mean")) %>%
    dplyr::rename_at(channel_names(.), ~ chr_remove(., "_mean")))
  expect_equal(data_var, data_mean_var %>%
    dplyr::select(dplyr::ends_with("var")) %>%
    dplyr::rename_at(channel_names(.), ~ chr_remove(., "_var")))

  expect_equal(data.table::as.data.table(data_mean$.signal), signal_means)
  expect_equal(data_mean$.segments, segment_summ)
})


test_that("summarize ats with extra args", {
  data_mean <- data_grouped_descr %>%
    dplyr::summarize_at(channel_names(.), mean, na.rm = TRUE)

  signal_means <- eeguana:::left_join_dt(
    data_grouped_descr$.signal,
    data.table::data.table(data_grouped_descr$.segments)
  ) %>%
    .[, lapply(.SD, mean, na.rm = TRUE), .SDcols = channel_names(data_grouped_descr), by = "description"] %>%
    dplyr::select(-description) %>%
    cbind(data.table::data.table(.id = c(1L, 2L), .sample = sample_int(c(NA, NA), 500)), .)

  data.table::setkey(signal_means, ".id", ".sample")
  data_mean_var <- data_grouped_descr %>%
    dplyr::summarize_at(channel_names(.), list(~ mean(., na.rm = TRUE), ~ var(., na.rm = TRUE)))

  data_var <- data_grouped_descr %>%
    dplyr::summarize_at(channel_names(.), var, na.rm = TRUE)

  expect_equal(data_mean, data_grouped_descr %>%
    dplyr::summarize_at(channel_names(.), "mean", na.rm = TRUE))
  expect_equal(data_mean, data_grouped_descr %>%
    dplyr::summarize_at(channel_names(.), ~ mean(., na.rm = TRUE)))
  expect_equal(data_mean, data_grouped_descr %>%
    dplyr::summarize_at(channel_names(.), list(~ mean(., na.rm = TRUE))))
  expect_equal(
    data_mean %>%
      dplyr::rename_at(channel_names(.), ~ paste0(., "_M")),
    data_grouped_descr %>%
      dplyr::summarize_at(channel_names(.), list(M = ~ mean(., na.rm = TRUE)))
  )
  expect_equal(data_mean, data_grouped_descr %>%
    dplyr::summarize_at(channel_names(.), list(M = ~ mean(., na.rm = TRUE))) %>%
    dplyr::rename_at(channel_names(.), ~ chr_remove(., "_M")))
  expect_equal(data_mean, data_mean_var %>%
    dplyr::select(dplyr::ends_with("mean")) %>%
    dplyr::rename_at(channel_names(.), ~ chr_remove(., "_mean")))
  expect_equal(data_var, data_mean_var %>%
    dplyr::select(dplyr::ends_with("var")) %>%
    dplyr::rename_at(channel_names(.), ~ chr_remove(., "_var")))

  expect_equal(data.table::as.data.table(data_mean$.signal), signal_means)
  expect_equal(data_mean$.segments, segment_summ)
})



vars <- data_grouped_descr %>%
  dplyr::summarize_at(channel_names(.), var, na.rm = TRUE)

vars2 <- data_grouped_descr %>%
  dplyr::summarize_at(channel_names(.), "var", na.rm = TRUE)

vars3 <- data_grouped_descr %>%
  dplyr::summarize_at(channel_names(.), ~ var(., na.rm = TRUE))

vars4 <- data_grouped_descr %>%
  dplyr::summarize_at(channel_names(.), list(~ var(., na.rm = TRUE)))

# Change title
vars5 <- data_grouped_descr %>%
  dplyr::summarize_at(channel_names(.), list(M = ~ var(., na.rm = TRUE)))

signal_vars <- eeguana:::left_join_dt(data_grouped_descr$.signal, data.table::data.table(data_grouped_descr$.segments)) %>%
  .[, lapply(.SD, var, na.rm = TRUE), .SDcols = channel_names(data_grouped_descr), by = "description"] %>%
  dplyr::select(-description) %>%
  cbind(data.table::data.table(.id = c(1L, 2L), .sample = sample_int(c(NA, NA), 500)), .)
data.table::setkey(signal_vars, ".id", ".sample")

# vars was problematic with summarize_at, the call was giving the inside of the function
test_that("summarize ats vars", {
  expect_equal(vars, vars2)
  expect_equal(vars, vars3)
  expect_equal(vars, vars4)
  expect_equal(vars, vars5 %>%
    dplyr::rename_at(channel_names(.), ~ chr_remove(., "_M")))
  expect_true(all(sapply(vars$.signal[, channel_names(vars), with = FALSE], is_channel_dbl)))
  expect_equal(vars$.segments, segment_summ)
})

###
varifs <- data_grouped_descr %>%
  dplyr::summarize_if(is_channel_dbl, var, na.rm = TRUE)

funs <- dplyr:::manip_if(data_grouped_descr, is_channel_dbl, .funs = var, rlang::enquo(.funs), rlang::caller_env(), .caller = "summarise_if")

dplyr::summarise(data_grouped_descr, !!!funs)

varifs2 <- data_grouped_descr %>%
  dplyr::summarize_if(is_channel_dbl, "var", na.rm = TRUE)

varifs3 <- data_grouped_descr %>%
  dplyr::summarize_if(is_channel_dbl, ~ var(., na.rm = TRUE))

varifs4 <- data_grouped_descr %>%
  dplyr::summarize_if(is_channel_dbl, list(~ var(., na.rm = TRUE)))

# Change title
varifs5 <- data_grouped_descr %>%
  dplyr::summarize_if(is_channel_dbl, list(M = ~ var(., na.rm = TRUE)))

test_that("summarize if", {
  expect_equal(varifs, varifs2)
  expect_equal(varifs, varifs3)
  expect_equal(varifs, varifs4)
  expect_equal(varifs, varifs5 %>%
    dplyr::rename_at(channel_names(.), ~ chr_remove(., "_M")))
  expect_equal(as.matrix(varifs$.signal), as.matrix(signal_vars))
  expect_true(all(sapply(varifs$.signal[, channel_names(vars), with = FALSE], is_channel_dbl)))
  expect_equal(varifs$.segments, segment_summ)
})

data_grouped_descr %>%
  dplyr::filter(.id != 2) %>%
  dplyr::group_by(.sample) %>%
  dplyr::summarize_at(channel_names(.), ~ (mean(.[description == "s70"] -
    .[description == "s71"], na.rm = TRUE)))


## pow <- function(x) x^2

## means<- data %>% dplyr::transmute_at("Fp1", pow)

## means2 <- data_grouped_descr  %>%
##     dplyr::summarize_at(channel_names(.), "mean")

## means3 <- data_grouped_descr %>%
##     dplyr::summarize_at(channel_names(.), ~ mean(.))

## means4 <- data_grouped_descr %>%
##     dplyr::summarize_at(channel_names(.), list(~ mean(.)))

##                                         # Change title
## means5 <- data_grouped_descr %>%
##     dplyr::summarize_at(channel_names(.), list(M= ~ mean(.)))

## means6 <- data_grouped_descr %>%
##     dplyr::summarize_at(channel_names(.), list(~mean(.), ~ var(.)))

## signal_means_baseline <- eeguana:::left_join_dt(data$.signal, data.table::data.table(data$.segments)) %>%
##     .[,lapply(.SD,mean), .SDcols = channel_names(data), by ="description"] %>%
##     dplyr::select(-description) %>% cbind(data.table::data.table(.id=c(1L,2L),.sample=sample_int(c(NA,NA),500)),.)


## data %>% dplyr::filter(.id !=2) %>% dplyr::group_by( .sample) %>%
##     dplyr::summarize_at(channel_names(.), funs(mean(.[description == "s70"] -
##                                               .[description == "s71"], na.rm=TRUE)))

## microbenchmark::microbenchmark(
## eval(parse(text = "mean(1:10^6)")),
##  eval(rlang::expr(mean(1:10^6))),
##  rlang::eval_tidy(rlang::expr(mean(1:10^6))),
## rlang::eval_tidy(rlang::quo(mean(1:10^6))),
##  mean(1:10^6)
## )

message("\n******")
message("check that channels remain channels")
message("*******\n")
