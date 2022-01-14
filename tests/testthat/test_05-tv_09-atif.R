library(eeguana)
options(eeguana.verbose = FALSE)
expect_equal_plain_df <- eeguana:::expect_equal_plain_df
expect_equal_but_sgl <- eeguana:::expect_equal_but_sgl
expect_equal_but_cnt_sgl <- eeguana:::expect_equal_but_cnt_sgl
expect_equal_but_sgm <- eeguana:::expect_equal_but_sgm
expect_equal_but_cnt_sgm <- eeguana:::expect_equal_but_cnt_sgm
expect_equal_eeg_lst <- eeguana:::expect_equal_eeg_lst


chr_remove <- eeguana:::chr_remove
data("data_faces_10_trials")

# create fake dataset

data_1 <- eeguana:::data_sincos3id
data_2 <- eeg_mutate(data_1, .recording = "recording2", 
                     X = sin(X + 10),
                     Y = cos(Y - 10),
                     condition = c("b", "a", "b"))
data <- bind(data_1, data_2)

# for checks later
reference_data <- data.table::copy(data)
data_grouped_descr <- data_faces_10_trials %>%
  eeg_segment(.description %in% c("s70", "s71"), .lim = c(-1, 1)) %>%
  eeg_events_to_NA(.description == "Bad Min-Max") %>%
  eeg_group_by(description, .recording)

segment_summ <- data.table::data.table(data_grouped_descr$.segments) %>%
  .[, .(.recording = unique(.recording)), by = "description"] %>%
  dplyr::bind_cols(dplyr::tibble(.id = c(1L, 2L)), .) %>%
  dplyr::select(eeguana:::obligatory_cols[[".segments"]], dplyr::everything()) %>%
  data.table::as.data.table()
data.table::setkey(segment_summ, .id)


test_that("eeg_summarize across works correctly on ungrouped data", {
summarize_def_eeg <- eeg_summarize(data,  X = mean(X), Y = mean(Y))
summarize_ac_eeg <- eeg_summarize(data, across(channel_names(data), mean))
summarize_ac2_eeg <- eeg_summarize(data, across(where(is_channel_dbl), mean))
summarize_ac3_eeg <- eeg_summarize(data, across(channel_names(data), "mean"))
summarize_ac4_eeg <- dplyr::summarize(data, dplyr::across(where(is_channel_dbl), mean))
summarize_ac5_eeg <- eeg_summarize(data, across(channel_names(data), ~ mean(.x)))
summarize_ac6_eeg <- eeg_summarize(data, across(channel_names(data), list(mean = ~ mean(.))))
expect_equal(summarize_ac_eeg, summarize_def_eeg)
expect_equal(summarize_ac_eeg, summarize_ac2_eeg)
expect_equal(summarize_ac_eeg, summarize_ac3_eeg)
expect_equal(summarize_ac_eeg, summarize_ac4_eeg)
expect_equal(summarize_ac_eeg, summarize_ac5_eeg)
expect_equal(summarize_ac_eeg %>% eeg_rename(X_mean = X, Y_mean = Y),
             summarize_ac6_eeg)
})

test_that("eeg_mutate across works correctly on ungrouped data", {
  mutate_def_eeg <- eeg_mutate(data,  X = scale(X), Y = scale(Y))
  mutate_ac_eeg <- eeg_mutate(data, across(channel_names(data), scale))
  mutate_ac2_eeg <- eeg_mutate(data, across(where(is_channel_dbl), scale))
  mutate_ac3_eeg <- eeg_mutate(data, across(channel_names(data), "scale"))
  mutate_ac4_eeg <- dplyr::mutate(data, dplyr::across(where(is_channel_dbl), scale))
  mutate_ac5_eeg <- eeg_mutate(data, across(channel_names(data), ~ scale(.x)))
  mutate_ac6_eeg <- eeg_mutate(data, across(channel_names(data), list(scale = ~ scale(.))))
  expect_equal(mutate_ac_eeg, mutate_def_eeg)
  expect_equal(mutate_ac_eeg, mutate_ac2_eeg)
  expect_equal(mutate_ac_eeg, mutate_ac3_eeg)
  expect_equal(mutate_ac_eeg, mutate_ac4_eeg)
  expect_equal(mutate_ac_eeg, mutate_ac5_eeg)
  expect_equal(eeg_mutate(data,  X_scale = scale(X), Y_scale = scale(Y)),
               mutate_ac6_eeg)
})



test_that("summarize across  no extra args", {
  data_grouped_descr <- data_faces_10_trials %>%
  eeg_segment(.description %in% c("s70", "s71"), .lim = c(-1, 1)) %>%
  eeg_events_to_NA(.description == "Bad Min-Max") %>%
  eeg_group_by(description, .recording)


  data_mean <- data_grouped_descr %>%
    eeg_summarize(across_ch(mean))

  data_mean_2 <- data_grouped_descr %>%
    eeg_summarize(across_ch( "mean"))

  data_mean_3 <- data_grouped_descr %>%
    eeg_summarize(across_ch( ~ mean(.)))
  
  data_mean_4 <- data_grouped_descr %>%
    eeg_summarize(across_ch( "mean"))
  data_mean_5 <- data_grouped_descr %>%
    eeg_summarize(across_ch( list(~ mean(.))))
  data_mean_6 <- data_grouped_descr %>%
    eeg_summarize(across_ch( list(M =~ mean(.))))
 expect_equal(data_mean, data_mean_2)
 expect_equal(data_mean, data_mean_3)
 expect_equal(data_mean, data_mean_4)
 expect_equal(data_mean_5, eeg_rename_with(data_mean, function(x) paste0(x,"_1")))
 expect_equal(data_mean_6, eeg_rename_with(data_mean, function(x) paste0(x,"_M")))
 expect_equal_plain_df(data_mean$.segments, segment_summ)

})


test_that("summarize across with extra args", {
  data_grouped_descr <- data_faces_10_trials %>%
  eeg_segment(.description %in% c("s70", "s71"), .lim = c(-1, 1)) %>%
  eeg_events_to_NA(.description == "Bad Min-Max") %>%
  eeg_group_by(description, .recording)


  data_mean <- data_grouped_descr %>%
    eeg_summarize(across_ch( mean, na.rm = TRUE))

  data_mean_var <- data_grouped_descr %>%
    eeg_summarize(across_ch(list(~ mean(., na.rm = TRUE), ~ var(., na.rm = TRUE))))

if(0){
#TODO: check why this doesn't work only in tests
  data_mean_var <- data_grouped_descr %>%
    eeg_summarize(across(channel_names(data_grouped_descr), list(~ mean(., na.rm = TRUE), ~ var(., na.rm = TRUE))))

}

  #throws warning, I think this is ok
  expect_warning(   data_var <- data_grouped_descr %>%
    eeg_summarize(across_ch( var, na.rm = TRUE)))
  # 
  expect_equal(data_mean, data_grouped_descr %>%
    eeg_summarize(across_ch( "mean", na.rm = TRUE)))
  expect_equal(data_mean, data_grouped_descr %>%
    eeg_summarize(across_ch( ~ mean(., na.rm = TRUE))))
  expect_equal(data_mean %>% eeg_rename_with(~ paste0(.x,"_1") ), data_grouped_descr %>%
    eeg_summarize(across_ch( list(~ mean(., na.rm = TRUE)))))
  expect_equal(
    data_mean %>%
      eeg_rename_with(~ paste0(., "_M")),
    data_grouped_descr %>%
      eeg_summarize(across_ch(list(M = ~ mean(., na.rm = TRUE))))
  )
  expect_equal(data_mean, data_grouped_descr %>%
    eeg_summarize(across_ch( list(M = ~ mean(., na.rm = TRUE)))) %>%
    eeg_rename_with( ~ chr_remove(., "_M")))
  expect_equal(data_mean, data_mean_var %>%
   eeg_select(dplyr::ends_with("_1")) %>%
     eeg_rename_with( ~ chr_remove(., "_1")))
  expect_warning( expect_warning( expect_equal(data_var, data_mean_var %>%
                 eeg_select(dplyr::ends_with("_2")) %>%
                 eeg_rename_with( ~ chr_remove(., "_2"),dplyr::ends_with("_2")))
  ))

})



# vars <- data_grouped_descr %>%
#   eeg_summarize(across(channel_names(data_grouped_descr), var, na.rm = TRUE)
# 
# vars2 <- data_grouped_descr %>%
#   eeg_summarize(across(channel_names(data_grouped_descr), "var", na.rm = TRUE)
# 
# vars3 <- data_grouped_descr %>%
#   eeg_summarize(across(channel_names(data_grouped_descr), ~ var(., na.rm = TRUE))
# 
# vars4 <- data_grouped_descr %>%
#   eeg_summarize(across(channel_names(data_grouped_descr), list(~ var(., na.rm = TRUE)))
# 
# # Change title
# vars5 <- data_grouped_descr %>%
#   eeg_summarize(across(channel_names(data_grouped_descr), list(M = ~ var(., na.rm = TRUE)))
# 
# signal_vars <- eeguana:::left_join_dt(data_grouped_descr$.signal, data.table::data.table(data_grouped_descr$.segments)) %>%
#   .[, lapply(.SD, var, na.rm = TRUE), .SDcols = channel_names(data_grouped_descr), by = "description"] %>%
#   dplyr::select(-description) %>%
#   cbind(data.table::data.table(.id = c(1L, 2L), .sample = sample_int(c(NA, NA), 500)), .)
# data.table::setkey(signal_vars, ".id", ".sample")
# 
# # vars was problematic with summarize_at, the call was giving the inside of the function
# test_that("summarize ats vars", {
#   expect_equal(vars, vars2)
#   expect_equal(vars, vars3)
#   expect_equal(vars, vars4)
#   expect_equal(vars, vars5 %>%
#     dplyr::rename_at(channel_names(.), ~ chr_remove(., "_M")))
#   expect_true(all(sapply(vars$.signal[, channel_names(vars), with = FALSE], is_channel_dbl)))
#   expect_equal(vars$.segments, segment_summ)
# })

# ###
# varifs <- data_grouped_descr %>%
#   dplyr::summarize_if(is_channel_dbl, var, na.rm = TRUE)
# 
# funs <- dplyr:::manip_if(data_grouped_descr, is_channel_dbl, .funs = var, rlang::enquo(.funs), rlang::caller_env(), .caller = "summarise_if")
# 
# dplyr::summarise(data_grouped_descr, !!!funs)
# 
# varifs2 <- data_grouped_descr %>%
#   dplyr::summarize_if(is_channel_dbl, "var", na.rm = TRUE)
# 
# varifs3 <- data_grouped_descr %>%
#   dplyr::summarize_if(is_channel_dbl, ~ var(., na.rm = TRUE))
# 
# varifs4 <- data_grouped_descr %>%
#   dplyr::summarize_if(is_channel_dbl, list(~ var(., na.rm = TRUE)))
# 
# # Change title
# varifs5 <- data_grouped_descr %>%
#   dplyr::summarize_if(is_channel_dbl, list(M = ~ var(., na.rm = TRUE)))
# 
# test_that("summarize if", {
#   expect_equal(varifs, varifs2)
#   expect_equal(varifs, varifs3)
#   expect_equal(varifs, varifs4)
#   expect_equal(varifs, varifs5 %>%
#     dplyr::rename_at(channel_names(.), ~ chr_remove(., "_M")))
#   expect_equal(as.matrix(varifs$.signal), as.matrix(signal_vars))
#   expect_true(all(sapply(varifs$.signal[, channel_names(vars), with = FALSE], is_channel_dbl)))
#   expect_equal(varifs$.segments, segment_summ)
# })
# 
# data_grouped_descr %>%
#   dplyr::filter(.id != 2) %>%
#   dplyr::group_by(.sample) %>%
#   eeg_summarize(across(channel_names(data_grouped_descr), ~ (mean(.[description == "s70"] -
#     .[description == "s71"], na.rm = TRUE)))
# 
# 
# ## pow <- function(x) x^2
# 
# ## means<- data %>% dplyr::transmute_at("Fp1", pow)
# 
# ## means2 <- data_grouped_descr  %>%
# ##     dplyr::summarize_at(channel_names(.), "mean")
# 
# ## means3 <- data_grouped_descr %>%
# ##     dplyr::summarize_at(channel_names(.), ~ mean(.))

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
