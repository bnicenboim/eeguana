

library(eeguana)
data("data_faces_10_trials")

data <- data_faces_10_trials %>% eeg_segment(.description %in% c("s70","s71"),lim=c(-1,1)) %>%
    eeg_events_to_NA(.description == "Bad Min-Max")

means <- data %>% dplyr::group_by(description) %>%
    dplyr::summarize_at(channel_names(.), mean)

means2 <- data %>% dplyr::group_by(description)  %>%
    dplyr::summarize_at(channel_names(.), "mean")

means3 <- data %>% dplyr::group_by(description) %>%
    dplyr::summarize_at(channel_names(.), ~ mean(.))

means4 <- data %>% dplyr::group_by(description) %>%
    dplyr::summarize_at(channel_names(.), list(~ mean(.)))

# Change title
means5 <- data %>% dplyr::group_by(description) %>%
    dplyr::summarize_at(channel_names(.), list(M= ~ mean(.)))

means6 <- data %>% dplyr::group_by(description) %>%
    dplyr::summarize_at(channel_names(.), list(~mean(.), ~ var(.)))

signal_means_baseline <- eeguana:::left_join_dt(data$.signal, data.table::data.table(data$.segments)) %>%
    .[,lapply(.SD,mean), .SDcols = channel_names(data), by ="description"] %>%
    dplyr::select(-description) %>% cbind(data.table::data.table(.id=c(1L,2L),.sample=sample_int(c(NA,NA),500)),.)
data.table::setkey(signal_means_baseline, ".id",".sample")
segment_summ <- data.table::data.table(data$.segments) %>%
    .[,.(.recording = unique(.recording)), by ="description"] %>%
    dplyr::bind_cols(dplyr::tibble(.id = c(1L,2L)),.)

test_that("summarize ats no extra args", {
expect_equal(means, means2)
expect_equal(means, means3)
expect_equal(means, means4)
expect_equal(means, means5 %>%
                    dplyr::rename_at(channel_names(.),~stringr::str_remove(.,"_M")))
expect_equal(means, means6 %>%
                    dplyr::select_at(channel_names(means6)[stringr::str_detect(channel_names(means6),"_mean")],~stringr::str_remove(.,"_mean")))
expect_equal(data.table::as.data.table(means$.signal), signal_means_baseline)
expect_equal(means$.segments, segment_summ)
})


meansa <- data %>% dplyr::group_by(description) %>%
    dplyr::summarize_at(channel_names(.), mean, na.rm = TRUE)

meansa2 <- data %>% dplyr::group_by(description)  %>%
    dplyr::summarize_at(channel_names(.), "mean", na.rm =TRUE)

meansa3 <- data %>% dplyr::group_by(description) %>%
    dplyr::summarize_at(channel_names(.), ~ mean(., na.rm = TRUE))

meansa4 <- data %>% dplyr::group_by(description) %>%
    dplyr::summarize_at(channel_names(.), list(~ mean(., na.rm = TRUE)))

# Change title
meansa5 <- data %>% dplyr::group_by(description) %>%
    dplyr::summarize_at(channel_names(.), list(M= ~ mean(.,na.rm = TRUE)))

meansa6 <- data %>% dplyr::group_by(description) %>%
    dplyr::summarize_at(channel_names(.), list(~mean(., na.rm = TRUE), ~ var(., na.rm = TRUE)))

signal_meansa_baseline <- eeguana:::left_join_dt(data$.signal, data.table::data.table(data$.segments)) %>%
    .[,lapply(.SD,mean, na.rm=TRUE), .SDcols = channel_names(data), by ="description"] %>%
    dplyr::select(-description) %>% cbind(data.table::data.table(.id=c(1L,2L),.sample=sample_int(c(NA,NA),500)),.)
data.table::setkey(signal_meansa_baseline, ".id",".sample")


test_that("summarize ats with extra args", {
expect_equal(meansa, meansa2)
expect_equal(meansa, meansa3)
expect_equal(meansa, meansa4)
expect_equal(meansa, meansa5 %>%
                    dplyr::rename_at(channel_names(.),~stringr::str_remove(.,"_M")))
expect_equal(meansa, meansa6 %>%
                    dplyr::select_at(channel_names(meansa6)[stringr::str_detect(channel_names(means6),"_mean")],~stringr::str_remove(.,"_mean")))
expect_equal(data.table::as.data.table(meansa$.signal), signal_meansa_baseline)
})



vars <- data %>% dplyr::group_by(description) %>%
    dplyr::summarize_at(channel_names(.), var, na.rm=TRUE)

vars2 <- data %>% dplyr::group_by(description)  %>%
    dplyr::summarize_at(channel_names(.), "var", na.rm=TRUE)

vars3 <- data %>% dplyr::group_by(description) %>%
    dplyr::summarize_at(channel_names(.), ~ var(., na.rm=TRUE))

vars4 <- data %>% dplyr::group_by(description) %>%
    dplyr::summarize_at(channel_names(.), list(~ var(., na.rm=TRUE)))

                                        # Change title
vars5 <- data %>% dplyr::group_by(description) %>%
    dplyr::summarize_at(channel_names(.), list(M= ~ var(., na.rm=TRUE)))
signal_vars_baseline <- eeguana:::left_join_dt(data$.signal, data.table::data.table(data$.segments)) %>%
    .[,lapply(.SD,var, na.rm=TRUE), .SDcols = channel_names(data), by ="description"] %>%
    dplyr::select(-description) %>% cbind(data.table::data.table(.id=c(1L,2L),.sample=sample_int(c(NA,NA),500)),.)
data.table::setkey(signal_vars_baseline, ".id",".sample")

#vars was problematic with summarize_at, the call was giving the inside of the function
test_that("summarize ats vars", {
    expect_equal(vars, vars2)
    expect_equal(vars, vars3)
    expect_equal(vars, vars4)
    expect_equal(vars, vars5 %>%
                        dplyr::rename_at(channel_names(.),~stringr::str_remove(.,"_M")))
    expect_equal(vars, meansa6 %>%
                       dplyr::select_at(channel_names(meansa6)[stringr::str_detect(channel_names(meansa6),"_var")],~stringr::str_remove(.,"_var")))
    expect_equal(as.matrix(vars$.signal), as.matrix(signal_vars_baseline))
    expect_true(all(sapply(vars$.signal[,channel_names(vars), with=FALSE], is_channel_dbl)))
    expect_equal(vars$.segments, segment_summ)
})

###
varifs <- data %>% dplyr::group_by(description) %>%
    dplyr::summarize_if(is_channel_dbl, var, na.rm=TRUE)

varifs2 <- data %>% dplyr::group_by(description)  %>%
    dplyr::summarize_if(is_channel_dbl, "var", na.rm=TRUE)

varifs3 <- data %>% dplyr::group_by(description) %>%
    dplyr::summarize_if(is_channel_dbl, ~ var(., na.rm=TRUE))

varifs4 <- data %>% dplyr::group_by(description) %>%
    dplyr::summarize_if(is_channel_dbl, list(~ var(., na.rm=TRUE)))

                                        # Change title
varifs5 <- data %>% dplyr::group_by(description) %>%
    dplyr::summarize_if(is_channel_dbl, list(M= ~ var(., na.rm=TRUE)))

test_that("summarize ats no extra args", {
    expect_equal(varifs, varifs2)
    expect_equal(varifs, varifs3)
    expect_equal(varifs, varifs4)
    expect_equal(varifs, varifs5 %>%
                        dplyr::rename_at(channel_names(.),~stringr::str_remove(.,"_M")))
    expect_equal(varifs, meansa6 %>%
                         dplyr::select_at(channel_names(meansa6)[stringr::str_detect(channel_names(meansa6),"_var")],~stringr::str_remove(.,"_var")))
    expect_equal(as.matrix(varifs$.signal), as.matrix(signal_vars_baseline))
    expect_true(all(sapply(varifs$.signal[,channel_names(vars), with=FALSE], is_channel_dbl)))
    expect_equal(varifs$.segments, segment_summ)
})

data %>% dplyr::filter(.id !=2) %>% dplyr::group_by( .sample) %>%
    dplyr::summarize_at(channel_names(.), ~(mean(.[description == "s70"] -
                                                 .[description == "s71"], na.rm=TRUE)))


## pow <- function(x) x^2

## means<- data %>% dplyr::transmute_at("Fp1", pow)

## means2 <- data %>% dplyr::group_by(description)  %>%
##     dplyr::summarize_at(channel_names(.), "mean")

## means3 <- data %>% dplyr::group_by(description) %>%
##     dplyr::summarize_at(channel_names(.), ~ mean(.))

## means4 <- data %>% dplyr::group_by(description) %>%
##     dplyr::summarize_at(channel_names(.), list(~ mean(.)))

##                                         # Change title
## means5 <- data %>% dplyr::group_by(description) %>%
##     dplyr::summarize_at(channel_names(.), list(M= ~ mean(.)))

## means6 <- data %>% dplyr::group_by(description) %>%
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
