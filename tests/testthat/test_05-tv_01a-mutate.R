library(eeguana)
options(eeguana.verbose = TRUE)
expect_equal_plain_df <- eeguana:::expect_equal_plain_df
expect_equal_but_sgl <- eeguana:::expect_equal_but_sgl
expect_equal_but_cnt_sgl <- eeguana:::expect_equal_but_cnt_sgl
expect_equal_but_sgm <- eeguana:::expect_equal_but_sgm
expect_equal_but_cnt_sgm <- eeguana:::expect_equal_but_cnt_sgm
expect_equal_eeg_lst <- eeguana:::expect_equal_eeg_lst

# tests when factors are used should be done.
data_1 <- eeguana:::data_sincos3id

# just some different X and Y
data_2 <- dplyr::mutate(data_1,
  .recording = "recording2",
  X = sin(X + 10),
  Y = cos(Y - 10),
  condition = c("b", "a", "b")
)

# bind it all together
suppressMessages(data <- bind(data_1, data_2))

# for checks later
reference_data <- data.table::copy(data)


##############################################
### test dplyr dplyr::mutate on ungrouped eeg_lst ###
##############################################
test_mutates_sgl <- function(data, keep = TRUE, .by_ref = FALSE) {
  ref_data <- data.table::copy(data)
  data <- data.table::copy(data)
  ref_events <- data.table::copy(data$.events)
  groups <- eeg_group_vars(data)
  signal_df <- as.data.frame(data$.signal) %>%
    dplyr::left_join(data$.segments, by = ".id") %>%
    dplyr::group_by_at(dplyr::all_of(groups))
  grouped <- length(eeg_group_vars(data)) > 0
  to_remove <- colnames(data$.segments)[-1]
  if (keep) {
    fun <- purrr::partial(eeg_mutate, .by_reference = .by_ref)
    dfun <- dplyr::mutate
  }  else {
     fun <- purrr::partial(eeg_transmute, .by_reference = .by_ref)
     dfun <- dplyr::transmute
  }

  mutate_c <- function(tbl, ...) {
    dfun(tbl, .id = .id, .sample = .sample, ...) %>%
      dplyr::ungroup() %>%
      dplyr::select(-dplyr::any_of(to_remove))
  }

  
  expect_message(data_X10 <- fun(data, X = X + 10), 
                 regexp = ifelse(keep, 
                                 # no message if it's regular mutate
                                 NA,
                                 # message otherwise
                                 ""))
  
  expect_equal_plain_df(
    data_X10$.signal,
    mutate_c(signal_df, X = X + 10)
  )

  if (.by_ref) {
    expect_equal_eeg_lst(data_X10, data)
    data <- data.table::copy(ref_data)
  } else {
    expect_equal_eeg_lst(ref_data, data)
  }

  if (keep) expect_equal_but_cnt_sgl(data_X10, data)
  if (!keep) {

 if(.by_ref == FALSE){
       expect_equal_but_cnt_sgl(data_X10, data %>% eeg_select(-Y))
    } else {
      expect_equal(data_X10$.events,
                   ref_events[.channel == "Y",names(ref_data$.events) := NA][])
      ref_events <- data.table::copy(ref_data$.events)
    }
  }

  
  expect_message(data_Xs10 <- fun(data, X = X *10 * segment), 
                 regexp = ifelse(keep, 
                                 # no message if it's regular mutate
                                 NA,
                                 # message otherwise
                                 ""))
  
    if (.by_ref) {
      expect_equal_eeg_lst(data_Xs10, data)
    data <- data.table::copy(ref_data)
  }

  expect_equal_plain_df(
    data_Xs10$.signal,
    mutate_c(signal_df, X = X *10* segment) %>% as.data.frame()
  )

  if (keep) {
    expect_equal_but_cnt_sgl(
      data_Xs10,
      data
    )
  }


 if (!keep) {

 if(.by_ref == FALSE){
       expect_equal_but_cnt_sgl(data_Xs10, data %>% eeg_select(-Y))
    } else {
      expect_equal(data_Xs10$.events,
                   ref_events[.channel == "Y",names(ref_data$.events) := NA][])
      ref_events <- data.table::copy(ref_data$.events)
    }
  }

  
  expect_message(data_ZZX10 <- fun(data, ZZ = X + 10), 
                 regexp = ifelse(keep, 
                                 # no message if it's regular mutate
                                 NA,
                                 # message otherwise
                                 ""))
    if (.by_ref) {
      expect_equal_eeg_lst(data_ZZX10, data)
    data <- data.table::copy(ref_data)
  }
  expect_equal_plain_df(
    data_ZZX10$.signal,
    mutate_c(signal_df, ZZ = X + 10)
  )
  if (keep) {
    expect_equal_but_sgl(
      data_ZZX10,
      data
    )
  }
  if (!keep) {
 if(.by_ref == FALSE){
    expect_equal_but_cnt_sgl(
      data_ZZX10,
      data %>% eeg_mutate(ZZ = X + 10) %>%
        eeg_select(-X, -Y)
    )
  } else {
     expect_equal(data_ZZX10$.events,
                   ref_events[.channel %in% c("X", "Y"), names(ref_data$.events) := NA][])
      ref_events <- data.table::copy(ref_data$.events)
  }
}
  expect_true(nrow(dplyr::filter(data_ZZX10$.events, .channel == "ZZ")) == 0)
  expect_true(nrow(dplyr::filter(channels_tbl(data_ZZX10), .channel == "ZZ")) > 0)

  expect_message(data_mean <- fun(data, mean = mean(X)), 
                 regexp = ifelse(keep, 
                                 # no message if it's regular mutate
                                 NA,
                                 # message otherwise
                                 ""))
  
    if (.by_ref) {
      expect_equal_eeg_lst(data_mean, data)
    data <- data.table::copy(ref_data)
  }
  expect_equal_plain_df(
    data_mean$.signal,
    mutate_c(signal_df, mean = mean(X))
  )
  if (keep) {
    expect_equal_but_sgl(
      data_mean,
      data
    )
  }
  if (!keep) {

 if(.by_ref == FALSE){
    expect_equal_but_sgl(
      data_mean,
      data %>% eeg_mutate(mean = mean(X)) %>%
        eeg_select(-X, -Y)
    )
   } else {
     expect_equal(data_mean$.events,
                   ref_events[.channel %in% c("X", "Y"), names(ref_data$.events) := NA][])
      ref_events <- data.table::copy(ref_data$.events)
  }
}
  expect_message(data_mean <- fun(data, mean = mean(X), m = X + 2), 
                 regexp = ifelse(keep, 
                                 # no message if it's regular mutate
                                 NA,
                                 # message otherwise
                                 ""))
  
  if (.by_ref) {
    expect_equal_eeg_lst(data_mean, data)
    data <- data.table::copy(ref_data)
   }
  expect_equal_plain_df(
    data_mean$.signal,
    mutate_c(signal_df, mean = mean(X), m = X + 2)
  )

  if (keep) {
    expect_equal_but_sgl(
      data_mean,
      data
    )
  }


if(!keep){
if(.by_ref == FALSE){
   expect_equal_but_sgl(
      data_mean,
      data %>% eeg_mutate(mean = mean(X) + 10, m = X + 2) %>%
        eeg_select(-X, -Y)
    )
   } else {
     expect_equal(data_mean$.events,
                   ref_events[.channel %in% c("X", "Y"), names(ref_data$.events) := NA][])
      ref_events <- data.table::copy(ref_data$.events)
  }
}





  if (!grouped & keep) {
    expect_message(data_NULL <- fun(data, Y = NULL))
    if(.by_ref == FALSE){
     expect_equal_eeg_lst(
       data_NULL,
        eeg_select(ref_data, -Y)
     )
    } else {
      expect_equal_plain_df(mutate_c(signal_df, Y= NULL),
                   data_NULL$.signal)
      expect_equal(data$.events, 
                   ref_events[.channel == "Y",names(ref_data$.events) := NA][])
      ref_events <- data.table::copy(ref_data$.events)
    }
  }

  if (!grouped & !keep) {
     expect_message(expect_warning(data_NULL <- fun(data, Y = NULL)))
    if(!.by_ref){
      expect_warning(expect_equal(
        data_NULL,
        eeg_select(ref_data, -Y, -X)
      ))
    }

  }
  
  if (!grouped) {
    expect_equal_plain_df(
      data_NULL$.signal,
      mutate_c(signal_df, Y = NULL)
    )
  }

  if (grouped & keep) {
    expect_message(fun(data, Y = NULL))
  }
  if (grouped & !keep) {
    expect_warning(expect_message(fun(data, Y = NULL)))
  }
  
  if (.by_ref) {
    expect_equal_eeg_lst(data_NULL, data)
    data <- data.table::copy(ref_data)
  }
  
  if (!grouped & keep) {
    # it doesn't throw an message anymore
      data_cst <- fun(data, Y = 10)
  }

  if (!grouped & !keep) {
    expect_message(data_cst <- fun(data, Y = 10))
  }

  if (grouped) {
    # when groupped it keeps being a channel
    expect_message(data_cst <- fun(data, Y = 10), regexp = ifelse(keep, NA, ""))

  }
 
  expect_equal_plain_df(
    data_cst$.signal,
    mutate_c(signal_df, Y = 10)
  )

  if (!grouped & keep) {

    if(.by_ref == FALSE){
      #keeps the events table as it is
      expect_equal_but_sgl(
        data_cst,
        ref_data
      )
    } else {
      expect_equal_plain_df(mutate_c(signal_df, Y= 10),
                            data_cst$.signal)
      expect_equal(data_cst$.events, 
                   ref_events[.channel == "Y",names(ref_data$.events) := NA][])
      ref_events <- data.table::copy(ref_data$.events)
    }
  }


  if (!grouped & !keep) {
if(!.by_ref){
    expect_equal_but_sgl(
      data_cst,
      eeg_select(ref_data,  -X)
    )
  }
}

  if (grouped & keep) {
    expect_equal_but_sgl(
      data_cst,
      data
    )
  }

  if (grouped & !keep) {
    expect_equal_but_sgl(
      data_cst,
      data %>% eeg_select(-X)
    )
  }
   if (.by_ref) {
     expect_equal_eeg_lst(data_cst, data)
      data <- data.table::copy(ref_data)
    }

  if (keep & !grouped) {
    expect_message(expect_message(data_cst2 <- fun(data, Y = 1:n())))

    expect_equal_but_sgl(
      data_cst2,
      eeg_select(data, -Y)
    )
  }

  if (keep & grouped) {
    data_cst2 <- fun(data, Y = 1:n())
    expect_equal_but_sgl(
      data_cst2,
      eeg_select(data)
    )
  }


  if (!keep & !grouped) {
    expect_warning(expect_message(expect_message(data_cst2 <- fun(data, Y = 1:n()))))
    if(!.by_ref){
    expect_warning(expect_equal_but_sgl(
      data_cst2,
      eeg_select(data, -Y, -X)
    ))
  }
    }

  if (!keep & grouped) {
    expect_message(data_cst2 <- fun(data, Y = 1:n()))
    expect_equal_but_sgl(
      data_cst2,
      eeg_select(data, -X)
    )
  }

  expect_equal_plain_df(
    data_cst2$.signal,
    mutate_c(signal_df, Y = 1:length(Y))
  )
  if (.by_ref) {
      expect_equal_eeg_lst(data_cst2, data)
      data <- data.table::copy(ref_data)
  }
  if(!keep){
    expect_message(data_ch <- fun(data, Y = channel_dbl(10)))
  } else {
    expect_message(data_ch <- fun(data, Y = channel_dbl(10)), regexp = NA)
  }

 if (.by_ref) {
      expect_equal_eeg_lst(data_ch, data)
      data <- data.table::copy(ref_data)
    }
  expect_equal_plain_df(
    data_ch$.signal,
    mutate_c(signal_df, Y = 10)
  )
  if (keep) {
    expect_equal_but_sgl(
      data_ch,
      eeg_select(data)
    )
  } else {
    if(!.by_ref){
    expect_equal_but_sgl(
      data_ch,
      eeg_select(data, -X)
    )
    }
  }

  if(!keep){
  expect_message(data_ch2 <- fun(data, Y = channel_dbl(1:length(Y))))
  } else {
  expect_message(data_ch2 <- fun(data, Y = channel_dbl(1:length(Y))), regexp = NA)
  }

 if (.by_ref) {
      expect_equal(data_ch2, data)
      data <- data.table::copy(ref_data)
    }
  expect_equal_plain_df(
    data_ch2$.signal,
    mutate_c(signal_df, Y = 1:length(Y))
  )
  if (keep) {
    expect_equal_but_sgl(
      data_ch2,
      data
    )
  } else {
    # rows in events cannot be removed by reference
    if(.by_ref != TRUE){
    expect_equal_but_sgl(
      data_ch2,
       data %>% eeg_select(-X)
    )
    }
  }
  expect_equal_eeg_lst(data, ref_data)
  NULL
}



test_that("dplyr::mutate functions work correctly on ungrouped signal_tbl", {
  test_mutates_sgl(data)
})

if(0){ #not in use for now
test_that("dplyr::mutate functions work correctly on ungrouped signal_tbl by reference", {
  test_mutates_sgl(data, .by_ref = TRUE)
})
}

test_that("dplyr::transmute functions work correctly on ungrouped signal_tbl", {
  test_mutates_sgl(data, keep = FALSE)
})

if(0){
test_that("dplyr::transmute functions work correctly on ungrouped signal_tbl and by ref", {
  test_mutates_sgl(data, keep = FALSE, .by_ref = TRUE)
})
}


nsamples <- 100
nsamples_ <- 100
data_nsamples_ <- data %>% eeg_mutate(Z = X + nsamples_)
data_100 <- data %>% eeg_mutate(Z = X + 100)
data_nsamples <- data %>% eeg_mutate(Z = X + nsamples)

test_that("dplyr:mutate functions understand the right scope", {
  expect_equal(data_100, data_nsamples)
  expect_equal(data_nsamples_, data_nsamples)
})



############################################
### test dplyr mutate on grouped eeg_lst ###
############################################
grouped_data <- list()
grouped_data[[1]] <- eeg_group_by(data, .sample)
grouped_data[[2]] <- eeg_group_by(data, .id)
grouped_data[[3]] <- eeg_group_by(data, .recording)
grouped_data[[4]] <- eeg_group_by(data, .sample, .recording)
grouped_data[[5]] <- eeg_group_by(data, .id, .recording)
grouped_data[[6]] <- eeg_group_by(data, .id, .sample, .recording)
grouped_data[[7]] <- eeg_group_by(data, .sample, condition)

test_that("dplyr::mutate functions work correctly on grouped signal_tbl", {
  for (d in grouped_data) {
    # print(group_vars(d))
    test_mutates_sgl(d)
  }
})

test_that("dplyr::transmute functions work correctly on grouped signal_tbl", {
  for (d in grouped_data) {
    # print(group_vars(d))
    test_mutates_sgl(d, keep = FALSE)
  }
})

# check against original data
test_that("data didn't change after grouping and mutate functions", {
  expect_equal(reference_data, data)
})




### test as_time conversion  ###

test_that("as_time works as expected", {
  expect_warning(
    expect_message(eeg_time <-
                eeg_mutate(data, 
                  .time = as_time(.sample, .unit = "seconds")) %>%
  eeg_summarize(mean = mean(.time)), regexp = "The following"))

  tbl_time <- data  %>%
  dplyr::as_tibble() %>%
  dplyr::summarize(mean = mean(.time))

  expect_equal(as.double(eeg_time$.signal[["mean"]]), tbl_time$mean)
  expect_warning(expect_message(eeg_mutate(data, .time = as_time(.sample, .unit = "seconds")) %>%
    eeg_summarize(mean = mean(.time))))
})



test_that("mutation of samples works when it should",{
  expect_message(msample_1 <- data %>%
                   eeg_mutate(bin = dplyr::ntile(.sample, 5)), regexp= "The following")
  msample_1_dt <- data.table::copy(data)
  msample_1_dt$.signal[, bin := dplyr::ntile(.sample,5)]
  expect_equal(msample_1, msample_1_dt)
  #TODO better error
  expect_warning(expect_error(data %>% eeg_mutate(.sample = NULL)))
  
  ## No more warning #TODO double warning is unnecessary
  # expect_warning(expect_message(expect_warning(data %>% eeg_mutate(.sample = 3),
  #                                              regexp = "Values of .sample should be samples"), regexp = "The following"))

})

### TESTS

test_that("equivalent functions", {
  expect_equal(eeg_mutate(data, X = X *10), 
               dplyr::mutate(data, X = X *10))
  expect_equal(suppressMessages(eeg_transmute(data, X = X *10)), 
               suppressMessages(dplyr::transmute(data, X = X *10)))
})


message("\n***")
message("test mutate when there are ICAs")
message("test by groups by reference")
message("do mutate_across  etc haven't been tested")
message("***\n")

