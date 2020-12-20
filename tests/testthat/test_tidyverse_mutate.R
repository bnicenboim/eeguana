context("test tidyverse dplyr::mutate")
library(eeguana)
library(dplyr)
expect_equal_plain_df <- eeguana:::expect_equal_plain_df
expect_equal_but_sgl <- eeguana:::expect_equal_but_sgl
expect_equal_but_cnt_sgl <- eeguana:::expect_equal_but_cnt_sgl
expect_equal_but_sgm <- eeguana:::expect_equal_but_sgm
expect_equal_but_cnt_sgm <- eeguana:::expect_equal_but_cnt_sgm

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
data <- bind(data_1, data_2)

# for checks later
reference_data <- data.table::copy(data)



##############################################
### test dplyr dplyr::mutate on ungrouped eeg_lst ###
##############################################
test_mutates_sgl <- function(data, keep = TRUE) {
  ref_data <- data.table::copy(data)
  groups <- group_vars(data)
  signal_df <- as.data.frame(data$.signal)%>%
    left_join(data$.segments, by =".id") %>%
    group_by_at(all_of(groups))
  grouped <- length(group_vars(data))>0
  to_remove <- colnames(data$.segments)[-1]
  if(keep) fun <- mutate else fun <- transmute
  mutate_c <- function(tbl,...) {
    fun(tbl, .id =.id, .sample =.sample,...) %>%
      ungroup() %>%
      select( -any_of(to_remove))
  }

  data_X10 <- fun(data, X = X + 10)
  expect_equal_plain_df(data_X10$.signal,
                        mutate_c(signal_df, X= X +10))
  if(keep)
    expect_equal_but_cnt_sgl(data_X10,
                             data)
  if(!keep)
    expect_equal_but_cnt_sgl(data_X10,
                             data %>% select(-Y))

  data_Xs10 <- fun(data, X = X * segment)
  expect_equal_plain_df(data_Xs10$.signal,
                        mutate_c(signal_df, X = X * segment)
                        )

  if(keep)
    expect_equal_but_cnt_sgl(data_Xs10,
                             data)
  if(!keep)
    expect_equal_but_cnt_sgl(data_Xs10,
                             data %>% select(-Y))

  data_ZZX10 <- fun(data, ZZ = X + 10)
  expect_equal_plain_df(data_ZZX10$.signal,
                        mutate_c(signal_df, ZZ = X + 10))
  if(keep)
    expect_equal_but_sgl(data_ZZX10,
                         data)
  if(!keep)
    expect_equal_but_cnt_sgl(data_ZZX10,
                             data %>% mutate(ZZ = X + 10) %>%
                               select(-X, -Y))

  expect_true(nrow(filter(data_ZZX10$.events, .channel == "ZZ")) == 0)
  expect_true(nrow(filter(channels_tbl(data_ZZX10), .channel == "ZZ")) > 0)

  data_mean <- fun(data, mean = mean(X))
  expect_equal_plain_df(data_mean$.signal,
                        mutate_c(signal_df, mean = mean(X)))
  if(keep)
    expect_equal_but_sgl(data_mean,
                         data)
  if(!keep)
    expect_equal_but_sgl(data_mean,
                         data %>% mutate(mean = mean(X)) %>%
                           select(-X, -Y))

  data_mean <- fun(data, mean = mean(X), m = X +2)
  expect_equal_plain_df(data_mean$.signal,
                        mutate_c(signal_df, mean = mean(X), m = X +2))

  if(keep)
    expect_equal_but_sgl(data_mean,
                         data)
  if(!keep)
    expect_equal_but_sgl(data_mean,
                         data %>% mutate(mean = mean(X) + 10, m = X+2) %>%
                           select(-X, -Y))

  if(!grouped & keep){
    data_NULL <- fun(data, Y = NULL)
    expect_equal(data_NULL,
                 select(data, -Y))
  }

  if(!grouped & !keep){
    data_NULL <- expect_warning(fun(data, Y = NULL))
    expect_warning(expect_equal(data_NULL,
                                select(data, -Y, -X)))
  }
  if(!grouped)
    expect_equal_plain_df(data_NULL$.signal,
                          mutate_c(signal_df, Y = NULL))

  if(grouped)
    expect_error(fun(data, Y = NULL))

  if(!grouped & keep)
    expect_message(data_cst <- fun(data, Y =10) )

  if(!grouped & !keep)
    expect_warning(expect_message(data_cst <- fun(data, Y =10) ))

  if(grouped)
    # when groupped it keeps being a channel
    data_cst <- fun(data, Y =10)

  expect_equal_plain_df(data_cst$.signal,
                        mutate_c(signal_df, Y = 10))

  if(!grouped & keep )
    expect_equal_but_sgl(data_cst,
                         select(data, -Y))


  if(!grouped & !keep )
    expect_warning( expect_equal_but_sgl(data_cst,
                                         select(data, -Y, -X)))


  if(grouped & keep)
    expect_equal_but_sgl(data_cst,
                         data)

  if(grouped & !keep)
    expect_equal_but_sgl(data_cst,
                         data %>% select(-X))

  if(keep & !grouped){
      expect_message(data_cst2 <- fun(data, Y = 1:length(.sample)))
      expect_equal_but_sgl(data_cst2,
                           select(data, - Y))
      }

if(keep & grouped){
      data_cst2 <- fun(data, Y = 1:length(.sample))
      expect_equal_but_sgl(data_cst2,
                           select(data))
    }


 if(!keep & !grouped){
    expect_warning(expect_message(data_cst2 <- fun(data, Y = 1:length(.sample))))
    expect_warning(expect_equal_but_sgl(data_cst2,
                                        select(data, - Y, -X)))
  }
if(!keep & grouped){
  data_cst2 <- fun(data, Y = 1:length(.sample))
  expect_equal_but_sgl(data_cst2,
                                        select(data, -X))
}

  expect_equal_plain_df(data_cst2$.signal,
                        mutate_c(signal_df, Y = 1:length(Y)))



  expect_message(data_ch <- fun(data, Y = channel_dbl(10)), regexp = NA)
  expect_equal_plain_df(data_ch$.signal,
                        mutate_c(signal_df, Y = 10))
  if(keep){
    expect_equal_but_sgl(data_ch,
                         select(data))
  } else {
    expect_equal_but_sgl(data_ch,
                         select(data, -X))
  }
  expect_message(data_ch2 <- fun(data, Y = channel_dbl(1:length(Y))),regexp = NA)
  expect_equal_plain_df(data_ch2$.signal,
                        mutate_c(signal_df, Y = 1:length(Y)))
  if(keep){
    expect_equal_but_sgl(data_ch2,
                         data)
  } else{
    expect_equal_but_sgl(data_ch2,
                         data %>% select(-X))
  }
  expect_equal(data, ref_data)
}

### TESTS

test_that("dplyr::mutate functions work correctly on ungrouped signal_tbl", {
  test_mutates_sgl(data)
})


test_that("dplyr::mutate functions work correctly on ungrouped segments_tbl", {
  data <- as_eeg_lst(data)
  segments_tbl <- data$.segments
  data_seg10 <- mutate(data, segment = segment + 10)
  expect_equal_plain_df(data_seg10$.segments,
                        mutate(segments_tbl, segment = segment + 10))
  expect_equal_but_cnt_sgm(data_seg10,
                           data)

  data_ZZseg10 <- mutate(data, ZZ = segment + 10)
  expect_equal_plain_df(data_ZZseg10$.segments,
                        mutate(segments_tbl, ZZ = segment + 10))
  expect_equal_but_sgm(data_ZZseg10,
                       data)


  data_NULL <- mutate(data, segment = NULL)
  expect_equal_plain_df(data_NULL$.segments,
                        mutate(segments_tbl, segment = NULL))
  expect_equal(data_NULL,
               select(data, -segment))

  data_cst <- mutate(data, segment =10)
  expect_equal_plain_df(data_cst$.segments,
                        mutate(segments_tbl, segment = 10))
  expect_equal_but_sgm(data_cst,
                       data)

  data_cst <- mutate(data, segment =1:6)
  expect_equal_plain_df(data_cst$.segments,
                        mutate(segments_tbl, segment = 1:6))
  expect_equal_but_sgm(data_cst,
                       data)
  expect_equal(data, reference_data)
})

message("mutation of .id and .sample should be tested")

mutate4_eeg_lst <- mutate(data, subject = .recording)


nsamples <- 100
nsamples_ <- 100
data_nsamples_ <- data %>% mutate(Z = X + nsamples_)
data_100 <- data %>% mutate(Z = X + 100)
data_nsamples <- data %>% mutate(Z = X + nsamples)

test_that("dplyr:mutate functions understand the right scope",{
  expect_equal(data_100, data_nsamples)
  expect_equal(data_nsamples_, data_nsamples)
})



# TODO  I don't think these functions exist yet
message("mutate_at and mutate_all, etc haven't been tested")



test_that("dplyr::transmute functions work correctly on ungrouped signal_tbl", {
  test_mutates_sgl(data, keep = FALSE)
})


############################################
### test dplyr mutate on grouped eeg_lst ###
############################################
grouped_data <- list()
grouped_data[[1]] <- group_by(data, .sample)
grouped_data[[2]] <- group_by(data, .id)
grouped_data[[3]] <- group_by(data, .recording)
grouped_data[[4]] <- group_by(data, .sample, .recording)
grouped_data[[5]] <- group_by(data, .id, .recording)
grouped_data[[6]] <- group_by(data, .id, .sample, .recording)
grouped_data[[7]] <- group_by(data, .sample, condition)

test_that("dplyr::mutate functions work correctly on grouped signal_tbl", {
  for(d in grouped_data){
   # print(group_vars(d))
    test_mutates_sgl(d)
  }

})

test_that("dplyr::transmute functions work correctly on grouped signal_tbl", {
  for(d in grouped_data){
    #print(group_vars(d))
    test_mutates_sgl(d, keep = FALSE)
  }
})

# check against original data
test_that("data didn't change after grouping and mutate functions", {
  expect_equal(reference_data, data)
})




### test as_time conversion  ###
eeg_time <- suppressWarnings(mutate(data, .time = as_time(.sample, .unit = "seconds")) %>%
                               summarize(mean = mean(.time)))

tbl_time <- data %>%
  as_tibble() %>%
  summarize(mean = mean(.time))

test_that("as_time works as expected", {
  expect_equal(as.double(eeg_time$.signal[["mean"]]), tbl_time$mean)
  expect_warning(mutate(data, .time = as_time(.sample, .unit = "seconds")) %>%
                   summarize(mean = mean(.time)))
})




###########################
### test serial mutates ###
###########################

# Bruno's note: Maybe it's fine that the following fails:
# mutate(data, .time = as_time(.sample, .unit = "milliseconds")) %>%
#   group_by(.time) %>%
#   summarize(mean(X))

# create new variable with mutate
eeg_mutate_1 <- data %>%
  mutate(bin = ntile(.sample, 5))

tbl_mutate_1 <- data %>%
  as_tibble() %>%
  mutate(bin = ntile(.time, 5))

# use new variable in second variable doesn't work in eeg_lst (#35)
## eeg_mutate_2 <- data %>% mutate(.time = as_time(.sample, .unit = "ms"), bin = ntile(time, 5))
# work around:
eeg_mutate_2 <- data %>%
  mutate(.time = as_time(.sample, .unit = "ms")) %>%
  mutate(bin = ntile(.time, 5))

tbl_mutate_2 <- data %>%
  as_tibble() %>%
  mutate(test = .time + 1, bin = ntile(test, 5))

# can't summarize by a mutated variable within eeg_lst (#43)
eeg_mutate_3 <- data %>%
  mutate(bin = ntile(.sample, 5)) %>%
  group_by(bin) %>%
  summarize(mean = mean(X))

tbl_mutate_3 <- data %>%
  as_tibble() %>%
  mutate(bin = ntile(.time, 5)) %>%
  group_by(bin) %>%
  summarize(mean = mean(.value[.key == "X"]))


test_that("mutate works the same on eeg_lst as on tibble", {
  expect_equal(eeg_mutate_1$.signal[["bin"]], tbl_mutate_1$bin[tbl_mutate_1$.key == "X"])
  expect_equal(eeg_mutate_2$.signal[["bin"]], tbl_mutate_2$bin[tbl_mutate_1$.key == "X"])
  expect_equal(eeg_mutate_3$.signal[["bin"]], tbl_mutate_3$bin)
})


message("test mutate when there are ICAs")
