library(eeguana)
options(eeguana.verbose = TRUE)
#library(dplyr)
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
data <- bind(data_1, data_2)

# for checks later
reference_data <- data.table::copy(data)

test_mutates_sgm <- function(data, keep = TRUE, .by_ref = FALSE) {
  ref_data <- data.table::copy(data)
  data <- data.table::copy(data)
  ref_events <- data.table::copy(data$.events)
  groups <- group_vars(data)
  signal_df <- as.data.frame(data$.signal) %>%
    dplyr::left_join(data$.segments, by = ".id") %>%
    dplyr::group_by_at(all_of(groups))
  grouped <- length(group_vars(data)) > 0
  to_remove <- colnames(data$.signal)[-1]
  if (keep) {
    fun <- purrr::partial(eeg_mutate, .by_reference = .by_ref)
    dfun <- dplyr::mutate
  }  else {
    fun <- purrr::partial(eeg_transmute, .by_reference = .by_ref)
    dfun <- dplyr::transmute
  }
  
  mutate_s <- function(tbl, ...) {
    dfun(tbl, .id = .id, .recording = .recording, ...) 
  }
  
  segments_tbl <- data$.segments
  
  data_s10 <- fun(data, segment = segment + 10)
  expect_equal_plain_df(
    data_s10$.segment,
    mutate_s(data$.segment, segment = segment + 10)
  )
  
  if (.by_ref) {
    expect_equal(data_s10, data)
    data <- data.table::copy(ref_data)
  }
  
  if (keep) expect_equal_but_cnt_sgm(data_s10, data)
  if (!keep) {
    
    if(.by_ref == FALSE){
      expect_equal_but_cnt_sgm(data_s10, data %>% select(segment))
    } else {
      #fix
      expect_equal(data_s10$.events,
                   ref_events[.channel == "Y",names(ref_data$.events) := NA][])
      ref_events <- data.table::copy(ref_data$.events)
    }
  }
  

  data_news10 <- fun(data, news = segment + 10)
  if (.by_ref) {
    expect_equal(data_news10, data)
    data <- data.table::copy(ref_data)
  }
  expect_equal_plain_df(
    data_news10$.segments,
    mutate_s(data_news10$.segments, news = segment + 10)
  )
  if (keep) {
    expect_equal_but_sgm(
      data_news10,
      data
    )
  }
  if (!keep) {
    if(.by_ref == FALSE){
      expect_equal_but_cnt_sgm(
        data_news10,
        data %>% mutate(news = segment + 10) %>%
          select(news)
      )
    } else {
      #fix
      expect_equal(data_ZZX10$.events,
                   ref_events[.channel %in% c("X", "Y"), names(ref_data$.events) := NA][])
      ref_events <- data.table::copy(ref_data$.events)
    }
  }
 
  data_mean <- fun(data, mean = mean(segment))
  if (.by_ref) {
    expect_equal(data_mean, data)
    data <- data.table::copy(ref_data)
  }
  expect_equal_plain_df(
    data_mean$.segments,
    mutate_s(data$.segments, mean = mean(segment))
  )
  if (keep) {
    expect_equal_but_sgm(
      data_mean,
      data
    )
  }
  if (!keep) {
    
    if(.by_ref == FALSE){
      expect_equal_but_sgm(
        data_mean,
        data %>% mutate_s(mean = mean(segment)) %>%
          select(mean)
      )
    } else {
      #fix
      expect_equal(data_mean$.events,
                   ref_events[.channel %in% c("X", "Y"), names(ref_data$.events) := NA][])
      ref_events <- data.table::copy(ref_data$.events)
    }
  }
  
  data_mean <- fun(data, mean = mean(segment), m =segment + 2)
  if (.by_ref) {
    expect_equal(data_mean, data)
    data <- data.table::copy(ref_data)
  }
  expect_equal_plain_df(
    data_mean$.segment,
    mutate_s(data$.segment, mean = mean(segment), m = segment + 2)
  )
  
  if (keep) {
    expect_equal_but_sgm(
      data_mean,
      data
    )
  }
  
  if(!keep){
    if(.by_ref == FALSE){
      expect_equal_but_sgm(
        data_mean,
        data %>% mutate_s(mean = mean(segment) + 10, m = segment + 2) %>%
          select(mean, m)
      )
    } else {
      expect_equal(data_mean$.events,
                   ref_events[.channel %in% c("X", "Y"), names(ref_data$.events) := NA][])
      ref_events <- data.table::copy(ref_data$.events)
    }
  }
  
  
  if (!grouped & keep) {
    data_NULL <- fun(data, segment = NULL)
    if(.by_ref == FALSE){
      expect_equal_eeg_lst(
        data_NULL,
        select(ref_data, -segment)
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
    data_NULL <- expect_warning(fun(data, segment = NULL))
    if(!.by_ref){
      expect_warning(expect_equal(
        data_NULL,
        select(ref_data, -segment, -condition)
      ))
    }
    
  }
  if (!grouped) {
    expect_equal_plain_df(
      data_NULL$.segment,
      mutate_s(data$.segment, segment = NULL)
    )
  }
  
  if (grouped) {
    expect_error(fun(data, segment = NULL))
  }
  
  if (.by_ref) {
    expect_equal(data_NULL, data)
    data <- data.table::copy(ref_data)
  }
  
  data_cst <- fun(data, segment = 10)
  
  if (.by_ref) {
    expect_equal(data_cst, data)
    data <- data.table::copy(ref_data)
  }
  
  expect_equal_plain_df(
    data_cst$.segment,
    mutate_s(data$.segment, segment = 10)
  )
  
  expect_equal_but_sgm(
    data_cst,
    data
  )

   
  data_cst2 <- fun(data, segment = 0:(length(segment)-1))
  if (.by_ref) {
    expect_equal(data_cst2, data)
    data <- data.table::copy(ref_data)
  }
  expect_equal_but_sgm(
    data_cst2,
    data
  )
  expect_equal_plain_df(
    data_cst2$.segments,
    mutate_s(data$.segments, segment = 0:(length(segment)-1))
  )

  if (keep & grouped) {
    
    expect_equal_but_sgm(
      data_cst2,
      select(data)
    )
  }
  
  
  if (!keep & !grouped) {
    expect_warning(expect_message(data_cst2 <- fun(data, Y = 1:length(.sample))))
    if(!.by_ref){
      expect_warning(expect_equal_but_sgl(
        data_cst2,
        select(data, -Y, -X)
      ))
    }
  }
  
  if (!keep & grouped) {
    data_cst2 <- fun(data, Y = 1:length(.sample))
    expect_equal_but_sgl(
      data_cst2,
      select(data, -X)
    )
  }
  

  expect_equal_eeg_lst(data, ref_data)
  NULL
}  
  
 
### TESTS

test_that("dplyr::mutate functions work correctly on ungrouped segments_tbl", {
  test_mutates_sgm(data)
})

test_that("dplyr::mutate functions work correctly on ungrouped segments_tbl by reference", {
  test_mutates_sgm(data, .by_ref = TRUE)
})

test_that("dplyr::transmute functions work correctly on ungrouped segments_tbl", {
  test_mutates_sgl(data, keep = FALSE)
})


test_that("dplyr::transmute functions work correctly on ungrouped segments_tbl and by ref", {
  test_mutates_sgl(data, keep = FALSE, .by_ref = TRUE)
})
