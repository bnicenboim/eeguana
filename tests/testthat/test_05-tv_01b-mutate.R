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
suppressMessages(data <- bind(data_1, data_2) %>% eeg_mutate(val = 1:6))
# for checks later
reference_data <- data.table::copy(data)

test_mutates_sgm <- function(data, keep = TRUE, .by_ref = FALSE) {
  ref_data <- data.table::copy(data)
  data <- data.table::copy(data)
  ref_events <- data.table::copy(data$.events)
  groups <- eeg_group_vars(data)
  signal_df <- as.data.frame(data$.signal) %>%
    dplyr::left_join(data$.segments, by = ".id") %>%
    dplyr::group_by_at(dplyr::all_of(groups))
  grouped <- length(eeg_group_vars(data)) > 0
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
  
  segments_tbl <- data.table::copy(data)$.segments %>% 
    dplyr::group_by_at(dplyr::vars(intersect(eeg_group_vars(data), 
                                             colnames(data$.segments) )))
  
  data_s10 <- fun(data, segment = segment + 10)
  if (.by_ref) {
    expect_equal_eeg_lst(data_s10, data)
    data <- data.table::copy(ref_data)
  }
  
  expect_equal_plain_df(
    data_s10$.segment,
    mutate_s(segments_tbl, segment = segment + 10)
  )
  
  
  if (keep) expect_equal_but_cnt_sgm(data_s10, data)
  
  if (!keep & grouped & ".recording" %in% groups  ) {
      expect_message(expect_equal_but_cnt_sgm(data_s10, data %>% eeg_select(segment)),
                     regexp = "Adding missing grouping variables:") 
  } else if (!keep ) {
    expect_equal_but_cnt_sgm(data_s10, data %>% eeg_select(segment))
  }
  

  data_news10 <- fun(data, news = segment + 10)
  if (.by_ref) {
    expect_equal_eeg_lst(data_news10, data)
    data <- data.table::copy(ref_data)
  }
  expect_equal_plain_df(
    data_news10$.segments,
    mutate_s(segments_tbl, news = segment + 10)
  )
  if (keep) {
    expect_equal_but_sgm(
      data_news10,
      data
    )
  }
  if (!keep) {
      expect_equal_but_cnt_sgm(
        data_news10,
        data %>% eeg_mutate(news = segment + 10) %>%
          eeg_select(news)
      )
    
  }
 
  data_max <- fun(data, max = max(val))
  if (.by_ref) {
    expect_equal_eeg_lst(data_max, data)
    data <- data.table::copy(ref_data)
  }
  expect_equal_plain_df(
    data_max$.segments,
    mutate_s(segments_tbl, max = max(val))
  )
  
    expect_equal_but_sgm(
      data_max,
      data
    )
  
  data_mean <- fun(data, mean = mean(segment), m =segment + 2)
  if (.by_ref) {
    expect_equal_eeg_lst(data_mean, data)
    data <- data.table::copy(ref_data)
  }
  expect_equal_plain_df(
    data_mean$.segment,
    mutate_s(segments_tbl, mean = mean(segment), m = segment + 2)
  )
  
    expect_equal_but_sgm(
      data_mean,
      data
    )
  
  
  if (!grouped & keep) {
    data_NULL <- fun(data, segment = NULL)
    if (.by_ref) {
      expect_equal_eeg_lst(data_NULL, data)
      data <- data.table::copy(ref_data)
    }
  
      expect_equal_eeg_lst(
        data_NULL,
        eeg_select(data, -segment)
      )
  }
  
  if (!grouped & !keep) {
    data_NULL <- fun(data, segment = NULL)
    if (.by_ref) {
      expect_equal_eeg_lst(data_NULL, data)
      data <- data.table::copy(ref_data)
    }
    

      expect_equal(
        data_NULL,
        eeg_select(ref_data, -segment, -condition, -val)
      )
    
    
  }
  if (!grouped) {
    expect_equal_plain_df(
      data_NULL$.segment,
      mutate_s(segments_tbl, segment = NULL)
    )
  }
  
  if (grouped) {
    fun(data, segment = NULL)
  }

  
  data_cst <- fun(data, segment = 10)
  
  if (.by_ref) {
    expect_equal_eeg_lst(data_cst, data)
    data <- data.table::copy(ref_data)
  }
  
  expect_equal_plain_df(
    data_cst$.segment,
    mutate_s(segments_tbl, segment = 10)
  )
  
  expect_equal_but_sgm(
    data_cst,
    data
  )

   
  data_cst2 <- fun(data, segment = 0:(length(segment)-1))
  if (.by_ref) {
    expect_equal_eeg_lst(data_cst2, data)
    data <- data.table::copy(ref_data)
  }
  expect_equal_but_sgm(
    data_cst2,
    data
  )
  expect_equal_plain_df(
    data_cst2$.segments,
    mutate_s(segments_tbl, segment = 0:(length(segment)-1))
  )

  
  # if (!keep & grouped) {
  #   data_cst2 <- fun(data, Y = 1:length(.sample))
  #   expect_equal_but_sgl(
  #     data_cst2,
  #     select(data, -X)
  #   )
  # }
  # 
  
  expect_equal_eeg_lst(data, ref_data)
  NULL
}  
  
 
### TESTS

test_that("dplyr::mutate functions work correctly on ungrouped segments_tbl", {
  test_mutates_sgm(data)
})

if(0){
test_that("dplyr::mutate functions work correctly on ungrouped segments_tbl by reference", {
  test_mutates_sgm(data, .by_ref = TRUE)
})
}
test_that("dplyr::transmute functions work correctly on ungrouped segments_tbl", {
  test_mutates_sgm(data, keep = FALSE)
})

if(0){
test_that("dplyr::transmute functions work correctly on ungrouped segments_tbl and by ref", {
  test_mutates_sgm(data, keep = FALSE, .by_ref = TRUE)
})
}

d_grouped <- data %>% dplyr::group_by(.recording)
d_grouped2 <- data %>% dplyr::group_by(.sample)

test_that("dplyr::mutate functions work correctly on grouped segments_tbl", {
  test_mutates_sgm(d_grouped)
  test_mutates_sgm(d_grouped2)
})

if(0){
test_that("dplyr::mutate functions work correctly on grouped segments_tbl by reference", {
  test_mutates_sgm(d_grouped, .by_ref = TRUE)
  test_mutates_sgm(d_grouped2, .by_ref = TRUE)
})
}

test_that("dplyr::transmute functions work correctly on grouped segments_tbl", {
  test_mutates_sgm(d_grouped, keep = FALSE)
  test_mutates_sgm(d_grouped2, keep = FALSE)
  
})

if(0){
test_that("dplyr::transmute functions work correctly on ungrouped segments_tbl and by ref", {
  test_mutates_sgm(d_grouped, keep = FALSE, .by_ref = TRUE)
  test_mutates_sgm(d_grouped2, keep = FALSE, .by_ref = TRUE)
  
})
}
