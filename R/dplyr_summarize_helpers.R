summarize_eeg_lst <- function(.eeg_lst, dots, .groups) {
  .eeg_lst$.signal <- summarize_eval_signal(.eeg_lst, dots)
  update_summarized_eeg_lst(.eeg_lst, .groups)
}

update_summarized_eeg_lst <- function(.eeg_lst,  .groups) {

  ## Restructure segments table to fit the new signal table
  if (nrow(.eeg_lst$.signal) != 0) {
    last_id <- max(.eeg_lst$.signal$.id)
  } else {
    last_id <- integer(0)
  }

  .eeg_lst$.segments <- summarize_segments(
    segments = .eeg_lst$.segments,
    segments_groups = group_vars_segments(.eeg_lst),
    last_id = last_id,
    .groups =  .groups
  )
  ## Restructure events table
  # TODO maybe I can do some type of summary of the events table, instead
  .eeg_lst$.events <- new_events_tbl(sampling_rate = sampling_rate(.eeg_lst))
  # update channels in the events and the meta data (summarize deletes the metadata of the channels)
  .eeg_lst <- update_events_channels(.eeg_lst) #
  validate_eeg_lst(.eeg_lst)
}


#' @noRd
summarize_segments <- function(segments, segments_groups, last_id, .groups) {
  # data.table::data.table(segments)[,unique(.SD),.SDcols=c(segments_groups)][] %>% dplyr::as_tibble() %>%
  if (length(segments_groups) != 0) {
    ### IMPORTANT: segments need to be grouped and summarized as data.table does, which is differently than dplyr
    # unique(data.table::data.table(segments), by = c(segments_groups))[, segments_groups, with = FALSE] %>%
      # dplyr::as_tibble() %>%
    grouped_seg <- segments %>% 
      dplyr::group_by_at(dplyr::vars(segments_groups))
    
      ## see if I can add recording as a group
        group_rec <- dplyr::group_by(grouped_seg, .recording, .add = TRUE)
       if(same_grouping(x = grouped_seg, y =group_rec)){
         grouped_seg <- group_rec
       }
        
        grouped_seg <- grouped_seg %>% 
          dplyr::summarize(.groups = .groups) %>%
          dplyr::ungroup()

        if (!".id" %in% dplyr::tbl_vars(grouped_seg)) {
          grouped_seg <- hd_add_column(grouped_seg, .id = seq_len(last_id))
        } 
        if (!".recording" %in% dplyr::tbl_vars(grouped_seg)) {
          grouped_seg <-  hd_add_column(grouped_seg, .recording = NA)
        } 
        
        grouped_seg <- grouped_seg %>% 
          dplyr::select(obligatory_cols[[".segments"]], dplyr::everything())
  } else {
    dplyr::tibble(.id = seq_len(last_id), .recording = NA)
  }
}

same_grouping <-  function(x,y) {
  identical(attributes(x)$groups$.rows, attributes(y)$groups$.rows)
}

summarize_eval_signal <- function(.eeg_lst, dots) {
   # https://community.rstudio.com/t/clarifying-question-when-is-manual-data-mask-needed-in-rlang-eval-tidy/11186
  # https://stackoverflow.com/questions/14837902/how-to-write-a-function-that-calls-a-function-that-calls-data-table
  # https://stackoverflow.com/questions/15790743/data-table-meta-programming
  cond_cols <- names_other_col(.eeg_lst, dots, ".segments")
  extended_signal <- extended_signal(.eeg_lst, cond_cols)
  by <- dplyr::group_vars(.eeg_lst)
  dots <- rlang::quos_auto_name(dots)
  add_names <- rlang::quos_auto_name(dots) %>% names()
  old_attributes <- purrr::map(
    add_names %>% strsplit("_"),
    ~ attributes(.eeg_lst$.signal[[.x[[1]]]])
  )
  old_attributes <- stats::setNames(old_attributes, add_names)
  # extended_signal <-extended_signal[, lapply(dots, rlang::eval_tidy, 
  #                                            data= rlang::as_data_mask(.SD)), keyby = c(by)]
  # 
  if(length(by)>0) {  
    extended_signal <-extended_signal[, lapply(dots, rlang::eval_tidy, 
                                               data= rlang::as_data_mask(cbind(.SD,data.table::as.data.table(.BY)))),
                                      keyby = c(by)]
    
    # extended_signal[, `:=`(names(dots),
    #                        eval(parse(text = rlang::quo_text(dots)), 
    #                             # so that I can use elements of the .SD or the group
    #                             envir = cbind(.SD,data.table::as.data.table(.BY)), # envir = .SD,
    #                             # in case I need something outside the data table,
    #                             # it should be from the caller env, and not from inside the package
    #                             enclos = rlang::caller_env())), by = c(by)]
  } else {
    extended_signal <-extended_signal[, lapply(dots, rlang::eval_tidy, 
                                               data= rlang::as_data_mask(.SD))]
    
    # extended_signal[, `:=`(names(dots),
    #                        eval(parse(text = rlang::quo_text(dots)), 
    #                             # so that I can use elements of the .SD or the group
    #                             envir = .SD, # envir = .SD,
    #                             # in case I need something outside the data table,
    #                             # it should be from the caller env, and not from inside the package
    #                             enclos = rlang::caller_env()))]
  }

 
  
     
     
# microbenchmark::microbenchmark(
#  {dots_txt <- purrr::map(dots2, rlang::quo_text) %>%
#     paste(collapse = ", ") %>%
#     {
#       paste(".(", ., ")")
#     }
#   env <- lapply(dots2, rlang::quo_get_env) %>% unique()
#  extended_signal[, eval(parse(text = dots_txt), envir = env), keyby = c(by)]},
#  # extended_signal[, .(eval(rlang::get_expr(dots)), envir = env), keyby = c(by)],
#   rlang::eval_tidy(rlang::quo(extended_signal[, .(!!!dots), keyby = c(by)]), data= extended_signal)
# )

 
     #add class to the columns that lost their class
  extended_signal[, (add_names) := purrr::map2(.SD, old_attributes, ~
  if (is_channel_dbl(.x) | is_component_dbl(.x)) .x else `attributes<-`(.x, .y)), .SDcols = add_names]


  update_summarized_signal(extended_signal, .eeg_lst)
}




update_summarized_signal <- function(extended_signal, .eeg_lst) {
  attr_sample_id <- attributes(.eeg_lst$.signal$.sample)

  # TODO open bug to data.table, it looses the class
  if (!is_signal_tbl(extended_signal)) {
    class(extended_signal) <- c("signal_tbl", class(extended_signal))
  }

  ## Restructure signal table
  # Recover lost attributes and columns of signal_id
  # Add obligatory cols (.id, .sample) in case they are missing  :
  if (!".sample" %in% colnames(extended_signal)) {
    extended_signal[, .sample := sample_int(NA_integer_, attr_sample_id$sampling_rate)]
  } else {
    attributes(extended_signal$.sample) <- attr_sample_id
  }

  # Add .id in case it was removed by a summary
  if (!".id" %in% colnames(extended_signal)) {
    extended_signal[, .id := seq_len(.N), by = .sample]
  }

  if (length(group_vars_only_segments(.eeg_lst)) > 0) {
    extended_signal[, (group_vars_only_segments(.eeg_lst)) := NULL]
  }

  data.table::setkey(extended_signal, .id, .sample)
  data.table::setcolorder(extended_signal, c(".id", ".sample"))
  extended_signal[]
}
