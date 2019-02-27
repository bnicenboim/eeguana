summarize_eeg_lst <- function(.eeg_lst, dots){

   .eeg_lst$signal <- summarize_eval_signal(.eeg_lst, dots)
   update_summarized_eeg_lst(.eeg_lst)
}

summarize_at_eeg_lst <- function(.eeg_lst, vars, funs){
   .eeg_lst$signal <-   summarize_at_eval_eeg_lst(.eeg_lst, vars, funs)
   update_summarized_eeg_lst(.eeg_lst)

}

update_summarized_eeg_lst <- function(.eeg_lst){

      ## Restructure segments table to fit the new signal table
  if (nrow(.eeg_lst$signal) != 0) {
    last_id <- max(.eeg_lst$signal$.id)
  } else {
    last_id <- integer(0)
  }

  .eeg_lst$segments <- summarize_segments(segments = .eeg_lst$segments, 
                                            segments_groups = group_chr_segments(.eeg_lst),
                                            last_id= last_id ) 
    ## Restructure events table
    # TODO maybe I can do some type of summary of the events table, instead
  .eeg_lst$events <- .eeg_lst$events %>% 
                      dplyr::filter(FALSE) %>% 
                      data.table::data.table() 

    #update channels in the events and the meta data (summarize deletes the metadata of the channels)
  .eeg_lst <- update_events_channels(.eeg_lst) #%>% update_channels_tbl(channels_info)

  validate_eeg_lst(.eeg_lst)
}


#' @noRd
summarize_segments <-  function(segments, segments_groups, last_id){
    # data.table::data.table(segments)[,unique(.SD),.SDcols=c(segments_groups)][] %>% dplyr::as_tibble() %>%
    if(length(segments_groups)!=0){
    data.table::data.table(segments)[,.(segment_n = .N),by=c(segments_groups)][] %>%
        dplyr::as_tibble() %>%
    # segments %>% 
          # dplyr::group_by_at(vars(segments_groups)) %>%
          # dplyr::summarize() %>%
          # dplyr::ungroup() %>%
      {
       if (!".id" %in% dplyr::tbl_vars(.)) {
         hd_add_column(., .id = seq_len(last_id))
       } else {
         .
       }
      } %>%
    dplyr::select(.id, dplyr::everything())
    } else {
        tibble::tibble(.id = seq_len(last_id))
    }
}


summarize_eval_signal <- function(.eeg_lst, dots){

  cond_cols <- names_segments_col(.eeg_lst, dots)
  extended_signal <- extended_signal(.eeg_lst, cond_cols) 
  by <- dplyr::group_vars(.eeg_lst)

 # https://community.rstudio.com/t/clarifying-question-when-is-manual-data-mask-needed-in-rlang-eval-tidy/11186
  #left joins then evaluates the summary by groups:
  # The following is very slow when grouping by ".sample_id"    "segment" 
  # col_expr <- rlang::get_expr(.dots)
  # extended_signal <- rlang::quo(.eeg_lst$signal[segments, ..all_cols][
  #                 ,.(!!!col_expr), by = c(by)]) %>% 
  #   rlang::eval_tidy(data = .eeg_lst$signal)
  # .dots_expr <- rlang::get_expr(.dots)
# https://stackoverflow.com/questions/14837902/how-to-write-a-function-that-calls-a-function-that-calls-data-table
# https://stackoverflow.com/questions/15790743/data-table-meta-programming

 dots_txt <- purrr::map(dots, rlang::quo_text) %>%
     paste(collapse =", ") %>%
     {paste(".(",.,")")}

 extended_signal <- extended_signal[, eval(parse(text = dots_txt)), by = c(by)]
 added_cols <- paste0("V",seq_len(length(dots)))

 # add class to the columns that lost their class
 extended_signal[, (added_cols) := lapply(.SD, function(x) 
                  if(!is_channel_dbl(x)) {channel_dbl(x)} else {x}), .SDcols = added_cols]

 data.table::setnames(extended_signal, added_cols, rlang::quos_auto_name(dots) %>% names()) 

 update_summarized_signal(extended_signal,.eeg_lst)
  
}

summarize_at_eval_eeg_lst <- function(.eeg_lst, vars, funs){
  
  fun_quo <- funs[[1]]
  cond_cols <- names_segments_col(.eeg_lst, fun_quo)
  names(cond_cols) <- cond_cols

  extended_signal <- extended_signal(.eeg_lst, cond_cols) 
  by <- dplyr::group_vars(.eeg_lst)

  fun_txt <- rlang::quo_text(fun_quo)
  
  #save attributes, then do the function, then keep attributes
  myfun <- function(., args){
       attr <- attributes(.)
      `attributes<-`(with(args,eval(parse(text=fun_txt))),attr)
    }
 
  extended_signal <-  extended_signal[,
                      # apply my fun to all ther relevant columns
                      lapply(.SD, myfun, 
                        # adding the auxiliary columns that are used as conditions (e.g., .[cond=1])
                        args = lapply(cond_cols, function(.) get(.))
                        ),
                      .SDcols = as.character(vars),
                       by = c(by)]
   #give a name if the name wasn't given
  if(!is.null(names(funs))){
    data.table::setnames(extended_signal, as.character(vars), paste0(as.character(vars),"_",names(funs)))
  }                     
  update_summarized_signal(extended_signal,.eeg_lst)
}



update_summarized_signal <- function(extended_signal, .eeg_lst){
   attr_sample_id <- attributes(.eeg_lst$signal$.sample_id)

#TODO open bug to data.table, it looses the class
  if(!is_signal_tbl(extended_signal)) {
    class(extended_signal) <- c("signal_tbl", class(extended_signal))
  }

  ## Restructure signal table
  # Recover lost attributes and columns of signal_id
  #Add obligatory cols (.id, .sample_id) in case they are missing  :
  if(!".sample_id" %in% colnames(extended_signal)) {
   extended_signal[,.sample_id := sample_int(NA_integer_, attr_sample_id$sampling_rate)]
  } else {
   attributes(extended_signal$.sample_id) <- attr_sample_id 
  }

  # Add .id in case it was removed by a summary
  if(!".id" %in% colnames(extended_signal)) {
   extended_signal[,.id := seq_len(.N), by = .sample_id]
  }
  
  if(length(group_chr_only_segments(.eeg_lst))>0){
   extended_signal[, (group_chr_only_segments(.eeg_lst)) := NULL] 
  }

  data.table::setkey(extended_signal,.id,.sample_id)
  data.table::setcolorder(extended_signal,c(".id",".sample_id"))
  extended_signal[]
}
