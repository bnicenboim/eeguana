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
                                            segments_groups = group_vars_segments(.eeg_lst),
                                            last_id= last_id ) 
    ## Restructure events table
    # TODO maybe I can do some type of summary of the events table, instead
  .eeg_lst$events <- new_events_tbl(sampling_rate = sampling_rate(.eeg_lst)) 
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

  cond_cols <- names_other_col(.eeg_lst, dots, "segments")
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
    add_names <- rlang::quos_auto_name(dots) %>% names()

    old_attributes <- purrr::map(add_names %>% stringr::str_split("_"),
               ~ attributes(.eeg_lst$signal[[.x[[1]]]]))

    old_attributes <- setNames(old_attributes, add_names)

    env <- lapply(dots, rlang::quo_get_env) %>% unique()
    if(length(env)!=1) stop("Need to fix env", env)
    extended_signal <- extended_signal[, eval(parse(text = dots_txt), envir = env), by = c(by)]
    
    added_cols <- paste0("V",seq_len(length(dots)))
    data.table::setnames(extended_signal, added_cols, add_names) 
 # add class to the columns that lost their class
    extended_signal[, (add_names) := purrr::map2(.SD, old_attributes,~ `attributes<-`(.x,.y)), .SDcols = add_names]


 update_summarized_signal(extended_signal,.eeg_lst)
  
}

summarize_at_eval_eeg_lst <- function(.eeg_lst, vars, funs){
  
  fun_quo <- funs[[1]]
  cond_cols <- names_other_col(.eeg_lst, fun_quo, "segments")
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
  
  if(length(group_vars_only_segments(.eeg_lst))>0){
   extended_signal[, (group_vars_only_segments(.eeg_lst)) := NULL] 
  }

  data.table::setkey(extended_signal,.id,.sample_id)
  data.table::setcolorder(extended_signal,c(".id",".sample_id"))
  extended_signal[]
}
