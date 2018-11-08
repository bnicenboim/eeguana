#' @noRd
filter_eval <- function(.dots){
# https://stackoverflow.com/questions/16573995/subset-by-group-with-data-table
  dots_txt <- purrr::map(.dots, ~  rlang::quo_text(.x)) %>%
     paste0(., collapse = " & ")
  # sprintf("extended_signal[extended_signal[,.I[%s], by = c(by)]$V1,..orig_cols]", dots_txt)
  sprintf("extended_signal[extended_signal[,.I[%s], by = c(by)]$V1][,..out_cols]", dots_txt)
} 

#' @noRd
mutate_eval <- function(.dots){
  dots_txt <- purrr::imap(.dots, ~ 
          {if(.y!="") {
                         paste(.y, ":=", rlang::quo_text(.x)) 
                       } else {
                         paste0("`",rlang::quo_text(.x),"`" , " := ", rlang::quo_text(.x)) 
                       }} %>%
                       paste0("[, ",.,", by = c(by)]")) %>% paste0(collapse = "")

  sprintf("extended_signal%s[,..out_cols][]", dots_txt)
} 

#' @noRd
summarize_eval <- function(.dots){
  # https://community.rstudio.com/t/clarifying-question-when-is-manual-data-mask-needed-in-rlang-eval-tidy/11186
  #left joins then evaluates the summary by groups:
  # The following is very slow when grouping by ".sample_id"    "segment" 
  # col_expr <- rlang::get_expr(.dots)
  # extended_signal <- rlang::quo(.eeg_lst$signal[segments, ..all_cols][
  #                 ,.(!!!col_expr), by = c(by)]) %>% 
  #   rlang::eval_tidy(data = .eeg_lst$signal)
  # .dots_expr <- rlang::get_expr(.dots)
 dots_txt <- rlang::quos_auto_name(.dots) %>%  
            purrr::imap( ~  paste0("`",.y,"`", " = ", rlang::quo_text(.x))) %>%
     paste0(., collapse = ", ")
  sprintf("extended_signal[,.(%s), by = c(by)]", dots_txt)
} 

#' @noRd
summarize_at_eval <- function(.vars, .fun ){
  fun_txt <- rlang::quo_text(.fun[[1]])
  vars_txt <- paste0("'",.vars,"'",collapse =", ")
  sprintf("extended_signal[,purrr::map(.SD,~ %s),.SDcols = c(%s) , by = c(by)]", fun_txt, vars_txt)
} 

#' @noRd
rollup_eval <- function(.dots){
  dots_txt <- purrr::imap(.dots, ~ if(.y!="") paste(.y, "=", rlang::quo_text(.x)) else rlang::quo_text(.x)) %>%
     paste0(., collapse = ", ")
  sprintf("rollup(extended_signal,.(%s), by = c(by))", dots_txt)
}

#' @noRd
rollup_at_eval <- function(.vars, .fun){
  fun_txt <- rlang::quo_text(.fun[[1]])
  vars_txt <- paste0("'",.vars,"'",collapse =", ")
  sprintf("rollup(extended_signal,purrr::map(.SD,~ %s),.SDcols = c(%s), by = c(by))", fun_txt, vars_txt)
}


#' @noRd
mutate_cols_eval <- function(.dots){
  
 
  dots_txt <- purrr::imap(.dots, ~ 
          {if(.y!="") {
                         paste(.y, ":=", rlang::quo_text(.x)) 
                       } else {
                         paste0("`",rlang::quo_text(.x),"`" , " := ", rlang::quo_text(.x)) 
                       }} %>%
                       paste0("[, ",.,", by = c(by)]")) %>% paste0(collapse = "")

  sprintf("new_signal%s[,..signal_cols][]", dots_txt)
   
} 
