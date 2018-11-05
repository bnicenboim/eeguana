#' @export
rollup_all_ch <- function(.tbl, ...) {
  UseMethod("rollup_all_ch")
}

#' @export
rollup <- function(.data, ...) {
  UseMethod("rollup")
}

#' @export
rollup_at_ch <- function(.tbl, ...) {
  UseMethod("rollup_at_ch")
}

#' @rdname dplyr
#' @export
rollup.eeg_lst <- function(.data, ...) {
  dots <- rlang::quos(...)
 rollup_eeg_lst(.data, dots)
}

#' @rdname dplyr
#' @export
rollup_at_ch.eeg_lst <- function(.tbl,.vars,  .funs, ...) {
  #TODO look for a rlang alternative for dplyr:::as_fun_list and dplyr:::tbl_at_syms
  funs <- dplyr:::as_fun_list(.funs, rlang::enquo(.funs), rlang::caller_env(),...) # fun_list class, contains a quosure such as ^mean(.)
  vars <- dplyr:::tbl_at_syms(.tbl, .vars) #list of chars
  rollup_at_eeg_lst(.tbl, vars, funs) 
}

#' @rdname dplyr
#' @export
rollup_all_ch.eeg_lst <- function(.tbl, .funs, ...) {
  funs <- dplyr:::as_fun_list(.funs, rlang::enquo(.funs), rlang::caller_env(),...) # fun_list class, contains a quosure such as ^mean(.)
  vars <- as.list(channel_names(.tbl))
  rollup_at_eeg_lst(.tbl, vars, funs) 
}

#' @export
summarize_at_ch <- function(.tbl, ...) {
  UseMethod("summarize_at_ch")
}

#' @export
summarise_at_ch <- function(.tbl, ...) {
  UseMethod("summarise_at_ch")
}

#' @export
summarize_all_ch <- function(.tbl, ...) {
  UseMethod("summarize_all_ch")
}

#' @export
summarise_all_ch <- function(.tbl, ...) {
  UseMethod("summarise_all_ch")
}

#' @rdname dplyr
#' @export
summarize_at_ch.eeg_lst <- function(.tbl,.vars,  .funs, ...) {
  #TODO look for a rlang alternative for dplyr:::as_fun_list and dplyr:::tbl_at_syms
  funs <- dplyr:::as_fun_list(.funs, rlang::enquo(.funs), rlang::caller_env(),...) # fun_list class, contains a quosure such as ^mean(.)
  vars <- dplyr:::tbl_at_syms(.tbl, .vars) #list of chars
  summarize_at_eeg_lst(.tbl, vars, funs) 
}
  
#' @rdname dplyr
#' @export
summarise_at_ch.eeg_lst <- summarize_at_ch.eeg_lst

#' @rdname dplyr
#' @export
summarize_all_ch.eeg_lst <- function(.tbl, .funs, ...) {
  funs <- dplyr:::as_fun_list(.funs, rlang::enquo(.funs), rlang::caller_env(),...) # fun_list class, contains a quosure such as ^mean(.)
  vars <- as.list(channel_names(.tbl))
  summarize_at_eeg_lst(.tbl, vars, funs) 
}
  
#' @rdname dplyr
#' @export
summarise_all_ch.eeg_lst <- summarize_all_ch.eeg_lst


