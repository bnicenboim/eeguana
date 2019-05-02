# From dplyr, https://github.com/tidyverse/dplyr/blob/d3c2365c88a041cbf9aa1bd8b1f21a15ccc31215/R/colwise.R

# Requires tbl_vars() method
tbl_at_vars <- function(tbl, vars) {

  tibble_vars <-  dplyr::setdiff(dplyr::tbl_vars(tbl), dplyr::group_vars(tbl))

  if (rlang::is_null(vars)) {
    character()
  } else if (rlang::is_character(vars)) {
    vars
  } else if (rlang::is_integerish(vars)) {
    tibble_vars[vars]
  } else if (rlang::is_quosures(vars)) {
    out <- tidyselect::vars_select(tibble_vars, !!!vars)
    if (!any(rlang::have_name(vars))) {
      names(out) <- NULL
    }
    out
  } else {
    stop(".vars must be a character/numeric vector or a `vars()` object."
    )
  }
}
tbl_at_syms <- function(tbl, vars) {
  vars <- tbl_at_vars(tbl, vars)
  rlang::set_names(rlang::syms(vars), names(vars))
}

# Requires tbl_vars(), `[[`() and length() methods
tbl_if_vars <- function(.tbl, .p, .env, ..., .include_group_vars = FALSE) {
  if (.include_group_vars) {
    tibble_vars <- dplyr::tbl_vars(.tbl)
  } else {
    tibble_vars <- dplyr::tbl_nongroup_vars(.tbl)
  }

  if (rlang::is_logical(.p)) {
    stopifnot(length(.p) == length(tibble_vars))
    return(rlang::syms(tibble_vars[.p]))
  }

  if (inherits(.tbl, "tbl_lazy")) {
    rlang::inform("Applying predicate on the first 100 rows")
    .tbl <- dplyr::collect(.tbl, n = 100)
  }

  if (is_fun_list(.p)) {
    if (length(.p) != 1) {
      stop(".predicate must have length 1")
    }
    .p <- .p[[1]]
  }
  if (rlang::is_quosure(.p)) {
    .p <- quo_as_function(.p)
  } else {
    .p <- rlang::as_function(.p, .env)
  }

  n <- length(tibble_vars)
  selected <- rlang::new_logical(n)
  for (i in seq_len(n)) {
    print(.p)
    print(pull(.tbl(tibble_var[[i]]
                    )))
    selected[[i]] <- .p(pull(.tbl, tibble_vars[[i]]), ...)
  }
  tibble_vars[selected]
}
tbl_if_syms <- function(.tbl, .p, .env, ..., .include_group_vars = FALSE) {
  rlang::syms(tbl_if_vars(.tbl, .p, .env, ..., .include_group_vars = .include_group_vars))
}

pull.eeg_lst <-function (.data, var = -1) 
{
  if( var %in% colnames(.data$signal)){
    .data$signal[[var]]
  } else if( var %in% colnames(.data$segments)){
    .data$segments[[var]]
  } else {
    stop("var not found")
  }
}

