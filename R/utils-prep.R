# Adapted from: https://github.com/markfairbanks/tidytable/blob/main/R/utils-prep_exprs.R
# "Prepare" quosures/expressions for use in a "[.data.table" call
# Allows the use of functions like n() and across()
# Replaces these functions with the necessary data.table translations
# General idea follows dt_squash found here: https://github.com/tidyverse/dtplyr/blob/master/R/tidyeval.R
prep_exprs <- function(x, data, .by = NULL, j = FALSE) {
  x <- lapply(x, prep_expr, data, {{ .by }}, j = j)
  list_flatten(x)
}

prep_expr <- function(x, data, .by = NULL, j = FALSE) {
  if (rlang::is_quosure(x)) {
    x <- rlang::get_expr(x)
  }

  if (rlang::is_symbol(x) || rlang::is_atomic(x) || rlang::is_null(x)) {
    x
  } else if (rlang::is_call(x, call_fns)) {
    prep_expr_call(x, data, {{ .by }}, j)
  } else {
    x[-1] <- lapply(x[-1], prep_expr, data, {{ .by }})
    x
  }
}

call_fns <- c(
  "across_ch",
  "c_across_ch"
)

prep_expr_call <- function(x, data, .by = NULL, j = FALSE) {
  if (rlang::is_call(x, c("across_ch"))) {
    call_dots <- as.list(x[-1])
    x <- rlang::call2("across", rlang::expr(where(is_channel_dbl)), !!!call_dots)
    prep_expr(x, data, {{ .by }}, j = j)
  }
}

get_dt_env <- function(x, ...) {
  if (length(x) == 0) {
    dt_env <- rlang::caller_env(2)
  } else if (rlang::is_quosures(x)) {
    x <- x[[1]]
    dt_env <- rlang::get_env(x)
  } else {
    dt_env <- rlang::get_env(x)
  }
  if (identical(dt_env, rlang::empty_env())) {
    dt_env <- rlang::caller_env(2)
  }
  rlang::env(dt_env, ...)
}

prep_dots <- function(dots, data, .by = NULL, j = FALSE) {
  dt_env <- get_dt_env(dots)
  dots <- prep_exprs(x = dots, data, .by = !!by, j = TRUE)
  rlang::as_quosures(dots, env = dt_env)
}
