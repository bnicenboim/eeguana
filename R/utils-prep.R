# FRom https://github.com/markfairbanks/tidytable/blob/main/R/utils-prep_exprs.R

# "Prepare" quosures/expressions for use in a "[.data.table" call
# Allows the use of functions like n() and across.()
# Replaces these functions with the necessary data.table translations
# General idea follows dt_squash found here: https://github.com/tidyverse/dtplyr/blob/master/R/tidyeval.R
prep_exprs <- function(x, data, .by = NULL, j = FALSE) {
  x <- lapply(x, prep_expr, data, {{ .by }}, j = j)
  rlang::squash(x)
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
  "across", "c_across", 
  "across_ch",
  "c_across_ch"
)

prep_expr_call <- function(x, data, .by = NULL, j = FALSE) {
  if (rlang::is_call(x, c("across"))) {
    x[[1]] <- quote(across.)
    x[-1] <- lapply(x[-1], prep_expr, data, {{ .by }})
    x
  } else if (rlang::is_call(x, c("c_across"))) {
    x[[1]] <- quote(c_across.)
    x[-1] <- lapply(x[-1], prep_expr, data, {{ .by }})
    x
  } 
}

across <- function (.cols = everything(), .fns = NULL, ..., .names = NULL) 
{
 # stop("Error: `across()` must only be used inside dplyr-like verbs.")
}
