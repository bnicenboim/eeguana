# Based on https://github.com/markfairbanks/tidytable/blob/fe8371503499d652a460933b6d009c75fe46ca40/R/utils-general.R

# Creates a shallow copy
# Can add new columns or rename columns without modify-by-reference
shallow <- function(x) {
  x[TRUE]
}

# Create a call to `[.data.table` (i position)
call2_i <- function(.df, i = NULL) {
  # Use enquo(.df) to clean up error messages, #305
  call2("[", enquo(.df), i)
}

# Create a call to `[.data.table` (j position)
call2_j <- function(.df, j = NULL, .by = NULL, ...) {
  dt_expr <- call2("[", enquo(.df), , j, by = .by, ...)
  call2("[", dt_expr)
}

# Call a data.table function
# Squashes quosures
call2_dt <- function(.fn, ..., .ns = "data.table") {
  call <- call2(.fn, ..., .ns = .ns)
  quo_squash(call)
}

# Uses fast by trick for i position using .I
# For use in slice/filter
call2_fast_by_i <- function(.df, j, .by) {
  dt_expr <- call2_j(.df, j, .by)
  dt_expr <- call2("$", dt_expr, expr(V1))
  dt_expr <- call2_i(.df, dt_expr)
  dt_expr
}

# Extract environment from quosures to build the evaluation environment
get_dt_env <- function(x, ...) {
  if (length(x) == 0) {
    dt_env <- caller_env(2)
  } else if (is_quosures(x)) {
    x <- x[[1]]
    dt_env <- get_env(x)
  } else {
    dt_env <- get_env(x)
  }
  
  if (identical(dt_env, empty_env())) {
    dt_env <- caller_env(2)
  }
  
  env(dt_env, ...)
}

# Repair names of a data.table
df_name_repair <- function(.df, .name_repair = "unique") {
  names(.df) <- vec_as_names(
    names(.df),
    repair = .name_repair
  )
  .df
}

# Reduce a list of calls to a single combined call
call_reduce <- function(x, fun) {
  Reduce(function(x, y) call2(fun, x, y), x)
}

# radix sort
# proxy for data.table::fsort since negative values aren't supported, #282
f_sort <- function(x, decreasing = FALSE, na.last = FALSE) {
  # # Can switch to data.table::fsort once negative doubles are handled
  # suppressWarnings(
  #   fsort(x, decreasing = decreasing, na.last = na.last)
  # )
  
  sort(x, decreasing = decreasing, na.last = na.last, method = "radix")
}
