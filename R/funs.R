# Taken dplyr: https://github.com/tidyverse/dplyr/blob/2e6ca1676ece390b7a3e0a76b52c6bbf1464a180/R/funs.R
# TODO: I might be able to remove this at some point.

new_funs <- function(funs) {
  attr(funs, "have_name") <- any(rlang::names2(funs) != "")

  # Workaround until rlang:::label() is exported
  temp <- purrr::map(funs, function(fn) rlang::node_car(rlang::quo_get_expr(fn)))
  temp <- rlang::exprs_auto_name(temp)
  names(funs) <- names(temp)

  class(funs) <- "fun_list"
  funs
}

as_fun_list <- function(.x, .quo, .env, ...) {
  # Capture quosure before evaluating .x
  force(.quo)

  # If a fun_list, update args
  args <- rlang::list2(...)
  if (is_fun_list(.x)) {
    if (!rlang::is_empty(args)) {
      .x[] <- purrr::map(.x, call_modify, !!!args)
    }
    return(.x)
  }

  # Take functions by expression if they are supplied by name. This
  # way we can evaluate it hybridly.
  if (rlang::is_function(.x) && rlang::quo_is_symbol(.quo)) {
    .x <- list(.quo)
  } else if (rlang::is_character(.x)) {
    .x <- as.list(.x)
  } else if (rlang::is_bare_formula(.x, lhs = FALSE)) {
    .x <- list(rlang::as_function(.x))
  } else if (!rlang::is_list(.x)) {
    .x <- list(.x)
  }

  funs <- purrr::map(.x, as_fun, .env = fun_env(.quo, .env), args)
  new_funs(funs)
}

as_fun <- function(.x, .env, .args) {
  quo <- rlang::as_quosure(.x, .env)

  # For legacy reasons, we support strings. Those are enclosed in the
  # empty environment and need to be switched to the caller environment.
  quo <- rlang::quo_set_env(quo, fun_env(quo, .env))

  expr <- rlang::quo_get_expr(quo)

  if (rlang::is_call(expr, c("function", "~"))) {
    top_level <- rlang::as_string(expr[[1]])
    bad_args(rlang::quo_text(expr), "must be a function name (quoted or unquoted) or an unquoted call, not `{top_level}`")
  }

  if (rlang::is_call(expr) && !rlang::is_call(expr, c("::", ":::"))) {
    expr <- rlang::call_modify(expr, !!!.args)
  } else {
    expr <- rlang::call2(expr, quote(.), !!!.args)
  }

  rlang::set_expr(quo, expr)
}

quo_as_function <- function(quo) {
  rlang::new_function(rlang::exprs(. = ), rlang::quo_get_expr(quo), rlang::quo_get_env(quo))
}

fun_env <- function(quo, default_env) {
  env <- rlang::quo_get_env(quo)
  if (rlang::is_null(env) || identical(env, rlang::empty_env())) {
    default_env
  } else {
    env
  }
}

is_fun_list <- function(x) {
  inherits(x, "fun_list")
}
