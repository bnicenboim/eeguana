# nocov start - compat-lazyeval (last updated: rlang 0.3.0)

# This file serves as a reference for compatibility functions for lazyeval.
# Please find the most recent version in rlang's repository.


warn_underscored <- function() {
  return(NULL)
  rlang::warn(paste(
    "The underscored versions are deprecated in favour of",
    "tidy evaluation idioms. Please see the documentation",
    "for `quo()` in rlang"
  ))
}
warn_text_se <- function() {
  return(NULL)
  rlang::warn("Text parsing is deprecated, please supply an expression or formula")
}

compat_lazy <- function(lazy, env = rlang::caller_env(), warn = TRUE) {
  if (warn) warn_underscored()

  if (missing(lazy)) {
    return(rlang::quo())
  }
  if (rlang::is_quosure(lazy)) {
    return(lazy)
  }
  if (rlang::is_formula(lazy)) {
    return(rlang::as_quosure(lazy, env))
  }

  out <- switch(typeof(lazy),
    symbol = ,
    language = rlang::new_quosure(lazy, env),
    character = {
      if (rlang::warn) warn_text_se()
      rlang::parse_quo(lazy[[1]], env)
    },
    logical = ,
    integer = ,
    double = {
      if (length(lazy) > 1) {
        rlang::warn("Truncating vector to length 1")
        lazy <- lazy[[1]]
      }
      rlang::new_quosure(lazy, env)
    },
    list =
      if (inherits(lazy, "lazy")) {
        lazy = rlang::new_quosure(lazy$expr, lazy$env)
      }
  )

  if (rlang::is_null(out)) {
    rlang::abort(sprintf("Can't convert a %s to a quosure", typeof(lazy)))
  } else {
    out
  }
}

compat_lazy_dots <- function(dots, env, ..., .named = FALSE) {
  if (missing(dots)) {
    dots <- list()
  }
  if (inherits(dots, c("lazy", "formula"))) {
    dots <- list(dots)
  } else {
    dots <- unclass(dots)
  }
  dots <- c(dots, list(...))

  warn <- TRUE
  for (i in seq_along(dots)) {
    dots[[i]] <- compat_lazy(dots[[i]], env, warn)
    warn <- FALSE
  }

  named <- rlang::have_name(dots)
  if (.named && any(!named)) {
    nms <- vapply(dots[!named], function(x) rlang::expr_text(rlang::get_expr(x)), character(1))
    names(dots)[!named] <- nms
  }

  names(dots) <- rlang::names2(dots)
  dots
}

compat_as_lazy <- function(quo) {
  structure(class = "lazy", list(
    expr = rlang::get_expr(quo),
    env = rlang::get_env(quo)
  ))
}
compat_as_lazy_dots <- function(...) {
  structure(class = "lazy_dots", lapply(rlang::quos(...), compat_as_lazy))
}


# nocov end
