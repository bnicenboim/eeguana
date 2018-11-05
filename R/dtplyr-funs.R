
# # from
# # https://github.com/hadley/dtplyr/blob/2308ff25e88bb81fe84f9051e37ddd9d572189ee/R/utils.R

# dt_subset <- function(dt, i, j, env = parent.frame(), sd_cols = NULL, groups = NULL) {
#   env <- new.env(parent = env, size = 2L)
#   env$`_dt` <- dt
#   env$`_vars` <- deparse_all(groups)

#   args <- list(
#     i = if (missing(i)) quote(expr =) else dt_replace(i),
#     j = if (missing(j)) quote(expr =) else dt_replace(j)
#   )

#   if (missing(j)) {
#     call <- substitute(`_dt`[i], args)
#   } else {
#     call <- substitute(`_dt`[i, j, by = `_vars`], args)
#     call$.SDcols = sd_cols
#   }
#   # print(call)

#   eval(call, env)
# }

# deparse_all <- function(x) {
#   deparse2 <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")
#   vapply(x, deparse2, FUN.VALUE = character(1))
# }