# #Taken from https://github.com/hadley/dtplyr/blob/7a9223e8888f7d01f8f4e95e503ecfc55ef7ef9e/R/compat-dplyr-0.6.0.R


# .onLoad <- function(libname, pkgname) {
#   # if (utils::packageVersion("dplyr") > "0.5.0") {
#   #    register_s3_method("dplyr", "mutate", "eegbl")
#   #    register_s3_method("dplyr", "rename", "eegbl")
#   #    register_s3_method("dplyr", "select", "eegbl")
#   #    register_s3_method("dplyr", "filter", "eegbl")
#   # }

#   register_s3_method("dplyr", "left_join", "eegbl")
#   register_s3_method("dplyr", "semi_join", "eegbl")
#   register_s3_method("dplyr", "anti_join", "eegbl")
#   register_s3_method("dplyr", "mutate_", "eegbl")
#   register_s3_method("dplyr", "transmute_", "eegbl")
#   register_s3_method("dplyr", "rename_", "eegbl")
#   register_s3_method("dplyr", "select", "eegbl")
#   register_s3_method("dplyr", "filter_", "eegbl")
#   # register_s3_method("dplyr", "group_by_", "eegbl")
#   # register_s3_method("dplyr", "ungroup", "eegbl")

#   invisible()
#  }

# register_s3_method <- function(pkg, generic, class, fun = NULL) {
#   stopifnot(is.character(pkg), length(pkg) == 1)
#   envir <- asNamespace(pkg)

#   stopifnot(is.character(generic), length(generic) == 1)
#   stopifnot(is.character(class), length(class) == 1)
#   if (is.null(fun)) {
#     fun <- get(paste0(generic, ".", class), envir = parent.frame())
#   }
#   stopifnot(is.function(fun))


#   if (pkg %in% loadedNamespaces()) {
#     registerS3method(generic, class, fun, envir = envir)
#   }

#   # Always register hook in case package is later unloaded & reloaded
#   setHook(
#     packageEvent(pkg, "onLoad"),
#     function(...) {
#       registerS3method(generic, class, fun, envir = envir)
#     }
#   )
# }


# # ## I'm repeating code here from dplyr, so that the user doesn't need to load dplyr first

# compat_as_lazy <- function(quo) {
#   structure(class = "lazy", list(
#     expr = rlang::get_expr(quo),
#     env = rlang::get_env(quo)
#   ))
# }
# compat_as_lazy_dots <- function(...) {
#   structure(class = "lazy_dots", purrr::map(rlang::quos(...), compat_as_lazy))
# }

# #' @export
# mutate <- function(.data, ...) {
#   UseMethod("mutate")
# }
# #' @export
# mutate.default <- function(.data, ...) {
#   dplyr::mutate_(.data, .dots = compat_as_lazy_dots(...))
# }


# #' @export
# mutate_ <- function(.data, ..., .dots = list()) {
#   UseMethod("mutate_")
# }


# #' @export
# rename <- function(.data, ...) {
#   UseMethod("rename")
# }
# #' @export
# rename.default <- function(.data, ...) {
#   dplyr::rename_(.data, .dots = compat_as_lazy_dots(...))
# }

# #' @export
# rename_ <- function(.data, ..., .dots = list()) {
#   UseMethod("rename_")
# }


# #' @export
# select <- function(.data, ...) {
#   UseMethod("select")
# }
# #' @export
# select.default <- function(.data, ...) {
#   dplyr::select_(.data, .dots = compat_as_lazy_dots(...))
# }

# #' @export
# select_ <- function(.data, ..., .dots = list()) {
#   UseMethod("select_")
# }


# #' @export
# filter <- function(.data, ...) {
#   UseMethod("filter")
# }
# #' @export
# filter.default <- function(.data, ...) {
#   dplyr::filter_(.data, .dots = compat_as_lazy_dots(...))
# }

# #' @export
# filter_ <- function(.data, ..., .dots = list()) {
#   UseMethod("filter_")
# }


# #' @export
# left_join <- function(.data, ...) {
#   UseMethod("left_join")
# }

# #' @export
# semi_join <- function(.data, ...) {
#   UseMethod("semi_join")
# }

# #' @export
# anti_join <- function(.data, ...) {
#   UseMethod("anti_join")
# }
