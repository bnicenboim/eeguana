.onLoad <- function(libname, pkgname) {
  register_s3_method("dplyr", "group_by", "eeg_lst")
  register_s3_method("dplyr", "groups", "eeg_lst")
  register_s3_method("dplyr", "group_vars", "eeg_lst")
  register_s3_method("dplyr", "ungroup", "eeg_lst")
  register_s3_method("dplyr", "tbl_vars", "eeg_lst")

  register_s3_method("dplyr", "filter", "eeg_lst")
  register_s3_method("dplyr", "filter_", "eeg_lst")
  register_s3_method("dplyr", "summarise", "eeg_lst")
  register_s3_method("dplyr", "summarise_", "eeg_lst")
  register_s3_method("dplyr", "mutate", "eeg_lst")
  register_s3_method("dplyr", "mutate_", "eeg_lst")
  register_s3_method("dplyr", "transmute", "eeg_lst")
  register_s3_method("dplyr", "transmute_", "eeg_lst")
  register_s3_method("dplyr", "select", "eeg_lst")
  register_s3_method("dplyr", "rename", "eeg_lst")

  register_s3_method("dplyr", "group_by_", "eeg_lst")
  register_s3_method("dplyr", "left_join", "eeg_lst")
  register_s3_method("dplyr", "semi_join", "eeg_lst")
  register_s3_method("dplyr", "anti_join", "eeg_lst")
  register_s3_method("dplyr", "as_tibble", "eeg_lst")
  register_s3_method("dplyr", "as_data_frame", "eeg_lst")

  invisible()
}

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  envir <- asNamespace(pkg)

  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)
  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  }
  stopifnot(is.function(fun))


  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = envir)
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = envir)
    }
  )
}