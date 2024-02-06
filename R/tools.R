#' Install the Python packages needed for `eeguana`
#'
#' @description
#' `install_py_eeguana` facilitates the installation of Python packages required by `eeguana` within an R environment.
#' It leverages the `reticulate` package to manage Python environments and supports various installation methods,
#' environment configurations, and Python versions.
#'
#'
#' @param envname The name of the virtual environment to create or use. Default is 'r-eeguana'.
#' @param restart_session Whether to restart the R session after installation. Default is TRUE.
#' @param forge Whether to use conda-forge to install packages. Default is TRUE.
#' @param ... Additional arguments passed to `reticulate::py_install`.
#' @param new_env Whether to create a new environment if `envname` is 'r-eeguana'. Default behavior is determined by the identity of `envname`.
#' @inheritParams reticulate::py_install
#'
#' @details
#' The function selects an appropriate method for environment management and Python installation, focusing on ease of use and flexibility.
#' It supports creating new virtual or conda environments, installing necessary packages, and handling Python version requirements.
#'
#' The function is designed to be robust, handling various scenarios such as existing environments, and provides detailed feedback during the process.
#'
#' @return
#' Returns `NULL` invisibly and prints a message upon successful installation. If `restart_session` is TRUE and the R session is running within RStudio, the session will restart automatically.
#'
#' @examples
#' # Install Python dependencies in a new conda environment named 'r-eeguana'
#' install_py_eeguana(envname = "r-eeguana", restart_session = FALSE)
#'
#' @export
install_py_eeguana <- function(conda = "auto",
                               envname = "r-eeguana",
                               restart_session = TRUE,
                               forge = TRUE,
                               ...,
                               new_env = identical(envname, "r-eeguana"),
                               python_version = NULL){
 method =  "conda"
 #c("auto","conda","virtualenv"),
# method <- match.arg(method)


  # should I limit python version?
  ## if(method %in% c("auto", "virtualenv") &&
  ##    is.null(python_version)) {
  ##   #  MNE-Python requires Python version 3.8 or higher
  ##   available <- reticulate::virtualenv_starter(version = ">=3.12.1", all = TRUE)
  ##   # pick the smallest minor version, ignoring patchlevel
  ##   if(nrow(available))
  ##     python_version <- min(available$version[, 1:2])
  ## }

  if (isTRUE(new_env)) {

    if (method %in% c("auto", "virtualenv") &&
        reticulate::virtualenv_exists(envname))
      reticulate::virtualenv_remove(envname = envname, confirm = FALSE)

    if (method %in% c("auto", "conda")) {
      if (!is.null(tryCatch(reticulate::conda_python(envname, conda = conda),
                            error = function(e) NULL)))
        reticulate::conda_remove(envname, conda = conda)
    }

  }
  packages <- c("mne-base", "pandas")


  reticulate::py_install(
    packages       = packages,
    envname        = envname,
    method         = method,
    conda          = conda,
    python_version = python_version,
    pip            = FALSE,
    pip_ignore_installed = FALSE,
    forge = forge,
    ...
  )

  cat("\nInstallation complete.\n\n")

  if (restart_session &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::hasFun("restartSession"))
    rstudioapi::restartSession()

  invisible(NULL)

}
