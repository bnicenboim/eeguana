#sourced by devtools::load_all() 
#(so they’re available interactively when developing your packages) and by 
#test_check() and friends (so that they’re available no matter how your tests are executed).
suppress_python_output <- function(x) {
  invisible(reticulate::py_capture_output(x))
}


skip_if_no_python_stuff <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    skip("reticulate not installed")
  }
  for (mod in c("mne", "scipy")) {
    if (!reticulate::py_module_available(mod)) {
      skip(paste0(mod, " not available for testing; run dev_python() from dev/dev.R"))
    }
  }
}

#' Path to a cached large test file, by its name in inst/fixtures.csv.
#' @noRd
fixture_path <- function(name) eeguana:::eeg_fixture_path(name)

#' Skip unless every named fixture is in the local cache.
#' @noRd
skip_if_nofixture <- function(...) eeguana:::eeg_skip_if_no_fixture(...)

skip_if_nofiles <- function(files) {
  # Check if each file exists
  missing_files <- files[!file.exists(files)]
  
  # If there are any missing files
  if (length(missing_files) > 0) {
    # Inform the user about the missing files
    skip(paste0("The following file(s) do not exist: ", paste(missing_files, collapse = ", ")))
  }
}
#' @noRd
as_plain_df <- function(df) {
  df <- lapply(df, function(c) `attributes<-`(c, NULL))
  as.data.frame(df)
}

#' @noRd
expect_equal_plain_df <- function(object, expected, ..., info = NULL, label = NULL, expected.label = NULL) {
  eval(bquote(expect_equal(as_plain_df(.(object)), as_plain_df(.(expected)))))
}

#' @noRd
remove_index <- function(.eeg_lst) {
  attributes(.eeg_lst$.signal)$index <- NULL
  attributes(.eeg_lst$.segments)$index <- NULL
  attributes(.eeg_lst$.events)$index <- NULL
  .eeg_lst
}

#' @noRd
expect_equal_eeg_lst <- function(object, expected, ..., info = NULL, label = NULL, expected.label = NULL) {
  eval(bquote(expect_equal(remove_index(.(object)), remove_index(.(expected)), ...)))
}

#' @noRd
expect_equal_but_cnt_sgl <- function(object, expected, ..., info = NULL, label = NULL, expected.label = NULL) {
  object$.signal <- object$.signal[0, ]
  expected$.signal <- expected$.signal[0, ]
  eval(bquote(expect_equal_eeg_lst(.(object), .(expected))))
}

#' @noRd
expect_equal_but_sgl <- function(object, expected, ..., info = NULL, label = NULL, expected.label = NULL) {
  object$.signal <- NULL
  expected$.signal <- NULL
  eval(bquote(expect_equal_eeg_lst(.(object), .(expected))))
}

#' @noRd
expect_equal_but_cnt_sgm <- function(object, expected, ..., info = NULL, label = NULL, expected.label = NULL) {
  object$.segments <- object$.segments[0, ]
  expected$.segments <- expected$.segments[0, ]
  eval(bquote(expect_equal_eeg_lst(.(object), .(expected))))
}

#' @noRd
expect_equal_but_sgm <- function(object, expected, ..., info = NULL, label = NULL, expected.label = NULL) {
  object$.segments <- NULL
  expected$.segments <- NULL
  eval(bquote(expect_equal_eeg_lst(.(object), .(expected))))
}

#' helper functions (borrowed from github.com/stan-dev/bayesplot/R/helpers-testthat.R)
#' @noRd
expect_gg <- function(x) {
  testthat::expect_s3_class(x, "ggplot")
  invisible(ggplot2::ggplot_build(x))
}
