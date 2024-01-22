#sourced by devtools::load_all() 
#(so they’re available interactively when developing your packages) and by 
#test_check() and friends (so that they’re available no matter how your tests are executed).

other_testfiles <- file.path(testthat::test_path(), "other_files")

skip_if_no_python_stuff <- function() {
  have_mne  <- reticulate::py_module_available("mne")
  have_scipy  <- reticulate::py_module_available("scipy")
  
  if (!have_mne) {
    skip("mne not available for testing")
  }
  if (!have_scipy) {
    skip("scipy not available for testing")
  }
}

skip_if_nofiles <- function(files) {
  # Check if each file exists
  missing_files <- files[!file.exists(files)]
  
  # If there are any missing files
  if (length(missing_files) > 0) {
    # Inform the user about the missing files
    message("The following file(s) do not exist: ", paste(missing_files, collapse = ", "))
    
    # Call the skip function (assuming it's defined elsewhere in your environment)
    skip()
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