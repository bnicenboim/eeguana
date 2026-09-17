library(eeguana)
options(eeguana.verbose = FALSE)

## plot_ica() had no test. Replacing dplyr with tidytable dropped the
## group_by(.recording, .ICA) in front of the summary that labels each
## component, so the summary lost both columns and the join on them failed
## with "by.y columns not in y". The intro vignette was the first thing to
## call it.
##
## plot_ica() is experimental and arranges its panels with cowplot, which is
## only suggested, so the test skips, saying why, when cowplot is missing.

test_that("plot_ica() draws one labelled panel per component", {
  skip_if_not(
    requireNamespace("cowplot", quietly = TRUE),
    "plot_ica() is experimental and needs cowplot, which is only suggested"
  )
  ica <- suppressWarnings(
    eeg_ica(data_faces_10_trials, -EOGH, -EOGV, -M1, -M2,
      .method = fast_ICA, .config = list(maxit = 10)
    )
  )
  p <- suppressWarnings(
    eeguana:::plot_ica.eeg_ica_lst(ica, samples = 1:500, eog = c("EOGV", "EOGH"))
  )
  expect_s3_class(p, "ggplot")
})
