## Large test files are not kept in the source tree. They live in a per-user
## cache directory (see R/test_utils.R and inst/fixtures.csv) and are fetched
## with dev_fixtures() from dev/dev.R.
##
## Tests that need one call skip_if_nofixture(), so a checkout without the
## cache still runs the rest of the suite.

if (interactive() || nzchar(Sys.getenv("EEGUANA_FETCH_FIXTURES"))) {
  # Convenience for local runs only: never download during R CMD check.
  for (nm in c("EEG01.mat")) {
    try(eeguana:::eeg_fixture_download_one(nm, quiet = TRUE), silent = TRUE)
  }
}
