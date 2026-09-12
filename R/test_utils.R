## Internal helpers for the large test fixtures that live outside the package.
##
## The package ships only small files in `inst/testdata`. Anything big (tens of
## megabytes and up) is listed in `inst/fixtures.csv` and downloaded on demand
## into a per-user cache directory, so it survives reinstalling or reinstalling
## the package and never ends up in the source tree, in git, or in the tarball.
##
## Nothing here is exported. Use the `dev/dev.R` helpers interactively.

#' Directory holding the downloaded test fixtures
#'
#' `tools::R_user_dir()` is the location CRAN policy allows a package to write
#' to, and it is outside the library tree, so reinstalling `eeguana` does not
#' remove the fixtures. Override with the `EEGUANA_FIXTURES` environment
#' variable (useful to point several checkouts at one copy).
#' @noRd
eeg_fixture_dir <- function() {
  from_env <- Sys.getenv("EEGUANA_FIXTURES", unset = "")
  if (nzchar(from_env)) {
    return(path.expand(from_env))
  }
  file.path(tools::R_user_dir("eeguana", "cache"), "fixtures")
}

#' The fixture manifest shipped in `inst/fixtures.csv`
#'
#' Columns: `name`, `file`, `url`, `sha256`, `bytes`, `used_by`, `notes`.
#' An empty `sha256` means "download it but do not verify it".
#' @noRd
eeg_fixture_manifest <- function() {
  path <- system.file("fixtures.csv", package = "eeguana")
  if (!nzchar(path) || !file.exists(path)) {
    return(NULL)
  }
  m <- utils::read.csv(path, stringsAsFactors = FALSE, comment.char = "#")
  m[!startsWith(trimws(m$name), "#"), , drop = FALSE]
}

#' @noRd
eeg_fixture_entry <- function(name) {
  m <- eeg_fixture_manifest()
  if (is.null(m)) {
    return(NULL)
  }
  entry <- m[m$name == name, , drop = FALSE]
  if (nrow(entry) != 1L) {
    return(NULL)
  }
  entry
}

#' Path a fixture would have on disk, whether or not it is there
#'
#' @param name Fixture name, as in the `name` column of `inst/fixtures.csv`.
#' @noRd
eeg_fixture_path <- function(name) {
  entry <- eeg_fixture_entry(name)
  file_name <- if (is.null(entry)) name else entry$file
  file.path(eeg_fixture_dir(), file_name)
}

#' @noRd
eeg_fixture_sha256 <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    return(NA_character_)
  }
  digest::digest(path, algo = "sha256", file = TRUE)
}

#' Is a fixture present, the right size, and (if a hash is known) intact?
#'
#' Returns TRUE/FALSE, with the reason for a FALSE in the "why" attribute.
#' @noRd
eeg_fixture_ok <- function(name, verify = TRUE) {
  path <- eeg_fixture_path(name)
  fail <- function(why) structure(FALSE, why = why)

  if (!file.exists(path)) {
    return(fail(paste0("not downloaded: ", path)))
  }

  entry <- eeg_fixture_entry(name)
  if (is.null(entry)) {
    return(structure(TRUE, why = NA_character_))
  }

  expected_bytes <- suppressWarnings(as.numeric(entry$bytes))
  if (!is.na(expected_bytes) && expected_bytes > 0 &&
    file.size(path) != expected_bytes) {
    return(fail(sprintf(
      "wrong size: %s bytes on disk, %s expected",
      file.size(path), format(expected_bytes, scientific = FALSE)
    )))
  }

  expected_sha <- trimws(entry$sha256 %||% "")
  if (verify && nzchar(expected_sha) && !is.na(expected_sha)) {
    got <- eeg_fixture_sha256(path)
    if (!is.na(got) && !identical(got, expected_sha)) {
      return(fail(sprintf("sha256 mismatch: got %s", got)))
    }
  }

  structure(TRUE, why = NA_character_)
}

#' Skip a test unless every named fixture is available
#'
#' Meant for `testthat`: fixtures are a local-only convenience, so a checkout
#' without them (CRAN, CI, a fresh clone) skips the tests instead of failing.
#' @noRd
eeg_skip_if_no_fixture <- function(...) {
  names_ <- unlist(list(...), use.names = FALSE)
  missing <- character()
  for (nm in names_) {
    ok <- eeg_fixture_ok(nm, verify = FALSE)
    if (!isTRUE(ok)) missing <- c(missing, paste0(nm, " (", attr(ok, "why"), ")"))
  }
  if (length(missing)) {
    testthat::skip(paste0(
      "missing test fixture(s): ", paste(missing, collapse = "; "),
      "\nRun eeguana's dev_fixtures() to download them."
    ))
  }
  invisible(TRUE)
}

#' Download one fixture into the cache directory
#'
#' Downloads to a temporary file next to the target and renames on success, so
#' an interrupted download never leaves a half file that looks valid.
#' @noRd
eeg_fixture_download_one <- function(name, refresh = FALSE, quiet = FALSE) {
  entry <- eeg_fixture_entry(name)
  if (is.null(entry)) {
    stop("No fixture called '", name, "' in inst/fixtures.csv.", call. = FALSE)
  }
  url <- trimws(entry$url)
  path <- eeg_fixture_path(name)

  if (!refresh && isTRUE(eeg_fixture_ok(name))) {
    if (!quiet) message("  ok      ", name)
    return(invisible(path))
  }
  if (!nzchar(url)) {
    warning("Fixture '", name, "' has no url in inst/fixtures.csv; ",
      "upload the file and fill the url in.",
      call. = FALSE
    )
    return(invisible(NA_character_))
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".part")
  on.exit(unlink(tmp), add = TRUE)

  if (!quiet) message("  fetch   ", name, " <- ", url)
  ok <- tryCatch(
    {
      utils::download.file(url, tmp, mode = "wb", quiet = quiet)
      TRUE
    },
    error = function(e) {
      warning("Could not download '", name, "': ", conditionMessage(e), call. = FALSE)
      FALSE
    }
  )
  if (!ok) {
    return(invisible(NA_character_))
  }

  file.rename(tmp, path)
  check <- eeg_fixture_ok(name)
  if (!isTRUE(check)) {
    warning("Downloaded '", name, "' but ", attr(check, "why"), call. = FALSE)
  }
  invisible(path)
}
