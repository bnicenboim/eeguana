## Local development helpers for eeguana.
##
##   source("dev/dev.R")
##   dev_test()          # check everything is installed, then run the tests
##
## This file is tracked by git but excluded from the built package
## (`^dev$` in .Rbuildignore), so a fresh clone can rebuild the whole local
## setup without any of it reaching CRAN or the installed package.

# Per-session state, so the fixture question is asked at most once even if
# dev_test() is called repeatedly.
.dev_env <- new.env(parent = emptyenv())

#' Install the package from source
#'
#' The tests run against an installed eeguana rather than a `load_all()`
#' shim, because that is what a user actually gets: NAMESPACE and the S3
#' registrations are real, `system.file()` points at the installed `inst/`,
#' and nothing leaks in from the source directory.
#'
#' @param quick Skip docs and vignettes. Fast, and fine for running tests.
dev_install <- function(quick = TRUE, quiet = TRUE) {
  message("* Installing eeguana from source")
  # devtools::install() takes upgrade as TRUE/FALSE/NA, unlike
  # remotes::install_deps() which takes the string "never".
  devtools::install(dev_pkg_root(),
    quick = quick, upgrade = FALSE,
    dependencies = FALSE, quiet = quiet, reload = FALSE
  )
  invisible(TRUE)
}

#' Make sure an installed eeguana is available to these helpers
#'
#' They reach into eeguana internals, so something has to provide them. This
#' prefers the installed package and installs it if it is missing or too old
#' to have the fixture helpers.
dev_load <- function() {
  ok <- requireNamespace("eeguana", quietly = TRUE) &&
    !is.null(tryCatch(get("eeg_fixture_manifest", envir = asNamespace("eeguana")),
      error = function(e) NULL
    ))
  if (!ok) {
    message("  (installed eeguana missing or out of date, installing)")
    dev_install()
  }
  invisible(TRUE)
}

dev_pkg_root <- function() {
  root <- tryCatch(rprojroot::find_root(rprojroot::has_file("DESCRIPTION")),
    error = function(e) getwd()
  )
  root
}

# ---------------------------------------------------------------- R packages --

#' Install any missing R dependency, including Suggests
#'
#' @param upgrade Passed to `remotes::install_deps()`. "never" is the fast
#'   default; use "always" before a release to test against current CRAN.
dev_r_deps <- function(upgrade = "never", quiet = FALSE) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    utils::install.packages("remotes")
  }
  for (pkg in c("devtools", "testthat", "rprojroot", "digest")) {
    if (!requireNamespace(pkg, quietly = TRUE)) utils::install.packages(pkg)
  }
  message("* R dependencies (including Suggests)")
  remotes::install_deps(
    pkgdir = dev_pkg_root(),
    dependencies = TRUE,
    upgrade = upgrade,
    quiet = quiet
  )
  invisible(TRUE)
}

#' Report dependencies that are behind the latest available version
#'
#' Asks CRAN what is available and compares against what is installed. Checked
#' at most once per session, because it needs a network round trip.
#'
#' @param upgrade TRUE updates everything that is behind, FALSE only
#'   reports, NA (the default) asks when the session is interactive and
#'   reports otherwise.
#' @param ask Offer to upgrade what is behind. Ignored when `upgrade` is
#'   TRUE or FALSE.
#' @param reset Check again even if this session already did.
dev_check_updates <- function(upgrade = NA, ask = interactive(), reset = FALSE) {
  if (reset) .dev_env$updates_checked <- NULL
  if (isTRUE(.dev_env$updates_checked)) {
    return(invisible(NULL))
  }
  .dev_env$updates_checked <- TRUE

  message("* Checking for newer versions of dependencies")
  deps <- tryCatch(
    suppressWarnings(remotes::dev_package_deps(dev_pkg_root(), dependencies = TRUE)),
    error = function(e) NULL
  )
  if (is.null(deps)) {
    message("  could not reach CRAN, skipping")
    return(invisible(NULL))
  }

  d <- as.data.frame(unclass(deps)[c("package", "installed", "available", "diff")],
    stringsAsFactors = FALSE
  )
  behind <- d[!is.na(d$diff) & d$diff < 0, , drop = FALSE]
  behind <- behind[order(behind$diff, behind$package), , drop = FALSE]

  if (!nrow(behind)) {
    message("  everything is up to date")
    return(invisible(behind))
  }

  # diff -2 means not installed at all, -1 means an older version
  absent <- behind[is.na(behind$installed), , drop = FALSE]
  older <- behind[!is.na(behind$installed), , drop = FALSE]
  if (nrow(absent)) {
    message("  not installed (", nrow(absent), "): ",
            paste(absent$package, collapse = ", "))
  }
  if (nrow(older)) {
    message("  newer version available (", nrow(older), "):")
    for (i in seq_len(nrow(older))) {
      message(sprintf("    %-18s %s -> %s", older$package[i],
                      older$installed[i], older$available[i]))
    }
  }

  if (isTRUE(upgrade)) {
    message("  updating")
    dev_r_deps(upgrade = "always")
    return(invisible(behind))
  }
  if (isFALSE(upgrade) || !ask) {
    message("  run dev_r_deps(upgrade = \"always\") to update, or ",
            "dev_check_updates(upgrade = TRUE)")
    return(invisible(behind))
  }

  cat("\nUpdate these now? [y/N] ")
  if (startsWith(tolower(trimws(readline())), "y")) {
    dev_r_deps(upgrade = "always")
  } else {
    message("  left as is")
  }
  invisible(behind)
}

# -------------------------------------------------------------------- Python --

#' Report on the Python side of the package
#'
#' `eeguana` reads FIF files and compares filters against MNE, both through
#' `reticulate`. Returns TRUE when mne, pandas and scipy are all importable.
dev_python_ok <- function(verbose = TRUE) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    if (verbose) message("  reticulate not installed")
    return(FALSE)
  }
  needed <- c("mne", "pandas", "scipy", "numpy")
  have <- vapply(needed, reticulate::py_module_available, logical(1))
  if (verbose) {
    cfg <- tryCatch(reticulate::py_config(), error = function(e) NULL)
    message("  python:  ", if (is.null(cfg)) "not configured" else cfg$python)
    for (nm in needed) message("  ", if (have[[nm]]) "ok      " else "MISSING ", nm)
  }
  all(have)
}

#' Make sure the Python packages the tests need are installed
#'
#' Prefers the `r-eeguana` conda environment, because that is the one
#' `.onLoad()` looks for. Falls back to a virtualenv when conda is not
#' available. `scipy` is added on top of what `install_py_eeguana()` installs,
#' since the tests use it but the package itself does not.
#'
#' @param install Install what is missing. FALSE only reports.
dev_python <- function(install = TRUE, envname = "r-eeguana") {
  dev_load()
  message("* Python (reticulate)")
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    if (!install) return(invisible(FALSE))
    utils::install.packages("reticulate")
  }

  if (dev_python_ok(verbose = TRUE)) return(invisible(TRUE))
  if (!install) return(invisible(FALSE))

  has_conda <- !is.null(tryCatch(reticulate::conda_binary(),
                                 error = function(e) NULL))

  if (has_conda) {
    message("  installing mne + pandas + scipy into the conda env '", envname, "'")
    # install_py_eeguana() restarts RStudio on success, which would abort a
    # test run, so keep the session alive.
    eeguana::install_py_eeguana(envname = envname, restart_session = FALSE)
    reticulate::py_install("scipy", envname = envname, method = "conda", pip = FALSE)
  } else {
    message("  no conda found; installing into the virtualenv '", envname, "' with pip")
    if (!reticulate::virtualenv_exists(envname)) reticulate::virtualenv_create(envname)
    reticulate::py_install(c("mne", "pandas", "scipy"),
                           envname = envname, method = "virtualenv")
    message(
      "  note: .onLoad() looks for a *conda* env called '", envname, "'.\n",
      "  Point reticulate at the virtualenv with RETICULATE_PYTHON in ~/.Renviron:\n",
      "    RETICULATE_PYTHON=", reticulate::virtualenv_python(envname)
    )
  }

  # reticulate binds one interpreter per session, so a freshly created
  # environment usually only takes effect after a restart.
  if (reticulate::py_available(initialize = FALSE)) {
    message(
      "  Python was already initialised in this session, so the new packages\n",
      "  are not visible yet. Restart R and run dev_test() again."
    )
    return(invisible(FALSE))
  }

  ok <- dev_python_ok(verbose = TRUE)
  if (!ok) {
    message(
      "  Python is still incomplete, so the Python-dependent tests will skip.\n",
      "  If the packages are installed but not found, reticulate picked the wrong\n",
      "  interpreter: set RETICULATE_PYTHON in ~/.Renviron, or clear the interpreter\n",
      "  in RStudio under Tools / Global Options / Python."
    )
  }
  invisible(ok)
}

# ------------------------------------------------------------------ fixtures --

#' Where the large test files are cached
#'
#' Outside the package library, so reinstalling `eeguana` never deletes them.
dev_fixture_dir <- function() {
  dev_load()
  eeguana:::eeg_fixture_dir()
}

#' What is in the fixture cache and what is missing
dev_fixture_status <- function(verify = FALSE) {
  dev_load()
  m <- eeguana:::eeg_fixture_manifest()
  if (is.null(m) || !nrow(m)) {
    message("No inst/fixtures.csv found.")
    return(invisible(NULL))
  }
  status <- vapply(m$name, function(nm) {
    ok <- eeguana:::eeg_fixture_ok(nm, verify = verify)
    if (isTRUE(ok)) "ok" else attr(ok, "why")
  }, character(1))
  out <- data.frame(
    name = m$name,
    MB = round(suppressWarnings(as.numeric(m$bytes)) / 1e6, 1),
    used_by = m$used_by,
    status = unname(status),
    stringsAsFactors = FALSE
  )
  message("fixture cache: ", dev_fixture_dir())
  print(out, row.names = FALSE)
  invisible(out)
}

#' Which fixtures are missing from the cache
#'
#' Cheap check: existence and size only, no checksums.
dev_fixtures_missing <- function() {
  dev_load()
  m <- eeguana:::eeg_fixture_manifest()
  if (is.null(m) || !nrow(m)) return(character())
  m$name[!vapply(m$name, function(n) isTRUE(eeguana:::eeg_fixture_ok(n, verify = FALSE)),
                 logical(1))]
}

#' Make sure the large test files are present, asking first
#'
#' Called by dev_test(). If anything is missing it asks whether to download,
#' once per session: answering no (or running non-interactively) is remembered,
#' so a run that skips the fixture-dependent tests is not interrupted again.
#'
#' @param ask Prompt when files are missing. FALSE reports and moves on.
#' @param reset Forget a previous answer and ask again.
dev_fixtures_ensure <- function(ask = interactive(), reset = FALSE) {
  if (reset) .dev_env$fixtures_asked <- NULL
  missing <- dev_fixtures_missing()
  if (!length(missing)) {
    message("* Large test fixtures: all present in ", dev_fixture_dir())
    return(invisible(TRUE))
  }

  m <- eeguana:::eeg_fixture_manifest()
  mb <- round(sum(suppressWarnings(as.numeric(m$bytes[m$name %in% missing])), na.rm = TRUE) / 1e6, 1)
  message("* Large test fixtures: ", length(missing), " missing (", mb, " MB): ",
          paste(missing, collapse = ", "))

  if (isTRUE(.dev_env$fixtures_asked)) {
    message("  already asked this session, leaving them; those tests will skip")
    return(invisible(FALSE))
  }
  .dev_env$fixtures_asked <- TRUE

  if (!ask) {
    message("  not interactive; those tests will skip. Run dev_fixtures() to download.")
    return(invisible(FALSE))
  }

  cat("\nDownload them now into ", dev_fixture_dir(), "? [y/N] ", sep = "")
  answer <- tolower(trimws(readline()))
  if (!startsWith(answer, "y")) {
    message("  skipped; the tests that need them will skip too")
    return(invisible(FALSE))
  }

  dev_fixtures(names = missing)
  invisible(length(dev_fixtures_missing()) == 0L)
}

#' Download the large test files into the cache
#'
#' @param names Fixture names to fetch. Default: everything in the manifest.
#' @param refresh Re-download even files that are already valid.
dev_fixtures <- function(names = NULL, refresh = FALSE, quiet = FALSE) {
  dev_load()
  m <- eeguana:::eeg_fixture_manifest()
  if (is.null(m) || !nrow(m)) {
    message("* Fixtures: no inst/fixtures.csv, nothing to do")
    return(invisible(NULL))
  }
  names <- names %||% m$name
  message("* Large test fixtures -> ", dev_fixture_dir())
  for (nm in names) eeguana:::eeg_fixture_download_one(nm, refresh = refresh, quiet = quiet)
  invisible(TRUE)
}

#' Add a local file to the fixture cache without downloading it
#'
#' Useful for data you cannot redistribute, and to seed the cache from copies
#' you already have on disk. Prints the manifest row to paste into
#' `inst/fixtures.csv`.
dev_fixture_adopt <- function(path, name = basename(path), url = "", notes = "") {
  dev_load()
  stopifnot(file.exists(path))
  dest <- file.path(dev_fixture_dir(), basename(path))
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  file.copy(path, dest, overwrite = TRUE)
  sha <- eeguana:::eeg_fixture_sha256(dest)
  message("copied to ", dest)
  message("manifest row:")
  cat(paste(name, basename(path), url, sha, file.size(dest), "", notes, sep = ","), "\n")
  invisible(dest)
}

#' Minimal GitHub REST helpers for release assets
#'
#' piggyback is deliberately not used here. `pb_upload()` calls
#' `pb_releases()` internally, which caches its result and also errors on a
#' repo containing a release with an empty tag name; when the release was
#' created moments earlier it tries to create it again and fails with
#' "HTTP 422", then dies in `parse_url()` for want of an upload URL. Talking
#' to the API directly avoids all of that.
#' @noRd
dev_gh_token <- function() {
  tok <- c(Sys.getenv("GITHUB_PAT"), Sys.getenv("GITHUB_TOKEN"))
  tok <- tok[nzchar(tok)]
  if (!length(tok)) {
    stop("No GITHUB_PAT or GITHUB_TOKEN found. Put one in ~/.Renviron.",
      call. = FALSE
    )
  }
  tok[1]
}

#' @noRd
dev_gh <- function(path, ..., method = "GET", base = "https://api.github.com") {
  f <- switch(method, GET = httr::GET, POST = httr::POST, DELETE = httr::DELETE)
  f(paste0(base, path),
    httr::add_headers(
      Authorization = paste("token", dev_gh_token()),
      Accept = "application/vnd.github+json"
    ), ...
  )
}

#' Releases on the repo, tolerating ones with an empty tag
#' @noRd
dev_gh_releases <- function(repo = "bnicenboim/eeguana") {
  r <- dev_gh(paste0("/repos/", repo, "/releases"), query = list(per_page = 100))
  httr::stop_for_status(r)
  httr::content(r)
}

#' @noRd
dev_gh_release_tags <- function(repo = "bnicenboim/eeguana") {
  tags <- vapply(dev_gh_releases(repo), function(x) x$tag_name %||% "", character(1))
  tags[nzchar(tags)]
}

#' Find a release by tag, creating it if absent
#' @noRd
dev_gh_release <- function(tag, repo = "bnicenboim/eeguana", create = TRUE) {
  hit <- Filter(function(x) identical(x$tag_name, tag), dev_gh_releases(repo))
  if (length(hit)) {
    return(hit[[1]])
  }
  if (!create) {
    return(NULL)
  }
  message("  creating release '", tag, "'")
  r <- dev_gh(paste0("/repos/", repo, "/releases"),
    method = "POST", encode = "json",
    body = list(
      tag_name = tag, name = tag,
      body = paste(
        "Large files used by the test suite and vignettes.",
        "Downloaded on demand by dev_fixtures(); see inst/fixtures.csv",
        "for provenance and licensing of each file."
      )
    )
  )
  if (httr::status_code(r) >= 300) {
    d <- httr::content(r)
    stop("could not create release '", tag, "': HTTP ", httr::status_code(r),
      " - ", d$message %||% "",
      if (length(d$errors)) paste0(" (", d$errors[[1]]$code %||% "", ")"),
      call. = FALSE
    )
  }
  httr::content(r)
}

#' Upload one file as a release asset, replacing any asset of the same name
#' @noRd
dev_gh_upload_asset <- function(path, release, repo = "bnicenboim/eeguana") {
  nm <- basename(path)
  old <- Filter(function(a) identical(a$name, nm), release$assets)
  for (a in old) {
    message("    replacing existing asset")
    dev_gh(paste0("/repos/", repo, "/releases/assets/", a$id), method = "DELETE")
  }
  r <- dev_gh(paste0("/repos/", repo, "/releases/", release$id, "/assets"),
    method = "POST",
    base = "https://uploads.github.com",
    query = list(name = nm),
    body = httr::upload_file(path, type = "application/octet-stream")
  )
  if (httr::status_code(r) >= 300) {
    stop("upload of ", nm, " failed: HTTP ", httr::status_code(r), " - ",
      httr::content(r)$message %||% "",
      call. = FALSE
    )
  }
  invisible(httr::content(r)$browser_download_url)
}

#' Upload the cached fixtures to a GitHub release and rewrite the manifest
#'
#' Uploads every file the manifest lists that is present in the local cache,
#' then rewrites the `url` column of `inst/fixtures.csv` to point at the
#' release. Release assets are the durable home: no authentication to
#' download, under 2 GiB per file, up to 1000 files per release, no total size
#' or bandwidth limit.
#'
#' Needs a GITHUB_PAT with `repo` scope in ~/.Renviron.
#'
#' @param tag Release tag to attach the files to. Created if absent.
#' @param repo "owner/repo".
#' @param names Fixture names to upload. Default: all that are cached.
#' @param dry_run TRUE (the default) only reports what it would do. Pass FALSE
#'   to actually create the release and upload, which publishes the files
#'   publicly if the repo is public. Check the licensing of each file first;
#'   `inst/fixtures.csv` records it in the notes column.
dev_fixtures_publish <- function(tag = "testdata",
                                 repo = "bnicenboim/eeguana",
                                 names = NULL,
                                 dry_run = TRUE) {
  dev_load()
  m <- eeguana:::eeg_fixture_manifest()
  if (is.null(m) || !nrow(m)) stop("no inst/fixtures.csv", call. = FALSE)

  names <- names %||% m$name
  rows <- m[m$name %in% names, , drop = FALSE]
  if (!nrow(rows)) stop("no manifest rows match: ", paste(names, collapse = ", "), call. = FALSE)
  rows$path <- vapply(rows$name, eeguana:::eeg_fixture_path, character(1))
  absent <- rows[!file.exists(rows$path), , drop = FALSE]
  rows <- rows[file.exists(rows$path), , drop = FALSE]

  if (nrow(absent)) {
    message(
      "not in the cache, skipping: ", paste(absent$name, collapse = ", "),
      "\n  run dev_fixtures() first if you want these uploaded"
    )
  }
  if (!nrow(rows)) {
    message("nothing to upload")
    return(invisible(NULL))
  }

  total <- sum(file.size(rows$path))
  message(
    "* ", nrow(rows), " file(s), ", round(total / 1e6, 1), " MB -> ",
    repo, " release '", tag, "'"
  )
  for (i in seq_len(nrow(rows))) {
    message(sprintf("    %-16s %7.1f MB", rows$file[i], file.size(rows$path[i]) / 1e6))
  }
  oversize <- rows[file.size(rows$path) >= 2 * 1024^3, , drop = FALSE]
  if (nrow(oversize)) {
    stop("over the 2 GiB per-file limit: ", paste(oversize$file, collapse = ", "),
      call. = FALSE
    )
  }

  if (dry_run) {
    message(
      "\n  dry run. Nothing uploaded and inst/fixtures.csv untouched.\n",
      "  Re-run with dry_run = FALSE to publish."
    )
    return(invisible(rows))
  }

  release <- dev_gh_release(tag, repo)
  for (i in seq_len(nrow(rows))) {
    message(sprintf("  uploading %s (%.1f MB)", rows$file[i],
                    file.size(rows$path[i]) / 1e6))
    dev_gh_upload_asset(rows$path[i], release, repo)
  }

  # point the manifest at the release
  csv <- file.path(dev_pkg_root(), "inst", "fixtures.csv")
  man <- utils::read.csv(csv, stringsAsFactors = FALSE)
  hit <- man$name %in% rows$name
  man$url[hit] <- sprintf(
    "https://github.com/%s/releases/download/%s/%s",
    repo, tag, man$file[hit]
  )
  utils::write.csv(man, csv, row.names = FALSE, quote = FALSE)
  message("\n  updated ", csv)
  invisible(rows)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ---------------------------------------------------------------------- test --

#' Install everything the tests need, then run them
#'
#' The one entry point: R dependencies, Python packages, and the large
#' fixtures, then `devtools::test()`.
#'
#' @param filter Passed to `devtools::test()`, e.g. "reading" for
#'   test_01-reading.R only.
#' @param deps,install,python,fixtures,updates Set any of these to FALSE to
#'   skip that step, which is what you want on a quick second run. `updates`
#'   needs a network round trip and only runs once per session anyway. With
#'   `install = FALSE` the tests run against whatever eeguana is already
#'   installed, so re-installing is what picks up your edits.
#' @param upgrade Passed to `dev_r_deps()`.
dev_test <- function(filter = NULL,
                     deps = TRUE,
                     install = TRUE,
                     python = TRUE,
                     fixtures = TRUE,
                     updates = TRUE,
                     upgrade = "never",
                     ...) {
  root <- dev_pkg_root()

  if (isTRUE(deps)) dev_r_deps(upgrade = upgrade)
  if (isTRUE(updates)) dev_check_updates()
  if (isTRUE(install)) dev_install()
  if (isTRUE(python)) dev_python(install = TRUE)
  if (isTRUE(fixtures)) dev_fixtures_ensure()

  message("* Running tests against the installed package")
  # devtools::test() sets NOT_CRAN itself; test_dir() does not, and without it
  # every skip_on_cran() fires and six tests silently disappear.
  withr::local_envvar(c(NOT_CRAN = "true"))
  testthat::test_dir(
    file.path(root, "tests", "testthat"),
    package = "eeguana",
    load_package = "installed",
    filter = filter,
    ...
  )
}

#' Everything dev_test() does, then a full R CMD check
#' @param upgrade Passed to `dev_check_updates()`. TRUE, the default,
#'   brings dependencies up to date first, because CI installs them fresh
#'   and a check against stale local versions does not reproduce it. Pass
#'   NA to be asked, or FALSE to check against what you have.
dev_check <- function(..., upgrade = TRUE) {
  # One call, not dev_r_deps() followed by this: dev_check_updates(upgrade =
  # TRUE) installs what is missing and upgrades what is behind, Remotes
  # included. Calling dev_r_deps() first resolved the tree twice and, with its
  # "never" default, printed "R dependencies" while leaving a stale version in
  # place. R.matlab 3.8.0 broke read_ft() that way while the check looked
  # green.
  dev_check_updates(upgrade = upgrade, reset = TRUE)
  if (!isTRUE(upgrade)) {
    message(
      "  NOTE: checking against the versions you have installed, which may ",
      "not be what CI installs"
    )
  }
  dev_python(install = TRUE)
  dev_fixtures_ensure()
  devtools::check(pkg = dev_pkg_root(), ...)
}

message(
  "eeguana dev helpers loaded:\n",
  "  dev_test()            set up, install eeguana, run the tests\n",
  "  dev_install()         reinstall eeguana from source\n",
  "  dev_test(deps = FALSE, python = FALSE, fixtures = FALSE)   just the tests\n",
  "  dev_fixture_status()  what large files are cached, and where\n",
  "  dev_fixtures_ensure() ask once per session about missing large files\n",
  "  dev_fixtures()        download the large files\n",
  "  dev_python()          set up mne/pandas/scipy\n",
  "  dev_check_updates()   once-per-session check for newer dependencies\n",
  "  dev_fixtures_publish() upload the cached files to a GitHub release\n",
  "  dev_check()           the above, then R CMD check"
)
