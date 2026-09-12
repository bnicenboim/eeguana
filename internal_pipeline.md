# eeguana internal pipeline

Maintainer notes. Not part of the package: excluded from the build via
`^internal_pipeline\.md$` in `.Rbuildignore`, but tracked in git so a fresh
clone gets it.

Opening the project in RStudio runs `.Rprofile`, which sources `dev/dev.R` and
defines everything below. No `source()` needed. In a plain `Rscript` (where
`interactive()` is FALSE) it is not loaded, so source it yourself:

```r
source("dev/dev.R")
```

## Running the tests

```r
dev_test()
```

That is the full path, in order:

1. `dev_r_deps()` installs missing R packages, `Suggests` included. Some tests
   fail rather than skip without `R.matlab`, `plotly`, or `akima`.
2. `dev_check_updates()` lists dependencies with a newer version available.
   Once per session; reports only.
3. `dev_install()` installs eeguana from source.
4. `dev_python()` sets up the `r-eeguana` conda environment with `mne`,
   `pandas`, and `scipy`. `.onLoad()` looks for that environment by name.
5. `dev_fixtures_ensure()` checks the large test files, and if any are missing
   asks once per session whether to download them.
6. The tests run against the **installed** package, not a `load_all()` shim,
   so `NAMESPACE`, the S3 registrations, and `system.file()` are all real.

Because step 3 is what picks up your edits, `install = FALSE` runs the tests
against whatever is already installed.

| what you want | command |
|---|---|
| everything checked | `dev_test()` |
| re-install and test, skip the network | `dev_test(deps = FALSE, python = FALSE, updates = FALSE)` |
| test the installed build, no re-install | `dev_test(install = FALSE, deps = FALSE, python = FALSE, updates = FALSE)` |
| one file | `dev_test(filter = "reading")` |
| full `R CMD check` | `dev_check()` |

`devtools::test()` still works and is faster, but it uses `load_all()` and it
skips *silently* when fixtures or MNE are missing, so a green run can look
like more coverage than it is.

## Where the large test files live

Not in the package sources. They sit in a per-user cache:

```r
dev_fixture_dir()   # ~/.cache/R/eeguana/fixtures
```

That is `tools::R_user_dir("eeguana", "cache")`: outside the library tree, so
reinstalling eeguana never deletes it, and the one place CRAN policy lets a
package write to. Override with the `EEGUANA_FIXTURES` environment variable to
share one copy between checkouts.

`inst/fixtures.csv` is the manifest, and it ships with the package (a few
hundred bytes) so the tests can find the files from any installation:

| column | meaning |
|--------|---------|
| `name` | how tests refer to it, e.g. `skip_if_nofixture("EEG01.mat")` |
| `file` | filename in the cache |
| `url` | direct download URL; empty means "cannot be redistributed" |
| `sha256` | checked after download; empty skips verification |
| `bytes` | expected size |
| `used_by` | which test or vignette needs it |
| `notes` | provenance |

Useful commands:

```r
dev_fixture_status()               # what is cached, what is missing
dev_fixture_status(verify = TRUE)  # also check every checksum (slow)
dev_fixtures()                     # download everything missing
dev_fixtures(refresh = TRUE)       # re-download even valid files
```

Tests that need one call `skip_if_nofixture("name")` and use
`fixture_path("name")`, so a machine without the cache still runs the rest of
the suite. CI and CRAN never populate the cache, by design.

## Uploading the large files

They live as **GitHub release assets** on `bnicenboim/eeguana`, under the
`testdata` tag. Downloads need no authentication, which is what keeps
`dev_fixtures()` a plain `download.file()`. Uploading needs a `GITHUB_PAT`
with `repo` scope in `~/.Renviron`.

```r
dev_fixtures_publish()                  # dry run: what it would upload
dev_fixtures_publish(dry_run = FALSE)   # create the release and upload
```

It uploads every manifest file present in the cache, then rewrites the `url`
column of `inst/fixtures.csv` to point at the release. Commit that change.

Verify the round trip once, since this is the whole recovery story:

```r
unlink(dev_fixture_dir(), recursive = TRUE)
dev_fixtures()
dev_fixture_status(verify = TRUE)
```

### GitHub release limits

| | limit |
|---|---|
| per file | under 2 GiB |
| files per release tag | 1000 |
| total size | no limit |
| bandwidth | no limit |

Release assets do not count toward repository size. That is why this works and
why Git LFS, which is metered on the free tier, does not.

## Adding a new large dataset

Yes, the same place: one more row in the manifest and one more release asset.

1. Put the file in the cache and get its checksum:

   ```r
   dev_fixture_adopt("~/somewhere/NEW_DATA.mat")
   ```

   That copies it into the cache and prints a ready-made manifest row.

2. Paste the row into `inst/fixtures.csv`, filling in `used_by` and `notes`.
   Leave `url` empty for now.

3. Upload it and let the manifest be rewritten:

   ```r
   dev_fixtures_publish(names = "NEW_DATA.mat", dry_run = FALSE)
   ```

4. Use it from a test:

   ```r
   test_that("reads the new format", {
     skip_if_nofixture("NEW_DATA.mat")
     x <- read_set(fixture_path("NEW_DATA.mat"))
     expect_s3_class(x, "eeg_lst")
   })
   ```

Once past 1000 files, or to group things, use a second tag:
`dev_fixtures_publish(tag = "testdata-v2", ...)`. The manifest stores a full
URL per file, so tags can be mixed freely.

### Licensing: check before uploading

A release on a public repo is public, so only upload data you have the right
to redistribute. Record the provenance and the licence in the manifest's
`notes` column while you still remember it.

What each current fixture is:

| fixture | source | redistributable |
|---------|--------|-----------------|
| `EEG01.mat` | Frank, Otten, Galli & Vigliocco (2015) *Brain and Language* 140:1-11, [doi:10.1016/j.bandl.2014.10.006](https://doi.org/10.1016/j.bandl.2014.10.006) | Yes. The version of record is **CC BY 3.0** (confirmed via Crossref and Unpaywall), so redistribution is allowed **with attribution**. |
| `s04.*` | [FieldTrip preprocessing_erp tutorial](https://www.fieldtriptoolbox.org/tutorial/preprocessing_erp/) | Almost certainly, but confirm FieldTrip's terms. |
| `s1_faces.*` | your own OSF project [tbwvz](https://osf.io/tbwvz/) | Yours. |

For anything you cannot republish (participant data under consent
restrictions, for instance), leave `url` empty and seed the cache from your
own copy with `dev_fixture_adopt()`. Tests skip cleanly wherever a file is
absent, so the suite stays green for everyone else.

## Building and previewing the pkgdown site

```r
pkgdown::build_site(preview = TRUE)   # build everything, then open it
pkgdown::preview_site()               # open an already-built site
pkgdown::build_home()                 # just the home page, fast
pkgdown::build_reference()            # just the function reference
pkgdown::build_article("intro")       # one vignette
```

`preview_site()` opens `docs/index.html` over `file://`, which is fine for a
look but leaves the search box and some relative links broken. Serve it over
HTTP instead:

```bash
python3 -m http.server -d docs 8000    # then open http://localhost:8000
```

or, from R, with auto-reload on rebuild:

```r
install.packages("servr")
servr::httd("docs")
```

`docs/` is in `.gitignore` on purpose: the pkgdown GitHub Action builds the
site and deploys it to the `gh-pages` branch, and `docs/` was never tracked on
master. Local builds are for preview only and will not dirty your commits.

### Images need alt text

pkgdown checks accessibility and warns `Missing alt-text in README.md` for any
image without it. `README.md` is generated from `README.Rmd`, so fix the
source or it comes back on the next knit:

- plots: add `fig.alt = "what the plot shows"` to the chunk options
- hand-written `<img>`: add an `alt="..."` attribute

Both files currently carry alt text for the logo and the two README plots. If
you only edit `README.Rmd` you must re-knit, and note that re-knitting
re-downloads the ~69 MB faces dataset.

## Release checklist

```r
dev_test()          # 0 failures
dev_check()         # R CMD check
devtools::document()
```

Then:

- update `NEWS.md` and bump `Version:` in `DESCRIPTION`
- `codemetar::write_codemeta(".")`
- `spelling::spell_check_package()`
- `goodpractice::gp()`
- before a release, test against current CRAN: `dev_r_deps(upgrade = "always")`
- commit the rewritten `inst/fixtures.csv` if any fixture moved
- push and check that the GitHub Actions runs pass, including the Python steps

## Known issues

- `data.table::setcolorder()` corrupts a table with 64+ columns and no
  over-allocation: it moves column names without moving the data. Present in
  1.18.4 and 1.18.6.1. Reproduction in `dev/datatable-setcolorder-bug.R`;
  regression tests in `tests/testthat/test_21-setcolorder_selfref.R`. The
  summarize paths are guarded, but `validate_signal_tbl()` and
  `validate_psd_tbl()` are still exposed at 64+ channels (two skipped tests
  record this).
- `_snaps/01-reading.md` was regenerated and its contents have not been
  audited against what EMP01 should produce.
