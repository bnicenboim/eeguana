# Local development setup

Everything here is tracked by git but excluded from the built package
(`^dev$` in `.Rbuildignore`), so it never reaches CRAN or an installed copy.

```r
source("dev/dev.R")
dev_test()          # install anything missing, then run the test suite
```

`dev_test()` does four things before calling `devtools::test()`:

1. `dev_r_deps()` installs missing R packages, `Suggests` included. Several
   tests need `R.matlab`, `plotly`, or `akima`, and they fail rather than skip
   when those are absent.
2. `dev_check_updates()` asks CRAN what is newer than what you have and lists
   it. Once per session, since it needs a network round trip. It only reports;
   `dev_r_deps(upgrade = "always")` is what actually updates.
3. `dev_python()` sets up the `r-eeguana` conda environment with `mne`,
   `pandas`, and `scipy`. `.onLoad()` looks for exactly that environment name.
4. `dev_fixtures_ensure()` checks the large test files and, if any are
   missing, asks once per session whether to download them. Answer no and it
   remembers, so repeated runs do not nag; the tests that need them skip.

Skip the parts you have already done: `dev_test(deps = FALSE, python = FALSE)`,
or filter to one file with `dev_test(filter = "reading")`. Every stage has its
own flag: `deps`, `updates`, `python`, `fixtures`.

## Plain devtools::test() vs dev_test()

`devtools::test()` on its own does **not** check dependencies, Python, or the
fixtures. It is safe to run: tests that need a missing fixture call
`skip_if_nofixture()` and tests that need MNE call
`skip_if_no_python_stuff()`, so they skip rather than fail. The cost is that
they skip *silently*, and you can mistake a green run for full coverage.

Use `dev_test()` when you want to know what is missing, and plain
`devtools::test()` for a fast iteration loop once the setup is in place.

Other entry points: `dev_fixture_status()`, `dev_python_ok()`,
`dev_check_updates()`, `dev_fixture_upload_gh()`, `dev_check()`.

## Large test files

Files too big for git live in a per-user cache directory, not in the package
sources:

```
tools::R_user_dir("eeguana", "cache")/fixtures
```

That location is outside the library tree, so **reinstalling or rebuilding
`eeguana` never deletes it**, and it is the only place CRAN policy allows a
package to write to. Override it with the `EEGUANA_FIXTURES` environment
variable if you want several checkouts to share one copy.

`inst/fixtures.csv` is the manifest: name, filename, download url, sha256,
size, and what uses each file. It ships with the package (it is a few hundred
bytes) so the tests can find the files from any installation. To recover the
whole set on a new machine:

```r
source("dev/dev.R")
dev_fixtures()
```

Downloads go to a `.part` file and are renamed only on success, then checked
against the recorded size and sha256, so an interrupted download cannot leave
something that looks valid.

### Adding a file

Upload it, then:

```r
dev_fixture_adopt("path/to/big_file.mat", url = "https://osf.io/xxxxx/?action=download")
```

That copies the file into the cache and prints the manifest row, checksum
included, to paste into `inst/fixtures.csv`.

### Using one in a test

```r
test_that("...", {
  skip_if_nofixture("EEG01.mat")
  x <- read_set(fixture_path("EEG01.mat"))
})
```

Tests skip rather than fail when a fixture is absent, so a fresh clone, CI, and
CRAN all still run the rest of the suite.
