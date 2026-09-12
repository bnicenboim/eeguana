# Where to keep the large test files

## If OSF is going away: GitHub releases for fixtures, Zenodo for anything citable

For pure test data, attach it to a **GitHub release** on the eeguana repo and
use the `piggyback` package. It is the least-effort durable option:

- No authentication to download, so `dev_fixtures()` stays a plain
  `download.file()`. Uploading needs a `GITHUB_PAT`; downloading needs nothing.
- 2 GB per file, and release assets do not count against repository size.
- Versioned alongside the code, and a release tag such as `testdata` can be
  updated in place as fixtures change.
- One call: `dev_fixture_upload_gh("path/to/file")` uploads it and prints the
  manifest row, checksum included.

URLs are predictable, which is what the manifest wants:

```
https://github.com/bnicenboim/eeguana/releases/download/testdata/EEG01.mat
```

Use **Zenodo** instead for anything that should outlive the repository or be
cited in a paper: 50 GB per record, a DOI per version plus a concept DOI that
always resolves to the latest, and `zen4R` for scripted deposits. The cost is
that records are immutable once published, so iterating on a fixture means a
new version. Good for the vignette datasets, overkill for scratch test files.

**Hugging Face dataset repos** are a third option worth knowing about: free,
generous, git-LFS backed, fast CDN, and direct URLs of the form
`https://huggingface.co/datasets/<user>/<repo>/resolve/main/<file>`. No DOI.

Whatever you pick, the manifest holds a plain URL per file, so switching hosts
is an edit to one column and a re-run of `dev_fixtures()`. Nothing in the
package or the tests refers to a host.

## Why the host is the less important half

`inst/fixtures.csv` holds a plain URL per file. Any host that serves a direct,
unauthenticated download works, so moving from OSF to Zenodo later is an edit
to one column, not a code change. The part that actually matters is that the
files live in `tools::R_user_dir("eeguana", "cache")` rather than in the source
tree: that is what survives reinstalling the package, and what keeps 235 MB out
of git and out of the tarball.

## Files you cannot redistribute

For participant data you are not allowed to upload anywhere, leave the `url`
column empty and seed the cache from your own copy:

```r
dev_fixture_adopt("~/private/EMP01_full.eeg")
```

The tests will skip cleanly on any machine where the file is absent.
