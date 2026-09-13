# Replacing dplyr with tidytable

Notes for the `nodplyr` branch. Survey done 2026-09-12. **Stage 1 is done**;
Stages 2 and 3 are not started.

## The short version

The internal calls can be replaced, and dplyr can leave `Imports` for
`Suggests` while `dplyr::filter(eeg)` goes on working, because the S3 methods
are registered through a load hook rather than at build time.

## dplyr is used in two ways that need different treatment

**Internal calls, 175 of them.** `dplyr::select()`, `dplyr::mutate()` and so on
inside eeguana's own functions. Straightforward to replace.

**S3 method registration, 20 verbs**, across 25 `register_s3_method()` lines in
`R/zzz.R` and 4 `S3method()` entries in `NAMESPACE` (some verbs are registered
for both `eeg_lst` and `events_tbl`):
`register_s3_method("dplyr", "filter", "eeg_lst")` and friends. These are why
`dplyr::filter(my_eeg)` works, which DESCRIPTION advertises as the point of the
package: "manipulating EEG data with dplyr-based functions ... extended to a
new class".

`dplyr::filter` and `tidytable::filter` are **different generic objects**. A
method registered on one is invisible to the other, so pointing these
registrations at tidytable does not move the methods across, it deletes the
dplyr ones. They stay registered on dplyr's generics; what changes is that the
registration happens lazily, so dplyr can sit in `Suggests`. See Stage 3.

## Registering on tidytable's generics is only half possible

This matters only for the optional extra in Stage 3, that is making
`tidytable::filter(eeg)` work. Measured against tidytable 0.11.2, **9 of the 20
verbs are generics there**, so a method can be registered. The other **11
cannot be**:

| generic | problem |
|---------|---------|
| `left_join`, `semi_join`, `anti_join` | exported by tidytable but **not generics**, so no method can be registered |
| `pull`, `bind_rows`, `group_vars`, `transmute` | same, not generics there |
| `tbl_vars`, `groups`, `as_tibble`, `as_data_frame` | **not in tidytable at all**, they are dplyr/tibble generics |

For the joins there is no workaround short of tidytable making them generic
upstream.

## What is easy

31 of the 42 verbs eeguana calls exist in tidytable under the same name. The
11 that do not are mostly superseded dplyr that is worth modernising anyway:

- `filter_at`, `group_by_at`, `mutate_all`, `summarize_at`, `vars`, `all_vars`
  become `across()` plus tidyselect, which tidytable has
- `one_of` becomes `any_of()` / `all_of()`
- `intersect` becomes the base R one
- `tibble()` / `as_tibble()` become `tidytable()` / `as_tidytable()` internally

## Plan

**Stage 1, internal calls only. DONE.** Of the 175 matches, 3 were roxygen
links or comments and 61 were commented-out code, leaving 111 live sites. All
111 are migrated. 4 guarded `dplyr::` calls remain on purpose: `across()` and
`c_across()` check `"dplyr" %in% .packages()`, and `tbl_group_vars()` /
`tbl_ungroup()` check `inherits(x, "grouped_df")`. Those are conditional use,
which is what Suggests allows.

It was not the mechanical swap this plan predicted. Three things had to be
handled by hand:

- **tidytable verbs drop the class and the data.table key.** Results flowing
  back into an `eeg_lst` go through the new `keep_dt_attrs()`. Nine test
  failures came from two lines that missed this.
- **The two grouping systems cannot read each other.**
  `tidytable::group_vars()` on a dplyr `grouped_df` silently returns the
  contents of dplyr's `.groups` attribute instead of the group names. The
  public `.data.frame` methods take user-grouped data, so `tbl_group_vars()`
  dispatches on the class.
- **`tidytable()` does not evaluate its arguments in order**, unlike
  `tibble()`, and `base::split()` reaches data.table's own method once an
  object becomes a data.table.

One API change, contrary to what this plan claimed: `eeg_interpolate_tbl()`
returns a tidytable rather than a tibble, so `plot_topo.tbl_df` was widened to
`plot_topo.data.frame`. It also uncovered that `drop_incomplete_segments()` had
been broken outright, erroring with `could not find function "na.omit"` on
every call, which is why nothing called it and nothing tested it.

**Stage 2, retire the dot-suffixed shims.** The 12 wrappers in `R/utils_dt.R`
(`mutate.`, `select.`, `left_join.` ...) exist to restore classes that
tidytable strips. Re-check which still strip: `relocate()` did not, when it was
tested for `validate_signal_tbl()`. Removing them also clears the `R CMD check`
NOTE about apparent S3 methods not registered, which is a false positive caused
by the trailing dot in their names.

**Stage 3, move dplyr to Suggests.** This is the goal: dplyr out of `Imports`,
with `dplyr::filter(eeg)` still working. It does work, because a user can only
type `dplyr::` if dplyr is installed and loaded, and eeguana's hook attaches
the method at that moment. eeguana itself never loads dplyr.

`register_s3_method()` in `R/compat_utils.R` already registers through
`setHook(packageEvent(pkg, "onLoad"), ...)`. One line defeats it:

```r
register_s3_method <- function(pkg, generic, class, fun = NULL) {
  envir <- asNamespace(pkg)      # runs unconditionally, so .onLoad loads dplyr
```

`asNamespace()` has to move inside both branches, the way rlang and vctrs do
it, so the namespace is only touched when dplyr is actually present.

Then:

1. `importFrom(dplyr, ...)` at `NAMESPACE:295` and the four `@importFrom dplyr`
   tags at `R/dplyr_verbs.R:50-53` get deleted. 12 bare call sites depend on
   them (`group_by` 3, `ungroup` 4, `mutate` 2, `select` 2, `group_vars` 1) and
   have to be qualified or rewritten.
2. The four `S3method(dplyr::...)` entries in `NAMESPACE` move into `.onLoad`
   as `register_s3_method()` calls. `S3method(pkg::generic, class)` requires
   the package in Imports or Depends, so it cannot stay.
3. `dplyr (>= 1.0.0)` moves from `Imports` to `Suggests`.

What makes this clean: **eeguana re-exports no dplyr verbs**. All 20 were
checked against `export()` in `NAMESPACE` and none is there, so users already
get `filter` from dplyr or tidytable themselves and there is no re-export to
keep alive.

All 20 verbs keep working through `dplyr::`, the 11 that tidytable has no
generic for included, since those go on dispatching on dplyr's generics.

Registering on tidytable's generics as well is a separate and optional choice.
It would make `tidytable::filter(eeg)` work, which errors today, but only for
the 9 verbs that are generics there, so the API would be uneven. Not needed for
the goal.

### What to watch

- `R CMD check` wants every example, test, and vignette that uses a Suggests
  package to be conditional. Check which of them call dplyr directly.
- The methods must not be roxygen `@export`ed, or they land back in `NAMESPACE`
  as `S3method()` and reinstate the hard dependency.
- Verify the result with `loadedNamespaces()` after `library(eeguana)` in a
  fresh session: dplyr must not appear.

## The thing that actually blocks "no data.table"

Keys. `setkey` is used throughout and `validate_signal_tbl()` warns when the
key is missing. tidytable has no equivalent concept. If the longer goal is
dropping data.table rather than dplyr, keys are the obstacle, not the verbs,
and that deserves its own look before investing in the stages above.

## How the survey was done

```sh
grep -rhoE "dplyr::[a-zA-Z_.]+" R/*.R | sort | uniq -c | sort -rn
grep -nE 'register_s3_method\("dplyr"' R/zzz.R
grep -E "^S3method\(dplyr::" NAMESPACE
```

and, in R, comparing `identical(dplyr::f, tidytable::f)` and checking for
`UseMethod` in each.
