# Replacing dplyr with tidytable

Notes for the `nodplyr` branch. Survey done 2026-09-12, nothing implemented yet.

## The short version

The internal calls can be replaced. The S3 method registrations mostly cannot,
and dropping dplyr entirely would break the package's advertised behaviour.

## Two kinds of dplyr usage, and only one is replaceable

**Internal calls, 175 of them.** `dplyr::select()`, `dplyr::mutate()` and so on
inside eeguana's own functions. Straightforward to replace.

**S3 method registration, 26 of them**, in `R/zzz.R` and `NAMESPACE`:
`register_s3_method("dplyr", "filter", "eeg_lst")` and friends. These are why
`dplyr::filter(my_eeg)` works, which DESCRIPTION advertises as the point of the
package: "manipulating EEG data with dplyr-based functions ... extended to a
new class".

`dplyr::filter` and `tidytable::filter` are **different generic objects**. A
method registered on one is invisible to the other, so dropping dplyr from
Imports does not move these methods to tidytable, it deletes them.

## What cannot be done at all

Nine of the 26 registrations have no tidytable equivalent:

| generic | problem |
|---------|---------|
| `left_join`, `semi_join`, `anti_join` | exported by tidytable but **not generics**, so no method can be registered |
| `pull`, `bind_rows`, `group_vars` | same, not generics there |
| `tbl_vars`, `groups`, `as_tibble` | **not in tidytable at all**, they are dplyr/tibble generics |

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

**Stage 1, internal calls only.** Replace the 175 `dplyr::` calls with
`tidytable::`, and rewrite the 11 superseded ones. Keep dplyr in Imports purely
for its generics. Mechanical, testable, no API change. Most of the work and all
of the safety.

**Stage 2, retire the dot-suffixed shims.** The 12 wrappers in `R/utils_dt.R`
(`mutate.`, `select.`, `left_join.` ...) exist to restore classes that
tidytable strips. Re-check which still strip: `relocate()` did not, when it was
tested for `validate_signal_tbl()`. Removing them also clears the `R CMD check`
NOTE about apparent S3 methods not registered, which is a false positive caused
by the trailing dot in their names.

**Stage 3, decide the generics.** Needs a decision:

- **(a)** keep dplyr in Imports for generics only, and also register on
  tidytable. Both `dplyr::filter(eeg)` and `tidytable::filter(eeg)` work. One
  dependency kept, nothing breaks. **Recommended.**
- **(b)** register on tidytable only. Drops dplyr, breaks `dplyr::filter(eeg)`
  for every user, and is impossible for the nine non-generics above.
- **(c)** keep dplyr generics and add tidytable ones alongside. Widest
  compatibility, most registration code.

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
