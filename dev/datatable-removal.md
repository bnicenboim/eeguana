# Reducing eeguana's use of data.table

Notes for the `nodatatable` branch, started 2026-09-13 from `f2ffc49`.

**Goal.** Take data.table out of `Imports` where that simplifies the code,
keep it only where it makes the package measurably faster, keep every test
passing, and do not slow anything down. Objects must keep printing the way they
do now.

## What removing it can and cannot achieve

**data.table stays installed either way.** tidytable 0.11.2 imports
data.table (>= 1.16.0), and eeguana imports tidytable. The gain is simpler code
and one fewer direct import, not a smaller install.

**Printing needs no work.** The current look (the `<sample_int>` type row, the
`1:` row numbers) is data.table's print method. It is chosen by S3 dispatch on
the class name `"data.table"` and needs only data.table's namespace to be
loaded, which tidytable guarantees. Checked with tidytable loaded, data.table
never attached, and eeguana not loaded: a table classed
`signal_tbl/data.table/data.frame` prints exactly as it does now. So the class
vector keeps `"data.table"`. Without keys, the only visible change is that the
`Key: <.id, .sample>` line above each table goes away.

## How much data.table there is

- 231 `data.table::` calls. The most common are `as.data.table` (50), `setkey`
  (47), `data.table()` (38), `copy` (22), `setnames` (15), `setcolorder` (11),
  and `setattr` (11).
- data.table's bracket syntax: 111 lines with `:=`, 116 with `by =`, 30 with
  `..var`, 19 with `.SD`, 14 with `.N`, and 13 with `on =`.
- 149 functions use data.table constructs, 543 in total. 55 of them touch the
  signal table and hold 284 of those constructs. Those are where replacing
  by-reference updates with tidytable verbs, which copy, could cost time and
  memory, since the signal table can be hundreds of Mb.
- The heaviest functions are `eeg_segment.eeg_lst()` (33), `read_set()` (22),
  `segment_events()` (19), `eeg_bind()` (16), `read_dat()` (15),
  `update_events()` (13), and `filter_lst()` (11).

## Keys

Keys are set in 49 places (`setkey()` 47, `setkeyv()` 2) and read in three: the
two "keys are missing" warnings in the table validators, and `keep_dt_attrs()`.

**No code looks rows up by key.** All 13 joins name their columns with `on =`,
and none of the bracket expressions is a keyed lookup.

**Nothing silently depends on the rows being sorted.** A pipeline of read,
segment, filter, mutate, downsample, grouped summarize, bind, and baseline was
run on a build with every `setkey()` turned into a no-op. At every step the
signal table stayed sorted by `.id` and `.sample`, in the same row order and
with the same values as the keyed build. The segments and events tables matched
too.

**The test suite without keys:** 5 of 248 tests fail and 1457 expectations pass,
against 1469 normally. The failing tests are the BDF-vs-MNE comparison
(test_01), both ICA tests (test_02), segmenting with `.lim` (test_04), and
`chs_fun` against `chs_mean` (test_08).

**Every one of those failures is the `sorted` attribute and nothing else.** In
each case one side carries data.table's key marker and the other does not; no
value, row order, or column differs. So keys do no functional work in the
package.

Keys also live outside the `setkey()` calls. In some failures the build without
keys is the side that still carries `sorted`: those objects come from stored
data, such as bundled datasets or test fixtures saved from a keyed build.
Dropping keys cleanly means stripping them there as well.

That stored data is `R/sysdata.rda` (563 KB: `data_sincos2id`, `data_blinks`,
`data_mne_bdf`, and the other internal test objects) and `data/*.rda` (the faces
datasets and the layout). Neither is practical to regenerate:
`data-raw/create_test_data.R` needs MNE through Python and scipy, and
`data-raw/create_faces_data.R` downloads the recordings from OSF. They do not
need regenerating. The objects can be loaded from the existing files, stripped
of `sorted`, and saved back with the same `xz` compression, leaving every value
unchanged, which is easy to verify object by object.

Every eeg object stored in the package carries keys: all 11 in `R/sysdata.rda`
and both faces datasets in `data/`, on `.signal` (or `.psd`) and `.segments`.
Only `layout_32_1020` has none.

**Speed with and without keys.** The 16 benchmark cases were run on both builds
in two rounds of alternating order, on the 34-channel `s1_faces` and 64-channel
`s04` recordings. Memory is unchanged in every case. Time is the same or better
without keys, with a median ratio of 0.945, and the biggest gains are in the
everyday verbs, because `setkey()` re-sorted the table on every call:

| case | keyed | without keys | ratio |
|---|---|---|---|
| `eeg_select()`, 8 of 34 channels | 0.0275 s | 0.0201 s | 0.73 |
| `eeg_group_by(.id)` + summarize | 0.735 s | 0.563 s | 0.77 |
| `eeg_mutate()`, new channel | 0.0758 s | 0.0594 s | 0.78 |
| `eeg_downsample(q = 4)` | 8.83 s | 7.90 s | 0.90 |
| `eeg_filter()` on `.sample` | 0.114 s | 0.106 s | 0.92 |
| `read_vhdr()`, 64 ch / 1.1M samples | 4.43 s | 4.43 s | 1.00 |
| `mutate()` on an events table | 0.0029 s | 0.0032 s | 1.12 |

The events-table case is a few milliseconds, where machine noise dominates. The
64-channel read first came out at 1.09 (4.47 s against 4.85 s). Rechecked over 15
reads per build in alternating rounds, both builds have a median of 4.426 s, with
ranges of 4.09 to 5.46 s and 4.08 to 5.55 s, so that was noise. Dropping keys
slows nothing down in any case measured.

## Where data.table may have to stay

**Range joins.** `eeg_segment()` and the BrainVision reader match samples to
segments with non-equi joins, such as
`on = .(.id, .sample >= .lower, .sample <= .upper)`. tidytable has no
`join_by()`, so it cannot express them. They either stay on data.table or move
to base R (for example `findInterval()`), whichever is not slower.

## data.table against tidytable, operation by operation

Measured on the `s1_faces` signal table (424,488 rows by 36 columns; 71,800
rows once segmented), median of 10 runs:

| operation | data.table | tidytable | ratio |
|---|---|---|---|
| mean of every channel per sample | 0.014 s | 0.046 s | 3.3 |
| add a column | 0.0011 s | 0.0022 s | 1.9 |
| change a column within segments | 0.0072 s | 0.0104 s | 1.5 |
| filter rows | 0.038 s | 0.043 s | 1.1 |
| select 8 columns | 0.0045 s | 0.0014 s | 0.3 |
| stack two signal tables | 0.21 s | 0.08 s | 0.4 |
| samples inside 200 event windows (range join) | 0.075 s | 0.014 s with base R `findInterval()` | 0.2 |

tidytable is not uniformly slower. It is faster at selecting and stacking, and
base R's `findInterval()` beats data.table's range join by 5x with identical
results, at least for non-overlapping windows on one recording. The clear cost
is grouped averaging across every channel, which is what `eeg_summarize()` does
for ERPs. Adding or changing a column is slower too, but in milliseconds.

## Plan

Each stage is its own commit, verified before it is proposed: the full test
suite, `R CMD check`, and the benchmark cases compared against the commit before
the stage.

**Stage A: drop keys.** Remove the 49 `setkey()`/`setkeyv()` calls, the two
"keys are missing" warnings, and the key handling in `keep_dt_attrs()`. Strip
`sorted` from the 13 stored objects and save them back with `xz` compression,
checking each object's values are unchanged. Expected: the same results, about
5% faster overall, and the `Key: <.id, .sample>` line gone from printed tables.
This is the lowest-risk stage and the one most of the code simplification rests
on.

**Stage B: move the operations where tidytable is as fast or faster.**
Selecting columns (`dt[, ..cols]`), stacking (`rbindlist()`), building small
tables (`data.table()` for events, segments, and channel tables), renaming
(`setnames()`), and reshaping (`melt()` to `pivot_longer()`). Tables stored in an
eeg object keep the class `c(<eeguana class>, "data.table", "data.frame")`, so
they print as now; tidytable's own class is removed before storing, as the
`tt_*` wrappers already do.

**Stage C: range joins to base R where the windows cannot overlap.** Replace a
non-equi join with `findInterval()` only where the code guarantees sorted,
non-overlapping windows. Where segments may overlap, as `eeg_segment()` allows,
keep the join.

**Stage D: keep data.table where it is measurably faster, and in one place.**
Grouped operations across every channel (`lapply(.SD, f), by =`), in-place
updates of the signal table in hot paths, the overlapping-window joins, and
`fread()` for reading text. These move into a single internal file with a
comment on each helper stating the measured reason it stays. data.table stays in
`Imports` for that file only.

## Decisions, 2026-09-13

**Nothing may get slower or use more memory.** Every change in every stage is
benchmarked against the commit before it, and a change that measures slower or
heavier does not go in. That rules out removing data.table completely: Stage D
stands, and data.table stays wherever it is faster.

**Stage C is not assumed.** The only range-join measurement so far is synthetic
(one recording, non-overlapping windows), where `findInterval()` was 5x faster
but used more memory, 18 Mb against 14 Mb. A range join switches to base R only
if measuring the real function shows it is neither slower nor heavier on memory.
Otherwise it stays on data.table.
