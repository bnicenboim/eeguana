# Bind eeg_lst objects.

Binds eeg_lst and throws a warning if there is a mismatch in the channel
information.

## Usage

``` r
eeg_bind(...)

bind(...)
```

## Arguments

- ...:

  `eeg_lst` objects to combine.

## Value

An `eeg_lst` object.

## See also

Other tidyverse-like functions:
[`dplyr_verbs`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md),
[`drop_incomplete_segments()`](https://bruno.nicenboim.me/eeguana/reference/drop_incomplete_segments.md),
[`eeg_slice_signal()`](https://bruno.nicenboim.me/eeguana/reference/eeg_slice_signal.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# Load multiple subjects, extracting subject IDs from file names.
faces_list <- lapply(list.files("./", "vhdr"), read_vhdr)
# Bind all the eeg_lsts into a large one:
faces <- eeg_bind(faces_list)
} # }
```
