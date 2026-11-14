# Remove (transform to NA) problematic events from the signal table of an eeg_lst.

Remove (transform to NA) problematic events from the signal table of an
eeg_lst.

## Usage

``` r
eeg_events_to_NA(
  x,
  ...,
  .n_chs = NULL,
  .entire_seg = TRUE,
  .drop_events = TRUE,
  .all_chs = FALSE
)
```

## Arguments

- x:

  An `eeg_lst` object.

- ...:

  Description of the problematic event.

- .n_chs:

  If set to `NULL` (default), it will only set `NA` to the relevant
  channel.If set to a number `N`, it will set all the channels of the
  entire segment or interval to `NA`, if `N` or more channels have a
  certain event, and otherwise only the relevant channel.

- .entire_seg:

  If set to `FALSE`, it will consider only the marked part of the
  segment, otherwise it will consider the entire segment (Default:
  .entire_seg = TRUE).

- .drop_events:

  If set to `TRUE` (default), the events that were used for setting
  signals to NA, will be removed from the events table.

- .all_chs:

  Deprecated.

## Value

An eeg_lst.

## See also

Other events functions:
[`eeg_artif`](https://bruno.nicenboim.me/eeguana/reference/eeg_artif.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# Signals with artifacts are turned into NA values:
faces_clean <- faces_seg_artif %>%
  eeg_events_to_NA(.type == "artifact", .entire_seg = TRUE, .drop_events = TRUE)


# Specific segments are turned into NA values:
faces_clean <- faces_seg_artif %>%
  eeg_events_to_NA(.id %in% c(1:3), .entire_seg = TRUE, .drop_events = TRUE)
} # }
```
