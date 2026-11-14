# Functions to get or set the channel information of an eeg_lst object.

Functions to get or set the channel information of an eeg_lst object.

## Usage

``` r
channels_tbl(.data, ...)

channels_tbl(.data) <- value
```

## Arguments

- .data:

  An eeg_lst object.

- ...:

  Not in use.

- value:

  A channel table.

## Value

A table.

## See also

Other functions to expose internal parts of eeg\_(ica\_)\_lst:
[`events_tbl()`](https://bruno.nicenboim.me/eeguana/reference/events_tbl.md),
[`ica_matrix_lst()`](https://bruno.nicenboim.me/eeguana/reference/ica_matrix_lst.md),
[`segments_tbl()`](https://bruno.nicenboim.me/eeguana/reference/segments_tbl.md),
[`signal_tbl()`](https://bruno.nicenboim.me/eeguana/reference/signal_tbl.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(tidytable) # faster alternative to dplyr
# Get channel information:
channels_tbl(faces_seg)
# Set channel information using tidytable's mutate and replace
channels_tbl(faces_seg) <- mutate(channels_tbl(faces_seg),
  .channel = replace(.channel, .channel == "HEOG", "EOGH"),
  .channel = replace(.channel, .channel == "VEOG", "EOGV")
)
} # }
```
