# Re-reference a channel or group of channels.

Re-reference a channel or group of channels.

## Usage

``` r
eeg_rereference(.data, ..., .ref = NULL, na.rm = FALSE)
```

## Arguments

- .data:

  An eeg_lst object.

- ...:

  Channels to include. All the channels by default, but eye channels
  should be removed.

- .ref:

  Channels that will be averaged as the reference. tidyselect
  compatible.

- na.rm:

  a logical evaluating to `TRUE` or `FALSE` indicating whether `NA`
  values should be stripped before the computation proceeds.

## Value

An eeg_lst with some channels re-referenced.

## Details

Notice that this function will also rereference the eye electrodes
unless excluded. See examples.

## See also

Other preprocessing functions:
[`eeg_baseline()`](https://bruno.nicenboim.me/eeguana/reference/eeg_baseline.md),
[`eeg_downsample()`](https://bruno.nicenboim.me/eeguana/reference/eeg_downsample.md),
[`eeg_ica()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica.md),
[`eeg_ica_keep()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_keep.md),
[`eeg_segment()`](https://bruno.nicenboim.me/eeguana/reference/eeg_segment.md),
[`filt`](https://bruno.nicenboim.me/eeguana/reference/filt.md)

## Examples

``` r
# Re-reference all channels using the left mastoid excluding the eye electrodes.
data_faces_ERPs_M1 <- data_faces_ERPs %>%
  eeg_rereference(-EOGV, -EOGH, .ref = M1)

# Re-reference using the linked mastoids excluding the eye electrodes.
data_faces_ERPs_M1M2 <- data_faces_ERPs %>%
  eeg_rereference(-EOGV, -EOGH, .ref = c(M1, M2))
```
