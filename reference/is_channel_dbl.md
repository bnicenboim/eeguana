# Test if the object is a channel or EOG channel

- `is_channel_dbl()` returns TRUE for all channels including EOG
  channels.

- `is_eog_channel_dbl()` returns TRUE only for EOG channels.

## Usage

``` r
is_channel_dbl(x)
```

## Arguments

- x:

  An object.

## Value

`TRUE` if the object inherits from the `channel_dbl` class.

## See also

Other channel:
[`as_channel_dbl()`](https://bruno.nicenboim.me/eeguana/reference/as_channel_dbl.md),
[`channel_dbl()`](https://bruno.nicenboim.me/eeguana/reference/channel_dbl.md)
