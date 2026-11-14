# Print an eeg_lst

Prints the signal, events, and segments tables of an `eeg_lst`, a
`psd_lst`, or an `eeg_ica_lst`. Recordings with many channels make the
signal table very wide; `.max_channels` shows only the first ones, and a
line lists the others.

## Usage

``` r
# S3 method for class 'eeg_lst'
print(x, ..., .max_channels = getOption("eeguana.print_max_channels", Inf))

# S3 method for class 'psd_lst'
print(x, ..., .max_channels = getOption("eeguana.print_max_channels", Inf))

# S3 method for class 'eeg_ica_lst'
print(x, ..., .max_channels = getOption("eeguana.print_max_channels", Inf))
```

## Arguments

- x:

  An `eeg_lst`, `psd_lst`, or `eeg_ica_lst`.

- ...:

  Passed to the [`print()`](https://rdrr.io/r/base/print.html) method of
  data.table, for example `topn`.

- .max_channels:

  Maximum number of channels shown in the signal (or PSD) table. By
  default, the value of the option `eeguana.print_max_channels`, see
  below.

## Value

`x`, invisibly.

## Option `eeguana.print_max_channels`

The option sets how many channels are shown whenever an `eeg_lst`,
`psd_lst`, or `eeg_ica_lst` is printed, also when its name is typed in
the console. It is `Inf`, all the channels, unless it is changed:

    options(eeguana.print_max_channels = 8)

For the whole session, it can be set in `.Rprofile`. `.max_channels`
overrides it in one call. See
[eeguana](https://bruno.nicenboim.me/eeguana/reference/eeguana-package.md)
for the other options.

## Examples

``` r
print(data_faces_ERPs, .max_channels = 4)
#> # EEG data:
#> 
#> # Signal table:
#>        .id      .sample           Fp1           Fpz           Fp2            F7
#>      <int> <sample_int> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:     1          -99    0.22353428     0.6683971   -0.03621027     0.8601528
#>   2:     1          -98    0.25257599     0.7361622   -0.04687375     0.8352839
#>   3:     1          -97    0.13301252     0.7052292   -0.15697831     0.7692707
#>   4:     1          -96   -0.08392458     0.6128529   -0.24638127     0.6806059
#>   5:     1          -95   -0.28915086     0.5225633   -0.25209382     0.6288176
#>  ---                                                                           
#> 448:     2          122  -13.17179185   -14.3696799  -13.51380498    -8.9363176
#> 449:     2          123  -13.51617769   -14.6111804  -13.53817011    -8.9676788
#> 450:     2          124  -13.45730441   -14.7025321  -13.46071115    -8.8665316
#> 451:     2          125  -13.29286410   -14.7043950  -13.39205897    -8.7878105
#> 452:     2          126  -13.22445326   -14.7907774  -13.53960969    -8.8170396
#> # ... and 30 more channels: F3, Fz, F4, F8, FC5, FC1, FC2, FC6, M1, T7, C3, Cz, C4, T8, M2, CP5, CP1, CP2, CP6, P7, P3, Pz, P4, P8, POz, O1, Oz, O2, EOGV, EOGH
#> # Use print(.max_channels = ...) or options(eeguana.print_max_channels = ...) to see more.
#> 
#> # Events table:
#> No events.
#> 
#> # Segments table:
#>      .id .recording condition
#>    <int>     <char>    <char>
#> 1:     1 faces.vhdr     faces
#> 2:     2 faces.vhdr non-faces

## the same for every print, until it is changed back
op <- options(eeguana.print_max_channels = 4)
data_faces_ERPs
#> # EEG data:
#> 
#> # Signal table:
#>        .id      .sample           Fp1           Fpz           Fp2            F7
#>      <int> <sample_int> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:     1          -99    0.22353428     0.6683971   -0.03621027     0.8601528
#>   2:     1          -98    0.25257599     0.7361622   -0.04687375     0.8352839
#>   3:     1          -97    0.13301252     0.7052292   -0.15697831     0.7692707
#>   4:     1          -96   -0.08392458     0.6128529   -0.24638127     0.6806059
#>   5:     1          -95   -0.28915086     0.5225633   -0.25209382     0.6288176
#>  ---                                                                           
#> 448:     2          122  -13.17179185   -14.3696799  -13.51380498    -8.9363176
#> 449:     2          123  -13.51617769   -14.6111804  -13.53817011    -8.9676788
#> 450:     2          124  -13.45730441   -14.7025321  -13.46071115    -8.8665316
#> 451:     2          125  -13.29286410   -14.7043950  -13.39205897    -8.7878105
#> 452:     2          126  -13.22445326   -14.7907774  -13.53960969    -8.8170396
#> # ... and 30 more channels: F3, Fz, F4, F8, FC5, FC1, FC2, FC6, M1, T7, C3, Cz, C4, T8, M2, CP5, CP1, CP2, CP6, P7, P3, Pz, P4, P8, POz, O1, Oz, O2, EOGV, EOGH
#> # Use print(.max_channels = ...) or options(eeguana.print_max_channels = ...) to see more.
#> 
#> # Events table:
#> No events.
#> 
#> # Segments table:
#>      .id .recording condition
#>    <int>     <char>    <char>
#> 1:     1 faces.vhdr     faces
#> 2:     2 faces.vhdr non-faces
options(op)
```
