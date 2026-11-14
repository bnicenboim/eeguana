# Pitfalls and gotchas

I address some potential sources of confusion here. To run this
examples, we first load eeguana and dplyr:

``` r

library(eeguana)
library(tidytable) # faster alternative to dplyr
```

## Tidyverse dplyr-like functions

The package {eeguana} allows for functions that *look* like {dplyr}
functions but they truly are powered by {tidy.table} and {data.table}.
This means that there are subtle differences between dplyr’s behavior
and eeguana dplyr-like function behavior.

### Default values

The default values of the arguments might be different, and some
arguments might not exist for the eeguana dplyr-like functions.

### Referring back to recently created channels

Unlike dplyr’s mutate, {eeguana}’s \[mutate\] doesn’t allow to refer
back to a recently created channel:

``` r

new_data <- data_faces_ERPs %>%
  eeg_mutate(X = scale(C3), Y = X * 2)
#> Error:
#> ! object 'X' not found
```

A workaround is to use two mutates:

``` r

new_data <- data_faces_ERPs %>%
  eeg_mutate(X = scale(C3)) %>%
  eeg_mutate(Y = X * 2)
```

Dplyr-like functions not only edit the eeg_lst objects but they also do
book-keeping: They remove unnecessary channels, or update their
information and they ensure that three tables (signal, segments, and
events) match. It’s then not recommended to edit the signal and segments
table directly.

Compare the correct way to filter out samples:

``` r

new_data <- data_faces_10_trials %>%
  eeg_filter(.sample < 20000)
events_tbl(new_data)
#>       .id        .type .description     .initial       .final .channel
#>     <int>       <char>       <char> <sample_int> <sample_int>   <char>
#>  1:     1 Bad Interval  Bad Min-Max        15000        15128      FC6
#>  2:     1 Bad Interval  Bad Min-Max        15000        15146       F8
#>  3:     1 Bad Interval  Bad Min-Max        15000        15142      Fp1
#>  4:     1 Bad Interval  Bad Min-Max        15000        15132      Fp2
#>  5:     1 Bad Interval  Bad Min-Max        15000        15140      Fpz
#>  6:     1 Bad Interval  Bad Min-Max        15000        15114       F7
#>  7:     1 Bad Interval  Bad Min-Max        15000        15091      FC5
#>  8:     1 Bad Interval  Bad Min-Max        15000        15103       Fz
#>  9:     1 Bad Interval  Bad Min-Max        15000        15103       F4
#> 10:     1 Bad Interval  Bad Min-Max        15000        15093       F3
#> 11:     1 Bad Interval  Bad Min-Max        15276        15905       F8
#> 12:     1 Bad Interval  Bad Min-Max        15276        15556       F7
#> 13:     1 Bad Interval  Bad Min-Max        15551        15956      Fp1
#> 14:     1 Bad Interval  Bad Min-Max        15552        15941      Fp2
#> 15:     1 Bad Interval  Bad Min-Max        15552        15946      Fpz
#> 16:     1 Bad Interval  Bad Min-Max        15569        15910       Fz
#> 17:     1 Bad Interval  Bad Min-Max        15569        15904       F4
#> 18:     1 Bad Interval  Bad Min-Max        15571        15907       F3
#> 19:     1 Bad Interval  Bad Min-Max        15575        15885       F7
#> 20:     1     Stimulus           s5        15767        15767     <NA>
#> 21:     1 Bad Interval  Bad Min-Max        16344        16816       F8
#> 22:     1 Bad Interval  Bad Min-Max        16345        16630      Fp2
#> 23:     1 Bad Interval  Bad Min-Max        16347        16628      Fpz
#> 24:     1 Bad Interval  Bad Min-Max        16370        16806       F7
#> 25:     1     Stimulus         s101        17162        17162     <NA>
#> 26:     1 Bad Interval  Bad Min-Max        17440        18030      Fp1
#> 27:     1 Bad Interval  Bad Min-Max        17525        18009      Fp2
#> 28:     1 Bad Interval  Bad Min-Max        17525        18028      Fpz
#> 29:     1 Bad Interval  Bad Min-Max        17531        17898       F8
#> 30:     1 Bad Interval  Bad Min-Max        17533        17875       Fz
#> 31:     1 Bad Interval  Bad Min-Max        17534        17863       F3
#> 32:     1 Bad Interval  Bad Min-Max        17535        17877       F4
#> 33:     1 Bad Interval  Bad Min-Max        17545        17861      FC6
#> 34:     1 Bad Interval  Bad Min-Max        17548        17766      FC2
#> 35:     1 Bad Interval  Bad Min-Max        17553        17766      FC1
#> 36:     1 Bad Interval  Bad Min-Max        17559        17763       F7
#> 37:     1 Bad Interval  Bad Min-Max        18084        18609      Fp1
#> 38:     1 Bad Interval  Bad Min-Max        18085        18541      Fpz
#> 39:     1 Bad Interval  Bad Min-Max        18086        18541      Fp2
#> 40:     1 Bad Interval  Bad Min-Max        18092        18524       F7
#> 41:     1 Bad Interval  Bad Min-Max        18093        18424       F8
#> 42:     1 Bad Interval  Bad Min-Max        18093        18425       Fz
#> 43:     1 Bad Interval  Bad Min-Max        18094        18430       F3
#> 44:     1 Bad Interval  Bad Min-Max        18094        18420       F4
#> 45:     1 Bad Interval  Bad Min-Max        18110        18372      FC2
#> 46:     1 Bad Interval  Bad Min-Max        18821        19377      Fp1
#> 47:     1 Bad Interval  Bad Min-Max        18822        19358      Fpz
#> 48:     1 Bad Interval  Bad Min-Max        18822        19356      Fp2
#> 49:     1 Bad Interval  Bad Min-Max        18830        19172       F8
#> 50:     1 Bad Interval  Bad Min-Max        18834        19163       Fz
#> 51:     1 Bad Interval  Bad Min-Max        18835        19110       F3
#> 52:     1 Bad Interval  Bad Min-Max        18835        19158       F4
#> 53:     1 Bad Interval  Bad Min-Max        18840        19014       F7
#> 54:     1 Bad Interval  Bad Min-Max        18845        19086      FC6
#> 55:     1 Bad Interval  Bad Min-Max        18849        19081      FC2
#> 56:     1 Bad Interval  Bad Min-Max        19074        19357       F7
#> 57:     1 Bad Interval  Bad Min-Max        19464        19849      Fp1
#> 58:     1 Bad Interval  Bad Min-Max        19467        19826      Fp2
#> 59:     1 Bad Interval  Bad Min-Max        19467        19999      Fpz
#> 60:     1 Bad Interval  Bad Min-Max        19471        19807       F7
#> 61:     1 Bad Interval  Bad Min-Max        19475        19794       F8
#> 62:     1 Bad Interval  Bad Min-Max        19475        19799       F3
#> 63:     1 Bad Interval  Bad Min-Max        19477        19798       Fz
#> 64:     1 Bad Interval  Bad Min-Max        19479        19791       F4
#>       .id        .type .description     .initial       .final .channel
#>     <int>       <char>       <char> <sample_int> <sample_int>   <char>
```

with the *non-recommended* version:

``` r

new_data_BAD <- data_faces_10_trials
new_data_BAD$.signal <- new_data_BAD$.signal %>%
  filter(.sample < 20000)
events_tbl(new_data_BAD)
#>        .id        .type .description     .initial       .final .channel
#>      <int>       <char>       <char> <sample_int> <sample_int>   <char>
#>   1:     1 Bad Interval  Bad Min-Max        15000        15128      FC6
#>   2:     1 Bad Interval  Bad Min-Max        15000        15146       F8
#>   3:     1 Bad Interval  Bad Min-Max        15000        15142      Fp1
#>   4:     1 Bad Interval  Bad Min-Max        15000        15132      Fp2
#>   5:     1 Bad Interval  Bad Min-Max        15000        15140      Fpz
#>  ---                                                                   
#> 540:     1 Bad Interval  Bad Min-Max        55058        55400      Fpz
#> 541:     1 Bad Interval  Bad Min-Max        55077        55314       F8
#> 542:     1     Stimulus         s130        56380        56380     <NA>
#> 543:     1     Stimulus          s70        56750        56750     <NA>
#> 544:     1     Stimulus         s130        58328        58328     <NA>
```

In some occasions, the events or the channels tables need to be edited.
In those cases, one can use `events_tbl(data_eeg) <- ...` or
`channels_tbl(data_eeg) <- ...`).

``` r

channels_tbl(data_faces_10_trials)
#>     .channel radius theta   phi    .x    .y    .z
#>       <char>  <num> <num> <num> <num> <num> <num>
#>  1:      Fp1      1   -90   -72 -0.31  0.95  0.00
#>  2:      Fpz      1    90    90  0.00  1.00  0.00
#>  3:      Fp2      1    90    72  0.31  0.95  0.00
#>  4:       F7      1   -90   -36 -0.81  0.59  0.00
#>  5:       F3      1   -60   -51 -0.55  0.67  0.50
#>  6:       Fz      1    45    90  0.00  0.71  0.71
#>  7:       F4      1    60    51  0.55  0.67  0.50
#>  8:       F8      1    90    36  0.81  0.59  0.00
#>  9:      FC5      1   -69   -21 -0.87  0.33  0.36
#> 10:      FC1      1   -31   -46 -0.36  0.37  0.86
#> 11:      FC2      1    31    46  0.36  0.37  0.86
#> 12:      FC6      1    69    21  0.87  0.33  0.36
#> 13:       M1      0     0     0    NA    NA    NA
#> 14:       T7      1   -90     0 -1.00  0.00  0.00
#> 15:       C3      1   -45     0 -0.71  0.00  0.71
#> 16:       Cz      1     0     0  0.00  0.00  1.00
#> 17:       C4      1    45     0  0.71  0.00  0.71
#> 18:       T8      1    90     0  1.00  0.00  0.00
#> 19:       M2      0     0     0    NA    NA    NA
#> 20:      CP5      1   -69    21 -0.87 -0.33  0.36
#> 21:      CP1      1   -31    46 -0.36 -0.37  0.86
#> 22:      CP2      1    31   -46  0.36 -0.37  0.86
#> 23:      CP6      1    69   -21  0.87 -0.33  0.36
#> 24:       P7      1   -90    36 -0.81 -0.59  0.00
#> 25:       P3      1   -60    51 -0.55 -0.67  0.50
#> 26:       Pz      1    45   -90  0.00 -0.71  0.71
#> 27:       P4      1    60   -51  0.55 -0.67  0.50
#> 28:       P8      1    90   -36  0.81 -0.59  0.00
#> 29:      POz      1    67   -90  0.00 -0.92  0.39
#> 30:       O1      1   -90    72 -0.31 -0.95  0.00
#> 31:       Oz      1    90   -90  0.00 -1.00  0.00
#> 32:       O2      1    90   -72  0.31 -0.95  0.00
#> 33:     EOGV     NA    NA    NA    NA    NA    NA
#> 34:     EOGH     NA    NA    NA    NA    NA    NA
#>     .channel radius theta   phi    .x    .y    .z
#>       <char>  <num> <num> <num> <num> <num> <num>
new_data <- data_faces_10_trials
channels_tbl(new_data) <- channels_tbl(new_data) %>%
  mutate(resolution = .1)
channels_tbl(new_data)
#>     .channel radius theta   phi    .x    .y    .z resolution
#>       <char>  <num> <num> <num> <num> <num> <num>      <num>
#>  1:      Fp1      1   -90   -72 -0.31  0.95  0.00        0.1
#>  2:      Fpz      1    90    90  0.00  1.00  0.00        0.1
#>  3:      Fp2      1    90    72  0.31  0.95  0.00        0.1
#>  4:       F7      1   -90   -36 -0.81  0.59  0.00        0.1
#>  5:       F3      1   -60   -51 -0.55  0.67  0.50        0.1
#>  6:       Fz      1    45    90  0.00  0.71  0.71        0.1
#>  7:       F4      1    60    51  0.55  0.67  0.50        0.1
#>  8:       F8      1    90    36  0.81  0.59  0.00        0.1
#>  9:      FC5      1   -69   -21 -0.87  0.33  0.36        0.1
#> 10:      FC1      1   -31   -46 -0.36  0.37  0.86        0.1
#> 11:      FC2      1    31    46  0.36  0.37  0.86        0.1
#> 12:      FC6      1    69    21  0.87  0.33  0.36        0.1
#> 13:       M1      0     0     0    NA    NA    NA        0.1
#> 14:       T7      1   -90     0 -1.00  0.00  0.00        0.1
#> 15:       C3      1   -45     0 -0.71  0.00  0.71        0.1
#> 16:       Cz      1     0     0  0.00  0.00  1.00        0.1
#> 17:       C4      1    45     0  0.71  0.00  0.71        0.1
#> 18:       T8      1    90     0  1.00  0.00  0.00        0.1
#> 19:       M2      0     0     0    NA    NA    NA        0.1
#> 20:      CP5      1   -69    21 -0.87 -0.33  0.36        0.1
#> 21:      CP1      1   -31    46 -0.36 -0.37  0.86        0.1
#> 22:      CP2      1    31   -46  0.36 -0.37  0.86        0.1
#> 23:      CP6      1    69   -21  0.87 -0.33  0.36        0.1
#> 24:       P7      1   -90    36 -0.81 -0.59  0.00        0.1
#> 25:       P3      1   -60    51 -0.55 -0.67  0.50        0.1
#> 26:       Pz      1    45   -90  0.00 -0.71  0.71        0.1
#> 27:       P4      1    60   -51  0.55 -0.67  0.50        0.1
#> 28:       P8      1    90   -36  0.81 -0.59  0.00        0.1
#> 29:      POz      1    67   -90  0.00 -0.92  0.39        0.1
#> 30:       O1      1   -90    72 -0.31 -0.95  0.00        0.1
#> 31:       Oz      1    90   -90  0.00 -1.00  0.00        0.1
#> 32:       O2      1    90   -72  0.31 -0.95  0.00        0.1
#> 33:     EOGV     NA    NA    NA    NA    NA    NA        0.1
#> 34:     EOGH     NA    NA    NA    NA    NA    NA        0.1
#>     .channel radius theta   phi    .x    .y    .z resolution
#>       <char>  <num> <num> <num> <num> <num> <num>      <num>
```

``` r

events_tbl(new_data) %>%
  head()
#>      .id        .type .description     .initial       .final .channel
#>    <int>       <char>       <char> <sample_int> <sample_int>   <char>
#> 1:     1 Bad Interval  Bad Min-Max        15000        15128      FC6
#> 2:     1 Bad Interval  Bad Min-Max        15000        15146       F8
#> 3:     1 Bad Interval  Bad Min-Max        15000        15142      Fp1
#> 4:     1 Bad Interval  Bad Min-Max        15000        15132      Fp2
#> 5:     1 Bad Interval  Bad Min-Max        15000        15140      Fpz
#> 6:     1 Bad Interval  Bad Min-Max        15000        15114       F7
events_tbl(new_data) <- events_tbl(new_data) %>%
  filter(.description != "s130")
events_tbl(new_data) %>%
  head()
#> # A tidytable: 6 × 6
#>     .id .type        .description .initial   .final     .channel
#>   <int> <chr>        <chr>        <sampl_nt> <sampl_nt> <chr>   
#> 1     1 Bad Interval Bad Min-Max  15000      15128      FC6     
#> 2     1 Bad Interval Bad Min-Max  15000      15146      F8      
#> 3     1 Bad Interval Bad Min-Max  15000      15142      Fp1     
#> 4     1 Bad Interval Bad Min-Max  15000      15132      Fp2     
#> 5     1 Bad Interval Bad Min-Max  15000      15140      Fpz     
#> 6     1 Bad Interval Bad Min-Max  15000      15114      F7
```

### Losing the channel properties

Some functions keep the channel properties, e.g., `mean`, `scale`.

``` r

head(signal_tbl(data_faces_10_trials)$Oz)
#> # radius: 1; theta: 90; phi: -90; .x: 0; .y: -1; .z: 0 
#> # Values 
#> [1] -16.631485 -12.549113  -8.650514  -8.433714 -12.844979 -19.946123
head(mean(signal_tbl(data_faces_10_trials)$Oz))
#> # radius: 1; theta: 90; phi: -90; .x: 0; .y: -1; .z: 0 
#> # Values 
#> [1] -0.7590534
```

Some primitive functions remove the channel properties, e.g., `min`,
`var`.

``` r

head(min(signal_tbl(data_faces_10_trials)$Oz))
#> [1] -68.00181
head(var(signal_tbl(data_faces_10_trials)$Oz))
#> [1] 107.7272
```

A possible workaround is the following:

``` r

head(min(signal_tbl(data_faces_10_trials)$Oz)) + 0 * signal_tbl(data_faces_10_trials)$Oz[1]
#> # radius: 1; theta: 90; phi: -90; .x: 0; .y: -1; .z: 0 
#> # Values 
#> [1] -68.00181
```

## R crashes when plotting after using MNE

Functions that use MNE through {reticulate}, such as
[`read_fif()`](https://bruno.nicenboim.me/eeguana/reference/read_fif.md),
make Python load matplotlib, and matplotlib brings its own copies of the
font libraries HarfBuzz and FreeType into the R session. When Python
comes from a conda environment, such as the one
[`install_py_eeguana()`](https://bruno.nicenboim.me/eeguana/reference/install_py_eeguana.md)
can create, these copies can be newer than the ones installed in the
system. R’s cairo graphics devices then use them by mistake, and R
crashes the first time it draws text. This affects
[`png()`](https://rdrr.io/r/grDevices/png.html) on Linux,
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
to a PNG file when {ragg} is not installed, and the PNG figures of R
Markdown documents. The crash looks like this:

     *** caught segfault ***
    address 0x55d9a05b63b0, cause 'invalid permissions'

    Traceback:
     1: grid.Call(C_stringMetric, as.graphicsAnnot(x$label))

The data and eeguana play no part in it: any R session that imported
matplotlib before a cairo device draws text can crash in this way. The
graphics devices of the {ragg} package do not crash, and neither does
[`pdf()`](https://rdrr.io/r/grDevices/pdf.html). So after using MNE,
draw PNG files with {ragg}:

``` r

ragg::agg_png("plot.png")
plot(data_faces_ERPs)
dev.off()

# ggsave() uses ragg for PNG files whenever ragg is installed
ggplot2::ggsave("plot.png", plot(data_faces_ERPs))
```

In R Markdown or Quarto documents, set the device of the figures in the
setup chunk:

``` r

knitr::opts_chunk$set(dev = "ragg_png")
```

Shiny apps, such as
[`eeg_browse()`](https://bruno.nicenboim.me/eeguana/reference/eeg_browse.md),
use {ragg} by themselves when it is installed. Another way out is to use
MNE in a separate R session: read the file there, save the result with
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html), and plot it in a
session that never loaded Python.
