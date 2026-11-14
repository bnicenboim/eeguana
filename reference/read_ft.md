# Read a Fieldtrip file into R

Creates an eeg_lst object from Matlab exported files.The function reads
a .mat file using `R.matlab`. If you do not already have `R.matlab`
installed in R, you will need to install it yourself. The .mat file
should have the structure described in this [Fieldtrip reference
article](http://www.fieldtriptoolbox.org/reference/ft_datatype_raw).

## Usage

``` r
read_ft(file, .layout = NULL, .recording = file)
```

## Arguments

- file:

  A .mat file containing a fieldtrip struct.

- .layout:

  A .mat [layout from
  Fieldtrip](http://www.fieldtriptoolbox.org/template/layout)

- .recording:

  Recording name, by default is the file name.

## Value

An `eeg_lst` object with signal_tbl and event from a Matlab file.

## See also

Other reading and writing functions:
[`read_edf()`](https://bruno.nicenboim.me/eeguana/reference/read_edf.md),
[`read_fif()`](https://bruno.nicenboim.me/eeguana/reference/read_fif.md),
[`read_set()`](https://bruno.nicenboim.me/eeguana/reference/read_set.md),
[`read_vhdr()`](https://bruno.nicenboim.me/eeguana/reference/read_vhdr.md)

## Examples

``` r
if (FALSE) { # \dontrun{
s1 <- read_ft("./subject1.mat", layout = "easycapM25.mat", .recording = 1)
} # }
```
