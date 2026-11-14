# Read EEGlab set files (Matlab files) into R

Creates an eeg_lst object from Matlab exported files. The function reads
a .mat or .set file using `R.matlab`. If you do not already have
`R.matlab` installed in R, you will need to install it yourself. The
file should have the structure described in this [Data structure
article](https://sccn.ucsd.edu/wiki/A05:_Data_Structures). This function
is experimental (there are many different formats of eeglab files). If
your file cannot be opened please open an issue with a link to the file
in [github](https://github.com/bnicenboim/eeguana/issues).

## Usage

``` r
read_set(file, .recording = file)
```

## Arguments

- file:

  A .mat or .set file containing a fieldtrip struct.

- .recording:

  Recording name, by default is the file name.

## Value

An `eeg_lst` object with signal_tbl and event from a Matlab file.

## See also

Other reading and writing functions:
[`read_edf()`](https://bruno.nicenboim.me/eeguana/reference/read_edf.md),
[`read_fif()`](https://bruno.nicenboim.me/eeguana/reference/read_fif.md),
[`read_ft()`](https://bruno.nicenboim.me/eeguana/reference/read_ft.md),
[`read_vhdr()`](https://bruno.nicenboim.me/eeguana/reference/read_vhdr.md)

## Examples

``` r
if (FALSE) { # \dontrun{
s1 <- read_ft("./subject1.set", .recording = 1)
} # }
```
