# Read a FIF file into R

Creates an eeg_lst object from EEG/MEG data in FIF format. The function
reads a .fif file using the `mne` Python package via `reticulate`. If
you do not already have `mne` installed in your Python environment,
ensure to install it prior to using this function using
[install_py_eeguana](https://bruno.nicenboim.me/eeguana/reference/install_py_eeguana.md).
The .fif file is a standard format used by MNE and other
neurophysiological data analysis software.

## Usage

``` r
read_fif(file, .recording = file)
```

## Arguments

- file:

  A .fif file containing EEG/MEG data.

- .recording:

  Recording name, by default is the file name. This parameter is
  currently not used in the function but reserved for future use or
  metadata purposes.

## Value

An `eeg_lst` object with signal_tbl and event information from a .fif
file.

## See also

Other reading and writing functions:
[`read_edf()`](https://bruno.nicenboim.me/eeguana/reference/read_edf.md),
[`read_ft()`](https://bruno.nicenboim.me/eeguana/reference/read_ft.md),
[`read_set()`](https://bruno.nicenboim.me/eeguana/reference/read_set.md),
[`read_vhdr()`](https://bruno.nicenboim.me/eeguana/reference/read_vhdr.md)

## Examples

``` r
if (FALSE) { # \dontrun{
eeg_data <- read_fiff("./subject1_raw.fif")
} # }
```
