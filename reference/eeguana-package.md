# eeguana: Flexible Manipulation of EEG Data

Flexible manipulation of EEG data in R. It provides a data.table powered
framework for manipulating EEG data with dplyr-based functions (e.g.,
\`mutate\`, \`filter\`, \`summarize\`) extended to a new class, eeg_lst,
other EEG-specialized functions, and \`ggplot\` wrapper functions. The
new class is inspired by tidyverse principles but it's not really "tidy"
(due to space considerations), it's a list of (i) a wide data table
(signal_tbl) that contains the signal amplitudes at every sample point
of the EEG, (ii) an events data table with information about markers (or
triggers), blinks and other exported information, and (iii) a long table
with experimental information, such as participant number (.recording),
conditions, etc. eeguana can do only basic pre-processing for now.

## Options

- `eeguana.verbose`: `TRUE` by default; with `FALSE`, the functions do
  not show messages about what they do, such as the filters they apply.

- `eeguana.print_max_channels`: maximum number of channels shown when an
  `eeg_lst`, `psd_lst`, or `eeg_ica_lst` is printed; `Inf`, all of them,
  by default. See
  [`print.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/print.eeg_lst.md).

- `eeguana.negative_up`: whether
  [`eeg_browse()`](https://bruno.nicenboim.me/eeguana/reference/eeg_browse.md)
  draws negative amplitudes up; `FALSE`, positive up, by default.

Set them with [`options()`](https://rdrr.io/r/base/options.html), for
example `options(eeguana.print_max_channels = 8)`.

## See also

Useful links:

- <https://bruno.nicenboim.me/eeguana>

- <https://github.com/bnicenboim/eeguana>

- Report bugs at <https://github.com/bnicenboim/eeguana/issues>

## Author

**Maintainer**: Bruno Nicenboim <bruno.nicenboim@tilburguniversity.edu>

Authors:

- Bruno Nicenboim <bruno.nicenboim@tilburguniversity.edu>

Other contributors:

- Kate Stone <kate.stone@uni-potsdam.de> \[contributor\]

- Mark Fairbanks <mark.t.fairbanks@gmail.com> \[contributor\]
