# Write an eeg_lst object to BrainVision file(s) (experimental)

The function will write each recording in a different file. The function
is highly experimental, if a file fails to open in BrainVision, please
report the bug in (https://github.com/bnicenboim/eeguana/issues).

## Usage

``` r
write_vhdr(x, file, overwrite = FALSE)
```

## Arguments

- x:

  `eeg_lst` object.

- file:

  Either a vector of file names (with the same length as the number of
  recordings), one file name (which will be appended the recording name
  if needed), or a path which will use the recording names as file
  names. By default, it will use the recording name in the current path
  as file names.

- overwrite:

  Stop writing if the file exists.
