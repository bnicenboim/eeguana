# Read a BrainVision file into R

Creates an eeg_lst object from BrainVision exported files.The function
reads metadata from the .vhdr BrainVision file, which draws on the .vmrk
and .dat/.eeg files. All three files must be in the same directory.

## Usage

``` r
read_vhdr(
  file,
  .sep = .type == "New Segment",
  .zero = .type == "Time 0",
  .recording = file
)
```

## Arguments

- file:

  A vhdr file in a folder that contains a .vmrk and .dat files

- .sep:

  Segment separation marker. By default: `.type == "New Segment"`

- .zero:

  Time zero marker. By default: `.type == "Time 0"`

- .recording:

  Recording name (`file` name, by default).

## Value

An `eeg_lst` object with signal_tbl and event from file_name.dat,
file_name.vhdr, and file_name.vmrk.

## See also

Other reading and writing functions:
[`read_edf()`](https://bruno.nicenboim.me/eeguana/reference/read_edf.md),
[`read_fif()`](https://bruno.nicenboim.me/eeguana/reference/read_fif.md),
[`read_ft()`](https://bruno.nicenboim.me/eeguana/reference/read_ft.md),
[`read_set()`](https://bruno.nicenboim.me/eeguana/reference/read_set.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# load a single subject
s1 <- read_vhdr("./faces.vhdr", .recording = "1")

# load multiple subjects, extracting subject IDs from file names
faces_list <- lapply(list.files("./", "vhdr"), read_vhdr)
faces <- bind(faces_list)
} # }
```
