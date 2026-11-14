# Read an edf/edf+/bdf file into R

Creates an eeg_lst object from edf, edf+, and bdf file export formats.

## Usage

``` r
read_edf(file, .recording = file, .trigger_channel = NULL)
```

## Arguments

- file:

  A edf/bdf file

- .recording:

  Recording name (file name, by default). If set to NULL or NA, the
  patient name will be used.

- .trigger_channel:

  Name of a channel carrying triggers, or `"last"` for the last channel
  that is not an annotation or status channel. `NULL`, the default,
  leaves it alone.

  Some recording systems write triggers as an ordinary analog channel
  with a label of their own, rather than as EDF+ annotations, and
  nothing in the header marks it as such. When `.trigger_channel` is
  given, that channel is turned into events and removed from the signal
  table: one event at the first sample and one wherever the value
  changes, with `.type` `"Trigger"` and `.description` holding the
  **digital code**. EDF stores a trigger as an integer and scales it
  into the channel's physical unit, so code 3072 may arrive as 767.4961
  uV; the code is what identifies the trigger.

  Reading the same file in EEGLAB gives the same latencies but different
  values. `pop_biosig()` falls back to the last channel whenever the
  annotations are empty, reports the scaled physical value rather than
  the code, and passes it through `mod(x, 65536)`, so a baseline of
  -0.375 appears as 65535.625.

  A warning is issued when more than half the channel's samples hold
  distinct values, which is what continuous data looks like: real EEG
  has almost as many distinct values as samples, while a trigger channel
  repeats a handful of codes however many it uses. The channel is still
  read. The check does not separate triggers from a flat or disconnected
  electrode, because nothing in the file does, so name the channel
  deliberately rather than relying on it.

## Value

An `eeg_lst` object.

## Details

When trigger information is stored in a "Status" or "Trigger" channel,
the trigger value is stored only when the value of the channel
increases. This follows the default behavior of [find_events in MNE
0.18](https://mne.tools/0.18/generated/mne.find_events.html?highlight=find_events#mne.find_events).
If you have a case where this assumption is incorrect, please open an
issue in <https://github.com/bnicenboim/eeguana/issues>.

## See also

Other reading and writing functions:
[`read_fif()`](https://bruno.nicenboim.me/eeguana/reference/read_fif.md),
[`read_ft()`](https://bruno.nicenboim.me/eeguana/reference/read_ft.md),
[`read_set()`](https://bruno.nicenboim.me/eeguana/reference/read_set.md),
[`read_vhdr()`](https://bruno.nicenboim.me/eeguana/reference/read_vhdr.md)

## Examples

``` r
if (FALSE) { # \dontrun{
s1 <- read_edf("./faces.edf", .recording = 1)
} # }
```
