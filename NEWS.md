# eeguana 0.0.5.9000
 - Changes
   - `read_edf()` was added.
   - 
 - Bugs
   - Bug in `channel_names()` was fixed.
   - Bug  #34 fixed.
 - Internal changes:
   - Better test structure.
   - Better validation of objects.  

# eeguana 0.0.4.9000
 - Changes
   - `chs_fun()` to apply arbitrary functions to the channels.
   - Fieldtrip tutorial added as a vignette.
 - Bugs
   - Channels appear in the right order in plots.
   - Other minor bugs.

# eeguana 0.0.3.9000
 - Changes
   - ggplot2 theme improved.
   - changed the default downsampling in plots.
   - better handling of weird channel names.
   - `segment()` accepts initial and final segments.
   - `summarize` adds a summarize_n column.
   - `events` function to visualize and edit events was added.
   - various filters were added.
 - Bugs
   - `ch_rereference` updates the relevanta attributes.
   - bugs in various reading functions were fixed (int encoding issues).
   - `mutate` recognize better channels.
   - `*_join` functions fixed.
   
# eeguana 0.0.2.9001

- Data table is now powered by data.table.


# eeguana 0.0.2.9000

* Added a `NEWS.md` file to track changes to the package.
