# Dplyr-like functions for manipulating eeg_lst objects.

Manipulate the signal table and the segments table of an eeg_lst with
dplyr-like functions.

## Usage

``` r
eeg_mutate(.data, ...)

eeg_transmute(.data, ...)

eeg_filter(.data, ..., .preserve = FALSE)

eeg_summarize(.data, ..., .groups = "keep")

eeg_summarise(.data, ..., .groups = "keep")

eeg_group_by(.data, ..., .add = FALSE, .drop = FALSE)

eeg_ungroup(.data, ...)

eeg_select(.data, ...)

eeg_rename(.data, ...)

eeg_rename_with(.data, .fn, .cols = where(is_channel_dbl), ...)

eeg_groups(x)

# S3 method for class 'eeg_lst'
eeg_groups(x)

eeg_group_vars(x)

eeg_left_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = FALSE
)

eeg_semi_join(x, y, by = NULL, copy = FALSE, ...)

eeg_anti_join(x, y, by = NULL, copy = FALSE, ...)

eeg_pull(.data, var = -1, name = NULL, ...)

across(.cols = everything(), .fns = NULL, ..., .names = NULL)

across_ch(.fns = NULL, ..., .names = NULL)

c_across_ch()

c_across(.cols = everything())
```

## Arguments

- .data:

  An eeg_lst.

- ...:

  Name-value pairs of expressions; see
  [dplyr](https://dplyr.tidyverse.org/reference/dplyr-package.html) for
  more help.

- .preserve:

  Not in use, for compatibility reasons.

- .groups:

  Only .groups = "keep" is available. Same grouping structure as .data.

- .add:

  When FALSE, the default, group_by() will override existing groups. To
  add to the existing groups, use .add = TRUE.

- .drop:

  Only .drop = FALSE is available, empty groups are never dropped.

- .fn:

  Function to transform the names with.

- .cols:

  Columns to rename. Defaults to all columns. `tidyselect` compatible.

- x:

  An eeg_lst.

- y:

  A data frame, tibble, or data.table.

- by:

  A character vector of variables to join by. If NULL, the default, the
  join will do a natural join, using all variables with common names
  across the two tables.

- copy:

  Accepted so that the join verbs match dplyr's generics argument for
  argument. An `eeg_lst` and `y` are always in the same place, so there
  is nothing to copy: anything other than `FALSE` is ignored with a
  warning. (Not inherited from tidytable, which has no `copy` argument.)

- suffix:

  Append created for duplicated column names when using
  [`full_join()`](https://markfairbanks.github.io/tidytable/reference/left_join.html)

- keep:

  Should the join keys from both `x` and `y` be preserved in the output?

- var:

  A variable specified as:

  - a literal variable name

  - a positive integer, giving the position counting from the left

  - a negative integer, giving the position counting from the right.

  The default returns the last column (on the assumption that's the
  column you've created most recently).

  This argument is taken by expression and supports
  [quasiquotation](https://rlang.r-lib.org/reference/topic-inject.html)
  (you can unquote column names and column locations).

- name:

  An optional parameter that specifies the column to be used as names
  for a named vector. Specified in a similar manner as `var`.

- .fns:

  Function to apply. Can be a purrr-style lambda. Can pass also list of
  functions.

- .names:

  A glue specification that helps with renaming output columns. `{.col}`
  stands for the selected column, and `{.fn}` stands for the name of the
  function being applied. The default (`NULL`) is equivalent to
  `"{.col}"` for a single function case and `"{.col}_{.fn}"` when a list
  is used for `.fns`.

## Value

An eeg_lst object.

## Details

Wrappers for
[`dplyr`](https://dplyr.tidyverse.org/reference/dplyr-package.html)'s
commands that act `eeg_lst` objects. Functions that drop or rename
columns won't remove columns starting with a dot. These functions are
powered by
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
through
[`tidytable`](https://markfairbanks.github.io/tidytable/reference/tidytable.html).

The following wrappers act in a special way for `eeg_lst` objects:

- `eeg_*_join()`: joins an external table to the *segments* table of the
  eeg_lst.

- `eeg_mutate()` and `eeg_transmute()`: mutates the signal_tbl table
  when is applied to a channel (or a column of the signal table), and
  mutates the segments table otherwise. It can also mutates by
  reference.

- `eeg_summarize()` summarizes the channel of the signal_tbl table.

- `eeg_pull()` only pulls columns of the signal table

In addition, `across()`, and `c_across()` work as well. Notice that
there are convienent wrappers `across_ch()` and `c_across_ch()` where
the argument `.cols` is always set to `where(is_channel_dbl)`.

### Gotchas

These functions emulate
[dplyr](https://dplyr.tidyverse.org/reference/dplyr-package.html)
functionality but they are actually powered by the wrapper of
[data.table](https://rdrr.io/pkg/data.table/man/data.table.html),
[tidytable](https://markfairbanks.github.io/tidytable/reference/tidytable.html)
and some times they might be behave a bit differently than the dplyr
counterpart.

- The default values of the arguments might be different, and some
  arguments might not exist for the eeguana dplyr-like functions.

- Grouped mutations behave slightly different than ungrouped ones:
  Channel properties are removed if the data is ungrouped and one does
  `eeg_mutate(data, channel = 0)`, but not if the data is grouped.

- eeguana's eeg_mutate doesn't allow to refer back to a recently created
  channel: `data_eeg %>% eeg_mutate(X = F1 *2, Y = X)` is not valid. One
  needs to do `data_eeg %>% eeg_mutate(X = F1) %>% eeg_mutate(Y = X)`.

- eeguana's eeg_mutate doesn't use the most updated value of a column
  from the same call. If X is a channel, then
  `data_eeg %>% eeg_mutate(X = X *2, Y = X+1)` will add `1` to the
  original value of `X`, and not to the latest one.

- eeg_filter behaves similarly to dplyr's
  [`filter`](https://dplyr.tidyverse.org/reference/filter.html). If you
  want to filter the signal using IIR or FIR filters use
  [eeg_filt\*](https://bruno.nicenboim.me/eeguana/reference/filt.md)
  functions.

### Pitfalls

These functions not only edit the eeg_lst objects but they also do
book-keeping: They remove unnecessary channels, or update their
information and they ensure that three tables (signal, segments, and
events) match. It's then not recommended to edit the signal and segments
table directly. (The events and channels table can be edited directly by
doing `events_tbl(data_eeg) <- ...` or `channels_tbl(data_eeg) <- ...`).

## See also

Other tidyverse-like functions:
[`drop_incomplete_segments()`](https://bruno.nicenboim.me/eeguana/reference/drop_incomplete_segments.md),
[`eeg_bind()`](https://bruno.nicenboim.me/eeguana/reference/eeg_bind.md),
[`eeg_slice_signal()`](https://bruno.nicenboim.me/eeguana/reference/eeg_slice_signal.md)

## Examples

``` r
# Create new channel in the signal table
data_faces_ERPs %>%
  eeg_mutate(tmp = Fz - Cz)
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
#>                 F3            Fz            F4            F8           FC5
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:      1.716951     0.6631765  -0.092648050  6.219736e-04     1.6053903
#>   2:      1.696101     0.6964380   0.007091334 -7.299999e-03     1.5366046
#>   3:      1.536542     0.6526393   0.011369770 -1.129255e-01     1.3769061
#>   4:      1.271216     0.5556773  -0.029683895 -2.124353e-01     1.1235654
#>   5:      0.976661     0.4588411  -0.091315813 -3.011045e-01     0.8645434
#>  ---                                                                      
#> 448:    -12.706181   -13.2277295 -14.096793251 -1.035504e+01    -7.9050573
#> 449:    -12.858616   -13.3456251 -14.190243051 -1.030666e+01    -8.2451572
#> 450:    -12.906408   -13.3703569 -14.177192252 -1.022589e+01    -8.3741325
#> 451:    -12.838448   -13.3490926 -14.130527072 -1.018281e+01    -8.2735300
#> 452:    -12.767814   -13.3761194 -14.161811583 -1.025209e+01    -8.1323938
#>                FC1           FC2           FC6            M1            T7
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:      1.543430     0.6253292     0.3170046  -0.010442968     0.7618859
#>   2:      1.570444     0.6661446     0.4931471   0.003450777     0.7753494
#>   3:      1.544950     0.6605502     0.5423985   0.033199993     0.8647610
#>   4:      1.454882     0.6033796     0.4838804   0.060024264     0.9365834
#>   5:      1.338942     0.5266072     0.3697913   0.104232711     0.9420319
#>  ---                                                                      
#> 448:     -9.123321   -11.4783766   -10.0753595  -1.698556249    -2.2955267
#> 449:     -9.170633   -11.5308694   -10.0855337  -1.601363953    -2.4438153
#> 450:     -9.188665   -11.5363037   -10.0955933  -1.471049249    -2.5843704
#> 451:     -9.206411   -11.5181813   -10.1111284  -1.316673196    -2.6855790
#> 452:     -9.258206   -11.5542280   -10.1808181  -1.189507126    -2.7373231
#>                 C3            Cz            C4            T8            M2
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:      2.333914     0.9975520      1.934568     0.1472479   0.010442968
#>   2:      2.412899     1.0106905      1.954956     0.2699401  -0.003450777
#>   3:      2.504287     0.9923178      1.936489     0.3409814  -0.033199993
#>   4:      2.518237     0.9274194      1.874189     0.3652337  -0.060024264
#>   5:      2.451159     0.8381762      1.789200     0.3596960  -0.104232711
#>  ---                                                                      
#> 448:     -3.660581    -7.7390500     -7.136071    -4.0330479   1.698556249
#> 449:     -3.674479    -7.7419611     -7.290376    -3.9037055   1.601363953
#> 450:     -3.729771    -7.7226622     -7.399979    -3.9050442   1.471049249
#> 451:     -3.866381    -7.7275323     -7.497261    -4.1678197   1.316673196
#> 452:     -4.060195    -7.7703272     -7.634738    -4.5113966   1.189507126
#>                CP5           CP1           CP2           CP6            P7
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:      1.429704     1.3638380      1.468067    1.41452662      1.054939
#>   2:      1.494439     1.4090212      1.481492    1.45458898      1.116253
#>   3:      1.616789     1.4711696      1.517598    1.48051308      1.353048
#>   4:      1.688633     1.4957534      1.541262    1.43353669      1.573497
#>   5:      1.712986     1.4788877      1.536297    1.33977245      1.710138
#>  ---                                                                      
#> 448:      5.591801     0.6443219     -2.855460    0.58107099     13.163428
#> 449:      5.522963     0.6451574     -2.885057    0.35450182     12.958382
#> 450:      5.467754     0.6242272     -2.863814    0.24704496     12.739427
#> 451:      5.445742     0.5686631     -2.874607    0.19361613     12.660220
#> 452:      5.437697     0.4595132     -2.963804    0.06697914     12.745258
#>                 P3            Pz            P4            P8           POz
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:      1.277304      1.144086      1.648778     0.4936695     0.9985673
#>   2:      1.285327      1.152601      1.671552     0.5009267     1.0276117
#>   3:      1.404817      1.202807      1.724722     0.5416201     1.1316823
#>   4:      1.529925      1.253876      1.762264     0.5757036     1.2251255
#>   5:      1.627608      1.292309      1.768303     0.6045430     1.2946317
#>  ---                                                                      
#> 448:      9.132918      2.897569      6.218040    10.6124393     7.8505588
#> 449:      9.087724      2.920497      6.107610    10.5269463     7.7910463
#> 450:      9.036488      2.945566      6.015894    10.3746554     7.7270142
#> 451:      9.010008      2.945949      5.923691    10.1381542     7.6785265
#> 452:      8.991352      2.911724      5.805668     9.8648190     7.6702641
#>                 O1            Oz            O2          EOGV          EOGH
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:     0.8084641     0.6250410     0.9080361     1.3865825      2.361059
#>   2:     0.8976453     0.7248799     0.9601037     1.3008403      2.446400
#>   3:     1.1302819     0.9194691     1.0825952     1.3182415      2.405026
#>   4:     1.2880753     1.0329156     1.1729106     1.4509676      2.221037
#>   5:     1.3478098     1.0808014     1.2469059     1.5167961      1.952367
#>  ---                                                                      
#> 448:     8.3873355     7.4918637    12.4155999     0.8569236      6.530531
#> 449:     8.0510860     7.2997719    12.3021953     0.5408142      6.170865
#> 450:     7.7371189     7.0718979    12.0573028     0.1136463      5.719135
#> 451:     7.6067531     6.9113667    11.7688274    -0.2998507      5.299783
#> 452:     7.6972328     6.9127040    11.6048341    -0.4865012      5.040388
#>                tmp
#>      <channel_dbl>
#>   1:    -0.3343755
#>   2:    -0.3142525
#>   3:    -0.3396785
#>   4:    -0.3717421
#>   5:    -0.3793351
#>  ---              
#> 448:    -5.4886796
#> 449:    -5.6036640
#> 450:    -5.6476947
#> 451:    -5.6215603
#> 452:    -5.6057921
#> 
#> # Events table:
#> No events.
#> 
#> # Segments table:
#>      .id .recording condition
#>    <int>     <char>    <char>
#> 1:     1 faces.vhdr     faces
#> 2:     2 faces.vhdr non-faces

# Create a new condition in the segments table
data_faces_ERPs %>%
  eeg_mutate(code = ifelse(condition == "faces", 1, -1))
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
#>                 F3            Fz            F4            F8           FC5
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:      1.716951     0.6631765  -0.092648050  6.219736e-04     1.6053903
#>   2:      1.696101     0.6964380   0.007091334 -7.299999e-03     1.5366046
#>   3:      1.536542     0.6526393   0.011369770 -1.129255e-01     1.3769061
#>   4:      1.271216     0.5556773  -0.029683895 -2.124353e-01     1.1235654
#>   5:      0.976661     0.4588411  -0.091315813 -3.011045e-01     0.8645434
#>  ---                                                                      
#> 448:    -12.706181   -13.2277295 -14.096793251 -1.035504e+01    -7.9050573
#> 449:    -12.858616   -13.3456251 -14.190243051 -1.030666e+01    -8.2451572
#> 450:    -12.906408   -13.3703569 -14.177192252 -1.022589e+01    -8.3741325
#> 451:    -12.838448   -13.3490926 -14.130527072 -1.018281e+01    -8.2735300
#> 452:    -12.767814   -13.3761194 -14.161811583 -1.025209e+01    -8.1323938
#>                FC1           FC2           FC6            M1            T7
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:      1.543430     0.6253292     0.3170046  -0.010442968     0.7618859
#>   2:      1.570444     0.6661446     0.4931471   0.003450777     0.7753494
#>   3:      1.544950     0.6605502     0.5423985   0.033199993     0.8647610
#>   4:      1.454882     0.6033796     0.4838804   0.060024264     0.9365834
#>   5:      1.338942     0.5266072     0.3697913   0.104232711     0.9420319
#>  ---                                                                      
#> 448:     -9.123321   -11.4783766   -10.0753595  -1.698556249    -2.2955267
#> 449:     -9.170633   -11.5308694   -10.0855337  -1.601363953    -2.4438153
#> 450:     -9.188665   -11.5363037   -10.0955933  -1.471049249    -2.5843704
#> 451:     -9.206411   -11.5181813   -10.1111284  -1.316673196    -2.6855790
#> 452:     -9.258206   -11.5542280   -10.1808181  -1.189507126    -2.7373231
#>                 C3            Cz            C4            T8            M2
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:      2.333914     0.9975520      1.934568     0.1472479   0.010442968
#>   2:      2.412899     1.0106905      1.954956     0.2699401  -0.003450777
#>   3:      2.504287     0.9923178      1.936489     0.3409814  -0.033199993
#>   4:      2.518237     0.9274194      1.874189     0.3652337  -0.060024264
#>   5:      2.451159     0.8381762      1.789200     0.3596960  -0.104232711
#>  ---                                                                      
#> 448:     -3.660581    -7.7390500     -7.136071    -4.0330479   1.698556249
#> 449:     -3.674479    -7.7419611     -7.290376    -3.9037055   1.601363953
#> 450:     -3.729771    -7.7226622     -7.399979    -3.9050442   1.471049249
#> 451:     -3.866381    -7.7275323     -7.497261    -4.1678197   1.316673196
#> 452:     -4.060195    -7.7703272     -7.634738    -4.5113966   1.189507126
#>                CP5           CP1           CP2           CP6            P7
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:      1.429704     1.3638380      1.468067    1.41452662      1.054939
#>   2:      1.494439     1.4090212      1.481492    1.45458898      1.116253
#>   3:      1.616789     1.4711696      1.517598    1.48051308      1.353048
#>   4:      1.688633     1.4957534      1.541262    1.43353669      1.573497
#>   5:      1.712986     1.4788877      1.536297    1.33977245      1.710138
#>  ---                                                                      
#> 448:      5.591801     0.6443219     -2.855460    0.58107099     13.163428
#> 449:      5.522963     0.6451574     -2.885057    0.35450182     12.958382
#> 450:      5.467754     0.6242272     -2.863814    0.24704496     12.739427
#> 451:      5.445742     0.5686631     -2.874607    0.19361613     12.660220
#> 452:      5.437697     0.4595132     -2.963804    0.06697914     12.745258
#>                 P3            Pz            P4            P8           POz
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:      1.277304      1.144086      1.648778     0.4936695     0.9985673
#>   2:      1.285327      1.152601      1.671552     0.5009267     1.0276117
#>   3:      1.404817      1.202807      1.724722     0.5416201     1.1316823
#>   4:      1.529925      1.253876      1.762264     0.5757036     1.2251255
#>   5:      1.627608      1.292309      1.768303     0.6045430     1.2946317
#>  ---                                                                      
#> 448:      9.132918      2.897569      6.218040    10.6124393     7.8505588
#> 449:      9.087724      2.920497      6.107610    10.5269463     7.7910463
#> 450:      9.036488      2.945566      6.015894    10.3746554     7.7270142
#> 451:      9.010008      2.945949      5.923691    10.1381542     7.6785265
#> 452:      8.991352      2.911724      5.805668     9.8648190     7.6702641
#>                 O1            Oz            O2          EOGV          EOGH
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:     0.8084641     0.6250410     0.9080361     1.3865825      2.361059
#>   2:     0.8976453     0.7248799     0.9601037     1.3008403      2.446400
#>   3:     1.1302819     0.9194691     1.0825952     1.3182415      2.405026
#>   4:     1.2880753     1.0329156     1.1729106     1.4509676      2.221037
#>   5:     1.3478098     1.0808014     1.2469059     1.5167961      1.952367
#>  ---                                                                      
#> 448:     8.3873355     7.4918637    12.4155999     0.8569236      6.530531
#> 449:     8.0510860     7.2997719    12.3021953     0.5408142      6.170865
#> 450:     7.7371189     7.0718979    12.0573028     0.1136463      5.719135
#> 451:     7.6067531     6.9113667    11.7688274    -0.2998507      5.299783
#> 452:     7.6972328     6.9127040    11.6048341    -0.4865012      5.040388
#> 
#> # Events table:
#> No events.
#> 
#> # Segments table:
#>      .id .recording condition  code
#>    <int>     <char>    <char> <num>
#> 1:     1 faces.vhdr     faces     1
#> 2:     2 faces.vhdr non-faces    -1

# Create a new channel and drop all others
data_faces_ERPs %>%
  eeg_transmute(Occipital = chs_mean(O1, O2, Oz,
    na.rm = TRUE
  ))
#> # EEG data:
#> 
#> # Signal table:
#>        .id      .sample     Occipital
#>      <int> <sample_int> <channel_dbl>
#>   1:     1          -99     0.7805137
#>   2:     1          -98     0.8608763
#>   3:     1          -97     1.0441154
#>   4:     1          -96     1.1646338
#>   5:     1          -95     1.2251724
#>  ---                                 
#> 448:     2          122     9.4315997
#> 449:     2          123     9.2176844
#> 450:     2          124     8.9554399
#> 451:     2          125     8.7623157
#> 452:     2          126     8.7382569
#> 
#> # Events table:
#> No events.
#> 
#> # Segments table:
#>      .id .recording condition
#>    <int>     <char>    <char>
#> 1:     1 faces.vhdr     faces
#> 2:     2 faces.vhdr non-faces

# Extract data associated with a condition
data_faces_ERPs %>%
  eeg_filter(condition == "faces")
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
#> 222:     1          122   -2.94918210    -4.5101606   -5.06047275    -2.9873033
#> 223:     1          123   -3.27351815    -4.7767685   -5.31172654    -3.0590449
#> 224:     1          124   -3.60232770    -5.0461123   -5.61465058    -3.1751720
#> 225:     1          125   -3.72426044    -5.1661769   -5.73948259    -3.2380343
#> 226:     1          126   -3.52445504    -5.0158643   -5.59983333    -3.1634585
#>                 F3            Fz            F4            F8           FC5
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:      1.716951     0.6631765  -0.092648050  0.0006219736     1.6053903
#>   2:      1.696101     0.6964380   0.007091334 -0.0072999994     1.5366046
#>   3:      1.536542     0.6526393   0.011369770 -0.1129255121     1.3769061
#>   4:      1.271216     0.5556773  -0.029683895 -0.2124352841     1.1235654
#>   5:      0.976661     0.4588411  -0.091315813 -0.3011045087     0.8645434
#>  ---                                                                      
#> 222:     -5.465723    -5.9787921  -7.268414620 -5.0495993187    -3.3844155
#> 223:     -5.650225    -6.1201041  -7.364111335 -5.0456244674    -3.3672154
#> 224:     -5.871726    -6.2458631  -7.463538062 -5.1291662134    -3.5245886
#> 225:     -5.978774    -6.3001693  -7.480928252 -5.1644797151    -3.6780080
#> 226:     -5.942431    -6.2329611  -7.361764714 -5.0965870591    -3.6977668
#>                FC1           FC2           FC6            M1            T7
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:      1.543430     0.6253292     0.3170046  -0.010442968     0.7618859
#>   2:      1.570444     0.6661446     0.4931471   0.003450777     0.7753494
#>   3:      1.544950     0.6605502     0.5423985   0.033199993     0.8647610
#>   4:      1.454882     0.6033796     0.4838804   0.060024264     0.9365834
#>   5:      1.338942     0.5266072     0.3697913   0.104232711     0.9420319
#>  ---                                                                      
#> 222:     -4.549848    -5.3434164    -5.1618290  -1.652771628    -1.7384178
#> 223:     -4.751060    -5.4377672    -5.3257419  -1.583403006    -1.7830883
#> 224:     -4.966285    -5.5437213    -5.5280940  -1.521513044    -1.8792932
#> 225:     -5.102491    -5.5923294    -5.6791349  -1.444548047    -1.9536678
#> 226:     -5.121900    -5.5163632    -5.6552421  -1.359428020    -1.9701087
#>                 C3            Cz            C4            T8            M2
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:      2.333914     0.9975520      1.934568     0.1472479   0.010442968
#>   2:      2.412899     1.0106905      1.954956     0.2699401  -0.003450777
#>   3:      2.504287     0.9923178      1.936489     0.3409814  -0.033199993
#>   4:      2.518237     0.9274194      1.874189     0.3652337  -0.060024264
#>   5:      2.451159     0.8381762      1.789200     0.3596960  -0.104232711
#>  ---                                                                      
#> 222:     -1.721975    -3.0634028     -2.730119    -1.4827466   1.652771628
#> 223:     -1.967438    -3.2382530     -3.015366    -1.5070728   1.583403006
#> 224:     -2.246942    -3.4281573     -3.263800    -1.7046259   1.521513044
#> 225:     -2.484138    -3.5731374     -3.404891    -1.9110615   1.444548047
#> 226:     -2.628690    -3.6230233     -3.399993    -2.0251590   1.359428020
#>                CP5           CP1           CP2           CP6            P7
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:      1.429704     1.3638380     1.4680673     1.4145266      1.054939
#>   2:      1.494439     1.4090212     1.4814922     1.4545890      1.116253
#>   3:      1.616789     1.4711696     1.5175983     1.4805131      1.353048
#>   4:      1.688633     1.4957534     1.5412618     1.4335367      1.573497
#>   5:      1.712986     1.4788877     1.5362970     1.3397724      1.710138
#>  ---                                                                      
#> 222:      2.520838     1.6828339     0.8020536     2.3775401      5.017926
#> 223:      2.285606     1.3649900     0.4781533     1.7982843      4.849579
#> 224:      2.050171     1.0174482     0.1467149     1.3049149      4.750217
#> 225:      1.859121     0.7112496    -0.1275205     1.0217818      4.692425
#> 226:      1.722453     0.4969751    -0.2703340     0.9117485      4.607363
#>                 P3            Pz            P4            P8           POz
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:      1.277304      1.144086      1.648778     0.4936695     0.9985673
#>   2:      1.285327      1.152601      1.671552     0.5009267     1.0276117
#>   3:      1.404817      1.202807      1.724722     0.5416201     1.1316823
#>   4:      1.529925      1.253876      1.762264     0.5757036     1.2251255
#>   5:      1.627608      1.292309      1.768303     0.6045430     1.2946317
#>  ---                                                                      
#> 222:      6.429410      4.410422      7.335071     7.9385283     7.2843393
#> 223:      6.070601      4.052613      6.900739     7.5721019     6.8981251
#> 224:      5.714930      3.677770      6.475128     7.2035631     6.5300319
#> 225:      5.412841      3.352800      6.124055     6.8945005     6.2105246
#> 226:      5.189596      3.133808      5.883263     6.6380102     5.9589774
#>                 O1            Oz            O2          EOGV          EOGH
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:     0.8084641     0.6250410     0.9080361      1.386582      2.361059
#>   2:     0.8976453     0.7248799     0.9601037      1.300840      2.446400
#>   3:     1.1302819     0.9194691     1.0825952      1.318241      2.405026
#>   4:     1.2880753     1.0329156     1.1729106      1.450968      2.221037
#>   5:     1.3478098     1.0808014     1.2469059      1.516796      1.952367
#>  ---                                                                      
#> 222:     4.4533680     5.1915785     9.9765440      4.875008      8.624626
#> 223:     4.2380301     4.9148969     9.6052505      4.764172      8.413440
#> 224:     4.1816044     4.7546588     9.2860807      4.667508      8.234419
#> 225:     4.1942757     4.6067733     8.9578564      4.484040      8.085010
#> 226:     4.1527416     4.3954874     8.5663634      4.178376      7.937325
#> 
#> # Events table:
#> No events.
#> 
#> # Segments table:
#>      .id .recording condition
#>    <int>     <char>    <char>
#> 1:     1 faces.vhdr     faces

# Group and summarize
data_faces_ERPs %>%
  # Convert samples to times, filter between timepoints
  eeg_filter(between(
    as_time(.sample, .unit = "ms"),
    100, 200
  )) %>%
  # Find mean amplitude of Fz for each condition
  eeg_group_by(condition) %>%
  eeg_summarize(mean.amplitude = mean(Fz))
#> # EEG data:
#> # Grouped by: condition, 
#> 
#> # Signal table:
#>      .id      .sample mean.amplitude
#>    <int> <sample_int>  <channel_dbl>
#> 1:     1           NA      -1.123384
#> 2:     2           NA      -5.701296
#> 
#> # Events table:
#> No events.
#> 
#> # Segments table:
#>      .id .recording condition
#>    <int>     <char>    <char>
#> 1:     1       <NA>     faces
#> 2:     2       <NA> non-faces

# Mean of each channel
data_faces_ERPs %>%
  eeg_summarize(across_ch(mean))
#> # EEG data:
#> 
#> # Signal table:
#>      .id      .sample           Fp1           Fpz           Fp2            F7
#>    <int> <sample_int> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#> 1:     1           NA     -1.332362     -1.567974     -1.821664     -1.064499
#>               F3            Fz            F4            F8           FC5
#>    <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#> 1:     -1.752809     -1.842959     -1.998877     -1.507871     -1.111193
#>              FC1           FC2           FC6            M1            T7
#>    <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#> 1:     -1.444029     -1.650133     -1.524692    -0.2682345    -0.3089361
#>               C3            Cz            C4            T8            M2
#>    <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#> 1:    -0.7513462     -1.146025     -1.147417     -0.764417     0.2682345
#>              CP5           CP1           CP2           CP6            P7
#>    <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#> 1:     0.8355049     0.1305391    -0.2710436    0.01620356       2.14035
#>               P3            Pz            P4            P8           POz
#>    <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#> 1:      1.800169     0.8448844       1.22103      1.378821      1.695645
#>               O1            Oz            O2          EOGV          EOGH
#>    <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#> 1:      1.526005      1.274255       2.30676     0.6111195     0.9612286
#> 
#> # Events table:
#> No events.
#> 
#> # Segments table:
#>      .id .recording
#>    <int>     <char>
#> 1:     1       <NA>

# Select specific electrodes
data_faces_ERPs %>%
  eeg_select(O1, O2, P7, P8)
#> # EEG data:
#> 
#> # Signal table:
#>        .id      .sample            O1            O2            P7            P8
#>      <int> <sample_int> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:     1          -99     0.8084641     0.9080361      1.054939     0.4936695
#>   2:     1          -98     0.8976453     0.9601037      1.116253     0.5009267
#>   3:     1          -97     1.1302819     1.0825952      1.353048     0.5416201
#>   4:     1          -96     1.2880753     1.1729106      1.573497     0.5757036
#>   5:     1          -95     1.3478098     1.2469059      1.710138     0.6045430
#>  ---                                                                           
#> 448:     2          122     8.3873355    12.4155999     13.163428    10.6124393
#> 449:     2          123     8.0510860    12.3021953     12.958382    10.5269463
#> 450:     2          124     7.7371189    12.0573028     12.739427    10.3746554
#> 451:     2          125     7.6067531    11.7688274     12.660220    10.1381542
#> 452:     2          126     7.6972328    11.6048341     12.745258     9.8648190
#> 
#> # Events table:
#> No events.
#> 
#> # Segments table:
#>      .id .recording condition
#>    <int>     <char>    <char>
#> 1:     1 faces.vhdr     faces
#> 2:     2 faces.vhdr non-faces

# Rename a variable
data_faces_ERPs %>%
  eeg_rename(Predictor = condition)
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
#>                 F3            Fz            F4            F8           FC5
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:      1.716951     0.6631765  -0.092648050  6.219736e-04     1.6053903
#>   2:      1.696101     0.6964380   0.007091334 -7.299999e-03     1.5366046
#>   3:      1.536542     0.6526393   0.011369770 -1.129255e-01     1.3769061
#>   4:      1.271216     0.5556773  -0.029683895 -2.124353e-01     1.1235654
#>   5:      0.976661     0.4588411  -0.091315813 -3.011045e-01     0.8645434
#>  ---                                                                      
#> 448:    -12.706181   -13.2277295 -14.096793251 -1.035504e+01    -7.9050573
#> 449:    -12.858616   -13.3456251 -14.190243051 -1.030666e+01    -8.2451572
#> 450:    -12.906408   -13.3703569 -14.177192252 -1.022589e+01    -8.3741325
#> 451:    -12.838448   -13.3490926 -14.130527072 -1.018281e+01    -8.2735300
#> 452:    -12.767814   -13.3761194 -14.161811583 -1.025209e+01    -8.1323938
#>                FC1           FC2           FC6            M1            T7
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:      1.543430     0.6253292     0.3170046  -0.010442968     0.7618859
#>   2:      1.570444     0.6661446     0.4931471   0.003450777     0.7753494
#>   3:      1.544950     0.6605502     0.5423985   0.033199993     0.8647610
#>   4:      1.454882     0.6033796     0.4838804   0.060024264     0.9365834
#>   5:      1.338942     0.5266072     0.3697913   0.104232711     0.9420319
#>  ---                                                                      
#> 448:     -9.123321   -11.4783766   -10.0753595  -1.698556249    -2.2955267
#> 449:     -9.170633   -11.5308694   -10.0855337  -1.601363953    -2.4438153
#> 450:     -9.188665   -11.5363037   -10.0955933  -1.471049249    -2.5843704
#> 451:     -9.206411   -11.5181813   -10.1111284  -1.316673196    -2.6855790
#> 452:     -9.258206   -11.5542280   -10.1808181  -1.189507126    -2.7373231
#>                 C3            Cz            C4            T8            M2
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:      2.333914     0.9975520      1.934568     0.1472479   0.010442968
#>   2:      2.412899     1.0106905      1.954956     0.2699401  -0.003450777
#>   3:      2.504287     0.9923178      1.936489     0.3409814  -0.033199993
#>   4:      2.518237     0.9274194      1.874189     0.3652337  -0.060024264
#>   5:      2.451159     0.8381762      1.789200     0.3596960  -0.104232711
#>  ---                                                                      
#> 448:     -3.660581    -7.7390500     -7.136071    -4.0330479   1.698556249
#> 449:     -3.674479    -7.7419611     -7.290376    -3.9037055   1.601363953
#> 450:     -3.729771    -7.7226622     -7.399979    -3.9050442   1.471049249
#> 451:     -3.866381    -7.7275323     -7.497261    -4.1678197   1.316673196
#> 452:     -4.060195    -7.7703272     -7.634738    -4.5113966   1.189507126
#>                CP5           CP1           CP2           CP6            P7
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:      1.429704     1.3638380      1.468067    1.41452662      1.054939
#>   2:      1.494439     1.4090212      1.481492    1.45458898      1.116253
#>   3:      1.616789     1.4711696      1.517598    1.48051308      1.353048
#>   4:      1.688633     1.4957534      1.541262    1.43353669      1.573497
#>   5:      1.712986     1.4788877      1.536297    1.33977245      1.710138
#>  ---                                                                      
#> 448:      5.591801     0.6443219     -2.855460    0.58107099     13.163428
#> 449:      5.522963     0.6451574     -2.885057    0.35450182     12.958382
#> 450:      5.467754     0.6242272     -2.863814    0.24704496     12.739427
#> 451:      5.445742     0.5686631     -2.874607    0.19361613     12.660220
#> 452:      5.437697     0.4595132     -2.963804    0.06697914     12.745258
#>                 P3            Pz            P4            P8           POz
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:      1.277304      1.144086      1.648778     0.4936695     0.9985673
#>   2:      1.285327      1.152601      1.671552     0.5009267     1.0276117
#>   3:      1.404817      1.202807      1.724722     0.5416201     1.1316823
#>   4:      1.529925      1.253876      1.762264     0.5757036     1.2251255
#>   5:      1.627608      1.292309      1.768303     0.6045430     1.2946317
#>  ---                                                                      
#> 448:      9.132918      2.897569      6.218040    10.6124393     7.8505588
#> 449:      9.087724      2.920497      6.107610    10.5269463     7.7910463
#> 450:      9.036488      2.945566      6.015894    10.3746554     7.7270142
#> 451:      9.010008      2.945949      5.923691    10.1381542     7.6785265
#> 452:      8.991352      2.911724      5.805668     9.8648190     7.6702641
#>                 O1            Oz            O2          EOGV          EOGH
#>      <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>   1:     0.8084641     0.6250410     0.9080361     1.3865825      2.361059
#>   2:     0.8976453     0.7248799     0.9601037     1.3008403      2.446400
#>   3:     1.1302819     0.9194691     1.0825952     1.3182415      2.405026
#>   4:     1.2880753     1.0329156     1.1729106     1.4509676      2.221037
#>   5:     1.3478098     1.0808014     1.2469059     1.5167961      1.952367
#>  ---                                                                      
#> 448:     8.3873355     7.4918637    12.4155999     0.8569236      6.530531
#> 449:     8.0510860     7.2997719    12.3021953     0.5408142      6.170865
#> 450:     7.7371189     7.0718979    12.0573028     0.1136463      5.719135
#> 451:     7.6067531     6.9113667    11.7688274    -0.2998507      5.299783
#> 452:     7.6972328     6.9127040    11.6048341    -0.4865012      5.040388
#> 
#> # Events table:
#> No events.
#> 
#> # Segments table:
#>      .id .recording Predictor
#>    <int>     <char>    <char>
#> 1:     1 faces.vhdr     faces
#> 2:     2 faces.vhdr non-faces
```
