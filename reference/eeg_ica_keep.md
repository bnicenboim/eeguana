# Select independent components (or sources) to keep.

This function will transform the channels according to the independent
components that are kept or removed.

## Usage

``` r
eeg_ica_keep(.data, ...)
```

## Arguments

- .data:

  An eeg_ica_lst object

- ...:

  Components to keep from the mixing matrix of the ICA transformation.
  See [dplyr::select](https://dplyr.tidyverse.org/reference/select.html)
  and
  [tidyselect::select_helpers](https://tidyselect.r-lib.org/reference/language.html)
  for details.

## See also

Other preprocessing functions:
[`eeg_baseline()`](https://bruno.nicenboim.me/eeguana/reference/eeg_baseline.md),
[`eeg_downsample()`](https://bruno.nicenboim.me/eeguana/reference/eeg_downsample.md),
[`eeg_ica()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica.md),
[`eeg_rereference()`](https://bruno.nicenboim.me/eeguana/reference/eeg_rereference.md),
[`eeg_segment()`](https://bruno.nicenboim.me/eeguana/reference/eeg_segment.md),
[`filt`](https://bruno.nicenboim.me/eeguana/reference/filt.md)

Other ICA functions:
[`eeg_browse()`](https://bruno.nicenboim.me/eeguana/reference/eeg_browse.md),
[`eeg_ica()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica.md),
[`eeg_ica_cor_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_cor_tbl.md),
[`eeg_ica_show()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_show.md),
[`eeg_ica_summary_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_summary_tbl.md),
[`eeg_ica_var_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_var_tbl.md),
[`plot_components()`](https://bruno.nicenboim.me/eeguana/reference/plot_components.md)

## Examples

``` r
# For demonstration only, since ICA won't converge
# Suppressing an important warning:
suppressWarnings(data_faces_10_trials %>%
  eeg_ica(-EOGH, -EOGV, -M1, -M2, .method = fast_ICA, .config = list(maxit = 10))) %>%
  eeg_ica_keep(-ICA1)
#> # ICA is being done using fast_ICA...
#> # ICA finished in 9 iterations. With a tolerance of 0.867572. (Maximum expected tolerance: 1e-06.)
#> # ICA took 0.95 secs
#> # EEG data:
#> 
#> # Signal table:
#>          .id      .sample           Fp1           Fpz           Fp2
#>        <int> <sample_int> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:     1        15000     -32.03772     -3.968597    -8.4987159
#>     2:     1        15001     -28.84587    -12.309618    -7.2442328
#>     3:     1        15002     -26.71349    -17.591189    -4.2747936
#>     4:     1        15003     -25.34805    -17.921685    -2.2218363
#>     5:     1        15004     -27.54505    -16.396015    -0.5024263
#>    ---                                                             
#> 43325:     1        58324     -19.24315    -13.735220    -9.3235750
#> 43326:     1        58325     -19.71708    -13.220225    -8.9909619
#> 43327:     1        58326     -19.05947    -13.772162    -9.6486507
#> 43328:     1        58327     -16.40457    -14.425095   -10.9738242
#> 43329:     1        58328     -14.71372    -14.823163   -11.5948577
#>                   F7            F3            Fz            F4            F8
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:   -5.67241940    -10.235658    -0.5476082      2.297534      1.688113
#>     2:    1.25205105    -10.045397    -3.9558972     -1.272019      4.593996
#>     3:    7.10311805     -7.430092    -6.5503562     -3.657950      6.139995
#>     4:    9.66161051     -5.572013    -7.8810599     -3.911785      5.095742
#>     5:    9.15650680     -4.818078    -7.0906426     -2.664365      5.631590
#>    ---                                                                      
#> 43325:    0.91127383     -9.572429    -7.3285647     -4.739345     -8.992822
#> 43326:   -0.07047391     -8.612993    -6.5320113     -3.258664     -8.531656
#> 43327:   -0.53121101     -8.625279    -6.0509106     -2.266851     -7.456936
#> 43328:   -0.29827692     -8.828017    -6.4062380     -1.751924     -6.626480
#> 43329:   -0.09640530     -8.980398    -6.4402553     -1.461449     -5.580530
#>                  FC5           FC1           FC2           FC6            M1
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:    -0.4189218   -9.05866016    -0.3519570     17.463311    -1.8668146
#>     2:     2.8091577   -8.87677057    -4.4643713      1.705770    -3.2960243
#>     3:    -1.4505617   -6.34174251    -7.8766192     -7.508212    -3.7450671
#>     4:    -7.7473456   -6.32177387    -9.8253183     -6.715195    -3.7946916
#>     5:    -5.2871140   -6.25663209    -8.9563856     -1.357393    -3.2719884
#>    ---                                                                      
#> 43325:    -5.2786577   -0.95049375    -2.6240959     -9.662344     0.8050708
#> 43326:    -7.4638600   -0.04769782    -1.3103302     -7.651502     0.9832335
#> 43327:    -6.1431128    0.30845023    -0.7994857     -5.853965     1.2051768
#> 43328:    -2.4415661   -1.19793772    -1.5183481     -4.692899     1.4103985
#> 43329:     2.7465307   -2.65622034    -1.9388766     -4.352476     1.1627738
#>                   T7            C3            Cz            C4            T8
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:   -15.8864827  -10.15275945   -15.1402010    7.77570030   -24.1176873
#>     2:    -8.0920275   -8.53098406   -13.9671274    3.70523889    -4.3880270
#>     3:    -3.6556686   -2.01651492   -12.0786491    0.94324401    12.4527352
#>     4:    -8.0850624    1.05365030   -14.4277733   -0.01479426    12.2945134
#>     5:   -13.4258403    1.04981723   -16.0723338    0.99563092     5.6898068
#>    ---                                                                      
#> 43325:     0.7787574    1.43639520    -2.9121664   -7.51135135    -4.9361314
#> 43326:     1.0050295    2.68495564    -1.3704375   -5.72118742    -4.7701151
#> 43327:     1.3114568    2.92717405    -0.1892081   -4.48192120    -0.8269399
#> 43328:     1.5299875    0.02638378    -1.3529570   -5.09426943     3.5952754
#> 43329:     1.5258261   -3.45342568    -2.7893651   -5.38603053     5.6778191
#>                   M2           CP5           CP1           CP2           CP6
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:     1.8668146    -1.2750393  -18.03852665     -9.755420      6.531375
#>     2:     3.2960243    -1.5963678  -16.40718104     -9.070656     10.389781
#>     3:     3.7450671     1.6362816  -12.45939019     -6.826473     14.683587
#>     4:     3.7946916     5.5466660  -12.52746982     -7.906036     16.589731
#>     5:     3.2719884     8.5288000  -13.27742308     -8.979031     14.924210
#>    ---                                                                      
#> 43325:    -0.8050708     4.6098791    2.22960706     -7.182287    -13.722584
#> 43326:    -0.9832335     4.5833106    3.36320261     -5.447730    -10.861379
#> 43327:    -1.2051768     4.1134998    3.88762889     -4.201434     -8.168015
#> 43328:    -1.4103985     2.1465410    1.97856443     -5.393110     -7.723902
#> 43329:    -1.1627738     0.4618584   -0.09151555     -6.693387     -7.488729
#>                   P7            P3            Pz            P4            P8
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:      1.640986     -6.805580   -13.5378976     -6.054900  -11.70590146
#>     2:      1.435525     -7.470898   -12.8896732     -4.301183   -5.62058012
#>     3:      3.000825     -5.283409   -10.7435598     -1.720283   -0.02033918
#>     4:      5.324658     -4.840805   -12.8003923     -2.292923    0.52752361
#>     5:      6.503271     -4.175095   -14.1933142     -4.111782   -3.08277210
#>    ---                                                                      
#> 43325:      7.401154      2.327765    -2.8667347     -9.569857  -10.98153869
#> 43326:      6.661535      2.396275    -1.6624056     -7.762487   -9.25577954
#> 43327:      4.630602      1.874207    -0.6347641     -5.724707   -6.27579332
#> 43328:      2.425310     -1.017862    -2.2322802     -5.583519   -4.07753789
#> 43329:      1.097599     -3.552791    -3.9882947     -5.889113   -3.35824792
#>                  POz            O1            Oz            O2          EOGV
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:   -16.0706958   -12.3645381    -16.081327   -14.9419315    -205.85693
#>     2:   -13.8486903    -9.3055018    -11.815381   -10.4668720    -214.15259
#>     3:   -10.9266705    -6.0120154     -8.005133    -6.5712175    -220.57965
#>     4:   -12.4986275    -5.2323339     -8.664577    -6.9822143    -218.24934
#>     5:   -14.5207400    -7.3194491    -13.001587   -11.5130483    -217.16670
#>    ---                                                                      
#> 43325:    -3.6178922     2.1014534     -4.480767    -7.9795951      18.41424
#> 43326:    -2.4015952     1.9271254     -3.357856    -6.2349723      19.22052
#> 43327:    -0.9933962     1.0459369     -1.790383    -3.2892020      19.74028
#> 43328:    -1.8074780    -0.4919869     -1.274445    -1.1153453      19.20309
#> 43329:    -2.9853486    -1.2950767     -1.400122    -0.7582439      18.72752
#>                 EOGH
#>        <channel_dbl>
#>     1:    164.320831
#>     2:    158.389755
#>     3:    157.559784
#>     4:    162.165161
#>     5:    168.559723
#>    ---              
#> 43325:      6.434383
#> 43326:      7.837689
#> 43327:      8.659687
#> 43328:      8.609533
#> 43329:      8.829977
#> 
#> ## Events table:
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
#> 
#> # ICA:
#> # Component_names:ICA2, ICA3, ICA4, ICA5, ICA6, ICA7, ICA8, ICA9, ICA10, ICA11, ICA12, ICA13, ICA14, ICA15, ICA16, ICA17, ICA18, ICA19, ICA20, ICA21, ICA22, ICA23, ICA24, ICA25, ICA26, ICA27, ICA28, ICA29, ICA30
#> # Channels_used: Fp1, Fpz, Fp2, F7, F3, Fz, F4, F8, FC5, FC1, FC2, FC6, T7, C3, Cz, C4, T8, CP5, CP1, CP2, CP6, P7, P3, Pz, P4, P8, POz, O1, Oz, O2
#> 
#> # Segments table:
#>      .id .recording segment
#>    <int>     <char>   <int>
#> 1:     1 faces.vhdr       1
```
