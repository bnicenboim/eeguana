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
#> # ICA finished in 9 iterations. With a tolerance of 0.797472. (Maximum expected tolerance: 1e-06.)
#> # ICA took 0.99 secs
#> # EEG data:
#> 
#> # Signal table:
#>          .id      .sample           Fp1           Fpz           Fp2
#>        <int> <sample_int> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:     1        15000     -35.71965     -5.012789   -8.04410145
#>     2:     1        15001     -31.64636    -13.124294   -6.99306197
#>     3:     1        15002     -28.37023    -18.086940   -4.06714074
#>     4:     1        15003     -28.14171    -18.650230   -1.70385874
#>     5:     1        15004     -30.11927    -17.068866    0.03567178
#>    ---                                                             
#> 43325:     1        58324     -18.61925    -13.603751   -9.69433395
#> 43326:     1        58325     -19.12426    -13.091771   -9.31125671
#> 43327:     1        58326     -18.44993    -13.643786  -10.00211744
#> 43328:     1        58327     -16.64525    -14.489154  -11.11063512
#> 43329:     1        58328     -15.20235    -14.940667  -11.64502389
#>                   F7            F3            Fz            F4            F8
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:   -7.24504604    -11.569131    -0.1418557      3.565765      3.828097
#>     2:    0.05215685    -11.062779    -3.6129867     -0.538378      6.110435
#>     3:    6.27133092     -8.046903    -6.5274862     -2.825325      7.186903
#>     4:    8.55963734     -6.606100    -7.5397843     -2.682806      6.845357
#>     5:    8.06124235     -5.764055    -6.8875206     -1.234897      7.361553
#>    ---                                                                      
#> 43325:    1.22844375     -9.309851    -7.1769387     -5.858255     -9.751597
#> 43326:    0.22763325     -8.371171    -6.4144471     -4.223686     -9.205534
#> 43327:   -0.20382753     -8.384009    -5.9050743     -3.346274     -8.183446
#> 43328:   -0.22899890     -8.902126    -6.0889928     -2.450916     -6.802766
#> 43329:   -0.11780562     -9.144773    -6.0983965     -1.960613     -5.562684
#>                  FC5           FC1           FC2           FC6            M1
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:     0.2990658   -8.71808381     0.4762644    17.9564132    -1.8668146
#>     2:     3.5319348   -8.41861435    -3.7928876     2.0140428    -3.2960243
#>     3:    -1.6774651   -6.57771009    -7.6410139    -7.1548619    -3.7450671
#>     4:    -7.2021701   -6.20887408    -9.1943589    -6.3163686    -3.7946916
#>     5:    -5.2207571   -6.49311430    -8.4744729    -0.8651967    -3.2719884
#>    ---                                                                      
#> 43325:    -4.4819759   -0.09752294    -2.5279863   -10.0233571     0.8050708
#> 43326:    -6.8071264    0.65939260    -1.2475666    -7.9644524     0.9832335
#> 43327:    -5.3546656    1.12278565    -0.7136213    -6.2111974     1.2051768
#> 43328:    -1.2922953   -0.26661344    -1.1929667    -4.9672448     1.4103985
#> 43329:     3.8928920   -1.78283893    -1.5680897    -4.5716957     1.1627738
#>                   T7            C3            Cz            C4            T8
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:    -16.292639   -10.0027772   -13.9297325     7.7438437   -23.7917136
#>     2:     -8.291252    -8.1070747   -12.8561080     3.9497875    -3.9686177
#>     3:     -4.036203    -2.5225169   -11.9260657     0.5316079    12.3835230
#>     4:     -8.454477     0.9263106   -13.6178362    -0.2840282    12.3480388
#>     5:    -13.946253     0.4127210   -15.6656908     0.3548741     5.5037129
#>    ---                                                                      
#> 43325:      1.325580     2.7921047    -2.2414371    -6.3759992    -4.2990812
#> 43326:      1.466991     3.8211471    -0.8327719    -4.7679625    -4.2367565
#> 43327:      1.830165     4.2353973     0.4503902    -3.3952150    -0.2099158
#> 43328:      1.981837     1.4079856    -0.3716865    -4.0455385     4.2301741
#> 43329:      1.912739    -2.1938640    -1.7986288    -4.4612906     6.2448660
#>                   M2           CP5           CP1           CP2           CP6
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:     1.8668146     -1.111336    -17.263973     -8.968033      7.233718
#>     2:     3.2960243     -1.261742    -15.514971     -8.200456     11.087867
#>     3:     3.7450671      1.272368    -12.643105     -6.955761     14.733833
#>     4:     3.7946916      5.549992    -12.188660     -7.516829     16.985411
#>     5:     3.2719884      8.142584    -13.453277     -9.065977     15.033022
#>    ---                                                                      
#> 43325:    -0.8050708      5.535908      3.438156     -6.116774    -13.131064
#> 43326:    -0.9832335      5.360409      4.362613     -4.568211    -10.375078
#> 43327:    -1.2051768      5.018117      5.043992     -3.179270     -7.597604
#> 43328:    -1.4103985      3.159852      3.330766     -4.163339     -6.995702
#> 43329:    -1.1627738      1.401790      1.181813     -5.526879     -6.789347
#>                   P7            P3            Pz            P4            P8
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:     0.7839023    -6.0690445   -11.9767173     -5.311166   -12.6111895
#>     2:     0.9012370    -6.5510108   -11.4187611     -3.524733    -6.0945343
#>     3:     2.4108772    -5.6306918   -10.5969702     -1.748035    -0.6537982
#>     4:     4.6083061    -4.5291354   -11.7939015     -1.877791    -0.3962791
#>     5:     5.6444449    -4.5071820   -13.7629222     -4.072803    -4.1940101
#>    ---                                                                      
#> 43325:     8.0379397     3.7973010    -1.8551301     -8.785783    -9.9879843
#> 43326:     7.2120159     3.6172828    -0.8436219     -7.117841    -8.4066205
#> 43327:     5.2503535     3.2959142     0.3371542     -4.969700    -5.3306012
#> 43328:     2.8726157     0.6435065    -0.8275180     -4.634504    -3.4261568
#> 43329:     1.4421245    -1.9901171    -2.5910960     -4.978634    -2.8702175
#>                  POz            O1            Oz            O2          EOGV
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1: -14.864523968   -12.6635216   -16.1606693   -15.7053839    -205.85693
#>     2: -12.650518676    -9.3465843   -11.6825888   -10.8590446    -214.15259
#>     3: -10.912674999    -6.4619137    -8.3525234    -7.1490064    -220.57965
#>     4: -11.757666706    -5.5865641    -8.8585521    -7.7358099    -218.24934
#>     5: -14.321204403    -7.9477036   -13.4842733   -12.4600198    -217.16670
#>    ---                                                                      
#> 43325:  -2.582931785     2.9425908    -3.6602644    -7.1045199      18.41424
#> 43326:  -1.556501472     2.6373321    -2.6699845    -5.4900511      19.22052
#> 43327:   0.003030794     1.8566870    -1.0021553    -2.4555997      19.74028
#> 43328:  -0.468156991     0.2725343    -0.4879543    -0.4949952      19.20309
#> 43329:  -1.676152348    -0.6253912    -0.6959924    -0.2702113      18.72752
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
