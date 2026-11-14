# Segment (and unsegment) an eeg_lst.

- `eeg_segment()` subdivides of the EEG into different segments or
  epochs. (Fieldtrip calls the segment "trials".) The limits of
  `segment` are inclusive: If, for example, `lim = c(0,0)`, the segment
  would contain only sample 1.

## Usage

``` r
eeg_segment(.data, ..., .lim = c(-0.5, 0.5), .end, .unit = "s")

eeg_unsegment(
  .data,
  .start = 1,
  .sep = c(.type = "New Segment", .description = ""),
  .zero = c(.type = "Time 0", .description = "")
)
```

## Arguments

- .data:

  An `eeg_lst` object.

- ...:

  Description of the event.

- .lim:

  Vector indicating the time before and after the event. Or dataframe
  with two columns, with nrow=total number of segments

- .end:

  Description of the event that indicates the end of the segment, if
  this is used, `.lim` is ignored.

- .unit:

  "seconds" (or "s"), "milliseconds" (or "ms"), or samples.

- .start:

  Initial sample when an object is unsegmented

- .sep:

  Segment separation marker. By default: `.type == "New Segment"`

- .zero:

  Time zero marker. By default: `.type == "Time 0"`

## Value

An `eeg_lst`.

## Details

- `eeg_unsegment()` does **not** reverse the segmentation, it simply
  concatenates all segments creating one object with only one segment.

When there is no segmentation, the `eeg_lst` contain one segment.

## See also

Other preprocessing functions:
[`eeg_baseline()`](https://bruno.nicenboim.me/eeguana/reference/eeg_baseline.md),
[`eeg_downsample()`](https://bruno.nicenboim.me/eeguana/reference/eeg_downsample.md),
[`eeg_ica()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica.md),
[`eeg_ica_keep()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_keep.md),
[`eeg_rereference()`](https://bruno.nicenboim.me/eeguana/reference/eeg_rereference.md),
[`filt`](https://bruno.nicenboim.me/eeguana/reference/filt.md)

## Examples

``` r

# Segments 500ms before and 1000ms after the triggers s70 and s71
data_faces_10_trials %>% eeg_segment(.description %in% c("s70", "s71"),
  .lim = c(-5, 1)
)
#> # Total of 9 segments found.
#> # Object size in memory 7.3 Mb after segmentation.
#> # EEG data:
#> 
#> # Signal table:
#>          .id      .sample           Fp1           Fpz           Fp2
#>        <int> <sample_int> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:     1        -2499    -21.109169    -28.948433   -25.2941818
#>     2:     1        -2498    -26.754496    -31.574997   -29.3033142
#>     3:     1        -2497    -28.524906    -30.061056   -29.8119774
#>     4:     1        -2496    -19.430933    -21.690756   -22.9545746
#>     5:     1        -2495     -5.725405    -12.675667   -14.1778440
#>    ---                                                             
#> 27005:     9          497     -8.819434    -10.321518    -4.1491461
#> 27006:     9          498     -6.959885    -10.197778    -3.9415770
#> 27007:     9          499     -1.756994     -7.125042    -1.8327932
#> 27008:     9          500      3.828817     -3.076797     0.1685476
#> 27009:     9          501      6.447233     -1.101932     0.8303175
#>                   F7            F3            Fz            F4            F8
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:  -10.91393471    -7.5658569    -8.7408314     -8.170362    -6.2060385
#>     2:  -15.56297112    -8.5204601    -8.6705694     -7.897309    -6.2139606
#>     3:  -18.79760742   -10.9199018    -8.0307159     -6.302173    -7.0704432
#>     4:  -14.15851784   -11.1185970    -5.9641218     -3.515551    -9.4960175
#>     5:   -2.97854924    -7.5502534    -3.7647977     -1.728970   -11.8486443
#>    ---                                                                      
#> 27005:   -0.04505777    -3.4396243    -0.3492746      1.448371    -0.9764605
#> 27006:   -0.39633083    -5.2603526    -0.5973411      2.423673    -1.4587488
#> 27007:    1.76945877    -4.0067720     0.8627796      3.911339    -2.3904738
#> 27008:    5.14945936    -1.2876422     2.6780972      4.573048    -3.9883673
#> 27009:    7.85265970     0.7570639     3.2915244      3.903461    -5.8100514
#>                  FC5           FC1           FC2           FC6            M1
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:   -10.3604956     -8.741921    -11.687920    -3.2395220   -1.33440495
#>     2:    -8.3586874     -9.393848    -11.007945    -2.0159216   -0.40914702
#>     3:    -6.6766038    -10.921223     -9.508024     0.6382501   -0.02219653
#>     4:    -5.3997726    -11.370429     -8.181183     2.3170936   -0.15380359
#>     5:    -1.5244772    -10.140000     -8.618706     0.1669424   -0.42611933
#>    ---                                                                      
#> 27005:    -3.1192555      7.583683      4.683777     2.0146487   -0.75850582
#> 27006:    -5.0121384      7.003987      5.093409     2.6218574   -0.48889470
#> 27007:    -3.8819036      8.010637      6.655658     2.4396117    0.37114978
#> 27008:     0.2056897      9.351544      7.915032     1.1819088    0.79920685
#> 27009:     3.5806384      9.694896      7.764079    -0.7410481    0.47170472
#>                   T7            C3            Cz            C4            T8
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:    -10.026085     -9.994508     -9.713416    -11.545010     -5.506052
#>     2:    -10.864231    -11.186291    -10.363641    -10.096234    -13.692937
#>     3:    -10.305115    -14.071175    -11.175298     -7.532410    -18.642357
#>     4:     -6.952291    -15.679218    -11.470225     -5.610895    -15.585937
#>     5:     -1.514339    -14.149639    -11.411907     -7.089185     -5.756573
#>    ---                                                                      
#> 27005:     -2.933141      1.959429      5.739326      3.235288      7.357582
#> 27006:     -3.713117      1.318202      5.491092      3.290012      7.295595
#> 27007:     -2.321592      1.811074      6.128494      3.688938      5.087297
#> 27008:      1.119921      2.753958      6.793680      3.092779      3.427744
#> 27009:      3.754653      3.358357      6.357634      1.617870      3.171353
#>                   M2           CP5           CP1           CP2           CP6
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:    1.33440495    -10.124557    -21.182615     -8.225979      1.352515
#>     2:    0.40914702    -10.541845    -22.123146     -8.299847      2.918134
#>     3:    0.02219653    -10.097946    -22.904871     -7.499836      6.240794
#>     4:    0.15380359     -7.715628    -22.575096     -6.102085      8.873845
#>     5:    0.42611933     -3.972811    -21.122660     -5.780276      7.216280
#>    ---                                                                      
#> 27005:    0.75850582      2.440120      9.009402      2.962071      2.134945
#> 27006:    0.48889470      2.300604      8.520119      3.144351      2.515271
#> 27007:   -0.37114978      2.277759      8.711225      3.692545      2.287699
#> 27008:   -0.79920685      2.035310      8.742649      3.526225     -0.904506
#> 27009:   -0.47170472      2.071527      8.072234      2.323087     -4.643136
#>                   P7            P3            Pz            P4            P8
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:    -12.080380    -10.113933     -8.703011   -4.20130539      3.195775
#>     2:    -11.193981    -10.194810     -9.080425   -4.14209843      2.318590
#>     3:     -7.487033     -9.285007     -8.754600   -2.61196470      3.541405
#>     4:     -1.335482     -7.199755     -7.912488   -0.50822496      6.676203
#>     5:      4.918743     -4.848968     -7.416552   -0.01726842      8.177627
#>    ---                                                                      
#> 27005:     -5.362492      3.755604      7.436291    0.44716191     -3.323942
#> 27006:     -5.735914      3.123381      7.172704    0.63476968     -2.597961
#> 27007:     -5.450908      3.175655      7.514575    0.96244740     -2.774142
#> 27008:     -4.410063      3.246389      7.394914    0.24067569     -3.957345
#> 27009:     -2.809917      2.920698      6.190672   -1.19312561     -4.774019
#>                  POz            O1            Oz            O2          EOGV
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:     -8.369589    -8.4397917    -3.8788650    -0.1158402    -19.684998
#>     2:     -9.067577    -8.5709276    -4.3982387    -0.5201874    -16.109802
#>     3:     -7.452994    -5.2471170    -1.8378611     1.8578585    -11.800674
#>     4:     -4.108239     1.1644255     3.2520020     6.4933400     -7.483185
#>     5:     -1.417105     7.4992466     8.0737114    10.8989525     -6.017853
#>    ---                                                                      
#> 27005:      5.305985    -2.0515025    -0.6655829    -1.8001112     -6.660660
#> 27006:      5.463353    -1.7843865    -0.1968485    -1.1158936     -5.629734
#> 27007:      5.724925    -0.8298787     0.3083435    -0.6791936     -5.186114
#> 27008:      5.367867     0.6219659     0.7365036    -0.5318193     -4.111660
#> 27009:      4.392406     2.3162208     1.4108977     0.1259224     -2.589244
#>                 EOGH
#>        <channel_dbl>
#>     1:    -54.449097
#>     2:    -68.097107
#>     3:    -83.815216
#>     4:   -100.941864
#>     5:   -118.664459
#>    ---              
#> 27005:      6.173569
#> 27006:      4.975996
#> 27007:      3.522355
#> 27008:      2.911364
#> 27009:      3.616112
#> 
#> # Events table:
#>        .id        .type .description     .initial       .final .channel
#>      <int>       <char>       <char> <sample_int> <sample_int>   <char>
#>   1:     1 Bad Interval  Bad Min-Max        -2499        -1905      Fp1
#>   2:     1 Bad Interval  Bad Min-Max        -2499        -1906      Fpz
#>   3:     1 Bad Interval  Bad Min-Max        -2499        -1905      Fp2
#>   4:     1 Bad Interval  Bad Min-Max        -2499        -1980       F7
#>   5:     1 Bad Interval  Bad Min-Max        -2499        -1939       Fz
#>  ---                                                                   
#> 150:     9 Bad Interval  Bad Min-Max        -1691        -1349      Fpz
#> 151:     8 Bad Interval  Bad Min-Max          277          501       F8
#> 152:     9 Bad Interval  Bad Min-Max        -1672        -1435       F8
#> 153:     9     Stimulus         s130         -369         -369     <NA>
#> 154:     9     Stimulus          s70            1            1     <NA>
#> 
#> # Segments table:
#>      .id .recording segment     type description
#>    <int>     <char>   <int>   <char>      <char>
#> 1:     1 faces.vhdr       1 Stimulus         s70
#> 2:     2 faces.vhdr       2 Stimulus         s71
#> 3:     3 faces.vhdr       3 Stimulus         s71
#> 4:     4 faces.vhdr       4 Stimulus         s71
#> 5:     5 faces.vhdr       5 Stimulus         s70
#> 6:     6 faces.vhdr       6 Stimulus         s71
#> 7:     7 faces.vhdr       7 Stimulus         s71
#> 8:     8 faces.vhdr       8 Stimulus         s70
#> 9:     9 faces.vhdr       9 Stimulus         s70

# Segments 500ms before and  after the triggers all the triggers (which start with s)
data_faces_10_trials %>% eeg_segment(startsWith(.description, "s"))
#> # Total of 22 segments found.
#> # Object size in memory 2.9 Mb after segmentation.
#> # EEG data:
#> 
#> # Signal table:
#>          .id      .sample           Fp1           Fpz           Fp2
#>        <int> <sample_int> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:     1         -249      78.62610      41.76394     34.068588
#>     2:     1         -248      78.20166      41.62954     33.610832
#>     3:     1         -247      77.59616      41.61887     33.336830
#>     4:     1         -246      77.26508      41.62726     33.294086
#>     5:     1         -245      77.91381      41.76098     33.804604
#>    ---                                                             
#> 10768:    22           -3     -20.42674     -14.87641    -10.669024
#> 10769:    22           -2     -19.79967     -13.65431     -9.698612
#> 10770:    22           -1     -18.61094     -13.91012    -10.128812
#> 10771:    22            0     -16.96312     -14.78610    -11.301287
#> 10772:    22            1     -15.31518     -15.08285    -11.722956
#>                   F7            F3            Fz            F4            F8
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:    61.0350227     34.753330     23.639185     12.744570    -13.112861
#>     2:    61.1616707     34.804611     23.652182     12.445484    -14.556956
#>     3:    61.4098625     35.196716     24.083496     12.643650    -15.556440
#>     4:    61.8770332     36.010979     24.737810     13.239056    -15.982599
#>     5:    62.2362175     37.131126     25.163519     14.187828    -15.586605
#>    ---                                                                      
#> 10768:    -0.5581126    -10.197008     -8.468801     -5.040645     -9.132243
#> 10769:    -0.6716754     -8.732699     -7.252934     -3.377731     -8.813243
#> 10770:    -0.7749598     -8.517799     -6.654031     -2.315117     -7.843120
#> 10771:    -0.7414806     -9.084275     -6.622538     -1.845731     -6.556810
#> 10772:    -0.3960888     -9.219618     -6.436240     -1.520404     -5.406781
#>                  FC5           FC1           FC2           FC6            M1
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:     30.128681    23.3925400     14.100018     -6.422720    12.4504757
#>     2:     30.302704    23.3768749     13.896832     -6.914124    12.6632786
#>     3:     30.541927    23.6706333     14.134787     -6.780725    12.4736462
#>     4:     30.937969    24.3952389     14.782417     -6.398545    12.6501217
#>     5:     31.381741    25.3208199     15.564636     -5.875418    13.6455660
#>    ---                                                                      
#> 10768:     -7.535339    -2.3418787     -3.478385     -9.384595     0.8050708
#> 10769:     -9.061241    -1.0433049     -1.930508     -7.475998     0.9832335
#> 10770:     -7.603340    -0.6127751     -1.379790     -5.710404     1.2051768
#> 10771:     -2.782797    -1.4022492     -1.640656     -4.640865     1.4103985
#> 10772:      2.897767    -2.5480108     -1.864611     -4.351316     1.1627738
#>                   T7            C3            Cz            C4            T8
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:  16.627613068    19.3569126     13.314775      9.826402     -4.390102
#>     2:  16.668287277    19.8309460     12.891417      9.151336     -4.906647
#>     3:  16.746711731    20.4183846     12.895780      8.738137     -5.022333
#>     4:  17.144357681    21.2801056     13.359244      8.833906     -5.103370
#>     5:  17.746068954    22.1702232     13.766891      9.318307     -5.340479
#>    ---                                                                      
#> 10768:   0.003161311    -0.1895013     -4.354355     -8.362184     -5.082950
#> 10769:   0.534484148     1.4632750     -2.513818     -6.418050     -5.036983
#> 10770:   0.933975220     1.7615180     -1.322806     -5.185612     -1.183164
#> 10771:   1.373799086    -0.1813622     -1.508159     -5.173932      3.655909
#> 10772:   1.512201071    -3.2800312     -2.576048     -5.242217      5.829760
#>                   M2           CP5           CP1           CP2           CP6
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:   -12.4504757     9.7454624    16.1605930     11.496984      2.658234
#>     2:   -12.6632786    10.6645718    16.2766094     10.781269      2.442920
#>     3:   -12.4736462    11.3375053    16.4452610     10.505079      2.383664
#>     4:   -12.6501217    11.8110027    16.8414211     10.822355      2.384418
#>     5:   -13.6455660    12.0349388    17.1879940     11.238800      2.295322
#>    ---                                                                      
#> 10768:    -0.8050708     3.3321576     0.7318851     -8.517914    -14.386089
#> 10769:    -0.9832335     3.6177154     2.1529536     -6.548975    -11.468288
#> 10770:    -1.2051768     3.1916180     2.6771717     -5.318815     -8.818744
#> 10771:    -1.4103985     1.9870222     1.8302985     -5.515146     -7.753020
#> 10772:    -1.1627738     0.5995106     0.1455368     -6.459312     -7.319424
#>                   P7            P3            Pz            P4            P8
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:    -1.7451426     7.0334272      8.180294      4.775116     -4.629857
#>     2:    -0.2636484     7.1291485      7.601710      4.440700     -4.131961
#>     3:     0.5830721     6.8668699      7.199402      4.265406     -3.937120
#>     4:     0.9209399     6.5586395      7.161300      4.300853     -4.057757
#>     5:     0.9128329     6.2931347      7.174585      4.265417     -4.283892
#>    ---                                                                      
#> 10768:     7.0251837     0.5382314     -4.589949    -10.580815    -11.031232
#> 10769:     6.4313159     0.9306827     -3.111303     -8.619126     -9.325615
#> 10770:     4.4492149     0.3993354     -2.119561     -6.606555     -6.365070
#> 10771:     2.3532932    -1.1857104     -2.376006     -5.663703     -4.065647
#> 10772:     1.0884135    -3.2549555     -3.662545     -5.691663     -3.320732
#>                  POz            O1            Oz            O2          EOGV
#>        <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl> <channel_dbl>
#>     1:      2.716347    -1.8138508     -1.052482    -3.7898300     221.72339
#>     2:      2.882783     0.9058186      1.491970    -1.8266290     222.24321
#>     3:      3.019314     3.1099281      4.017059    -0.1130857     221.69824
#>     4:      3.093190     4.1138868      5.513156     0.8811790     219.79274
#>     5:      2.720414     3.4851596      4.737876     0.5716844     216.66183
#>    ---                                                                      
#> 10768:     -5.136292     1.4443915     -5.164615    -8.2288170      18.41424
#> 10769:     -3.689569     1.4091530     -3.915735    -6.4314718      19.22052
#> 10770:     -2.319134     0.5369543     -2.351812    -3.4845843      19.74028
#> 10771:     -1.927569    -0.5624531     -1.339175    -1.1418194      19.20309
#> 10772:     -2.687999    -1.2014523     -1.284558    -0.7185705      18.72752
#>                 EOGH
#>        <channel_dbl>
#>     1:    186.836212
#>     2:    185.994202
#>     3:    186.654510
#>     4:    186.596741
#>     5:    184.106873
#>    ---              
#> 10768:      6.434383
#> 10769:      7.837689
#> 10770:      8.659687
#> 10771:      8.609533
#> 10772:      8.829977
#> 
#> # Events table:
#>       .id        .type .description     .initial       .final .channel
#>     <int>       <char>       <char> <sample_int> <sample_int>   <char>
#>  1:     1 Bad Interval  Bad Min-Max         -249          139       F8
#>  2:     1 Bad Interval  Bad Min-Max         -249         -210       F7
#>  3:     1 Bad Interval  Bad Min-Max         -215          190      Fp1
#>  4:     1 Bad Interval  Bad Min-Max         -214          175      Fp2
#>  5:     1 Bad Interval  Bad Min-Max         -214          180      Fpz
#>  6:     1 Bad Interval  Bad Min-Max         -197          144       Fz
#>  7:     1 Bad Interval  Bad Min-Max         -197          138       F4
#>  8:     1 Bad Interval  Bad Min-Max         -195          141       F3
#>  9:     1 Bad Interval  Bad Min-Max         -191          119       F7
#> 10:     1     Stimulus           s5            1            1     <NA>
#> 11:     2     Stimulus         s101            1            1     <NA>
#> 12:     3 Bad Interval  Bad Min-Max         -249          147      Fp1
#> 13:     4 Bad Interval  Bad Min-Max         -249          -78      Fp1
#> 14:     3 Bad Interval  Bad Min-Max         -249          147      Fpz
#> 15:     4 Bad Interval  Bad Min-Max         -249          -78      Fpz
#> 16:     3 Bad Interval  Bad Min-Max         -249          148      Fp2
#> 17:     4 Bad Interval  Bad Min-Max         -249          -77      Fp2
#> 18:     3 Bad Interval  Bad Min-Max         -249          122       F8
#> 19:     4 Bad Interval  Bad Min-Max         -249         -103       F8
#> 20:     3 Bad Interval  Bad Min-Max         -231          120       Fz
#> 21:     4 Bad Interval  Bad Min-Max         -249         -105       Fz
#> 22:     3 Bad Interval  Bad Min-Max         -230          119       F4
#> 23:     4 Bad Interval  Bad Min-Max         -249         -106       F4
#> 24:     3 Bad Interval  Bad Min-Max         -229          118       F3
#> 25:     4 Bad Interval  Bad Min-Max         -249         -107       F3
#> 26:     3 Bad Interval  Bad Min-Max         -224           76       F7
#> 27:     4 Bad Interval  Bad Min-Max         -249         -149       F7
#> 28:     3 Bad Interval  Bad Min-Max         -222           70      FC2
#> 29:     4 Bad Interval  Bad Min-Max         -249         -155      FC2
#> 30:     3 Bad Interval  Bad Min-Max         -222           71      FC1
#> 31:     4 Bad Interval  Bad Min-Max         -249         -154      FC1
#> 32:     3 Bad Interval  Bad Min-Max         -221           62      FC6
#> 33:     4 Bad Interval  Bad Min-Max         -249         -163      FC6
#> 34:     3 Bad Interval  Bad Min-Max         -217           70      FC5
#> 35:     4 Bad Interval  Bad Min-Max         -249         -155      FC5
#> 36:     3 Bad Interval  Bad Min-Max         -208           66       C3
#> 37:     4 Bad Interval  Bad Min-Max         -249         -159       C3
#> 38:     3 Bad Interval  Bad Min-Max         -205           66       Cz
#> 39:     4 Bad Interval  Bad Min-Max         -249         -159       Cz
#> 40:     3 Bad Interval  Bad Min-Max         -202           58      CP1
#> 41:     4 Bad Interval  Bad Min-Max         -249         -167      CP1
#> 42:     3 Bad Interval  Bad Min-Max         -200           56       C4
#> 43:     4 Bad Interval  Bad Min-Max         -249         -169       C4
#> 44:     3 Bad Interval  Bad Min-Max         -196           54      CP2
#> 45:     4 Bad Interval  Bad Min-Max         -249         -171      CP2
#> 46:     3 Bad Interval  Bad Min-Max         -191           51       Pz
#> 47:     4 Bad Interval  Bad Min-Max         -249         -174       Pz
#> 48:     3 Bad Interval  Bad Min-Max         -186           46       P3
#> 49:     4 Bad Interval  Bad Min-Max         -249         -179       P3
#> 50:     3 Bad Interval  Bad Min-Max         -186           -2      CP5
#> 51:     4 Bad Interval  Bad Min-Max         -249         -227      CP5
#> 52:     3     Stimulus         s111            1            1     <NA>
#> 53:     4     Stimulus         s111         -224         -224     <NA>
#> 54:     3     Stimulus         s130          226          226     <NA>
#> 55:     4     Stimulus         s130            1            1     <NA>
#> 56:     5     Stimulus          s70            1            1     <NA>
#> 57:     6     Stimulus         s130            1            1     <NA>
#> 58:     7     Stimulus          s71            1            1     <NA>
#> 59:     8     Stimulus         s130            1            1     <NA>
#> 60:     9     Stimulus          s71            1            1     <NA>
#> 61:    10     Stimulus         s130            1            1     <NA>
#> 62:    11     Stimulus          s71            1            1     <NA>
#> 63:    12     Stimulus         s130            1            1     <NA>
#> 64:    13     Stimulus          s70            1            1     <NA>
#> 65:    14     Stimulus         s130            1            1     <NA>
#> 66:    15     Stimulus          s71            1            1     <NA>
#> 67:    16     Stimulus         s130            1            1     <NA>
#> 68:    17     Stimulus          s71            1            1     <NA>
#> 69:    18     Stimulus         s130            1            1     <NA>
#> 70:    19     Stimulus          s70            1            1     <NA>
#> 71:    20     Stimulus         s130            1            1     <NA>
#> 72:    21     Stimulus          s70            1            1     <NA>
#> 73:    22     Stimulus         s130            1            1     <NA>
#>       .id        .type .description     .initial       .final .channel
#>     <int>       <char>       <char> <sample_int> <sample_int>   <char>
#> 
#> # Segments table:
#>       .id .recording segment     type description
#>     <int>     <char>   <int>   <char>      <char>
#>  1:     1 faces.vhdr       1 Stimulus          s5
#>  2:     2 faces.vhdr       2 Stimulus        s101
#>  3:     3 faces.vhdr       3 Stimulus        s111
#>  4:     4 faces.vhdr       4 Stimulus        s130
#>  5:     5 faces.vhdr       5 Stimulus         s70
#>  6:     6 faces.vhdr       6 Stimulus        s130
#>  7:     7 faces.vhdr       7 Stimulus         s71
#>  8:     8 faces.vhdr       8 Stimulus        s130
#>  9:     9 faces.vhdr       9 Stimulus         s71
#> 10:    10 faces.vhdr      10 Stimulus        s130
#> 11:    11 faces.vhdr      11 Stimulus         s71
#> 12:    12 faces.vhdr      12 Stimulus        s130
#> 13:    13 faces.vhdr      13 Stimulus         s70
#> 14:    14 faces.vhdr      14 Stimulus        s130
#> 15:    15 faces.vhdr      15 Stimulus         s71
#> 16:    16 faces.vhdr      16 Stimulus        s130
#> 17:    17 faces.vhdr      17 Stimulus         s71
#> 18:    18 faces.vhdr      18 Stimulus        s130
#> 19:    19 faces.vhdr      19 Stimulus         s70
#> 20:    20 faces.vhdr      20 Stimulus        s130
#> 21:    21 faces.vhdr      21 Stimulus         s70
#> 22:    22 faces.vhdr      22 Stimulus        s130
#>       .id .recording segment     type description
#>     <int>     <char>   <int>   <char>      <char>
```
