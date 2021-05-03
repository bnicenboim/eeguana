# ica summaries

    Code
      eeg_ica_summary_tbl(data_fast_ICA, "Fz")
    Output
         .recording EOG .ICA        cor        var
      1: recording1  Fz ICA2  0.7314961 0.77880353
      2: recording1  Fz ICA3 -0.6499962 0.16381304
      3: recording1  Fz ICA1  0.2059573 0.05738344

---

    Code
      eeg_ica_summary_tbl(data_fast_ICA2)
    Output
         .recording  EOG .ICA         cor        var
      1: recording1 XEOG ICA2 0.012486626 0.77880353
      2: recording1 XEOG ICA3 0.002487617 0.16381304
      3: recording1 XEOG ICA1 0.030948016 0.05738344

# summaries work

    Code
      eeg_ica_cor_tbl(data_fast_ICA, tidyselect::all_of(c("Fz", "Cz")))
    Output
         .recording EOG .ICA        cor
      1: recording1  Cz ICA2  0.8901164
      2: recording1  Fz ICA2  0.7314961
      3: recording1  Fz ICA3 -0.6499962
      4: recording1  Cz ICA3 -0.3953829
      5: recording1  Cz ICA1  0.2266388
      6: recording1  Fz ICA1  0.2059573

---

    Code
      eeg_ica_var_tbl(data_fast_ICA)
    Output
         .recording .ICA        var
      1: recording1 ICA2 0.77880353
      2: recording1 ICA3 0.16381304
      3: recording1 ICA1 0.05738344

