# ica summaries

    Code
      eeg_ica_summary_tbl(data_fast_ICA, "Fz")
    Output
         .recording    EOG   .ICA        cor        var
             <char> <char> <char>      <num>      <num>
      1: recording1     Fz   ICA2 -0.5629150 0.59371175
      2: recording1     Fz   ICA3 -0.8018246 0.33288750
      3: recording1     Fz   ICA1  0.2005095 0.07340075

---

    Code
      eeg_ica_summary_tbl(data_fast_ICA2)
    Output
         .recording    EOG   .ICA         cor        var
             <char> <char> <char>       <num>      <num>
      1: recording1   XEOG   ICA2  0.01487848 0.59371175
      2: recording1   XEOG   ICA3 -0.02058590 0.33288750
      3: recording1   XEOG   ICA1  0.03040823 0.07340075

# summaries work

    Code
      eeg_ica_cor_tbl(data_fast_ICA, tidyselect::all_of(c("Fz", "Cz")))
    Output
         .recording    EOG   .ICA        cor
             <char> <char> <fctr>      <num>
      1: recording1     Fz   ICA3 -0.8018246
      2: recording1     Cz   ICA2 -0.7817242
      3: recording1     Cz   ICA3 -0.5665764
      4: recording1     Fz   ICA2 -0.5629150
      5: recording1     Cz   ICA1  0.2605735
      6: recording1     Fz   ICA1  0.2005095

---

    Code
      eeg_ica_var_tbl(data_fast_ICA)
    Output
         .recording   .ICA        var
             <char> <char>      <num>
      1: recording1   ICA2 0.59371175
      2: recording1   ICA3 0.33288750
      3: recording1   ICA1 0.07340075

