# summarizing functions don't break

    Code
      channel_names(eeg_file)
    Output
       [1] "Fp1"  "Fpz"  "Fp2"  "F7"   "F3"   "Fz"   "F4"   "F8"   "FC5"  "FC1" 
      [11] "FC2"  "FC6"  "M1"   "T7"   "C3"   "Cz"   "C4"   "T8"   "M2"   "CP5" 
      [21] "CP1"  "CP2"  "CP6"  "P7"   "P3"   "Pz"   "P4"   "P8"   "POz"  "O1"  
      [31] "Oz"   "O2"   "HEOG" "VEOG"

---

    Code
      nchannels(eeg_file)
    Output
      [1] 34

---

    Code
      nsamples(eeg_file)
    Output
      [1] 4722

