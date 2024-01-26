
other_testfiles <- file.path(testthat::test_path(), "other_files")
if(!dir.exists(other_testfiles)) dir.create(other_testfiles)


library(httr)
GET("https://osf.io/6gpeh//?action=download",
    write_disk(file.path(other_testfiles, "EEG01.mat"), overwrite = TRUE), 
    progress()
)
