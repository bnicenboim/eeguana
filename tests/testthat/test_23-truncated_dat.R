library(eeguana)
options(eeguana.verbose = FALSE)

## A BrainVision header states how much data its .dat holds. When the two
## disagree, usually because the .dat was copied or downloaded incompletely,
## the read used to fail deep inside data.table with
##   Supplied 38410 items to be assigned to 38409 items of column '.id'
## which says nothing about the real problem.

# copies a real recording into a temporary directory, optionally resizing the
# .dat, and returns the path of the copied .vhdr
copy_bv <- function(dir, base = "bv_export_bv_txt_bin_multi", keep = 1) {
  for (ext in c("vhdr", "vmrk", "dat")) {
    file.copy(
      system.file("testdata", paste0(base, ".", ext), package = "eeguana"),
      file.path(dir, paste0(base, ".", ext))
    )
  }
  dat <- file.path(dir, paste0(base, ".dat"))
  if (keep != 1) {
    full <- readBin(dat, "raw", file.size(dat))
    resized <- if (keep < 1) {
      head(full, floor(length(full) * keep))
    } else {
      c(full, full[seq_len(floor(length(full) * (keep - 1)))])
    }
    writeBin(resized, dat)
  }
  file.path(dir, paste0(base, ".vhdr"))
}

test_that("the untouched copy still reads", {
  dir <- withr::local_tempdir()
  x <- read_vhdr(copy_bv(dir), .recording = "bv2")
  expect_s3_class(x, "eeg_lst")
  expect_equal(nrow(x$.signal), 4722L)
})

test_that("a truncated .dat is reported as such, with the numbers", {
  dir <- withr::local_tempdir()
  vhdr <- copy_bv(dir, keep = 0.4)
  expect_error(read_vhdr(vhdr, .recording = "bv2"), "does not match its header")
  expect_error(read_vhdr(vhdr, .recording = "bv2"), "looks truncated")
  # both sizes are named, so the cause is obvious from the message alone
  expect_error(read_vhdr(vhdr, .recording = "bv2"), "642,192 bytes")
})

test_that("an oversized .dat warns and is still read", {
  # only a short file is fatal. A long one is read in full, including the
  # extra samples the header does not declare, which the warning states
  dir <- withr::local_tempdir()
  vhdr <- copy_bv(dir, keep = 1.5)
  expect_warning(x <- read_vhdr(vhdr, .recording = "bv2"), "does not match its header")
  expect_warning(read_vhdr(vhdr, .recording = "bv2"), "extra data is read too")
  expect_s3_class(x, "eeg_lst")
  # the extra data is read rather than discarded, and the warning says so
  expect_gt(nrow(x$.signal), 4722L)
})

test_that("truncation is caught whatever the orientation", {
  dir <- withr::local_tempdir()
  expect_error(
    read_vhdr(copy_bv(dir, base = "bv_export_bv_txt_bin_vector", keep = 0.4), .recording = "bv2"),
    "does not match its header"
  )
})

test_that("truncation is caught whatever the binary width", {
  # the expected size depends on BinaryFormat: a 16 bit file must be measured
  # against 2 bytes per value, not 4
  dir <- withr::local_tempdir()
  expect_error(
    read_vhdr(copy_bv(dir, base = "bv_export_bv_txt_bin_multi_16bit", keep = 0.4), .recording = "bv2"),
    "does not match its header"
  )
  dir2 <- withr::local_tempdir()
  expect_s3_class(
    read_vhdr(copy_bv(dir2, base = "bv_export_bv_txt_bin_multi_16bit"), .recording = "bv2"),
    "eeg_lst"
  )
})
