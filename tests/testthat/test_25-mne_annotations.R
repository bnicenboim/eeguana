library(eeguana)

## MNE stores each annotation as "type/description". Converting from MNE used
## tidyr::separate(sep = "/", fill = "left") to split it. tidyr is no longer
## imported, so eeguana splits it itself, and these tests pin that the split
## still behaves exactly as tidyr did.

split <- eeguana:::split_type_description

test_that("type and description are split at the slash", {
  expect_equal(split("Stimulus/s70"), list(.type = "Stimulus", .description = "s70"))
})

test_that("a description without a slash has no type", {
  # this is the common case in MNE, e.g. "boundary" or "BAD_blink".
  # tidytable::separate() would put it in the type column instead.
  expect_equal(split("boundary"), list(.type = NA_character_, .description = "boundary"))
})

test_that("anything past a second slash is dropped", {
  expect_equal(split("a/b/c"), list(.type = "a", .description = "b"))
})

test_that("awkward descriptions split the way tidyr split them", {
  out <- split(c("", NA, "a/", "/b"))
  expect_equal(out$.type, c(NA, NA, "a", ""))
  expect_equal(out$.description, c("", NA, "", "b"))
})

test_that("a one-dimensional array comes back as plain vectors", {
  # reticulate hands MNE's descriptions over as a 1-d array. The first version
  # of this split kept its dim attribute, and the comparison against a
  # BrainVision read of the same recording in test_19 failed on it.
  out <- split(array(c("Stimulus/s70", "boundary"), dim = 2))
  expect_null(dim(out$.type))
  expect_null(dim(out$.description))
  expect_equal(out$.type, c("Stimulus", NA))
  expect_equal(out$.description, c("s70", "boundary"))
})
