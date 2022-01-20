library(eeguana)
options(eeguana.verbose = FALSE)

test_that("prints correctly", {
  expect_snapshot(data_faces_10_trials)
  expect_snapshot(data_faces_10_trials)
})

test_that("summary is correct", {
  expect_snapshot(summary(data_faces_10_trials))
})
