context("test time functions")
library(eeguana)


test_that("1 sample work fine", {
  s1 <- sample_int(1, 256)
  s1_as <- as_sample_int(1,256,.unit = "samples")
  s1_ast <- as_sample_int(as_time(s1, .unit = "second"), 256, .unit = "second")
  s1_ast2 <- as_sample_int(as_time(s1, .unit = "ms"), 256, .unit = "ms")
  expect_equal(s1, s1_as)
  expect_equal(s1, s1_ast)
  expect_equal(s1, s1_ast2)
  })

test_that("vector of samples work fine", {
  v <- c(1:10,20)
  sv <- sample_int(v, 256)
  sv_as <- as_sample_int(v,256,.unit = "samples")
  sv_ast <- as_sample_int(as_time(sv, .unit = "second"), 256, .unit = "second")
  sv_ast2 <- as_sample_int(as_time(sv, .unit = "ms"), 256, .unit = "ms")
  expect_equal(sv, sv_as)
  expect_equal(sv, sv_ast)
  expect_equal(sv, sv_ast2)
})

test_that("vector of samples with Inf work fine", {
  v <- c(1:10,-5, Inf, -Inf)
  sv <- sample_int(v, 256)
  sv_as <- as_sample_int(v,256,.unit = "samples")
  sv_ast <- as_sample_int(as_time(sv, .unit = "second"), 256, .unit = "second")
  sv_ast2 <- as_sample_int(as_time(sv, .unit = "ms"), 256, .unit = "ms")
  expect_equal(sv, sv_as)
  expect_equal(sv, sv_ast)
  expect_equal(sv, sv_ast2)
})

test_that("Inf work fine", {
  inf <-  Inf
  si <- sample_int(inf, 256)
  si_as <- as_sample_int(inf,256,.unit = "samples")
  si_ast <- as_sample_int(as_time(si, .unit = "second"), 256, .unit = "second")
  si_ast2 <- as_sample_int(as_time(si, .unit = "ms"), 256, .unit = "ms")
  expect_equal(si, si_as)
  expect_equal(si, si_ast)
  expect_equal(si, si_ast2)
})

test_that("time zeros work",{
expect_equal(as.integer(as_sample_int(0, 256, .unit = "second")),1)
expect_equal(as.integer(as_sample_int(1/256, 256, .unit = "second")),2)
expect_equal(as.integer(as_sample_int(-1/256, 256, .unit = "second")),0)
})
