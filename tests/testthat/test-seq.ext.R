test_that("seq.ext appends max value when sequence does not reach it", {
  result <- seq.ext(1, 9, 3)
  expect_equal(tail(result, 1), 9)
  expect_equal(result, c(1, 4, 7, 9))
})

test_that("seq.ext does not duplicate max when sequence already reaches it", {
  result <- seq.ext(1, 7, 3)
  expect_equal(result, c(1, 4, 7))
  expect_equal(sum(result == 7), 1L)
})

test_that("seq.ext with rm.last replaces last element instead of appending", {
  result <- seq.ext(1, 9, 3, rm.last = TRUE)
  expect_equal(tail(result, 1), 9)
  expect_equal(length(result), length(seq(1, 9, 3)))  # same length as base seq
  expect_equal(result, c(1, 4, 9))
})

test_that("seq.ext rm.last = FALSE is default behaviour", {
  r1 <- seq.ext(1, 9, 3)
  r2 <- seq.ext(1, 9, 3, rm.last = FALSE)
  expect_equal(r1, r2)
})
