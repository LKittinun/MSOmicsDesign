test_that("opt.windows returns a numeric vector of same length", {
  mass <- seq(400, 1000, 20)
  result <- opt.windows(mass)
  expect_type(result, "double")
  expect_equal(length(result), length(mass))
})

test_that("opt.windows output values are all greater than or equal to input", {
  mass <- seq(400, 1000, 20)
  result <- opt.windows(mass)
  expect_true(all(result >= mass))
})

test_that("opt.windows with phospho = TRUE produces different values", {
  mass <- seq(400, 1000, 20)
  r_normal <- opt.windows(mass)
  r_phospho <- opt.windows(mass, phospho = TRUE)
  expect_false(identical(r_normal, r_phospho))
  expect_equal(length(r_phospho), length(mass))
})

test_that("opt.windows handles a single value", {
  result <- opt.windows(500)
  expect_length(result, 1L)
  expect_type(result, "double")
})
