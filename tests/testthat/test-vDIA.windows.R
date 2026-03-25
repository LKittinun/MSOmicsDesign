test_that("vDIA.windows returns an mzdf data frame", {
  result <- vDIA.windows(mnc = 400, mxc = 1000, n.windows = 32)
  expect_true(inherits(result, "mzdf"))
  expect_true(is.data.frame(result))
})

test_that("vDIA.windows output.type range has expected columns", {
  result <- vDIA.windows(mnc = 400, mxc = 1000, n.windows = 32)
  expect_true(all(c("m/z range", "lower_windows", "upper_windows") %in% names(result)))
})

test_that("vDIA.windows output.type center has expected columns", {
  result <- vDIA.windows(mnc = 400, mxc = 1000, n.windows = 32, output.type = "center")
  expect_true(all(c("Center mass (m/z)", "Scan width (m/z)") %in% names(result)))
})

test_that("vDIA.windows output.type both contains all columns", {
  result <- vDIA.windows(mnc = 400, mxc = 1000, n.windows = 32, output.type = "both")
  expect_true(all(c("m/z range", "lower_windows", "upper_windows",
                    "Center mass (m/z)", "Scan width (m/z)") %in% names(result)))
})

test_that("vDIA.windows lower_windows < upper_windows for all rows", {
  result <- vDIA.windows(mnc = 400, mxc = 1000, n.windows = 32)
  expect_true(all(result$lower_windows < result$upper_windows))
})

test_that("vDIA.windows produces approximately n.windows rows", {
  result <- vDIA.windows(mnc = 400, mxc = 1000, n.windows = 32)
  # Variable windows won't be exact, but should be in range
  expect_gt(nrow(result), 0)
  expect_lte(nrow(result), 32)
})

test_that("vDIA.windows lower_windows start within the requested m/z range", {
  result <- vDIA.windows(mnc = 400, mxc = 1000, n.windows = 32)
  # lower bounds must be >= mnc; upper bound can slightly exceed mxc due to
  # coalesce + margin in the variable-width binning logic
  expect_true(all(result$lower_windows >= 400))
})

# --- errors ---

test_that("vDIA.windows errors when mnc is missing", {
  expect_error(vDIA.windows(mxc = 1000, n.windows = 32))
})

test_that("vDIA.windows errors when n.windows is missing", {
  expect_error(vDIA.windows(mnc = 400, mxc = 1000))
})

test_that("vDIA.windows errors when mnc >= mxc", {
  expect_error(vDIA.windows(mnc = 1000, mxc = 400, n.windows = 32))
})
