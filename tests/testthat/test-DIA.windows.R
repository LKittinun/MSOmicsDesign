# --- class and structure ---

test_that("DIA.windows returns an mzdf data frame", {
  result <- DIA.windows(mnc = 400, mxc = 1000, windows.width = 20)
  expect_true(inherits(result, "mzdf"))
  expect_true(is.data.frame(result))
})

# --- output.type = "range" (default) ---

test_that("DIA.windows output.type range has expected columns", {
  result <- DIA.windows(mnc = 400, mxc = 1000, windows.width = 20)
  expect_true(all(c("m/z range", "lower_windows", "upper_windows") %in% names(result)))
})

test_that("DIA.windows lower_windows < upper_windows for all rows", {
  result <- DIA.windows(mnc = 400, mxc = 1000, windows.width = 20)
  expect_true(all(result$lower_windows < result$upper_windows))
})

# --- output.type = "center" ---

test_that("DIA.windows output.type center has expected columns", {
  result <- DIA.windows(mnc = 400, mxc = 1000, windows.width = 20, output.type = "center")
  expect_true(all(c("Center mass (m/z)", "Scan width (m/z)") %in% names(result)))
})

# --- output.type = "both" ---

test_that("DIA.windows output.type both contains all columns", {
  result <- DIA.windows(mnc = 400, mxc = 1000, windows.width = 20, output.type = "both")
  expect_true(all(c("m/z range", "lower_windows", "upper_windows",
                    "Center mass (m/z)", "Scan width (m/z)") %in% names(result)))
})

# --- n.windows path ---

test_that("DIA.windows with n.windows returns approximately n.windows rows", {
  n <- 30
  result <- DIA.windows(mnc = 400, mxc = 1000, n.windows = n)
  # ceiling() on window width can produce n or n+1 rows
  expect_lte(nrow(result), n + 1)
  expect_gte(nrow(result), n - 1)
})

# --- stagger ---

test_that("DIA.windows stagger = TRUE doubles the number of windows", {
  r_plain   <- DIA.windows(mnc = 400, mxc = 1000, windows.width = 20, stagger = FALSE)
  r_stagger <- DIA.windows(mnc = 400, mxc = 1000, windows.width = 20, stagger = TRUE)
  expect_equal(nrow(r_stagger), nrow(r_plain) * 2)
})

# --- errors ---

test_that("DIA.windows errors when mnc is missing", {
  expect_error(DIA.windows(mxc = 1000, windows.width = 20))
})

test_that("DIA.windows errors when mxc is missing", {
  expect_error(DIA.windows(mnc = 400, windows.width = 20))
})

test_that("DIA.windows errors when mnc >= mxc", {
  expect_error(DIA.windows(mnc = 1000, mxc = 400, windows.width = 20))
})

test_that("DIA.windows errors when both windows.width and n.windows given", {
  expect_error(DIA.windows(mnc = 400, mxc = 1000, windows.width = 20, n.windows = 30))
})

test_that("DIA.windows errors when neither windows.width nor n.windows given", {
  expect_error(DIA.windows(mnc = 400, mxc = 1000))
})
