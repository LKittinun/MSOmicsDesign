# Build a simple isolation list spanning 400-1000 in 20 Da steps
iso_list <- paste(seq(400, 980, 20), seq(420, 1000, 20), sep = "-")
lower_w  <- seq(400, 980, 20)
upper_w  <- seq(420, 1000, 20)

# --- isolation_list input ---

test_that("GPF.windows returns a named list of mzdf data frames", {
  result <- GPF.windows(iso_list, mnc = 400, mxc = 1000)
  expect_type(result, "list")
  expect_true(all(vapply(result, inherits, logical(1), "mzdf")))
})

test_that("GPF.windows groups span windows_width = 100 by default", {
  result <- GPF.windows(iso_list, mnc = 400, mxc = 1000)
  # With 400-1000 range and 100 Da groups, expect 6 groups
  expect_equal(length(result), 6L)
})

test_that("GPF.windows list is named by m/z range", {
  result <- GPF.windows(iso_list, mnc = 400, mxc = 1000)
  expect_true(all(grepl("-", names(result))))
})

# --- lower/upper windows input ---

test_that("GPF.windows works with explicit lower_windows and upper_windows", {
  result <- GPF.windows(lower_windows = lower_w, upper_windows = upper_w,
                        mnc = 400, mxc = 1000)
  expect_type(result, "list")
  expect_true(all(vapply(result, inherits, logical(1), "mzdf")))
})

test_that("GPF.windows isolation_list and vector inputs produce same result", {
  r1 <- GPF.windows(iso_list, mnc = 400, mxc = 1000)
  r2 <- GPF.windows(lower_windows = lower_w, upper_windows = upper_w,
                    mnc = 400, mxc = 1000)
  expect_equal(length(r1), length(r2))
  expect_equal(names(r1), names(r2))
})

# --- output.type ---

test_that("GPF.windows output.type center has center columns", {
  result <- GPF.windows(iso_list, mnc = 400, mxc = 1000, output.type = "center")
  for (grp in result) {
    expect_true(all(c("Center mass (m/z)", "Scan width (m/z)") %in% names(grp)))
  }
})

test_that("GPF.windows output.type both has all columns", {
  result <- GPF.windows(iso_list, mnc = 400, mxc = 1000, output.type = "both")
  for (grp in result) {
    expect_true(all(c("m/z range", "lower_windows", "upper_windows") %in% names(grp)))
  }
})

# --- custom windows_width ---

test_that("GPF.windows custom windows_width changes number of groups", {
  result_100 <- GPF.windows(iso_list, mnc = 400, mxc = 1000, windows_width = 100)
  result_200 <- GPF.windows(iso_list, mnc = 400, mxc = 1000, windows_width = 200)
  expect_gt(length(result_100), length(result_200))
})
