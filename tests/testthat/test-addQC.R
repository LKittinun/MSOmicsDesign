samples <- LETTERS[1:20]

# --- interval ---

test_that("addQC with interval inserts QC at correct positions", {
  result <- addQC(samples, interval = 5)
  # QC inserted after every 5 samples: positions 6, 12, 18, 24
  qc_pos <- grep("^QC", result)
  expect_true(length(qc_pos) > 0)
  # Non-QC positions should still contain the original samples in order
  expect_equal(result[!grepl("^QC", result)], samples)
})

test_that("addQC with interval produces correct number of QC entries", {
  result <- addQC(samples, interval = 5)
  n_qc <- sum(grepl("^QC", result))
  expect_equal(n_qc, floor(length(samples) / 5))
})

test_that("addQC QC labels are sequentially numbered", {
  result <- addQC(samples, interval = 5)
  qc_labels <- result[grepl("^QC", result)]
  expect_equal(qc_labels, paste0("QC_", seq_along(qc_labels)))
})

test_that("addQC custom label prefix is applied", {
  result <- addQC(samples, interval = 5, label = "CTRL")
  expect_true(all(grepl("^CTRL", result[grepl("^CTRL", result)])))
  expect_false(any(grepl("^QC", result)))
})

# --- position, replace = TRUE (in-place) ---

test_that("addQC with replace=TRUE replaces samples at specified positions", {
  result <- addQC(samples, position = c(3, 7), replace = TRUE)
  expect_equal(result[3], "QC_1")
  expect_equal(result[7], "QC_2")
})

test_that("addQC with replace=TRUE keeps length equal to original", {
  result <- addQC(samples, position = c(3, 7), replace = TRUE)
  expect_equal(length(result), length(samples))
})

test_that("addQC with replace=TRUE leaves non-replaced positions unchanged", {
  result <- addQC(samples, position = c(3, 7), replace = TRUE)
  expect_equal(result[-c(3, 7)], samples[-c(3, 7)])
})

test_that("addQC with replace=TRUE respects custom label prefix", {
  result <- addQC(samples, position = c(5, 15), replace = TRUE, label = "POOL")
  expect_equal(result[5],  "POOL_1")
  expect_equal(result[15], "POOL_2")
})

test_that("addQC with replace=TRUE handles duplicate positions (last write wins)", {
  result <- addQC(samples, position = c(5, 5, 5), replace = TRUE)
  expect_equal(result[5], "QC_3")
})

test_that("addQC with replace=TRUE errors on non-positive position", {
  expect_error(addQC(samples, position = -1,  replace = TRUE))
  expect_error(addQC(samples, position = 3.5, replace = TRUE))
})

# --- position, replace = FALSE (insert) ---

test_that("addQC with replace=FALSE inserts QC and increases length", {
  result <- addQC(samples, position = c(3, 7))
  expect_equal(length(result), length(samples) + 2)
})

test_that("addQC with replace=FALSE inserts QC adjacent to specified positions", {
  result <- addQC(samples, position = c(3, 7))
  qc_pos <- grep("^QC", result)
  expect_equal(length(qc_pos), 2)
})

test_that("addQC with replace=FALSE preserves original samples", {
  result <- addQC(samples, position = c(3, 7))
  expect_equal(result[!grepl("^QC", result)], samples)
})

# --- errors ---

test_that("addQC errors when neither interval nor position is given", {
  expect_error(addQC(samples))
})

test_that("addQC errors when both interval and position are given", {
  expect_error(addQC(samples, interval = 5, position = 3))
})

test_that("addQC errors when interval exceeds sample length", {
  expect_error(addQC(samples, interval = 25))
})

test_that("addQC errors on non-integer interval", {
  expect_error(addQC(samples, interval = 3.5))
})
