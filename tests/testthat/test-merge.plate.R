a <- fill.plate(paste0("S", 1:20))
b <- fill.plate(paste0("S", 21:40))

a_lab <- fill.plate(paste0("S", 1:20), labels = paste0("L", 1:20))
b_lab <- fill.plate(paste0("S", 21:40), labels = paste0("L", 21:40))

a_large <- fill.plate(paste0("S", 1:60))
b_large <- fill.plate(paste0("S", 61:120))

# --- class and structure ---

test_that("merge_plate returns a plate_list", {
  m <- merge_plate(a, b)
  expect_true(inherits(m, "plate_list"))
})

test_that("merge_plate result has $plate and $data slots", {
  m <- merge_plate(a, b)
  expect_true(!is.null(m$plate))
  expect_true(!is.null(m$data))
})

# --- plate count ---

test_that("merge_plate combines plates from both inputs", {
  m <- merge_plate(a, b)
  expect_equal(length(m$plate), length(a$plate) + length(b$plate))
})

test_that("merge_plate combines large plate lists correctly", {
  m <- merge_plate(a_large, b_large)
  expect_equal(length(m$plate), length(a_large$plate) + length(b_large$plate))
})

# --- sample preservation ---

test_that("merge_plate preserves all samples in $data$samples", {
  m <- merge_plate(a, b)
  expect_equal(
    sort(m$data$samples),
    sort(c(a$data$samples, b$data$samples))
  )
})

# --- name conflict resolution ---

test_that("merge_plate renames conflicting plates with default suffixes", {
  m <- merge_plate(a_large, b_large)
  conflicts <- intersect(names(a_large$plate), names(b_large$plate))
  if (length(conflicts) > 0) {
    expect_true(any(grepl("_1$", names(m$plate))))
    expect_true(any(grepl("_2$", names(m$plate))))
  }
})

test_that("merge_plate respects custom suffixes", {
  m <- merge_plate(a_large, b_large, suffixes = c("_A", "_B"))
  conflicts <- intersect(names(a_large$plate), names(b_large$plate))
  if (length(conflicts) > 0) {
    expect_true(any(grepl("_A$", names(m$plate))))
    expect_true(any(grepl("_B$", names(m$plate))))
  }
})

test_that("merge_plate plate names are unique after merge", {
  m <- merge_plate(a_large, b_large)
  expect_equal(length(names(m$plate)), length(unique(names(m$plate))))
})

# --- labels ---

test_that("merge_plate merges labels when both inputs have them", {
  m <- merge_plate(a_lab, b_lab)
  expect_false(is.null(m$label))
  expect_equal(length(m$label), length(a_lab$label) + length(b_lab$label))
})

test_that("merge_plate drops labels with warning when only one side has them", {
  expect_warning(
    m <- merge_plate(a_lab, b),
    "Only one plate_list has labels"
  )
  expect_null(m$label)
})

test_that("merge_plate preserves all labels in $data$labels", {
  m <- merge_plate(a_lab, b_lab)
  expect_equal(
    sort(m$data$labels),
    sort(c(a_lab$data$labels, b_lab$data$labels))
  )
})

# --- input validation ---

test_that("merge_plate errors when x is not a plate_list", {
  expect_error(merge_plate(list(), b), "x must be a plate_list")
})

test_that("merge_plate errors when y is not a plate_list", {
  expect_error(merge_plate(a, list()), "y must be a plate_list")
})

test_that("merge_plate errors when suffixes is wrong length", {
  expect_error(
    merge_plate(a, b, suffixes = "_1"),
    "suffixes must be a length-2 character vector"
  )
})

test_that("merge.plate errors when suffixes is not character", {
  expect_error(
    merge.plate(a, b, suffixes = c(1, 2)),
    "suffixes must be a length-2 character vector"
  )
})
