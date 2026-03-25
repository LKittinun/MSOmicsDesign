test_that("generate.position returns a list by default", {
  fp <- fill.plate(1:10)
  result <- generate.position(fp)
  expect_type(result, "list")
})

test_that("generate.position entries follow plate_name:rowcol format", {
  fp <- fill.plate(1:10)
  result <- generate.position(fp)
  positions <- unlist(result)
  expect_true(all(grepl("^[A-Z]+:[A-Z][0-9]+$", positions)))
})

test_that("generate.position count matches number of filled wells", {
  samples <- LETTERS[1:10]
  fp <- fill.plate(samples)
  result <- generate.position(fp)
  total_positions <- sum(vapply(result, length, integer(1)))
  expect_equal(total_positions, length(samples))
})

test_that("generate.position with flatten = TRUE returns a flat character vector", {
  fp <- fill.plate(1:10)
  result <- generate.position(fp, flatten = TRUE)
  expect_type(result, "character")
  expect_true(!is.list(result))
})

test_that("generate.position flatten matches manual unlist", {
  fp <- fill.plate(1:30)
  r_list    <- generate.position(fp)
  r_flat    <- generate.position(fp, flatten = TRUE)
  r_unlist  <- unlist(r_list, recursive = TRUE, use.names = FALSE)
  expect_equal(r_flat, r_unlist)
})

test_that("generate.position works for multi-plate fill.plate results", {
  fp <- fill.plate(addQC(LETTERS[1:20], interval = 6))
  result <- generate.position(fp)
  expect_equal(length(result), length(fp$plate))
})

test_that("generate.position errors on non-plate input", {
  expect_error(generate.position(list(plate = list(matrix(1:6, 2, 3)))))
})
