test_that("no labels returns a list of ggplots, one per plate", {
  samples <- addQC(LETTERS[1:20], interval = 6)
  fp <- fill.plate(samples)

  result <- plot(fp)

  expect_type(result, "list")
  expect_equal(length(result), length(fp$plate))
  expect_true(all(vapply(result, inherits, logical(1), "ggplot")))
})

test_that("flat vector labels returns a list of ggplots (existing path)", {
  samples <- addQC(LETTERS[1:20], interval = 6)
  labels  <- paste0("lbl_", seq_along(samples))
  fp <- fill.plate(samples)

  result <- plot(fp, labels = labels)

  expect_type(result, "list")
  expect_equal(length(result), length(fp$plate))
  expect_true(all(vapply(result, inherits, logical(1), "ggplot")))
})

test_that("plate-class labels from fill.plate()$label returns a list of ggplots (new path)", {
  samples <- addQC(LETTERS[1:20], interval = 6)
  labels  <- paste0("lbl_", seq_along(samples))
  fp <- fill.plate(samples, labels = labels)

  expect_true("plate" %in% class(fp$label))

  result <- plot(fp, labels = fp$label)

  expect_type(result, "list")
  expect_equal(length(result), length(fp$plate))
  expect_true(all(vapply(result, inherits, logical(1), "ggplot")))
})

test_that("plate-class labels produce a 'labels' column in the plot dataframe", {
  samples <- addQC(LETTERS[1:20], interval = 6)
  labels  <- paste0("lbl_", seq_along(samples))
  fp <- fill.plate(samples, labels = labels)

  result <- plot(fp, labels = fp$label, return_df = TRUE)

  for (plate_result in result) {
    df <- plate_result[[2]]
    expect_true("labels" %in% names(df))
  }
})

test_that("flat vector labels produce a 'labels' column in the plot dataframe", {
  samples <- addQC(LETTERS[1:20], interval = 6)
  labels  <- paste0("lbl_", seq_along(samples))
  fp <- fill.plate(samples)

  result <- plot(fp, labels = labels, return_df = TRUE)

  for (plate_result in result) {
    df <- plate_result[[2]]
    expect_true("labels" %in% names(df))
  }
})

test_that("plate-class labels are not corrupted by padding/splitting", {
  samples <- addQC(LETTERS[1:20], interval = 6)
  labels  <- paste0("lbl_", seq_along(samples))
  fp <- fill.plate(samples, labels = labels)

  result <- plot(fp, labels = fp$label, return_df = TRUE)

  for (i in seq_along(result)) {
    df <- result[[i]][[2]]
    # Every non-blank well should have a matching label (no empty-string garbage)
    filled <- df[df$value != "Blank", ]
    expect_true(all(filled$labels != ""))
  }
})

test_that("single-plate fill.plate works with no labels", {
  fp <- fill.plate(LETTERS[1:10])

  result <- plot(fp)

  expect_equal(length(result), 1L)
  expect_true(inherits(result[[1]], "ggplot"))
})

test_that("single-plate fill.plate works with flat vector labels", {
  fp <- fill.plate(LETTERS[1:10])

  result <- plot(fp, labels = paste0("L", 1:10))

  expect_equal(length(result), 1L)
  expect_true(inherits(result[[1]], "ggplot"))
})
