make_plate <- function(n = 10) {
  fp <- fill.plate(LETTERS[1:n])
  fp$plate[[1]]
}

# --- basic output ---

test_that("plot.plate returns a ggplot", {
  p <- plot.plate(make_plate())
  expect_true(inherits(p, "ggplot"))
})

test_that("plot.plate errors on non-plate input", {
  m <- matrix(letters[1:6], nrow = 2)
  expect_error(plot.plate(m))
})

# --- return_df ---

test_that("plot.plate with return_df = TRUE returns list with ggplot and dataframe", {
  result <- plot.plate(make_plate(), return_df = TRUE)
  expect_type(result, "list")
  expect_equal(length(result), 2L)
  expect_true(inherits(result[[1]], "ggplot"))
  expect_true(is.data.frame(result[[2]]))
})

test_that("plot.plate dataframe has expected columns", {
  result <- plot.plate(make_plate(), return_df = TRUE)
  df <- result[[2]]
  expect_true(all(c("row", "col", "value") %in% names(df)))
})

# --- vector labels ---

test_that("plot.plate with vector labels adds labels column to dataframe", {
  plate <- make_plate(10)
  labels <- paste0("L", 1:54)   # full plate
  result <- plot.plate(plate, labels = labels, return_df = TRUE)
  df <- result[[2]]
  expect_true("labels" %in% names(df))
})

test_that("plot.plate pads short vector labels with empty strings", {
  plate <- make_plate(10)
  result <- plot.plate(plate, labels = paste0("L", 1:5), return_df = TRUE)
  df <- result[[2]]
  expect_true("labels" %in% names(df))
  expect_equal(nrow(df), 6 * 9)
})

# --- plate-class labels ---

test_that("plot.plate with plate-class labels adds labels column via inner_join", {
  fp <- fill.plate(LETTERS[1:10], labels = paste0("lbl_", 1:10))
  plate  <- fp$plate[[1]]
  labels <- fp$label[[1]]
  result <- plot.plate(plate, labels = labels, return_df = TRUE)
  df <- result[[2]]
  expect_true("labels" %in% names(df))
})

test_that("plot.plate plate-class labels contain correct label values", {
  lbl <- paste0("lbl_", 1:10)
  fp  <- fill.plate(LETTERS[1:10], labels = lbl)
  result <- plot.plate(fp$plate[[1]], labels = fp$label[[1]], return_df = TRUE)
  df <- result[[2]]
  # Use $ to extract vector from tibble (avoids 1-column tibble subsetting)
  filled_labels <- df$labels[df$value != "Blank"]
  expect_true(all(filled_labels %in% lbl))
})

# --- control.suffix ---

test_that("plot.plate strips control suffix from value grouping", {
  samples <- c(paste0("QC_", 1:3), LETTERS[1:7])
  fp <- fill.plate(samples)
  result <- plot.plate(fp$plate[[1]], control.suffix = "QC", return_df = TRUE)
  df <- result[[2]]
  expect_true(any(df$value == "QC"))
})
