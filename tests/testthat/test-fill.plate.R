samples_small <- LETTERS[1:10]        # fits on one plate (6x9 = 54 wells)
samples_large <- paste0("S", 1:60)    # 60 samples, spills onto two 6x9 plates

# --- class and structure ---

test_that("fill.plate returns a plate_list object", {
  fp <- fill.plate(samples_small)
  expect_true(inherits(fp, "plate_list"))
  expect_true(is.list(fp))
})

test_that("fill.plate result has $plate, $data slots", {
  fp <- fill.plate(samples_small)
  expect_true(!is.null(fp$plate))
  expect_true(!is.null(fp$data))
})

test_that("fill.plate $plate has plate class", {
  fp <- fill.plate(samples_small)
  expect_true("plate" %in% class(fp$plate))
})

test_that("each plate matrix inside $plate has plate class", {
  fp <- fill.plate(samples_small)
  for (m in fp$plate) {
    expect_true("plate" %in% class(m))
  }
})

# --- single plate ---

test_that("fill.plate produces one plate when samples fit on a single plate", {
  fp <- fill.plate(samples_small)
  expect_equal(length(fp$plate), 1L)
})

test_that("fill.plate matrix dimensions match nrows x ncols", {
  fp <- fill.plate(samples_small, nrows = 6, ncols = 9)
  m <- fp$plate[[1]]
  expect_equal(nrow(m), 6L)
  expect_equal(ncol(m), 9L)
})

test_that("fill.plate places samples in correct order", {
  fp <- fill.plate(samples_small)
  # plate is filled row-by-row; c(t(plate)) reads it back in that order
  wells <- c(t(fp$plate[[1]]))
  filled <- wells[wells != ""]
  expect_equal(filled, samples_small)
})

# --- multi-plate ---

test_that("fill.plate produces multiple plates when samples exceed one plate", {
  fp <- fill.plate(samples_large)
  expect_gt(length(fp$plate), 1L)
})

test_that("fill.plate all samples are present across all plates", {
  fp <- fill.plate(samples_large)
  all_wells <- unlist(lapply(fp$plate, function(m) c(m)[c(m) != ""]),
                      use.names = FALSE)
  expect_equal(sort(all_wells), sort(samples_large))
})

# --- labels ---

test_that("fill.plate without labels has no $label slot", {
  fp <- fill.plate(samples_small)
  expect_null(fp$label)
})

test_that("fill.plate with labels stores $label with plate class", {
  labels <- paste0("lbl_", seq_along(samples_small))
  fp <- fill.plate(samples_small, labels = labels)
  expect_true("plate" %in% class(fp$label))
})

test_that("fill.plate $label has same number of plates as $plate", {
  labels <- paste0("lbl_", seq_along(samples_large))
  fp <- fill.plate(samples_large, labels = labels)
  expect_equal(length(fp$label), length(fp$plate))
})

test_that("fill.plate errors when labels length differs from samples length", {
  expect_error(fill.plate(samples_small, labels = c("a", "b")))
})

# --- plate names ---

test_that("fill.plate uses custom plate_names", {
  fp <- fill.plate(samples_small, plate_names = c("X", "Y"))
  expect_equal(names(fp$plate), "X")
})

# --- row letters ---

test_that("fill.plate with row_letters = TRUE sets letter rownames", {
  fp <- fill.plate(samples_small, row_letters = TRUE)
  expect_equal(rownames(fp$plate[[1]]), LETTERS[1:6])
})

# --- input validation ---

test_that("fill.plate errors when ncols is not a whole number", {
  expect_error(fill.plate(samples_small, ncols = 3.5))
})

test_that("fill.plate errors when nrows is not a whole number", {
  expect_error(fill.plate(samples_small, nrows = 2.7))
})
