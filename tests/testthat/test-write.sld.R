make_fp <- function(with_labels = FALSE) {
  samples <- addQC(LETTERS[1:10], interval = 5)
  if (with_labels) {
    fill.plate(samples, labels = paste0("lbl_", seq_along(samples)))
  } else {
    fill.plate(samples)
  }
}

# --- errors ---

test_that("write.sld errors when filename is missing", {
  fp <- make_fp()
  expect_error(write.sld(fp))
})

# --- no labels: falls back to $data$samples ---

test_that("write.sld uses samples when no labels in fill.plate result", {
  fp  <- make_fp(with_labels = FALSE)
  f   <- tempfile(fileext = ".csv")
  df  <- write.sld(fp, filename = f, return_df = TRUE)
  expect_true(file.exists(f))
  expect_true("Sample Name" %in% names(df))
  expect_equal(nrow(df), length(fp$data$samples))
  unlink(f)
})

# --- with labels ---

test_that("write.sld uses labels when present in fill.plate result", {
  fp  <- make_fp(with_labels = TRUE)
  f   <- tempfile(fileext = ".csv")
  df  <- write.sld(fp, filename = f, return_df = TRUE)
  expect_equal(df$`Sample Name`, fp$data$labels)
  unlink(f)
})

# --- output structure ---

test_that("write.sld result has required Xcalibur columns", {
  fp <- make_fp()
  f  <- tempfile(fileext = ".csv")
  df <- write.sld(fp, filename = f, return_df = TRUE)
  expected_cols <- c("Sample Type", "Sample Name", "Sample ID",
                     "Path", "Inst Meth", "Proc Meth", "Position", "Inj Vol")
  expect_true(all(expected_cols %in% names(df)))
  unlink(f)
})

test_that("write.sld auto-detects QC sample type", {
  fp <- make_fp()
  f  <- tempfile(fileext = ".csv")
  df <- write.sld(fp, filename = f, return_df = TRUE)
  qc_rows <- df[grepl("^QC", df$`Sample Name`), ]
  expect_true(all(qc_rows$`Sample Type` == "QC"))
  unlink(f)
})

test_that("write.sld non-QC rows are typed as Unknown", {
  fp <- make_fp()
  f  <- tempfile(fileext = ".csv")
  df <- write.sld(fp, filename = f, return_df = TRUE)
  other_rows <- df[!grepl("^QC|blank", df$`Sample Name`, ignore.case = TRUE), ]
  expect_true(all(other_rows$`Sample Type` == "Unknown"))
  unlink(f)
})

test_that("write.sld written file starts with Bracket Type header", {
  fp   <- make_fp()
  f    <- tempfile(fileext = ".csv")
  write.sld(fp, filename = f, return_df = FALSE)
  first_line <- readLines(f, n = 1)
  expect_equal(first_line, "Bracket Type=1")
  unlink(f)
})

test_that("write.sld return_df = FALSE returns NULL invisibly", {
  fp <- make_fp()
  f  <- tempfile(fileext = ".csv")
  result <- write.sld(fp, filename = f, return_df = FALSE)
  expect_null(result)
  unlink(f)
})

# --- inj_vol ---

test_that("write.sld passes inj_vol value into Inj Vol column", {
  fp  <- make_fp()
  f   <- tempfile(fileext = ".csv")
  df  <- write.sld(fp, filename = f, return_df = TRUE, inj_vol = 2)
  expect_true(all(df$`Inj Vol` == 2))
  unlink(f)
})

test_that("write.sld leaves Inj Vol empty when inj_vol is NULL", {
  fp  <- make_fp()
  f   <- tempfile(fileext = ".csv")
  df  <- write.sld(fp, filename = f, return_df = TRUE)
  expect_true(all(df$`Inj Vol` == ""))
  unlink(f)
})
