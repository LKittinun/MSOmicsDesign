# --- print.plate_list ---

test_that("print.plate_list produces <plate_list> header", {
  fp  <- fill.plate(LETTERS[1:20])
  out <- capture.output(print(fp))
  expect_true(any(grepl("<plate_list>", out)))
})

test_that("print.plate_list returns x invisibly", {
  fp     <- fill.plate(LETTERS[1:20])
  result <- withVisible(print(fp))
  expect_false(result$visible)
  expect_identical(result$value, fp)
})

test_that("print.plate_list reports correct plate count", {
  fp  <- fill.plate(paste0("S", 1:60))   # 60 samples → 2 plates on default 6×9
  out <- paste(capture.output(print(fp)), collapse = "\n")
  expect_match(out, "Plates\\s*:\\s*2")
})

test_that("print.plate_list reports labels: yes when labels present", {
  fp  <- fill.plate(LETTERS[1:10], labels = paste0("lbl_", 1:10))
  out <- paste(capture.output(print(fp)), collapse = "\n")
  expect_match(out, "Labels\\s*:\\s*yes")
})

test_that("print.plate_list reports labels: no when no labels", {
  fp  <- fill.plate(LETTERS[1:10])
  out <- paste(capture.output(print(fp)), collapse = "\n")
  expect_match(out, "Labels\\s*:\\s*no")
})

# --- print.mzdf ---

test_that("print.mzdf produces <mzdf> header", {
  mz  <- DIA.windows(400, 1000, windows.width = 20)
  out <- capture.output(print(mz))
  expect_true(any(grepl("<mzdf>", out)))
})

test_that("print.mzdf returns x invisibly", {
  mz     <- DIA.windows(400, 1000, windows.width = 20)
  result <- withVisible(print(mz))
  expect_false(result$visible)
  expect_identical(result$value, mz)
})

test_that("print.mzdf reports correct window count", {
  mz  <- DIA.windows(400, 1000, windows.width = 20)
  n   <- nrow(mz)
  out <- paste(capture.output(print(mz)), collapse = "\n")
  expect_match(out, as.character(n))
})

# --- print.block.rand ---

test_that("print.block.rand produces <block.rand> header", {
  br  <- block.rand(ccc, time, response)
  out <- capture.output(print(br))
  expect_true(any(grepl("<block.rand>", out)))
})

test_that("print.block.rand returns x invisibly", {
  br     <- block.rand(ccc, time, response)
  result <- withVisible(print(br))
  expect_false(result$visible)
  expect_identical(result$value, br)
})

test_that("print.block.rand reports correct row count", {
  br  <- block.rand(ccc, time, response)
  out <- paste(capture.output(print(br)), collapse = "\n")
  expect_match(out, as.character(nrow(br)))
})
