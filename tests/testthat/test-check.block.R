df <- data.frame(
  id    = 1:12,
  group = rep(c("A", "B", "C"), 4)
)

test_that("check.block returns a list", {
  br <- suppressWarnings(block.rand(df, group))
  result <- check.block(br)
  expect_type(result, "list")
})

test_that("check.block list length equals number of blocks", {
  br <- suppressWarnings(block.rand(df, group))
  result <- check.block(br)
  n_blocks <- length(unique(br$block.id))
  expect_equal(length(result), n_blocks)
})

test_that("check.block list names match block IDs", {
  br <- suppressWarnings(block.rand(df, group))
  result <- check.block(br)
  expect_equal(sort(names(result)), sort(unique(br$block.id)))
})

test_that("check.block each table has a percent column", {
  br <- suppressWarnings(block.rand(df, group))
  result <- check.block(br)
  for (tbl in result) {
    expect_true("percent" %in% names(tbl))
  }
})

test_that("check.block errors on non-block.rand input", {
  expect_error(check.block(df))
})
