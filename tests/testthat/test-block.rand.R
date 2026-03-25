# Balanced 3-group data frame, 4 replicates each
df <- data.frame(
  id = 1:12,
  group = rep(c("A", "B", "C"), 4)
)

# --- class and structure ---

test_that("block.rand returns a block.rand data frame", {
  result <- suppressWarnings(block.rand(df, group))
  expect_true(inherits(result, "block.rand"))
  expect_true(is.data.frame(result))
})

test_that("block.rand result has required columns", {
  result <- suppressWarnings(block.rand(df, group))
  expect_true(all(
    c("block.id", "block.size", "unique_group", "unique_group_id") %in%
      names(result)
  ))
})

test_that("block.rand preserves all rows", {
  result <- suppressWarnings(block.rand(df, group))
  expect_equal(nrow(result), nrow(df))
})

# --- correctness ---

test_that("block.rand assigns every row a block.id", {
  result <- suppressWarnings(block.rand(df, group))
  expect_false(any(is.na(result$block.id)))
})

test_that("block.rand block.size reflects actual block row count", {
  result <- suppressWarnings(block.rand(df, group))
  computed <- tapply(result$block.id, result$block.id, length)
  expect_true(all(result$block.size == computed[result$block.id]))
})

test_that("block.rand unique_group_id is unique within each unique_group", {
  result <- suppressWarnings(block.rand(df, group))
  dupes <- duplicated(result$unique_group_id)
  expect_false(any(dupes))
})

# --- multi-covariate ---

test_that("block.rand handles multiple covariates", {
  df2 <- data.frame(
    id = 1:12,
    group = rep(c("A", "B"), 6),
    condition = rep(c("ctrl", "treat"), each = 6)
  )
  result <- suppressWarnings(block.rand(df2, group, condition))
  expect_true(inherits(result, "block.rand"))
  expect_true("unique_group" %in% names(result))
  expect_true(all(grepl("_", result$unique_group)))
})

# --- seed reproducibility ---

test_that("block.rand produces identical results with same seed", {
  r1 <- suppressWarnings(block.rand(df, group, seed = 42))
  r2 <- suppressWarnings(block.rand(df, group, seed = 42))
  expect_equal(r1$block.id, r2$block.id)
})

test_that("block.rand may produce different results with different seeds", {
  r1 <- suppressWarnings(block.rand(df, group, seed = 1))
  r2 <- suppressWarnings(block.rand(df, group, seed = 99))
  # Not guaranteed to differ, but usually will across random seeds
  expect_true(inherits(r1, "block.rand") && inherits(r2, "block.rand"))
})

# --- errors ---

test_that("block.rand errors on input that cannot be coerced to a data frame", {
  expect_error(block.rand("not a table", group))
})

test_that("block.rand accepts a tibble", {
  tbl <- tibble::tibble(id = 1:12, group = rep(c("A", "B", "C"), 4))
  result <- suppressWarnings(block.rand(tbl, group))
  expect_true(inherits(result, "block.rand"))
  expect_equal(nrow(result), 12)
})

test_that("block.rand accepts a named matrix by coercing to data frame", {
  mat <- matrix(
    c(1:12, rep(c("A", "B", "C"), 4)),
    ncol = 2,
    dimnames = list(NULL, c("id", "group"))
  )
  result <- suppressWarnings(block.rand(mat, group))
  expect_true(inherits(result, "block.rand"))
  expect_equal(nrow(result), 12)
})

test_that("block.rand errors when no covariates are given", {
  expect_error(block.rand(df))
})

test_that("block.rand errors when smallest group is too small for 2 blocks", {
  df_small <- data.frame(id = 1:3, group = c("A", "A", "B"))
  expect_error(suppressWarnings(block.rand(df_small, group)))
})

# --- warnings ---

test_that("block.rand warns when a group is below min_block_size", {
  # A=2, B=2; both < min_block_size=3 → "fewer than" warning; equal sizes → no Unequal warning
  df_small_group <- data.frame(id = 1:4, group = c(rep("A", 2), rep("B", 2)))
  expect_warning(
    block.rand(df_small_group, group, min_block_size = 3),
    regexp = "fewer than"
  )
})

test_that("block.rand warns when group sizes are unequal", {
  # A=8, B=4; A > B → Unequal warning; 8%%4=0 and 4%%4=0 → no divisible warning
  df_unequal <- data.frame(id = 1:12, group = c(rep("A", 8), rep("B", 4)))
  expect_warning(
    block.rand(df_unequal, group),
    regexp = "Unequal"
  )
})

test_that("block.rand warns when group size is not divisible by n_arms", {
  # A=5, B=5; block_size=2 → n_arms=floor(5/2)=2; 5%%2=1 → divisible warning; A=B → no Unequal warning
  df_remainder <- data.frame(id = 1:10, group = c(rep("A", 5), rep("B", 5)))
  expect_warning(
    block.rand(df_remainder, group, block_size = 2),
    regexp = "divisible"
  )
})

# --- max_unique_prop ---

test_that("block.rand errors when unique combinations exceed max_unique_prop", {
  # 6 unique groups out of 6 rows → prop = 1.0 > default 0.5
  df_all_unique <- data.frame(id = 1:6, group = c("A", "B", "C", "D", "E", "F"))
  expect_error(block.rand(df_all_unique, group), regexp = "Too many unique")
})

test_that("block.rand errors when unique prop exceeds custom max_unique_prop", {
  # 3 groups out of 6 rows = 0.5; strict threshold 0.4 triggers error
  df3 <- data.frame(id = 1:6, group = rep(c("A", "B", "C"), 2))
  expect_error(
    block.rand(df3, group, max_unique_prop = 0.4),
    regexp = "Too many unique"
  )
})

# --- block_size / n_blocks ---

test_that("block_size controls samples per group per block", {
  # A=4, B=4, C=4; block_size=2 → n_arms=floor(4/2)=2
  result <- suppressWarnings(block.rand(df, group, block_size = 2))
  expect_equal(length(unique(result$block.id)), 2)
})

test_that("n_blocks directly sets the number of blocks", {
  result <- suppressWarnings(block.rand(df, group, n_blocks = 2))
  expect_equal(length(unique(result$block.id)), 2)
})

test_that("block.rand errors when both block_size and n_blocks are given", {
  expect_error(block.rand(df, group, block_size = 2, n_blocks = 2))
})

test_that("block.rand errors when block_size makes n_arms < 2", {
  # A=3, B=3; block_size=2 → n_arms=floor(3/2)=1 < 2
  df_bs_err <- data.frame(id = 1:6, group = rep(c("A", "B"), 3))
  expect_error(block.rand(df_bs_err, group, block_size = 2))
})

test_that("block.rand errors when n_blocks < 2", {
  expect_error(block.rand(df, group, n_blocks = 1))
})

# --- seed = NULL ---

test_that("block.rand works without a seed", {
  result <- suppressWarnings(block.rand(df, group, seed = NULL))
  expect_true(inherits(result, "block.rand"))
  expect_equal(nrow(result), nrow(df))
})

# --- output properties ---

test_that("block.rand output is sorted by block.id", {
  result <- suppressWarnings(block.rand(df, group))
  expect_equal(result$block.id, sort(result$block.id))
})

test_that("block.rand block.id is zero-padded numeric string", {
  result <- suppressWarnings(block.rand(df, group))
  expect_true(all(grepl("^[0-9]+$", result$block.id)))
  expect_true(all(nchar(result$block.id) >= 2))
})

# --- covariate types ---

test_that("block.rand works with factor covariates", {
  df_factor <- data.frame(id = 1:12, group = factor(rep(c("A", "B", "C"), 4)))
  result <- suppressWarnings(block.rand(df_factor, group))
  expect_true(inherits(result, "block.rand"))
  expect_equal(nrow(result), 12)
})

test_that("block.rand works when all rows share the same covariate value", {
  df_one_group <- data.frame(id = 1:8, group = rep("A", 8))
  result <- suppressWarnings(block.rand(df_one_group, group))
  expect_true(inherits(result, "block.rand"))
  expect_equal(nrow(result), 8)
})

# --- unique_group_id correctness ---

test_that("block.rand unique_group_id follows 'group_N' format", {
  result <- suppressWarnings(block.rand(df, group))
  expect_true(all(grepl("^.+_[0-9]+$", result$unique_group_id)))
})

test_that("block.rand unique_group_id numbers each member within its group", {
  result <- suppressWarnings(block.rand(df, group))
  a_ids <- sort(result$unique_group_id[result$unique_group == "A"])
  expect_equal(a_ids, paste0("A_", 1:4))
})

# --- original columns preserved ---

test_that("block.rand preserves original column names and values", {
  result <- suppressWarnings(block.rand(df, group))
  expect_true(all(c("id", "group") %in% names(result)))
  expect_equal(sort(result$id), sort(df$id))
})

# --- numeric covariate ---

test_that("block.rand works with a numeric covariate column", {
  df_num <- data.frame(id = 1:12, group = rep(c(1, 2, 3), 4))
  result <- suppressWarnings(block.rand(df_num, group))
  expect_true(inherits(result, "block.rand"))
  expect_equal(nrow(result), 12)
})

# --- max_unique_prop boundary ---

test_that("block.rand passes when unique prop equals max_unique_prop exactly", {
  # 3 groups / 6 rows = 0.5; check is >, so 0.5 > 0.5 is FALSE → no error
  df3 <- data.frame(id = 1:6, group = rep(c("A", "B", "C"), 2))
  expect_no_error(suppressWarnings(block.rand(df3, group)))
})

# --- seed edge cases ---

test_that("block.rand works with seed = 0", {
  result <- suppressWarnings(block.rand(df, group, seed = 0))
  expect_true(inherits(result, "block.rand"))
})

# --- min_block_size = 1 never warns ---

test_that("block.rand does not warn for min_block_size = 1", {
  # groups of size 2; min_block_size=1 → no "fewer than" warning
  df_tiny <- data.frame(id = 1:4, group = c(rep("A", 2), rep("B", 2)))
  expect_no_warning(block.rand(df_tiny, group, min_block_size = 1))
})

# --- multi-covariate: unique_group values ---

test_that("block.rand multi-covariate unique_group contains correct combinations", {
  df2 <- data.frame(
    id = 1:8,
    grp  = rep(c("X", "Y"), 4),
    cond = rep(c("ctrl", "treat"), each = 4)
  )
  result <- suppressWarnings(block.rand(df2, grp, cond))
  expect_true(all(result$unique_group %in% c("X_ctrl", "X_treat", "Y_ctrl", "Y_treat")))
})

# --- n_blocks at boundary ---

test_that("block.rand works when n_blocks equals group size", {
  # A=B=C=4; n_blocks=4 → one sample per group per block
  result <- suppressWarnings(block.rand(df, group, n_blocks = 4))
  expect_equal(length(unique(result$block.id)), 4)
})
