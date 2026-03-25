#' @title Block randomization from a given data.frame
#' @description
#' Assigns samples to randomized blocks while balancing across covariate groups.
#' One or more columns are used as covariates; every unique combination of
#' covariate values defines a group, and samples are distributed evenly across
#' blocks within each group. Internally uses `randomizr::block_ra()`.
#'
#' @importFrom dplyr mutate select n join_by left_join rename arrange relocate count filter pick
#' @importFrom purrr partial reduce
#' @importFrom utils capture.output
#' @importFrom stringr str_pad
#' @importFrom randomizr block_ra
#'
#' @param df A data frame, tibble, data.table, or any object coercible to a
#'   data frame (e.g. a named matrix) containing sample identifiers and metadata.
#' @param ... Unquoted column names to use as covariates. All unique
#'   combinations of these columns define the groups used for block
#'   randomization.
#' @param block_size Target number of samples per covariate group per block.
#'   More samples per block means fewer, larger blocks. Default is `1` (one
#'   sample per group per block, maximising the number of blocks). Specify
#'   only one of `block_size` or `n_blocks`.
#' @param n_blocks Number of blocks to create. Directly sets the number of
#'   blocks regardless of group size. Specify only one of `block_size` or
#'   `n_blocks`.
#' @param min_block_size Minimum acceptable number of samples in any
#'   covariate group. Groups with fewer members than this threshold trigger
#'   a warning because their small size may lead to unreliable randomization.
#'   Default is `2`. This check is advisory only and does not stop execution
#'   unless the group is also too small to form 2 blocks (see `block_size`
#'   and `n_blocks`).
#' @param max_unique_prop Maximum allowed ratio of unique covariate
#'   combinations to total sample count. If
#'   `(number of unique groups) / (number of rows)` exceeds this value, an
#'   error is thrown. This guards against situations where nearly every sample
#'   is its own group, which makes block randomization meaningless. Default
#'   is `0.5`.
#' @param seed Integer seed passed to `set.seed()` for reproducibility.
#'   Set to `NULL` to skip seeding. Default is `1`.
#'
#' @return
#' A data frame with class `"block.rand"` containing all original columns plus:
#' * **block.id** — zero-padded block number each sample was assigned to.
#' * **block.size** — number of samples in that block.
#' * **unique_group** — covariate combination label for each sample
#'   (columns joined by `"_"`).
#' * **unique_group_id** — unique per-sample label within each covariate
#'   group (e.g. `"ctrl_treat_1"`, `"ctrl_treat_2"`).
#'
#' @family randomize
#'
#' @export
#'
#' @examples
#' data(ccc)
#'
#' # Default: maximise blocks (block_size = 1)
#' ccc_block <- block.rand(ccc, time, response)
#' class(ccc_block)
#' ccc_block
#'
#' # Fewer, larger blocks: 2 samples per group per block
#' block.rand(ccc, time, response, block_size = 2)
#'
#' # Directly specify the number of blocks
#' block.rand(ccc, time, response, n_blocks = 3)

block.rand <- function(
  df,
  ...,
  block_size = NULL,
  n_blocks = NULL,
  min_block_size = 2,
  max_unique_prop = 0.5,
  seed = 1
) {
  if (!is.data.frame(df)) {
    df <- tryCatch(
      as.data.frame(df),
      error = function(e) {
        stop(
          "Input must be a data frame, tibble, data.table, or coercible to one."
        )
      }
    )
  }
  if (missing(..1)) {
    stop("Please include covariates.")
  }
  if (!is.null(block_size) && !is.null(n_blocks)) {
    stop("Specify only one of `block_size` or `n_blocks`, not both.")
  }

  cols <- select(df, c(...))

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Create unique group
  if (length(cols) != 1) {
    unique_df <- df |>
      mutate(
        unique_group = reduce(pick(colnames(cols)), ~ paste(.x, .y, sep = "_"))
      ) |>
      mutate(
        unique_group_id = paste0(unique_group, "_", 1:n()),
        .by = unique_group
      )
  } else {
    unique_df <- df |>
      mutate(unique_group = cols[[1]]) |>
      mutate(
        unique_group_id = paste0(unique_group, "_", 1:n()),
        .by = unique_group
      )
  }

  # Check for severe group deviation
  group_sizes <- count(unique_df, unique_group)

  total_sample_size <- nrow(unique_df)
  num_unique_groups <- nrow(group_sizes)

  if (num_unique_groups / total_sample_size > max_unique_prop) {
    stop(paste(
      "Too many unique covariate combinations relative to sample size.",
      "Consider reducing the number of covariates or categories within covariates."
    ))
  }

  if (any(group_sizes$n < min_block_size)) {
    problem_groups <- group_sizes |> filter(n < min_block_size)
    warning(paste(
      "Some groups have fewer than",
      min_block_size,
      "members, which may lead to ineffective randomization:\n",
      paste(capture.output(print(problem_groups)), collapse = "\n")
    ))
  }

  min_group_size <- min(group_sizes$n)

  if (!is.null(n_blocks)) {
    n_arms <- n_blocks
  } else {
    bs <- if (!is.null(block_size)) block_size else 1
    n_arms <- floor(min_group_size / bs)
  }

  if (n_arms < 2) {
    if (!is.null(n_blocks)) {
      stop("`n_blocks` must be at least 2.")
    } else {
      stop(paste(
        "Cannot perform block randomization: the smallest covariate group has",
        min_group_size,
        "member(s), which is too few to form 2 or more balanced blocks.",
        "Use a smaller `block_size` or merge covariate levels."
      ))
    }
  }

  # Warn when groups are unequal (larger groups will dominate some blocks)
  unequal_groups <- group_sizes |> filter(n > min_group_size)
  if (nrow(unequal_groups) > 0) {
    warning(paste(
      "Unequal group sizes detected. Blocks will not be perfectly balanced.",
      "Groups larger than the minimum (",
      min_group_size,
      "):\n",
      paste(capture.output(print(unequal_groups)), collapse = "\n")
    ))
  }

  # Warn when any group size is not divisible by n_arms (some blocks get an extra sample)
  remainder_groups <- group_sizes |> filter(n %% n_arms != 0)
  if (nrow(remainder_groups) > 0) {
    warning(paste(
      "Some group sizes are not evenly divisible by the number of blocks (",
      n_arms,
      ").",
      "Those groups will have 1 extra sample in",
      nrow(remainder_groups),
      "block(s):\n",
      paste(capture.output(print(remainder_groups)), collapse = "\n")
    ))
  }

  n_unique_group <- length(unique(unique_df$unique_group))
  message("Unique groups = ", n_unique_group, " | Blocks = ", n_arms)

  unique_df <- unique_df |>
    mutate(block.id = randomizr::block_ra(unique_group, num_arms = n_arms)) |>
    mutate(
      block.id = stringr::str_pad(gsub("T", "", block.id), width = 2, pad = "0")
    ) |>
    arrange(block.id) |>
    mutate(block.size = n(), .by = block.id)

  class(unique_df) <- c("block.rand", "data.frame")

  return(unique_df)
}
