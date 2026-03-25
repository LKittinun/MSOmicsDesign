#' @title Check block distribution
#' @description
#' Check a covariates distribution in each block.
#'
#' @importFrom dplyr select pull group_map group_by
#' @importFrom janitor tabyl
#' @importFrom purrr set_names
#'
#' @param df An output object form block.rand()
#' @param block A column containing block id
#' @param group A column containing unique covariates
#'
#' @return
#' A list of table contains distribution of covariates in each block.
#'
#' @family randomize
#'
#' @export
#'
#' @examples
#' data(ccc)
#' ccc_block <- block.rand(ccc, time, response)
#' check.block(ccc_block)
#'

check.block <- function(df, block = block.id, group = unique_group){
  if(!("block.rand" %in% class(df)) ){
    stop("Please use block randomized result from block.rand()")
  }

  block_names <- df |> select({{block}}) |> pull() |> unique()

  df |>
    group_by({{block}}) |>
    group_map(~tabyl(.x, {{group}}) |>
                mutate(percent = round(percent*100,2))) |>
    set_names(block_names)

}



