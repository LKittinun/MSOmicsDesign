#' Example clinical covariates dataset
#'
#' A small example data frame used to demonstrate block randomisation.
#'
#' @usage data(ccc)
#' @format A data frame with sample metadata columns including:
#' \describe{
#'   \item{sample}{Sample identifier}
#'   \item{time}{Time-point covariate}
#'   \item{response}{Response-group covariate}
#' }
"ccc"

#' Example staggered DIA isolation list
#'
#' A data frame containing a staggered DIA window isolation list used to
#' demonstrate GPF window partitioning.
#'
#' @usage data(staggered_mz)
#' @format A data frame with one column:
#' \describe{
#'   \item{range}{Isolation window range as a character string (e.g. \code{"400-420"})}
#' }
"staggered_mz"
