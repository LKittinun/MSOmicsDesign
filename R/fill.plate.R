#' @title Fill plates with sample sequence.
#' @description
#'   This function will insert a vector of samples sequence in to plate matrix with specified rows and columns.
#'   If number of samples exceeded a plate, the function will generate next plate automatically.
#'
#' @param samples A vector containing sample names
#' @param nrows Number of rows in a plate
#' @param ncols Number of columns in a plate
#' @param row_letters If `TRUE` will display `rownames` as alphabets
#' @param labels Optional, additional labels matrices.
#' @param plate_names Vectors of plate names, will recycle if number of plates exceed names length
#' @param addQC If `TRUE` will addQC by function `addQC`
#' @param addQC.args Arguments for `addQC` functions
#' @param exclude Well positions to reserve on each plate. Either a character
#'   vector of positions (e.g. \code{c("A1","B3")}) applied to every plate, or
#'   a named list for per-plate control (e.g.
#'   \code{list(all = "A1", R = "B3")}). The \code{"all"} list key applies to
#'   every plate. Reserved wells are filled with \code{exclude_label} and
#'   skipped when placing samples. Default \code{NULL} (no reserved wells).
#' @param exclude_label Label placed in reserved wells. Default \code{"iQC"}.
#'
#' @return
#' * `plate` A list of matrix containing samples filled in plates. If samples exceed a set of `plate_names` it will recycle into a new list.
#' * `labels` A list of matrix containing their respective labels for samples. Only return if `labels` is not `NULL`
#' * `data` An original sample sequence
#'
#' @family plate
#' @export
#'
#' @examples
#' samples <- LETTERS[1:20]
#' fill.plate(samples)
#'
#' samples <- addQC(LETTERS[1:20], interval = 6)
#' fill.plate(samples)
#'
#' ## Can change plate names
#' fill.plate(samples, plate_names = c("B","Z","K","O"))
#'
#' ## When samples exceed a single plate (default 6 x 9 = 54 wells), additional
#' ## plates are created automatically and named by recycling plate_names
#' big_samples <- paste0("S", 1:100)
#' result <- fill.plate(big_samples)
#' length(result$plate)   # 2 plates: "R" (54 samples) and "G" (46 samples)
#' result$plate[["R"]]    # first plate matrix
#' result$plate[["G"]]    # second plate matrix (partially filled)
#'
#' ## Reserve position A1 on every plate as instrument QC
#' fill.plate(paste0("S", 1:20), exclude = "A1")
#'
#' ## Per-plate: different exclusions on plate R vs plate G
#' fill.plate(paste0("S", 1:100),
#'            exclude = list(all = "A1", G = "B3"))

fill.plate <- function(
  samples = NULL,
  labels = NULL,
  nrows = 6,
  ncols = 9,
  row_letters = TRUE,
  plate_names = c("R", "G", "B", "Y"),
  addQC = FALSE,
  addQC.args = list(),
  exclude = NULL,
  exclude_label = "iQC"
) {
  .addQC <- addQC # save logical before it could be confused with the function
  if (!is.vector(samples)) {
    samples <- try(as.character(samples))
  }

  stopifnot(
    "Please insert a vector of samples" = is.vector(samples),
    "Please insert plate names in vector format" = is.vector(plate_names),
    "Please insert nrows and ncols as whole numbers" = suppressWarnings(all(
      nrows == floor(nrows),
      ncols == floor(ncols)
    )),
    "addQC must be TRUE or FALSE" = is.logical(addQC) && length(addQC) == 1
  )

  if (!is.null(exclude)) {
    excl_vec <- if (is.character(exclude)) {
      exclude
    } else {
      unlist(exclude, use.names = FALSE)
    }
    stopifnot(
      "exclude must be a character vector or named list of well positions" = is.character(
        excl_vec
      ),
      "excluded positions cannot exceed total wells per plate" = length(unique(
        excl_vec
      )) <
        nrows * ncols
    )
  }

  if (!is.null(labels)) {
    stopifnot(
      "Labels must have the same length as samples" = length(labels) ==
        length(samples)
    )
  }

  if (.addQC) {
    if (!is.null(samples)) {
      addQC.args$samples <- samples
      samples <- do.call("addQC", addQC.args)
    }
    if (!is.null(labels)) {
      addQC.args$samples <- labels
      labels <- do.call("addQC", addQC.args)
    }
  }

  plt_mats <- fill.inner.plate(
    samples,
    nrows,
    ncols,
    row_letters,
    plate_names,
    exclude = exclude,
    exclude_label = exclude_label
  )

  result <- list("plate" = plt_mats)

  if (!is.null(labels)) {
    label_plates <- fill.inner.plate(
      labels,
      nrows,
      ncols,
      row_letters,
      plate_names,
      exclude = exclude,
      exclude_label = exclude_label
    )
    result$label <- label_plates
  }

  result$data <- list(samples = samples)

  if (!is.null(labels)) {
    result$data$labels <- labels
  }
  class(result) <- c("plate_list", class(result))
  return(result)
}
