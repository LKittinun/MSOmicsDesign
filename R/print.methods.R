#' @title Print method for plate_list objects
#' @description Concise summary of a `plate_list` instead of a raw list dump.
#'
#' @param x A `plate_list` object returned by [fill.plate()].
#' @param ... Further arguments (ignored).
#'
#' @return `x` invisibly.
#'
#' @family plate
#' @export

print.plate_list <- function(x, ...) {
  n_plates <- length(x$plate)
  dims <- dim(x$plate[[1]])
  n_filled <- sum(unlist(x$plate) != "")
  n_total <- n_plates * dims[1] * dims[2]
  has_labels <- !is.null(x$label)
  cat(sprintf(
    "<plate_list>\n  Plates  : %d (%d rows x %d cols, %d wells each)\n  Filled  : %d / %d wells\n  Labels  : %s\n",
    n_plates,
    dims[1],
    dims[2],
    dims[1] * dims[2],
    n_filled,
    n_total,
    if (has_labels) "yes" else "no"
  ))
  invisible(x)
}

#' @title Print method for block.rand objects
#' @description Shows a brief header then delegates to the data-frame print.
#'
#' @param x A `block.rand` object returned by [block.rand()].
#' @param ... Further arguments passed to [NextMethod()].
#'
#' @return `x` invisibly.
#'
#' @family block
#' @export

print.block.rand <- function(x, ...) {
  n_blocks <- length(unique(x$block.id))
  n_groups <- length(unique(x$unique_group))
  cat(sprintf(
    "<block.rand>  %d samples | %d unique groups | %d blocks\n",
    nrow(x),
    n_groups,
    n_blocks
  ))
  NextMethod()
  invisible(x)
}

#' @title Print method for mzdf objects
#' @description Shows a brief header then delegates to the data-frame print.
#'
#' @param x A `mzdf` object returned by [DIA.windows()].
#' @param ... Further arguments passed to [NextMethod()].
#'
#' @return `x` invisibly.
#'
#' @family m/z
#' @export
print.mzdf <- function(x, ...) {
  cat(sprintf("<mzdf>  %d windows\n", nrow(x)))
  NextMethod()
  invisible(x)
}
