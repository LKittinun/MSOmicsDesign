#' @title Merge two plate_list objects
#' @description
#'   Combines two `plate_list` objects (e.g. from two batches or two calls to
#'   [fill.plate()]) into a single `plate_list`. Plate names are deduplicated
#'   by appending a batch suffix when there are conflicts.
#'
#' @param x A `plate_list` object.
#' @param y A `plate_list` object.
#' @param suffixes Length-2 character vector of suffixes appended to conflicting
#'   plate names. Default `c("_1", "_2")`.
#' @param ... Currently unused.
#'
#' @return A `plate_list` with combined plates, labels (if present in both),
#'   and data slots.
#'
#' @family plate
#' @method merge plate_list
#' @export
#'
#' @examples
#' a <- fill.plate(paste0("S", 1:20))
#' b <- fill.plate(paste0("S", 21:40))
#' merged <- merge.plate_list(a, b)
#' length(merged$plate)
#'
#' ## Conflicting plate names get suffixes
#' c1 <- fill.plate(paste0("S", 1:60))
#' c2 <- fill.plate(paste0("S", 61:120))
#' merged2 <- merge.plate_list(c1, c2)
#' names(merged2$plate)

merge.plate_list <- function(x, y, suffixes = c("_1", "_2"), ...) {
  stopifnot(
    "x must be a plate_list" = inherits(x, "plate_list"),
    "y must be a plate_list" = inherits(y, "plate_list"),
    "suffixes must be a length-2 character vector" = is.character(suffixes) &&
      length(suffixes) == 2
  )

  x_names <- names(x$plate)
  y_names <- names(y$plate)
  conflicts <- intersect(x_names, y_names)

  if (length(conflicts) > 0) {
    names(x$plate)[x_names %in% conflicts] <- paste0(
      names(x$plate)[x_names %in% conflicts],
      suffixes[1]
    )
    names(y$plate)[y_names %in% conflicts] <- paste0(
      names(y$plate)[y_names %in% conflicts],
      suffixes[2]
    )
    if (!is.null(x$label)) {
      names(x$label)[x_names %in% conflicts] <- paste0(
        names(x$label)[x_names %in% conflicts],
        suffixes[1]
      )
    }
    if (!is.null(y$label)) {
      names(y$label)[y_names %in% conflicts] <- paste0(
        names(y$label)[y_names %in% conflicts],
        suffixes[2]
      )
    }
  }

  result <- list(
    plate = c(x$plate, y$plate)
  )

  has_labels_x <- !is.null(x$label)
  has_labels_y <- !is.null(y$label)

  if (has_labels_x && has_labels_y) {
    result$label <- c(x$label, y$label)
  } else if (has_labels_x || has_labels_y) {
    warning(
      "Only one plate_list has labels; labels slot dropped in merged result."
    )
  }

  result$data <- list(
    samples = c(x$data$samples, y$data$samples)
  )

  if (!is.null(x$data$labels) && !is.null(y$data$labels)) {
    result$data$labels <- c(x$data$labels, y$data$labels)
  }

  class(result) <- c("plate_list", class(result))
  result
}
#' @rdname merge.plate_list
#' @export
merge.plate <- function(x, y, suffixes = c("_1", "_2"), ...) {
  merge.plate_list(x, y, suffixes = suffixes, ...)
}

#' @rdname merge.plate_list
#' @export
merge_plate <- function(x, y, suffixes = c("_1", "_2"), ...) {
  merge.plate_list(x, y, suffixes = suffixes, ...)
}
