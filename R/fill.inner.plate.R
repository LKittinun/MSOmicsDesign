#' @title Fill a single plate with data
#' @description
#'   This function fills a single plate with the provided data.
#'
#' @param samples A vector of data to fill the plate
#' @param nrows Number of rows in a plate
#' @param ncols Number of columns in a plate
#' @param row_letters If `TRUE` will display `rownames` as alphabets
#' @param plate_names A vector of plate names to be used
#' @param exclude Well positions to reserve (character vector or named list)
#' @param exclude_label Label placed in reserved wells
#'
#' @return A list of matrices representing filled plates

fill.inner.plate <- function(
  samples,
  nrows,
  ncols,
  row_letters,
  plate_names,
  exclude = NULL,
  exclude_label = "iQC"
) {
  # --- helpers ---
  get_plate_excl <- function(plate_name) {
    if (is.null(exclude)) {
      return(character(0))
    }
    if (is.character(exclude)) {
      return(exclude)
    } # all plates
    excl <- character(0)
    if (!is.null(exclude[["all"]])) {
      excl <- c(excl, exclude[["all"]])
    }
    if (!is.null(exclude[[plate_name]])) {
      excl <- c(excl, exclude[[plate_name]])
    }
    excl
  }

  .fill.one <- function(plts, plate_excl) {
    plt_mat <- matrix("", nrow = nrows, ncol = ncols)
    colnames(plt_mat) <- seq_len(ncols)
    if (row_letters) {
      rownames(plt_mat) <- LETTERS[seq_len(nrows)]
    }
    for (pos in plate_excl) {
      row_part <- sub("^([A-Za-z]+|[0-9]+)(.*)$", "\\1", pos)
      col_part <- sub("^([A-Za-z]+|[0-9]+)(.*)$", "\\2", pos)
      plt_mat[row_part, col_part] <- exclude_label
    }
    avail <- which(c(t(plt_mat)) == "") # row-major available indices
    tmp <- t(plt_mat)
    tmp[avail[seq_along(plts)]] <- plts
    plt_mat <- t(tmp)
    if (row_letters) {
      class(plt_mat) <- c("plate", class(plt_mat))
    }
    plt_mat
  }

  # --- iterative plate filling ---
  plt_mats <- list()
  remaining <- samples
  plate_idx <- 1L

  while (length(remaining) > 0L) {
    pname <- plate_names[((plate_idx - 1L) %% length(plate_names)) + 1L]
    excl <- get_plate_excl(pname)
    capacity <- nrows * ncols - length(excl)
    n_take <- min(length(remaining), capacity)
    plt_mats[[plate_idx]] <- .fill.one(remaining[seq_len(n_take)], excl)
    names(plt_mats)[plate_idx] <- pname
    remaining <- remaining[-seq_len(n_take)]
    plate_idx <- plate_idx + 1L
  }

  class(plt_mats) <- c("plate", class(plt_mats))
  plt_mats
}
