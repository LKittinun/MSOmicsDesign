#' Plot a plate list
#'
#' @importFrom purrr map map2
#'
#' @param x A `plate_list` object returned by `fill.plate()`.
#' @param labels Optional. Either a flat character vector (split across plates
#'   automatically) or a `plate`-class list returned by `fill.plate()$label`
#'   (passed directly, one matrix per plate). If `NULL`, wells are labelled
#'   with their sample values.
#' @param control.suffix Character vector of control prefixes (e.g. `"QC"`).
#'   Strips trailing `_<number>` suffixes so all QC replicates share one colour.
#' @param text If `TRUE` (default), sample names are printed inside each well.
#' @param return_df If `TRUE`, each list element is `list(plot, data.frame)`
#'   instead of a plain ggplot object.
#' @param ... Currently unused; included for S3 method compatibility.
#'
#' @returns A list of `ggplot` objects, one per plate. If `return_df = TRUE`,
#'   each element is `list(plot, data.frame)`.
#' @export
#'
#' @family plate
#'
#' @examples
#' samples <- addQC(LETTERS[1:20], interval = 6)
#' fp <- fill.plate(samples)
#'
#' # No labels
#' plot(fp)
#'
#' # Plate-structure labels from fill.plate()
#' fp2 <- fill.plate(samples, labels = paste0("lbl_", seq_along(samples)))
#' plot(fp2, labels = fp2$label)

plot.plate_list <- function(
  x,
  labels = NULL,
  control.suffix = NULL,
  text = TRUE,
  return_df = FALSE,
  ...
) {
  plate_list <- x
  if (!is.null(labels)) {
    if ("plate" %in% class(labels)) {
      # labels is a plate structure from fill.plate()$label — already per-plate matrices
      map2(
        plate_list$plate,
        labels,
        ~ plot.plate(
          .x,
          labels = .y,
          control.suffix = control.suffix,
          text = text,
          return_df = return_df
        )
      )
    } else {
      # labels is a flat vector — split into per-plate chunks
      tot_wells <- length(unlist(plate_list$plate))
      if (length(labels) < tot_wells) {
        labels <- c(labels, rep("", tot_wells - length(labels)))
      }
      labels <- split(
        labels,
        ceiling(seq_along(labels) / length(plate_list$plate[[1]]))
      )
      map2(
        plate_list$plate,
        labels,
        ~ plot.plate(
          .x,
          labels = .y,
          control.suffix = control.suffix,
          text = text,
          return_df = return_df
        )
      )
    }
  } else {
    map(
      plate_list$plate,
      ~ plot.plate(
        .x,
        labels = NULL,
        control.suffix = control.suffix,
        text = text,
        return_df = return_df
      )
    )
  }
}
