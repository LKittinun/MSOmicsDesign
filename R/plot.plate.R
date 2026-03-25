#' @title Plot a plate
#' @description This function plot a plate colored by their groups.
#'
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr mutate inner_join
#' @importFrom forcats fct_rev
#'
#' @param x A plate matrix generated from fill.plate()
#' @param labels A vector of label in row order, can be ignored
#' @param text If `TRUE` will also display texts in the plate.
#' @param return_df If `TRUE` will also return a dataframe that is used to plot.
#' @param control.suffix Vectors of control, will remove their suffixes, which make them belong to the same group.
#' @param ... Currently unused; included for S3 method compatibility.
#'
#' @return
#' A `ggplot` object of a plate, can be further modified or saved by `ggplot2` function.
#'
#' @family plate
#' @export
#'
#' @examples
#' data(ccc)
#' block_df <- block.rand(ccc, time, response)
#' plate <- fill.plate(block_df$unique_group_id)
#' plot(plate$plate[[1]])
#' plot(plate$plate[[1]], labels = generate.position(plate)[[1]])

plot.plate <- function(
  x,
  labels = NULL,
  text = TRUE,
  control.suffix = NULL,
  return_df = FALSE,
  ...
) {
  plate <- x
  stopifnot("Data must has plate class." = ("plate" %in% class(plate)))

  plate_df <- as.data.frame(plate) |>
    rownames_to_column("row") |>
    pivot_longer(-row, names_to = "col") |>
    mutate(row = fct_rev(row)) |>
    mutate(col = factor(col, levels = colnames(plate))) |>
    mutate(value = ifelse(value == "", "Blank", value))

  if (!is.null(control.suffix)) {
    control.suffix <- paste0(
      "(^",
      paste(control.suffix, collapse = "|"),
      ")_\\d+"
    )
    plate_df <- mutate(plate_df, value = gsub(control.suffix, "\\1", value))
  }

  if (!is.null(labels)) {
    if ("plate" %in% class(labels)) {
      labels_df <- as.data.frame(labels) |>
        rownames_to_column("row") |>
        pivot_longer(-row, names_to = "col", values_to = "labels") |>
        mutate(row = fct_rev(row)) |>
        mutate(col = factor(col, levels = colnames(plate)))

      plate_df <- inner_join(plate_df, labels_df, by = c("row", "col"))
    } else {
      if (length(labels) < nrow(plate_df)) {
        labels <- c(labels, rep("", nrow(plate_df) - length(labels)))
      }
      plate_df <- mutate(plate_df, labels = labels)
    }
  }

  p <- ggplot(plate_df, aes(x = col, y = row, fill = value)) +
    geom_tile(color = "black", alpha = 0.4)

  if (is.null(labels) && text) {
    p <- p + geom_text(aes(label = value))
  }

  if (!is.null(labels) && text) {
    p <- p +
      geom_text(aes(label = value, vjust = 1)) +
      geom_text(aes(label = labels, vjust = -1))
  }

  if (!is.null(labels) && !text) {
    p <- p + geom_text(aes(label = labels))
  }

  p <- p +
    theme_minimal() +
    labs(x = "Column", y = "Row") +
    guides(fill = "none") +
    coord_equal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  if (return_df) {
    return(list(p, plate_df))
  } else {
    return(p)
  }
}
