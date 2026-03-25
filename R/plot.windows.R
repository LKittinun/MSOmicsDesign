#' Plot windows range.
#'
#' @import ggplot2
#'
#' @param x A dataframe with class `mzdf`
#' @param ... Currently unused; included for S3 method compatibility.
#'
#' @return
#' A `ggplot` object of windows range, can be further modified or saved by `ggplot2` function.
#'
#' @family m/z
#' @export
#'
#' @examples
#' dia_range <- DIA.windows(400,1000,20)
#' plot(dia_range)

plot.mzdf <- function(x, ...) {
  df <- x
  df |>
    mutate(row = 1:nrow(df)) |>
    ggplot(aes(x = row)) +
    geom_rect(
      aes(
        xmin = row - 0.2,
        xmax = row + 0.2,
        ymin = lower_windows,
        ymax = upper_windows
      ),
      col = "black",
      fill = "skyblue"
    ) +
    coord_flip() +
    scale_x_reverse(
      breaks = scales::pretty_breaks(n = nrow(df)),
      expand = c(0, 0)
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = nrow(df) / 2)) +
    geom_hline(
      yintercept = (df$lower_windows + df$upper_windows) / 2,
      alpha = 0.3,
      col = "darkred",
      linetype = "dashed"
    ) +
    labs(x = "Scan number", y = "m/z") +
    theme_classic()
}
