#' @title Generate DIA windows
#' @description
#' This function generate sequential windows for data-independent acquisition.
#'
#' @importFrom dplyr select mutate everything
#' @importFrom utils write.csv
#'
#' @param mnc A minimal cap of GPF windows
#' @param mxc A maximal cap of GPF windows
#' @param windows.width Isolation width. Specify only either `windows.width` or `n.windows`.
#' @param n.windows Number of windows. Specify only either `windows.width` or `n.windows`.
#' @param optimize If `TRUE` will place the edge on the peptides forbidden zones.
#' @param stagger If `TRUE` will generated series of staggered windows.
#' @param margin An isolation margin
#' @param output.type choose from `"range"`, `"center"`, `"both"`
#' @param save.csv If `TRUE` also write output to `.csv` files
#'
#' @return
#' A dataframe containing m/z sequential windows for data-independent acquisition
#' * If `output.type = "range"` will show an output as min-max range.
#' * If `output.type = "center"` will show an output as center mass and scan width.
#' * If `output.type = "both"` will show all columns mentioned earlier.
#'
#' @family m/z
#' @export
#'
#' @examples
#' DIA.windows(mnc = 400, mxc = 1000, windows.width = 20)
#' DIA.windows(mnc = 400, mxc = 1000, windows.width = 20, output.type = "center")
#' DIA.windows(mnc = 400, mxc = 1000, windows.width = 20, stagger = TRUE)
#' DIA.windows(mnc = 400, mxc = 1000, windows.width = 20, optimize = TRUE, stagger = TRUE)

DIA.windows <- function(
  mnc = NULL,
  mxc = NULL,
  windows.width = NULL,
  n.windows = NULL,
  optimize = TRUE,
  stagger = FALSE,
  margin = 0,
  output.type = "range",
  save.csv = FALSE
) {
  output.type <- match.arg(output.type, choices = c("range", "center", "both"))

  stopifnot(
    "Please specify minimal cap." = !is.null(mnc),
    "Please specify maximal cap." = !is.null(mxc),
    "Minimal cap cannot be higher or equal to maximal cap." = mnc < mxc,
    "Windows or their parameters cannot be negative or zero." = (all(
      mnc > 0,
      mxc > 0
    )),
    "Margin cannot be negative." = margin >= 0,
    "Please specify only windows.width or n.windows." = sum(
      !is.null(windows.width),
      !is.null(n.windows)
    ) ==
      1
  )

  if (!is.null(n.windows)) {
    stopifnot(
      "Number of windows must be a positive integer." = n.windows > 0 &&
        n.windows == as.integer(n.windows)
    )
    windows.width <- ceiling((mxc - mnc) / floor(n.windows))
  } else {
    stopifnot("Windows width must be positive." = windows.width > 0)
  }

  lower_windows <- seq(mnc - margin, mxc, windows.width - margin * 2)
  upper_windows <- lower_windows + windows.width

  if (optimize) {
    lower_windows <- opt.windows(lower_windows)
    upper_windows <- opt.windows(upper_windows)
  }
  if (stagger) {
    lower_windows <- c(lower_windows, lower_windows - windows.width / 2)
    upper_windows <- c(upper_windows, upper_windows - windows.width / 2)
  }

  windows_df <- data.frame(
    lower_windows = round(lower_windows, 4),
    upper_windows = round(upper_windows, 4)
  ) |>
    mutate(`m/z range` = paste(lower_windows, upper_windows, sep = "-")) |>
    mutate(`Center mass (m/z)` = (lower_windows + upper_windows) / 2) |>
    mutate(`Scan width (m/z)` = upper_windows - `Center mass (m/z)`)

  windows_df <- switch(
    output.type,
    "range" = select(windows_df, `m/z range`, lower_windows, upper_windows),
    "center" = select(windows_df, `Center mass (m/z)`, `Scan width (m/z)`),
    "both" = relocate(windows_df, `m/z range`, .before = everything())
  )

  class(windows_df) <- c("mzdf", "data.frame")

  if (save.csv) {
    write.csv(windows_df, "DIA_windows.csv", row.names = FALSE)
  }

  return(windows_df)
}
