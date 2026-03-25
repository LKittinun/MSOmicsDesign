#' @title Generate variable DIA windows
#' @description
#' This function generate variable sequential windows for data-independent acquisition. The windows are based on reference data provided.
#'
#' @importFrom dplyr arrange summarize mutate select relocate between filter across
#' @importFrom utils write.csv head
#' @importFrom scales rescale
#' @param mnc A minimal cap of GPF windows
#' @param mxc A maximal cap of GPF windows
#' @param n.windows Number of windows.
#' @param optimize If `TRUE` will place the edge on the peptides forbidden zones.
#' @param margin An isolation margin
#' @param ref.data Reference dataframe, should has integer `mz` and frequency `n` columns. If `NULL` will use `precursor_hela` data.
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
#' vDIA.windows(mnc = 400, mxc = 1000, n.windows = 32)

vDIA.windows <- function(mnc = NULL, mxc = NULL, n.windows = NULL,
                             optimize = TRUE, margin = 0, ref.data = NULL,
                             output.type = "range", save.csv = FALSE) {

  output.type <- match.arg(output.type, choices = c("range", "center", "both"))

  stopifnot("Please specify minimal cap." = !is.null(mnc),
            "Please specify maximal cap." = !is.null(mxc),
            "Please specify n.windows." = !is.null(n.windows),
            "Minimal cap cannot be higher or equal to maximal cap." = mnc<mxc,
            "Windows or their parameters cannot be negative or zero." = (all(mnc>0,mxc>0,n.windows>0)),
            "Margin must be non-negative." = margin >= 0)

  if(is.null(ref.data)){
    ref.data <- precursor_hela
  }

  windows_df <- ref.data |>
    arrange(mz) |>
    filter(between(mz,mnc,mxc)) |>
    mutate(cumsum_n = cumsum(n)) |>
    mutate(cumsum_n = scales::rescale(cumsum_n, to = c(1, n.windows))) |>
    mutate(bin = floor(cumsum_n)) |>
    dplyr::summarize(lower_windows = min(mz), max_mz = max(mz), .by = bin) |>
    mutate(
      next_min = dplyr::lead(lower_windows),
      upper_windows = dplyr::coalesce(next_min, max_mz),
      lower_windows = lower_windows - margin,
    ) |>
    mutate(upper_windows = upper_windows + margin) |>
    select(-next_min, -bin, -max_mz) |>
    head(-1)

   if(optimize){
    windows_df <- mutate(windows_df, across(c(lower_windows, upper_windows), ~round(opt.windows(.x), 3) ))
   }

  windows_df <- windows_df |>
    mutate(`m/z range` = paste(lower_windows, upper_windows, sep = "-")) |>
    mutate(`Center mass (m/z)` = (lower_windows+upper_windows)/2 ) |>
    mutate(`Scan width (m/z)` = upper_windows - `Center mass (m/z)`)

  windows_df <- switch(
    output.type,
    "range" = select(windows_df, `m/z range`, lower_windows, upper_windows),
    "center" = select(windows_df, `Center mass (m/z)`, `Scan width (m/z)`),
    "both" = relocate(windows_df, `m/z range`, .before = everything())
  )

  class(windows_df) <- c("mzdf", "data.frame")

  if(save.csv){
    write.csv(windows_df, "variable_windows.csv", row.names = FALSE)
  }

  return(windows_df)
}








