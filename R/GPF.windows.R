#' @title Make an isolation list for GPF-DIA
#' @description
#' The function utilizes a predefined list of isolation windows and partitions them into groups for gas-phase fractionation DIA experiments.
#'
#' @importFrom tidyr separate_wider_delim unite
#' @importFrom dplyr group_by group_split mutate select across
#' @importFrom purrr iwalk set_names
#' @importFrom stringr str_sort
#' @importFrom readr parse_number
#'
#' @param isolation_list A vector of isolation list with range, e.g. `400-420`
#' @param lower_windows Lower windows in isolation list. Ignore if `isolation_list` is used.
#' @param upper_windows Upper windows in isolation list. Ignore if `isolation_list` is used.
#' @param windows_width GPF-DIA scan range per group, default = `100`.
#' @param mnc A minimal cap of GPF windows. If `NULL` will determine from the lowest value of lower bound.
#' @param mxc A maximal cap of GPF windows. If `NULL` will determine from the highest value of lower bound.
#' @param output.type choose from `"range"`, `"center"`, `"both"`
#' @param save.csv If `TRUE` also write output to `.csv` files
#'
#' @return
#' A list of dataframe containing m/z separation windows for each GPF group.
#' * If `output.type = "range"` will show an output as min-max range.
#' * If `output.type = "center"` will show an output as center mass and scan width.
#' * If `output.type = "both"` will show all columns mentioned earlier.
#' @family m/z
#' @export
#'
#' @examples
#' data(staggered_mz)
#' GPF.windows(staggered_mz$range)
#'
#' ## Can also use vector of lower and upper m/z range.
#' mz_list <- strsplit(staggered_mz$range, split = "-")
#' lower_bound <- as.numeric(sapply(mz_list, "[[", 1))
#' upper_bound <- as.numeric(sapply(mz_list, "[[", 2))
#' GPF.windows(lower_windows = lower_bound, upper_windows = upper_bound)

GPF.windows <- function(
  isolation_list = NULL,
  lower_windows = NULL,
  upper_windows = NULL,
  windows_width = 100,
  mnc = NULL,
  mxc = NULL,
  output.type = "range",
  save.csv = FALSE
) {
  output.type <- match.arg(output.type, choices = c("range", "center", "both"))

  stopifnot(
    "windows_width must be positive" = windows_width > 0,
    "Minimal cap cannot be higher or equal to maximal cap." = is.null(mnc) ||
      is.null(mxc) ||
      mnc < mxc
  )

  if (!is.null(isolation_list)) {
    isolation_list <- strsplit(isolation_list, split = "-")
    lower_windows <- as.numeric(sapply(isolation_list, "[[", 1))
    upper_windows <- as.numeric(sapply(isolation_list, "[[", 2))
  } else {
    stopifnot(
      "Please specify lower_windows" = !is.null(lower_windows),
      "Please specify upper_windows." = !is.null(upper_windows),
      "lower_windows and upper_windows must have equal length" = length(
        lower_windows
      ) ==
        length(upper_windows)
    )
  }
  if (any(lower_windows > upper_windows)) {
    stop(
      "Position ",
      paste(which(lower_windows > upper_windows), collapse = ", "),
      " had lower windows > upper windows"
    )
  }

  seq_breaks <- seq.ext(
    max(mnc, min(lower_windows)),
    min(mxc, max(upper_windows)),
    windows_width,
    rm.last = T
  )
  GPF_df <- data.frame(lower_windows, upper_windows)

  if (!is.null(mnc)) {
    GPF_df <- dplyr::filter(GPF_df, lower_windows >= mnc)
  }
  if (!is.null(mxc)) {
    GPF_df <- dplyr::filter(GPF_df, upper_windows <= mxc)
  }

  GPF_df <- GPF_df |>
    mutate(
      group = as.character(cut(
        lower_windows,
        breaks = seq_breaks,
        include.lowest = T
      ))
    ) |>
    tidyr::fill(group, .direction = "down") |>
    separate_wider_delim(group, delim = ",", names = c("l", "u")) |>
    mutate(across(c(l, u), parse_number)) |>
    unite(
      "m/z range",
      c(lower_windows, upper_windows),
      sep = "-",
      remove = FALSE
    ) |>
    unite("group", c(l, u), sep = "-") |>
    mutate(`Center mass (m/z)` = (lower_windows + upper_windows) / 2) |>
    mutate(`Scan width (m/z)` = upper_windows - `Center mass (m/z)`) |>
    relocate(group, .after = everything())

  GPF_df <- switch(
    output.type,
    "range" = dplyr::select(
      GPF_df,
      `m/z range`,
      lower_windows,
      upper_windows,
      group
    ),
    "center" = dplyr::select(
      GPF_df,
      `Center mass (m/z)`,
      `Scan width (m/z)`,
      group
    ),
    "both" = GPF_df
  )

  GPF_df_list <- GPF_df |>
    group_by(group) |>
    group_split()

  GPF_df_list <- lapply(GPF_df_list, \(x) {
    class(x) <- c("mzdf", "data.frame")
    return(x)
  })
  GPF_df_list <- set_names(
    GPF_df_list,
    sapply(GPF_df_list, \(x) unique(x$group))
  )
  GPF_df_list <- GPF_df_list[str_sort(names(GPF_df_list), numeric = TRUE)]

  if (save.csv) {
    iwalk(GPF_df_list, ~ write.csv(.x, paste0(.y, ".csv"), row.names = FALSE))
  }

  return(GPF_df_list)
}
