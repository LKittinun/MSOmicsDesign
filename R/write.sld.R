#' @title Write a Thermo sequence file
#' @description
#' This function take a plate list from `fill.plate()` and make a `.csv` file that can be imported to Thermo's Xcalibur as `.sld` sequence.
#'
#' @param mat A list of matrix from `fill.plate()`
#' @param filename File name, should be .csv
#' @param return_df If `TRUE` will also return the resulting dataframe to console.
#' @param samp_type If `NULL` will use data from `mat`. Automatically check for QC or blank in names.
#' @param sample_name If `NULL` will use data from plate content.
#' @param path Path to save files when run experiment in Xcalibur. Should have `length = 1 or length(sample_name)`, can be ignored.
#' @param inst_method Path to instrument method. Should have `length = 1 or length(sample_name)`, can be ignored.
#' @param proc_meth Path to processing method. Should have `length = 1 or length(sample_name)`, can be ignored.
#' @param position If `NULL` will take plates' positions from `mat`
#' @param inj_vol Injection volumn in ul. Should have `length = 1 or length(sample_name)`, can be ignored.
#'
#' @return
#' A `.csv` file that can be imported to Xcalibur. Also return a dataframe if `return_df = TRUE`
#'
#' @importFrom utils write.table
#' @family plate
#' @export
#'
#' @examples
#' write.sld(fill.plate(addQC(1:100,5)), filename = "test.csv")

write.sld <- function(
  mat = NULL,
  filename = NULL,
  return_df = TRUE,
  samp_type = NULL,
  sample_name = NULL,
  path = "",
  inst_method = "",
  proc_meth = "",
  position = NULL,
  inj_vol = NULL
) {
  if (is.null(filename)) {
    stop("Please specify filename.")
  }

  seq_df <- data.frame(
    `Sample Name` = if (is.null(sample_name)) {
      if (!is.null(mat$data$labels)) mat$data$labels else mat$data$samples
    },
    Path = path,
    `Inst Meth` = inst_method,
    `Proc Meth` = proc_meth,
    `Position` = if (is.null(position)) {
      generate.position(mat, TRUE)
    } else {
      position
    },
    `Inj Vol` = if (!is.null(inj_vol)) inj_vol else "",
    check.names = F
  ) |>
    mutate(
      `Sample Type` = if (is.null(samp_type)) {
        dplyr::case_when(
          grepl("^QC", `Sample Name`, ignore.case = TRUE) ~ "QC",
          grepl("blank", `Sample Name`, ignore.case = TRUE) ~ "Blank",
          .default = "Unknown"
        )
      }
    ) |>
    mutate(`Sample ID` = 1:n()) |>
    relocate(`Sample Type`, .before = everything()) |>
    relocate(`Sample ID`, .after = `Sample Name`)

  writeLines("Bracket Type=1", filename)
  suppressWarnings(write.table(
    seq_df,
    filename,
    sep = ",",
    col.names = TRUE,
    row.names = FALSE,
    append = TRUE
  ))

  if (return_df) {
    return(seq_df)
  }
}
