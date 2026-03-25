utils::globalVariables(c(
  # DIA / GPF / vDIA column names (non-standard evaluation)
  "Center mass (m/z)", "m/z range", "Scan width (m/z)",
  # window boundary variables
  "lower_windows", "upper_windows",
  # GPF.windows internal variables
  "group", "l", "u",
  # vDIA.windows internal variables
  "mz", "cumsum_n", "bin", "next_min", "max_mz",
  # block.rand / check.block
  "unique_group", "block.id",
  # plot.plate
  "value",
  # seq.ext default argument
  "length.out",
  # vDIA.windows reference data
  "precursor_hela",
  # write.sld column names
  "Sample Type", "Sample ID", "Sample Name"
))
