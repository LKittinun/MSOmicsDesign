# MSOmicsDesign

<!-- badges: start -->
[![R-CMD-check](https://github.com/LKittinun/MSOmicsDesign/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/LKittinun/MSOmicsDesign/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**MSOmicsDesign** provides tools for designing mass spectrometry-based omics
experiments. It covers two main workflows:

- **Experiment design**: insert QC runs, block-randomise by experimental
  covariates, distribute samples across multi-well plates, visualise layouts,
  and export position lists for Thermo Xcalibur sequence files.
- **DIA window design**: generate fixed-width, variable-width
  (density-weighted), and gas-phase fractionation (GPF) isolation window lists
  with optional edge optimisation for reduced chimeric spectra.

---

## Installation

```r
# Using remotes
remotes::install_github("LKittinun/MSOmicsDesign")

# Using devtools
devtools::install_github("LKittinun/MSOmicsDesign")
```

---

## Plate Design Workflow

```r
library(MSOmicsDesign)

# 1. Define samples and insert QC every 6 injections
samples    <- paste0("S", 1:20)
samples_qc <- addQC(samples, interval = 6)

# 2. Fill into plates (6 × 9 by default, auto-spills to next plate)
fp <- fill.plate(samples_qc,
                 labels = paste0("lbl_", seq_along(samples_qc)))

# 3. Visualise: colour by group, show labels
plot(fp, labels = fp$label, control.suffix = "QC")

# 4. Export plate positions for Xcalibur
positions <- generate.position(fp, flatten = TRUE)

# 5. Write Xcalibur sequence file
write.sld(fp, filename = "sequence.csv")
```

### Block randomisation

When your experiment has multiple covariates, `block.rand()` ensures balanced
allocation across run-order blocks before filling plates.

```r
data(ccc)
ccc_block <- block.rand(ccc, time, response)
check.block(ccc_block)           # inspect balance per block

fp_rand <- fill.plate(ccc_block$unique_group_id)
plot(fp_rand)
```

---

## DIA Window Design Workflow

### Fixed-width windows

```r
# 30 equal windows between 400 – 1000 m/z, edges in forbidden zones
dia <- DIA.windows(mnc = 400, mxc = 1000, n.windows = 30)
plot(dia)
```

### Variable-width windows (density-weighted)

Windows are narrower in dense precursor regions (uses built-in HeLa reference).

```r
vdia <- vDIA.windows(mnc = 400, mxc = 1000, n.windows = 32)
plot(vdia)
```

### Staggered windows

```r
dia_stagger <- DIA.windows(mnc = 400, mxc = 1000, windows.width = 20,
                            stagger = TRUE)
```

### GPF-DIA partitioning

Split an existing isolation list into per-injection GPF groups.

```r
data(staggered_mz)
gpf <- GPF.windows(staggered_mz$range, mnc = 400, mxc = 1000)
names(gpf)      # one data frame per GPF group
plot(gpf[[1]])
```

---

## Functions

| Family | Function | Description |
|--------|----------|-------------|
| Plate | `addQC()` | Insert QC runs at intervals or positions |
| Plate | `replaceQC()` | Replace slots with QC labels (fixed length) |
| Plate | `fill.plate()` | Distribute samples across plate matrices |
| Plate | `plot.plate_list()` | Visualise a plate list |
| Plate | `plot.plate()` | Visualise a single plate |
| Plate | `generate.position()` | Export well positions for sequence files |
| Plate | `write.sld()` | Write Xcalibur `.sld` sequence CSV |
| Randomise | `block.rand()` | Block randomisation by covariates |
| Randomise | `check.block()` | Inspect covariate balance per block |
| m/z | `DIA.windows()` | Fixed-width DIA isolation windows |
| m/z | `vDIA.windows()` | Variable-width DIA isolation windows |
| m/z | `GPF.windows()` | GPF-DIA window partitioning |
| m/z | `opt.windows()` | Optimise window edge placement |
| m/z | `plot.mzdf()` | Visualise isolation windows |

---

## Vignettes

```r
vignette("plate-design",  package = "MSOmicsDesign")
vignette("dia-windows",   package = "MSOmicsDesign")
```

---

## License

CC BY 4.0, Kittinun Leetanaporn
