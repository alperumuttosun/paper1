# Temperature, VPD, and Global Crop Yields — Replication Materials

**Tosun, A.U. and Karaman, F. (2025)**  
*Manuscript under review — Global Change Biology*

---

## Overview

This repository contains the code and documentation needed to replicate all analyses in the paper. The study examines the effect of flowering-period maximum temperature (TMX) and vapor pressure deficit (VPD) on crop yields across eight staple crops using a global country-year panel dataset covering 1961–2024. A key contribution is the mechanistic separation of TMX-based and VPD-based yield effects via a Clausius–Clapeyron decomposition, and the application of CMIP6 climate projections to estimate end-of-century yield risks under SSP2-4.5 and SSP5-8.5.

> **Note on code scope:** This repository includes all scripts used during the project, some of which generate exploratory figures and diagnostics not included in the final manuscript. Scripts are clearly named by step; publication-ready outputs are produced by `paper1_step11_tables_figures.R` and `paper1_ga_figures.R`.

> **Note on raw data:** No raw data files are included. All input data are publicly available from the sources listed below.

---

## Data Sources

| Source | Variable | Link |
|--------|----------|------|
| FAOSTAT | Crop yield, harvested area, production, irrigation data | https://www.fao.org/faostat |
| CRU TS4.09 | Monthly max temperature, precipitation, vapour pressure | https://crudata.uea.ac.uk/cru/data/hrg |
| SPAM2020 | Crop-specific harvested area weights | https://www.mapspam.info |
| World Bank WDI | GDP per capita (constant 2015 USD) | https://data.worldbank.org |
| GADM v4.1 | Country boundaries | https://gadm.org |
| CMIP6 / WorldClim | Future temperature projections | https://worldclim.org/data/cmip6 |

**Panel structure:** Country × Year × Crop.  
**Coverage:** 61–99 countries depending on crop, 1961–2024.

---

## Key Variables

### Primary variables (M1–M4 baseline models)

| R variable | Description |
|------------|-------------|
| `ln_verim` | Log crop yield — hg/ha (Turkish: *verim* = yield) |
| `TMX` | Flowering-period maximum temperature (°C), production-weighted |
| `PRE` | Precipitation during grain-filling period (mm) |
| `PRE_sq` | Squared precipitation — nonlinear precipitation response; significant concave relationship for rye only (see Appendix Table A.11) |
| `irr_ratio` | Irrigation intensity (ratio, 0–1) — equipped irrigation area / cropland area |
| `ln_gdp` | Log GDP per capita (constant 2015 USD) |
| `Country_ISO` | Country identifier — ISO3 code (used as country fixed effect) |
| `Yil` | Year (Turkish: *yıl* = year; used as year fixed effect) |

### Derived variables (VPD decomposition — Step 10)

| R variable | Description |
|------------|-------------|
| `VPD` | Vapor pressure deficit during flowering (kPa), derived from CRU vapour pressure and TMX via Clausius–Clapeyron: eₛ(T) = 0.6108 × exp[17.27T/(T+237.3)] |

### Interaction terms (M5–M8 robustness specifications)

| R variable | Description |
|------------|-------------|
| `TMX × irr_ratio` | Temperature × irrigation interaction |
| `TMX × is_arid` | Temperature × aridity indicator (is_arid = 1 if annual PRE < 400 mm) |
| `TMX × ln_gdp` | Temperature × income interaction |
| `TMX × PRE` | Temperature × precipitation interaction |

### Nonlinearity tests (Appendix A.6 — see Appendix for full results)

| R variable | Description |
|------------|-------------|
| `TMX_sq` | Squared TMX — tested in Appendix Table A.10. Significant only for soybean (TMX* = 24.4°C) and sorghum (TMX* = 28.5°C); linear specification retained in main models for all eight crops. |

---

## Empirical Strategy

Two-way fixed effects (TWFE) panel model estimated separately for each crop using `fixest::feols`. Production-weighted estimates are the primary specification. VPD is derived from CRU vapour pressure and temperature data using the Clausius–Clapeyron relationship and translated into temperature-equivalent units to allow direct comparison with TMX-based projection losses.

---

## Code Structure

The project is organized into two scripts.

---

### `step00_datapreparation_paper1.R` — Data Preparation Pipeline

Builds the analysis-ready panel datasets from raw sources. Runs sequentially through 7 steps, each producing intermediate files (`pan1*` → `pan7*`).

| Step | What it does | Output |
|------|-------------|--------|
| Step 1 | CRU TS4.09 NetCDF + SPAM2020 rasters → monthly climate panels | `pan1*` |
| Step 2 | FAOSTAT yield + phenological windows → annual crop panels | `pan2*` |
| Step 3 | Extra 4 countries (IRN, EGY, NGA, SDN) added via supplementary FAO data | `pan3*` |
| Step 3b | Egypt boundary patch via rnaturalearth | `pan3*` update |
| Step 4 | FAOSTAT irrigation data joined | `pan4*` |
| Step 5 | Cropland area + `irr_ratio` constructed | `pan5*` |
| Step 6 | Aridity indicator (`is_arid`: annual PRE < 400 mm) added | `pan6*` |
| Step 7 | GDP per capita (World Bank WDI) joined; backfill fix + duplicate removal | `pan7*` |

**Input files** (all in `ana_yol`):

| File | Description |
|------|-------------|
| `CRU_TS4.09_*.nc` | CRU TS4.09 monthly climate grids |
| `spam2020_V2r0_global_H_*_A.tif` | SPAM2020 harvested area rasters |
| `Global_AgriClimate_v5.xlsx` | Phenological windows (flowering, grain-filling months) per country × crop |
| `Production_Crops_Livestock_E_All_Data_(Normalized).csv` | FAOSTAT crop yield, area, production |
| `GLOBAL_ATMOSPHERIC_CO2_NOAA.csv` | Atmospheric CO₂ (NOAA) |
| `gdp_per_person.csv` | Fallback GDP (overridden by WDI in Step 7) |
| `FAOSTAT_data_en_3-5-2026.xls` | FAOSTAT irrigation data |
| `sudanvs.xls` | FAO data for IRN, EGY, NGA, SDN |

**Final outputs:** `pan7wheat.xlsx` … `pan7sorghum.xlsx` (one file per crop)

**Required packages:** `terra`, `geodata`, `dplyr`, `readxl`, `writexl`, `data.table`, `tidyr`, `zoo`, `rnaturalearth`, `rnaturalearthdata`, `sf`, `WDI`

---

### `step01_dataanalysis_paper1.R` — Data Analysis and Figures

Consolidates all analysis scripts into a single file. Reads `pan7*.xlsx` outputs from Step 00 and produces all models, figures, and tables for the manuscript and appendix.

| Section | What it does | Output in manuscript? |
|---------|-------------|----------------------|
| Descriptive statistics | Summary plots and statistics | Partial |
| Correlation analysis | Correlation matrix and plots | No (exploratory) |
| Main TWFE models | Eight model variants (M1–M8), production- and area-weighted — saves `modeller_v4_objects.rds` | Yes |
| Precipitation robustness | PRE-squared analysis, optimal PRE estimates | Appendix |
| Coefficient summary | Forest plots, literature benchmark comparisons | Yes |
| Structural break & diagnostics | Chow tests, Pesaran CD tests, panel unit root | Yes |
| Leave-one-out robustness | Excluding USA, China, India, USSR bloc | Appendix |
| Driscoll-Kraay SE | Alternative inference, spatial autocorrelation | Appendix |
| Warming scenarios | Delta-T projections (+1°C to +4°C) | Yes |
| VPD decomposition | Clausius–Clapeyron VPD derivation, TMX vs VPD comparison — saves `adim10b_vpd_df.rds` | Yes |
| Publication figures & tables | Final manuscript figures, CMIP6 SSP2-4.5 / SSP5-8.5 projections | Yes |
| Graphical abstract | Figures B, C, D for graphical abstract | Yes |

**Required packages:** `fixest`, `ggplot2`, `dplyr`, `tidyr`, `readxl`, `writexl`, `scales`, `MASS`, `plm`, `zoo`, `data.table`, `FAOSTAT`

> **Model specifications (M1–M8):** Full model equations, robustness tests, and diagnostic results are documented in Appendix A.0 of the manuscript.

> **Dependencies:** Steps 04–11 require `modeller_v4_objects.rds` saved by Step 03. Step 11 additionally requires `adim10b_vpd_df.rds` saved by Step 10.

---

## How to Reproduce

1. Download all raw data from the sources listed above.
2. Set `ana_yol` at the top of each script to your local working directory.
3. Run `step00_datapreparation_paper1.R` first — produces `pan7*.xlsx` files for all eight crops.
4. Run `step01_dataanalysis_paper1.R` — reads `pan7*.xlsx` and produces all models, figures, and tables.

### Simplified reproduction (recommended)

The analysis-ready panel datasets (`pan7wheat.xlsx`, `pan7barley.xlsx`, `pan7oats.xlsx`, `pan7rye.xlsx`, `pan7rice.xlsx`, `pan7maize.xlsx`, `pan7soybean.xlsx`, `pan7sorghum.xlsx`) are provided directly in this repository. Researchers who wish to replicate the statistical analyses and figures without re-running the full data preparation pipeline can do so by:

1. Setting `ana_yol` to their local working directory.
2. Running `step01_dataanalysis_paper1.R` only.

All results reported in the manuscript can be reproduced in this way.

---

## Outputs

| Type | Location | Format |
|------|----------|--------|
| Manuscript figures | `figures/labeled/` and `figures/unlabeled/` | PDF, 300 dpi PNG, 600 dpi PNG |
| Intermediate panels | `pan7wheat.xlsx` … `pan7sorghum.xlsx` | Excel |
| Model objects | `modeller_v4_objects.rds` | R binary |
| VPD data | `adim10b_vpd_df.rds` | R binary |

---

## Software and Package Versions

All analyses were conducted in **R version 4.5.2**.

### Analytical packages (cited in main manuscript)

| Package | Role | Version |
|---------|------|---------|
| `fixest` | TWFE panel estimation, clustered SE | 0.13.2 |

### Data access packages

| Package | Role | Version |
|---------|------|---------|
| `FAOSTAT` | Automated FAOSTAT data download | 2.4.0 |
| `WDI` | World Bank WDI data download | 2.7.9 |
| `geodata` | Geographic and climate data download | 0.6.6 |

### Data processing packages

| Package | Role | Version |
|---------|------|---------|
| `dplyr` | Data manipulation | 1.2.0 |
| `tidyr` | Data reshaping | 1.3.2 |
| `data.table` | Fast data processing | 1.18.2 |
| `zoo` | Time series operations | 1.8.15 |
| `readxl` | Excel file input | 1.4.5 |
| `writexl` | Excel file output | 1.5.4 |
| `plm` | Panel data utilities, Driscoll-Kraay SE | 2.6.7 |
| `MASS` | Robust regression utilities | 7.3.65 |

### Spatial packages

| Package | Role | Version |
|---------|------|---------|
| `terra` | Raster processing (CRU, CMIP6 data) | 1.8.93 |
| `sf` | Vector spatial data | 1.0.24 |
| `rnaturalearth` | Country boundary maps | 1.2.0 |
| `rnaturalearthdata` | Natural Earth dataset dependency | 1.0.0 |

### Visualization packages

| Package | Role | Version |
|---------|------|---------|
| `ggplot2` | Figure generation | 4.0.2 |
| `scales` | Axis formatting in figures | 1.4.0 |

---

## Package Installation

```r
install.packages(c(
  "fixest", "ggplot2", "dplyr", "tidyr",
  "readxl", "writexl", "terra", "sf",
  "geodata", "rnaturalearth", "rnaturalearthdata",
  "scales", "MASS", "plm", "zoo",
  "data.table", "FAOSTAT", "WDI"
))
```

---

## Citation

If you use this code or data pipeline, please cite:

> Tosun, A.U. and Karaman, F. (2025). Temperature, Vapor Pressure Deficit, and Global Crop Yields: Evidence from a Multi-Crop Panel. *Manuscript under review, Global Change Biology.*

For the `fixest` package:

> Bergé, L. (2018). Efficient estimation of maximum likelihood models with multiple fixed-effects: the R package FENmlm. *CREA Discussion Papers*, no. 13. https://lrberge.github.io/fixest/

For R itself:

> R Core Team (2025). R: A Language and Environment for Statistical Computing. R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org/

---

## Contact

**Alper Umut Tosun**  
Yıldız Technical University, Istanbul, Turkey  
umut.tosun@std.yildiz.edu.tr  
ORCID: https://orcid.org/0000-0003-1167-8194

**Prof. Dr. Filiz Karaman** *(Corresponding author)*  
Yıldız Technical University, Istanbul, Turkey  
fkaraman@yildiz.edu.tr  
ORCID: https://orcid.org/0000-0002-8491-674X
