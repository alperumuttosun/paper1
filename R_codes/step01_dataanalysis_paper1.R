






# ==============================================================================
# PAPER 1 -- STEP 01: DATA DESCRIPTION
# Temperature, VPD, and Global Crop Yields: Evidence from a Multi-Crop Panel
#
# Contents:
#   Step 1 : Read all pan7* files, column check, duplicate check
#   Step 2 : Missing rate -- all crops x all variables
#   Step 3 : Descriptive statistics (within-SD, Burke & Emerick 2016)
#   Step 4 : Within-SD warning (Dell et al. 2014 FE check)
#   Step 5 : Within-country TMX vs ln_yield correlation
#   Step 6 : Outlier flag (within-country z-score > 4)
#   Step 7 : Coverage summary
#   Plots  : G_T1 to G_T5
#             -- R screen: labeled version first, then unlabeled
#             -- Saved: PDF + PNG 300dpi + PNG 600dpi x labeled + unlabeled
#
# UTF-8 NOTE: All special characters replaced with ASCII equivalents
#   degrees C  ->  degC
#   x (times)  ->  x or -
#   subscripts ->  plain text
#   em dash    ->  --
#
# References:
#   Dell et al. (2014, AER)        -- within-country variation
#   Burke & Emerick (2016, AEJ)    -- descriptive table structure
#   Lobell et al. (2011, Science)  -- panel coverage reporting
# ==============================================================================


# ==============================================================================
# 0. SETUP
# ==============================================================================

library(dplyr)
library(readxl)
library(writexl)
library(tidyr)
library(ggplot2)

ana_yol      <- "C:/Users/Alper.Tosun/Desktop/cru130"
urun_listesi <- c("wheat","barley","oats","rye","rice","maize","soybean","sorghum")

# Output directories
fig_labeled   <- file.path(ana_yol, "figures", "labeled")
fig_unlabeled <- file.path(ana_yol, "figures", "unlabeled")
dir.create(fig_labeled,   recursive = TRUE, showWarnings = FALSE)
dir.create(fig_unlabeled, recursive = TRUE, showWarnings = FALSE)

# Key variables to track
ana_degiskenler <- c(
  "yield_hg_per_ha",
  "area_harvested_ha",
  "production_tonnes",
  "TMX_floweringmean",
  "PRE_grainfillsum",
  "PRE_floweringsum",
  "GDD_growth",
  "SPEI3_memory",
  "GDP_per_Capita",
  "irr_ratio",
  "is_arid",
  "CO2_ppm")

# Crop colors -- consistent across all plots
renk_crop <- c(
  wheat   = "#1A5276",
  barley  = "#2E86C1",
  oats    = "#27AE60",
  rye     = "#1E8449",
  rice    = "#F39C12",
  maize   = "#E67E22",
  soybean = "#8E44AD",
  sorghum = "#C0392B")

# Crop display labels -- ASCII only
crop_labels <- c(
  wheat   = "Wheat",
  barley  = "Barley",
  oats    = "Oats",
  rye     = "Rye",
  rice    = "Rice",
  maize   = "Maize",
  soybean = "Soybean",
  sorghum = "Sorghum")


# ==============================================================================
# SAVE_PLOT FUNCTION
#
# For each plot:
#   1. Prints labeled version to R screen
#   2. Prints unlabeled version to R screen
#   3. Saves labeled:   PDF + PNG 300dpi + PNG 600dpi -> figures/labeled/
#   4. Saves unlabeled: PDF + PNG 300dpi + PNG 600dpi -> figures/unlabeled/
#
# Unlabeled strips: title, subtitle, caption, axis labels, legend title
# Keeps: all geom elements, axis ticks and tick labels, grid lines
# ==============================================================================

save_plot <- function(g_labeled, plot_name, width = 10, height = 7) {
  
  # Build unlabeled version
  g_unlabeled <- g_labeled +
    labs(title    = NULL,
         subtitle = NULL,
         caption  = NULL,
         x        = NULL,
         y        = NULL) +
    theme(legend.title = element_blank(),
          plot.margin  = margin(4, 4, 4, 4))
  
  # Print labeled to R screen
  cat(sprintf("  [SCREEN] %s -- LABELED\n", plot_name))
  print(g_labeled)
  
  # Print unlabeled to R screen
  cat(sprintf("  [SCREEN] %s -- UNLABELED\n", plot_name))
  print(g_unlabeled)
  
  # Save helper: PDF + 2 PNG resolutions
  save_one <- function(g, dir_path) {
    ggplot2::ggsave(
      file.path(dir_path, paste0(plot_name, ".pdf")),
      plot   = g,
      width  = width,
      height = height,
      device = "pdf")
    
    ggplot2::ggsave(
      file.path(dir_path, paste0(plot_name, "_300dpi.png")),
      plot   = g,
      width  = width,
      height = height,
      dpi    = 300,
      device = "png")
    
    ggplot2::ggsave(
      file.path(dir_path, paste0(plot_name, "_600dpi.png")),
      plot   = g,
      width  = width,
      height = height,
      dpi    = 600,
      device = "png")
  }
  
  save_one(g_labeled,   fig_labeled)
  save_one(g_unlabeled, fig_unlabeled)
  
  cat(sprintf("  [SAVED]  %s -> labeled/ + unlabeled/ (PDF + 300 + 600 dpi)\n\n",
              plot_name))
  invisible(NULL)
}


# ==============================================================================
# STEP 1 -- READ ALL PAN7 FILES: COLUMN CHECK + DUPLICATE CHECK
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("STEP 1 -- READ + COLUMN + DUPLICATE CHECK\n")
cat(strrep("=", 65), "\n\n")

pan_liste   <- list()
sutun_rapor <- list()
dup_rapor   <- list()

for (urun in urun_listesi) {
  dosya <- file.path(ana_yol, paste0("pan7", urun, ".xlsx"))
  if (!file.exists(dosya)) {
    cat(sprintf("[MISSING] pan7%s.xlsx\n", urun)); next
  }
  
  df <- as.data.frame(readxl::read_excel(dosya))
  
  # Fix decimal comma in character columns that should be numeric
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      trial <- suppressWarnings(as.numeric(gsub(",", ".", df[[col]])))
      if (mean(!is.na(trial)) > 0.80) df[[col]] <- trial
    }
  }
  df$Yil <- as.integer(as.character(df$Yil))
  df$urun <- urun
  
  # Log yield -- primary outcome variable in all models
  df$ln_yield <- suppressWarnings(log(df$yield_hg_per_ha))
  df$ln_yield[!is.finite(df$ln_yield)] <- NA
  
  # Squared precipitation for non-linear model terms
  if ("PRE_grainfillsum" %in% names(df))
    df$PRE_sq <- df$PRE_grainfillsum^2
  
  # Irrigation alias
  if (!"IRR" %in% names(df) && "irr_ratio" %in% names(df))
    df$IRR <- df$irr_ratio
  
  # Log GDP for M5/M6 models
  if (!"ln_gdp" %in% names(df) && "GDP_per_Capita" %in% names(df))
    df$ln_gdp <- suppressWarnings(log(df$GDP_per_Capita))
  
  pan_liste[[urun]] <- df
  
  # Record column presence for summary table
  sutun_rapor[[urun]] <- setNames(
    sapply(ana_degiskenler, function(v) v %in% names(df)),
    ana_degiskenler)
  
  # Duplicate check: each country-year must appear exactly once
  dup <- df %>%
    dplyr::group_by(Country_ISO, Yil) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n > 1)
  
  dup_rapor[[urun]] <- list(n_pairs = nrow(dup), detail = dup)
  
  dup_msg <- if (nrow(dup) > 0)
    sprintf(" [DUPLICATE: %d pairs -- CHECK!]", nrow(dup))
  else
    " [OK]"
  
  cat(sprintf("[%-8s] N=%5d | Countries=%3d | Years=%d-%d%s\n",
              urun, nrow(df),
              length(unique(df$Country_ISO)),
              min(df$Yil, na.rm = TRUE),
              max(df$Yil, na.rm = TRUE),
              dup_msg))
}

# Column presence summary
cat("\n--- Variable Presence (V = present, . = missing) ---\n")
sv_df <- as.data.frame(do.call(rbind,
                               lapply(sutun_rapor, function(x) ifelse(x, "V", "."))))
print(sv_df)

# Duplicate details
cat("\n--- Duplicate (Country x Year) Summary ---\n")
for (urun in names(dup_rapor)) {
  nd <- dup_rapor[[urun]]$n_pairs
  if (nd > 0) {
    cat(sprintf("  [%s] %d duplicate country-year pairs:\n", urun, nd))
    print(head(dup_rapor[[urun]]$detail, 5))
  }
}
cat("[OK] Duplicate check complete\n\n")


# ==============================================================================
# STEP 2 -- MISSING RATE: ALL CROPS x ALL VARIABLES
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("STEP 2 -- MISSING RATE (all crops)\n")
cat(strrep("=", 65), "\n\n")

missing_list <- list()

for (urun in names(pan_liste)) {
  df <- pan_liste[[urun]]
  for (v in ana_degiskenler) {
    na_pct <- if (v %in% names(df))
      round(mean(is.na(df[[v]])) * 100, 1)
    else
      NA_real_
    missing_list[[paste0(urun, "_", v)]] <- data.frame(
      crop     = urun,
      variable = v,
      na_pct   = na_pct,
      stringsAsFactors = FALSE)
  }
}
missing_df <- dplyr::bind_rows(missing_list)

# Report variables with > 5% missing
cat("--- Variables with > 5% missing ---\n")
hi_miss <- missing_df[!is.na(missing_df$na_pct) & missing_df$na_pct > 5, ]
hi_miss <- hi_miss[order(hi_miss$na_pct, decreasing = TRUE), ]
print(hi_miss, row.names = FALSE)

# Country-level GDP missingness -- wheat as representative case
df_w <- pan_liste[["wheat"]]
if (!is.null(df_w) && "GDP_per_Capita" %in% names(df_w)) {
  gdp_ulke <- df_w %>%
    dplyr::group_by(Country_ISO) %>%
    dplyr::summarise(
      gdp_na_pct = round(mean(is.na(GDP_per_Capita)) * 100, 0),
      n_yil      = dplyr::n(),
      .groups    = "drop") %>%
    dplyr::arrange(dplyr::desc(gdp_na_pct))
  cat("\n--- Top 20 countries by GDP missingness (wheat panel) ---\n")
  print(head(gdp_ulke, 20))
}


# ==============================================================================
# STEP 3 -- DESCRIPTIVE STATISTICS
# Structure: Burke & Emerick (2016, AEJ) Table 1
# Within-SD: Dell et al. (2014, AER)
# ==============================================================================

cat("\n", strrep("=", 65), "\n")
cat("STEP 3 -- DESCRIPTIVE STATISTICS\n")
cat(strrep("=", 65), "\n\n")

tanim_listesi <- list()

hedef_vars <- c("ln_yield", "TMX_floweringmean", "PRE_grainfillsum",
                "GDD_growth", "SPEI3_memory", "GDP_per_Capita",
                "irr_ratio", "CO2_ppm")

for (urun in names(pan_liste)) {
  df  <- pan_liste[[urun]]
  hvs <- intersect(hedef_vars, names(df))
  
  istatler <- lapply(hvs, function(v) {
    x <- df[[v]]
    if (sum(!is.na(x)) < 10) return(NULL)
    
    # Within-country demeaned values -- Dell et al. (2014)
    df_tmp        <- df[, c("Country_ISO", v), drop = FALSE]
    names(df_tmp)[2] <- "val"
    within_v <- df_tmp %>%
      dplyr::group_by(Country_ISO) %>%
      dplyr::mutate(dm = val - mean(val, na.rm = TRUE)) %>%
      dplyr::pull(dm)
    
    data.frame(
      crop      = urun,
      variable  = v,
      N         = sum(!is.na(x)),
      N_country = length(unique(df$Country_ISO[!is.na(x)])),
      Mean      = round(mean(x,              na.rm = TRUE), 3),
      SD        = round(sd(x,                na.rm = TRUE), 3),
      Min       = round(min(x,               na.rm = TRUE), 3),
      P25       = round(quantile(x, 0.25,    na.rm = TRUE), 3),
      Median    = round(median(x,            na.rm = TRUE), 3),
      P75       = round(quantile(x, 0.75,    na.rm = TRUE), 3),
      Max       = round(max(x,               na.rm = TRUE), 3),
      Within_SD = round(sd(within_v,         na.rm = TRUE), 3),
      NA_pct    = round(mean(is.na(x)) * 100,            1))
  })
  
  tablo <- dplyr::bind_rows(Filter(Negate(is.null), istatler))
  tanim_listesi[[urun]] <- tablo
  cat(sprintf("[%s]\n", urun))
  print(tablo[, c("variable", "N", "Mean", "SD", "Within_SD", "NA_pct")],
        row.names = FALSE)
  cat("\n")
}

tanim_df <- dplyr::bind_rows(tanim_listesi)


# ==============================================================================
# STEP 4 -- WITHIN-SD WARNING
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("STEP 4 -- WITHIN-SD WARNING\n")
cat("Ref: Dell et al. (2014) -- FE identification requires within variation\n")
cat("Rule: Within_SD < 0.01 -- variable cannot enter FE model\n")
cat(strrep("=", 65), "\n\n")

warn_df <- tanim_df[!is.na(tanim_df$Within_SD) & tanim_df$Within_SD < 0.01, ]
if (nrow(warn_df) > 0) {
  cat("WARNING -- Low within-SD variables:\n")
  print(warn_df[, c("crop", "variable", "Within_SD", "SD")], row.names = FALSE)
} else {
  cat("[OK] All variables have sufficient within-country variation\n\n")
}


# ==============================================================================
# STEP 5 -- WITHIN-COUNTRY CORRELATION: TMX vs ln_yield
# ==============================================================================

cat("\n", strrep("=", 65), "\n")
cat("STEP 5 -- TMX vs ln_yield WITHIN-COUNTRY CORRELATION\n")
cat("Ref: Lobell & Field (2007) -- within-country climate-yield signal\n")
cat(strrep("=", 65), "\n\n")

cor_list <- list()

for (urun in names(pan_liste)) {
  df <- pan_liste[[urun]]
  if (!all(c("TMX_floweringmean", "ln_yield", "Country_ISO") %in% names(df))) next
  
  dm <- df %>%
    dplyr::group_by(Country_ISO) %>%
    dplyr::mutate(
      tmx_dm = TMX_floweringmean - mean(TMX_floweringmean, na.rm = TRUE),
      lny_dm = ln_yield          - mean(ln_yield,           na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(tmx_dm) & !is.na(lny_dm))
  
  if (nrow(dm) < 30) next
  
  r  <- cor(dm$tmx_dm, dm$lny_dm, use = "complete.obs")
  pv <- tryCatch(cor.test(dm$tmx_dm, dm$lny_dm)$p.value,
                 error = function(e) NA_real_)
  
  cor_list[[urun]] <- data.frame(
    crop     = urun,
    r_within = round(r, 3),
    pval     = pv,
    n_obs    = nrow(dm),
    stringsAsFactors = FALSE)
  
  sig <- if (!is.na(pv) && pv < 0.001) "***" else
    if (!is.na(pv) && pv < 0.01)  "**"  else
      if (!is.na(pv) && pv < 0.05)  "*"   else ""
  
  cat(sprintf("  [%-8s] r_within = %+.3f  p = %.4f %s  N = %d\n",
              urun, r, pv, sig, nrow(dm)))
}
cor_df <- dplyr::bind_rows(cor_list)


# ==============================================================================
# STEP 6 -- OUTLIER FLAG
# ==============================================================================

cat("\n", strrep("=", 65), "\n")
cat("STEP 6 -- OUTLIER FLAG (within-country z-score on ln_yield)\n")
cat("Rule: |z_within| > 4 -- extreme observation\n")
cat(strrep("=", 65), "\n\n")

outlier_list <- list()

for (urun in names(pan_liste)) {
  df <- pan_liste[[urun]]
  if (!all(c("ln_yield", "Country_ISO") %in% names(df))) next
  
  df_flag <- df %>%
    dplyr::group_by(Country_ISO) %>%
    dplyr::mutate(
      lny_mean = mean(ln_yield, na.rm = TRUE),
      lny_sd   = sd(ln_yield,   na.rm = TRUE),
      z_within = (ln_yield - lny_mean) / lny_sd) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(z_within) & abs(z_within) > 4) %>%
    dplyr::select(Country_ISO, Yil, ln_yield, z_within) %>%
    dplyr::mutate(crop = urun)
  
  if (nrow(df_flag) > 0) {
    outlier_list[[urun]] <- df_flag
    cat(sprintf("  [%s] %d extreme observations (|z| > 4):\n",
                urun, nrow(df_flag)))
    print(df_flag[order(abs(df_flag$z_within), decreasing = TRUE), ],
          row.names = FALSE)
    cat("\n")
  } else {
    cat(sprintf("  [%s] No extreme outliers\n", urun))
  }
}
outlier_df <- dplyr::bind_rows(outlier_list)
if (nrow(outlier_df) == 0) cat("\n[OK] No extreme outliers detected\n")


# ==============================================================================
# STEP 7 -- COVERAGE SUMMARY
# ==============================================================================

cat("\n", strrep("=", 65), "\n")
cat("STEP 7 -- COVERAGE SUMMARY\n")
cat(strrep("=", 65), "\n\n")

kapsam <- lapply(names(pan_liste), function(urun) {
  df <- pan_liste[[urun]]
  data.frame(
    crop        = urun,
    N_obs       = nrow(df),
    N_country   = length(unique(df$Country_ISO)),
    Year_min    = min(df$Yil, na.rm = TRUE),
    Year_max    = max(df$Yil, na.rm = TRUE),
    GDP_pct     = round(mean(!is.na(df$GDP_per_Capita)) * 100, 0),
    IRR_pct     = round(mean(!is.na(df$irr_ratio))      * 100, 0),
    SPEI_pct    = round(mean(!is.na(df$SPEI3_memory))   * 100, 0),
    ln_yield_ok = sum(!is.na(df$ln_yield)),
    stringsAsFactors = FALSE)
})
kapsam_df <- dplyr::bind_rows(kapsam)
print(kapsam_df, row.names = FALSE)


# ==============================================================================
# PLOTS -- G_T1 to G_T5
# All text: ASCII only -- no Unicode degree sign, multiply sign, or subscripts
# Each plot printed on R screen (labeled, then unlabeled)
# Each plot saved as 6 files (3 labeled + 3 unlabeled)
# ==============================================================================

cat("\n", strrep("=", 65), "\n")
cat("GENERATING PLOTS (G_T1 to G_T5)\n")
cat("Each plot: screen print x2 (labeled + unlabeled), saved x6\n")
cat(strrep("=", 65), "\n")


# ------------------------------------------------------------------------------
# G_T1 -- MISSING RATE HEATMAP: ALL CROPS x ALL VARIABLES
# ------------------------------------------------------------------------------

cat("\n[G_T1] Building missing rate heatmap...\n")

miss_plot <- missing_df %>%
  dplyr::filter(!is.na(na_pct)) %>%
  dplyr::mutate(
    crop     = factor(crop,     levels = rev(urun_listesi)),
    variable = factor(variable, levels = rev(ana_degiskenler)),
    miss_cat = dplyr::case_when(
      na_pct == 0 ~ "0%",
      na_pct <  5 ~ "< 5%",
      na_pct < 20 ~ "5-20%",
      na_pct < 50 ~ "20-50%",
      TRUE        ~ "> 50%"),
    miss_cat = factor(miss_cat,
                      levels = c("0%", "< 5%", "5-20%", "20-50%", "> 50%")),
    cell_lbl = sprintf("%.0f%%", na_pct))

miss_cols <- c("0%"     = "#1A7A4A",
               "< 5%"   = "#82E0AA",
               "5-20%"  = "#FAD7A0",
               "20-50%" = "#E67E22",
               "> 50%"  = "#C0392B")

g_t1 <- ggplot(miss_plot,
               aes(x = variable, y = crop, fill = miss_cat)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = cell_lbl), size = 2.8,
            color = "grey15", fontface = "bold") +
  scale_fill_manual(values = miss_cols, name = "Missing Rate",
                    drop = FALSE) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    title    = "G_T1: Data Completeness -- All Crops x Key Variables",
    subtitle = "Cell value = % missing | Green = complete | Red = >50% missing",
    caption  = paste0("Source: pan7* dataset (FAOSTAT + CRU TS4.09 + WDI) | 1961-2024\n",
                      "Lobell et al. (2011); Burke & Emerick (2016)"),
    x = NULL, y = NULL) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid      = element_blank(),
    axis.text.x     = element_text(angle = 35, hjust = 1, size = 8.5),
    axis.text.y     = element_text(size = 9.5, face = "bold"),
    legend.position = "bottom",
    legend.key.size = unit(0.45, "cm"),
    legend.text     = element_text(size = 9),
    plot.title      = element_text(face = "bold", size = 12),
    plot.subtitle   = element_text(size = 8.5, color = "grey40"),
    plot.caption    = element_text(size = 7.5, color = "grey50"),
    plot.margin     = margin(8, 12, 8, 8))

save_plot(g_t1, "G_T1_missing_heatmap", width = 11, height = 7)
cat("[G_T1] DONE\n")


# ------------------------------------------------------------------------------
# G_T2 -- COVERAGE BAR CHART: OBSERVATIONS + COUNTRIES PER CROP
# ASCII fix: "country-year" not "country x year" (Unicode multiply causes error)
# ------------------------------------------------------------------------------

cat("\n[G_T2] Building coverage bar chart...\n")

g_t2 <- ggplot(kapsam_df,
               aes(x = reorder(crop, N_obs),
                   y = N_obs,
                   fill = N_country)) +
  geom_col(color = "white", width = 0.7) +
  geom_text(aes(label = paste0(N_country, " ctry")),
            hjust = -0.10, size = 3.2, color = "grey25") +
  geom_text(aes(label = format(N_obs, big.mark = ",")),
            hjust = 1.15, size = 3.0,
            color = "white", fontface = "bold") +
  scale_fill_gradient(low  = "#AED6F1",
                      high = "#1A5276",
                      name = "N Countries") +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, max(kapsam_df$N_obs) * 1.22)) +
  coord_flip() +
  labs(
    title    = "G_T2: Panel Coverage -- Observations and Countries per Crop",
    subtitle = "Bar = country-year observations | Label = number of countries",
    caption  = paste0("Source: pan7* dataset (FAOSTAT + CRU TS4.09) | 1961-2024\n",
                      "Lobell et al. (2011, Science)"),
    x = NULL,
    y = "Observations (country-year)") +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_text(size = 11, face = "bold"),
    axis.text.x        = element_text(size = 9),
    legend.position    = "right",
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(size = 8.5, color = "grey40"),
    plot.caption       = element_text(size = 7.5, color = "grey50"),
    plot.margin        = margin(8, 12, 8, 8))

save_plot(g_t2, "G_T2_coverage_bars", width = 10, height = 6)
cat("[G_T2] DONE\n")


# ------------------------------------------------------------------------------
# G_T3 -- WITHIN-SD DOT PLOT: FE IDENTIFICATION CHECK
# Ref: Dell et al. (2014, AER)
# ASCII fix: "CO2" not "CO2" with subscript 2
# ------------------------------------------------------------------------------

cat("\n[G_T3] Building within-SD dot plot...\n")

within_vars <- c("ln_yield", "TMX_floweringmean", "PRE_grainfillsum",
                 "GDP_per_Capita", "irr_ratio", "CO2_ppm")

# ASCII-only facet labels -- no special characters
within_labels <- c(
  ln_yield          = "ln(Yield)",
  TMX_floweringmean = "TMX Flowering",
  PRE_grainfillsum  = "PRE Grain-fill",
  GDP_per_Capita    = "GDP per Capita",
  irr_ratio         = "Irrigation Ratio",
  CO2_ppm           = "CO2 (ppm)")

within_plot <- tanim_df %>%
  dplyr::filter(variable %in% within_vars,
                !is.na(Within_SD), !is.na(SD), SD > 0) %>%
  dplyr::mutate(
    rel_within = Within_SD / SD,
    crop       = factor(crop, levels = urun_listesi),
    var_label  = dplyr::recode(variable, !!!within_labels),
    var_label  = factor(var_label,
                        levels = unname(within_labels)))

if (nrow(within_plot) > 0) {
  
  g_t3 <- ggplot(within_plot,
                 aes(x = rel_within, y = crop, color = crop)) +
    geom_vline(xintercept = 0,    linetype = "dashed",
               color = "grey60", linewidth = 0.5) +
    geom_vline(xintercept = 0.01, linetype = "dotted",
               color = "#C0392B", linewidth = 0.45, alpha = 0.8) +
    geom_point(size = 3.5, alpha = 0.9) +
    facet_wrap(~var_label, scales = "free_x", nrow = 2) +
    scale_color_manual(values = renk_crop, guide = "none") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title    = "G_T3: Within-Country Variation -- FE Identification Check",
      subtitle = paste0("Relative Within-SD = Within-Country SD / Total SD\n",
                        "Red dotted line = 1% threshold (Dell et al. 2014)"),
      caption  = paste0("Dell et al. (2014, AER); Burke & Emerick (2016, AEJ)"),
      x = "Within-SD / Total-SD", y = NULL) +
    theme_bw(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold", size = 9),
      strip.background = element_rect(fill = "grey95"),
      axis.text.y      = element_text(size = 9),
      plot.title       = element_text(face = "bold", size = 12),
      plot.subtitle    = element_text(size = 8, color = "grey40"),
      plot.caption     = element_text(size = 7.5, color = "grey50"),
      plot.margin      = margin(8, 12, 8, 8))
  
  save_plot(g_t3, "G_T3_within_sd", width = 11, height = 7)
  cat("[G_T3] DONE\n")
} else {
  cat("[G_T3] SKIPPED -- insufficient data\n")
}


# ------------------------------------------------------------------------------
# G_T4 -- TMX FLOWERING TEMPERATURE DISTRIBUTION: ALL 8 CROPS
# ASCII fix: "degC" not degree symbol -- this was the main UTF-8 error
# ------------------------------------------------------------------------------

cat("\n[G_T4] Building TMX violin plot...\n")

tmx_long <- dplyr::bind_rows(
  lapply(names(pan_liste), function(urun) {
    df <- pan_liste[[urun]]
    if (!"TMX_floweringmean" %in% names(df)) return(NULL)
    data.frame(crop = urun,
               TMX  = df$TMX_floweringmean,
               stringsAsFactors = FALSE)
  })) %>%
  dplyr::filter(!is.na(TMX)) %>%
  dplyr::mutate(crop = factor(crop, levels = urun_listesi))

if (nrow(tmx_long) > 0) {
  
  tmx_summ <- tmx_long %>%
    dplyr::group_by(crop) %>%
    dplyr::summarise(
      med = median(TMX, na.rm = TRUE),
      mn  = mean(TMX,   na.rm = TRUE),
      .groups = "drop")
  
  g_t4 <- ggplot(tmx_long,
                 aes(x = crop, y = TMX, fill = crop)) +
    geom_violin(alpha = 0.70, trim = TRUE, linewidth = 0.3) +
    geom_boxplot(width        = 0.12,
                 fill         = "white",
                 outlier.size  = 0.7,
                 outlier.alpha = 0.35,
                 linewidth     = 0.4) +
    geom_point(data  = tmx_summ, aes(y = mn),
               shape = 18, size = 3.2, color = "grey15") +
    scale_fill_manual(values = renk_crop, guide = "none") +
    scale_x_discrete(labels = crop_labels) +
    labs(
      title    = "G_T4: Flowering Temperature Distribution -- All 8 Crops",
      subtitle = paste0("Violin = density | Box = IQR | Diamond = mean\n",
                        "Cool-season cereals (wheat/barley/oats/rye) vs ",
                        "warm-season crops (maize/sorghum/rice/soybean)"),
      caption  = paste0("Source: CRU TS4.09 x phenology windows | pan7* | 1961-2024\n",
                        "Schlenker & Roberts (2009, PNAS); Lobell et al. (2011, Science)"),
      x = NULL,
      y = "TMX Flowering Mean (degC)") +   # ASCII: degC not Unicode degree sign
    theme_bw(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x      = element_text(size = 10, face = "bold"),
      axis.text.y      = element_text(size = 9),
      plot.title       = element_text(face = "bold", size = 12),
      plot.subtitle    = element_text(size = 8.5, color = "grey40"),
      plot.caption     = element_text(size = 7.5, color = "grey50"),
      plot.margin      = margin(8, 12, 8, 8))
  
  save_plot(g_t4, "G_T4_tmx_violin", width = 11, height = 7)
  cat("[G_T4] DONE\n")
} else {
  cat("[G_T4] SKIPPED -- no TMX data\n")
}


# ------------------------------------------------------------------------------
# G_T5 -- ln_YIELD TREND: PRODUCTION-WEIGHTED GLOBAL MEAN PER YEAR PER CROP
# ------------------------------------------------------------------------------

cat("\n[G_T5] Building ln_yield trend plot...\n")

trend_long <- dplyr::bind_rows(
  lapply(names(pan_liste), function(urun) {
    df <- pan_liste[[urun]]
    if (!all(c("ln_yield", "Yil", "production_tonnes") %in% names(df)))
      return(NULL)
    df %>%
      dplyr::filter(!is.na(ln_yield) &
                      !is.na(production_tonnes) &
                      production_tonnes > 0) %>%
      dplyr::group_by(Yil) %>%
      dplyr::summarise(
        ln_yield_pw = weighted.mean(ln_yield, production_tonnes,
                                    na.rm = TRUE),
        n_country   = dplyr::n_distinct(Country_ISO),
        .groups     = "drop") %>%
      dplyr::mutate(crop = urun)
  })) %>%
  dplyr::filter(!is.na(ln_yield_pw)) %>%
  dplyr::mutate(crop = factor(crop, levels = urun_listesi))

if (nrow(trend_long) > 0) {
  
  # Vertical annotation at 1990 structural break (Paper 2 finding)
  break_anno <- data.frame(
    Yil  = 1990,
    crop = factor(urun_listesi, levels = urun_listesi))
  
  g_t5 <- ggplot(trend_long,
                 aes(x = Yil, y = ln_yield_pw, color = crop)) +
    geom_vline(data = break_anno, aes(xintercept = Yil),
               linetype = "dashed", color = "grey60",
               linewidth = 0.4, alpha = 0.7) +
    geom_line(linewidth = 0.85, alpha = 0.9) +
    geom_smooth(method = "lm", se = FALSE,
                linetype = "dotted",
                linewidth = 0.55, alpha = 0.5) +
    facet_wrap(~crop, scales = "free_y", nrow = 2,
               labeller = labeller(crop = crop_labels)) +
    scale_color_manual(values = renk_crop, guide = "none") +
    scale_x_continuous(breaks = c(1970, 1990, 2010)) +
    labs(
      title    = "G_T5: Global ln(Yield) Trend -- Production-Weighted Annual Mean",
      subtitle = paste0("Solid = annual mean | Dotted = linear trend | ",
                        "Dashed vertical = 1990 structural break\n",
                        "Production-weighted global average across all panel countries"),
      caption  = paste0("Source: FAO / pan7* | 1961-2024\n",
                        "Lobell et al. (2011, Science); Zhao et al. (2017, Nature Plants)"),
      x = "Year",
      y = "ln(Yield) [hg/ha]") +
    theme_bw(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold", size = 9.5),
      strip.background = element_rect(fill = "grey95"),
      axis.text.x      = element_text(size = 8),
      axis.text.y      = element_text(size = 8),
      plot.title       = element_text(face = "bold", size = 12),
      plot.subtitle    = element_text(size = 8, color = "grey40"),
      plot.caption     = element_text(size = 7.5, color = "grey50"),
      plot.margin      = margin(8, 12, 8, 8))
  
  save_plot(g_t5, "G_T5_lnyield_trend", width = 13, height = 8)
  cat("[G_T5] DONE\n")
} else {
  cat("[G_T5] SKIPPED -- insufficient data\n")
}


# ==============================================================================
# SAVE RESULTS TO EXCEL
# ==============================================================================

cat("\nSaving adim1_tanim_raporu.xlsx...\n")

sheets_out <- list(
  Coverage    = kapsam_df,
  Descriptive = tanim_df,
  MissingRate = missing_df,
  WithinCorr  = cor_df,
  ColumnCheck = data.frame(crop = rownames(sv_df), sv_df,
                           stringsAsFactors = FALSE))

if (nrow(outlier_df) > 0)
  sheets_out[["Outliers"]] <- outlier_df

writexl::write_xlsx(
  sheets_out,
  file.path(ana_yol, "adim1_tanim_raporu.xlsx"))

cat("[SAVED] adim1_tanim_raporu.xlsx\n")


# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n", strrep("=", 65), "\n")
cat("STEP 01 COMPLETE\n")
cat(strrep("=", 65), "\n\n")

cat("COMPLETED:\n")
cat("  Step 1 : Read + column check + duplicate detection\n")
cat("  Step 2 : Missing rate -- all crops x all variables\n")
cat("  Step 3 : Descriptive statistics (within-SD, Burke & Emerick 2016)\n")
cat("  Step 4 : Within-SD warning (Dell et al. 2014)\n")
cat("  Step 5 : TMX vs ln_yield within-country correlation\n")
cat("  Step 6 : Outlier flag (within-country z-score > 4)\n")
cat("  Step 7 : Coverage summary\n\n")

cat("PLOTS (each printed labeled + unlabeled on screen):\n")
cat("  G_T1 : Missing rate heatmap\n")
cat("  G_T2 : Coverage bar chart\n")
cat("  G_T3 : Within-SD dot plot (FE check)\n")
cat("  G_T4 : TMX violin (flowering temperature by crop)\n")
cat("  G_T5 : ln(yield) trend (production-weighted)\n\n")

cat("FILES SAVED (6 per plot = 30 total):\n")
cat("  figures/labeled/   -- PDF + 300dpi PNG + 600dpi PNG\n")
cat("  figures/unlabeled/ -- PDF + 300dpi PNG + 600dpi PNG\n\n")

cat("OUTPUT: adim1_tanim_raporu.xlsx\n")
cat("  Sheets: Coverage, Descriptive, MissingRate,\n")
cat("          WithinCorr, ColumnCheck, Outliers\n\n")

cat("NEXT STEP: paper1_step02_models.R\n")
cat("  -- GDP_pct < 70%% -> M3/M4 subsample will be smaller\n")
cat("  -- Review duplicates and outliers before modelling\n")
cat(strrep("=", 65), "\n")












# ==============================================================================
# PAPER 1 -- STEP 02: CORRELATION ANALYSIS
# Temperature, VPD, and Global Crop Yields: Evidence from a Multi-Crop Panel
#
# Contents:
#   A) Raw correlation matrix -- all crops
#   B) Within-country correlation -- Dell et al. (2014) FE analogue
#   C) Year-trend confounding check -- CO2/GDP vs TMX
#   D) VIF -- Belsley et al. (1980)
#   Plots: G_K1 to G_K6
#          -- R screen: labeled then unlabeled
#          -- Saved: PDF + PNG 300dpi + PNG 600dpi x labeled + unlabeled
#
# UTF-8 NOTE: All special characters replaced with ASCII
#   degree sign  -> degC or omitted
#   superscripts -> plain text (PRE2 not PRE^2)
#   em dash      -> --
#   multiply     -> x or omitted
#
# References:
#   Dell et al. (2014, AER)   -- within-country correlation
#   Lobell et al. (2011)      -- TMX-yield signal
#   Belsley et al. (1980)     -- VIF threshold
#   Zhao et al. (2017, PNAS)  -- CO2 confounding
# ==============================================================================


# ==============================================================================
# 0. SETUP
# ==============================================================================

library(dplyr)
library(readxl)
library(writexl)
library(ggplot2)
library(tidyr)

ana_yol      <- "C:/Users/Alper.Tosun/Desktop/cru130"
urun_listesi <- c("wheat","barley","oats","rye","rice","maize","soybean","sorghum")

# Output directories
fig_labeled   <- file.path(ana_yol, "figures", "labeled")
fig_unlabeled <- file.path(ana_yol, "figures", "unlabeled")
dir.create(fig_labeled,   recursive = TRUE, showWarnings = FALSE)
dir.create(fig_unlabeled, recursive = TRUE, showWarnings = FALSE)

# Model variables
MODEL_DEGISKENLER <- c(
  "ln_yield",
  "TMX_floweringmean",
  "PRE_grainfillsum",
  "PRE_sq",
  "irr_ratio",
  "ln_gdp",
  "CO2_ppm",
  "GDD_growth",
  "SPEI3_memory")

# Short display names -- ASCII only, no special characters
kisa_isim <- c(
  ln_yield          = "ln(Yield)",
  TMX_floweringmean = "TMX",
  PRE_grainfillsum  = "PRE",
  PRE_sq            = "PRE2",
  irr_ratio         = "IRR",
  ln_gdp            = "ln(GDP)",
  CO2_ppm           = "CO2",
  GDD_growth        = "GDD",
  SPEI3_memory      = "SPEI3")

# Crop colors
renk_crop <- c(
  wheat   = "#1A5276",
  barley  = "#2E86C1",
  oats    = "#27AE60",
  rye     = "#1E8449",
  rice    = "#F39C12",
  maize   = "#E67E22",
  soybean = "#8E44AD",
  sorghum = "#C0392B")

# Variable display order for heatmap axes
VAR_ORDER <- c("ln(Yield)", "TMX", "PRE", "PRE2",
               "IRR", "ln(GDP)", "CO2", "GDD", "SPEI3")


# ==============================================================================
# SAVE_PLOT FUNCTION
# Prints labeled + unlabeled to R screen, saves 6 files per plot
# ==============================================================================

save_plot <- function(g_labeled, plot_name, width = 10, height = 7) {
  
  g_unlabeled <- g_labeled +
    labs(title    = NULL,
         subtitle = NULL,
         caption  = NULL,
         x        = NULL,
         y        = NULL) +
    theme(legend.title = element_blank(),
          plot.margin  = margin(4, 4, 4, 4))
  
  # Print to R screen
  cat(sprintf("  [SCREEN] %s -- LABELED\n", plot_name))
  print(g_labeled)
  cat(sprintf("  [SCREEN] %s -- UNLABELED\n", plot_name))
  print(g_unlabeled)
  
  # Save helper
  save_one <- function(g, dir_path) {
    ggplot2::ggsave(
      file.path(dir_path, paste0(plot_name, ".pdf")),
      plot = g, width = width, height = height, device = "pdf")
    ggplot2::ggsave(
      file.path(dir_path, paste0(plot_name, "_300dpi.png")),
      plot = g, width = width, height = height, dpi = 300, device = "png")
    ggplot2::ggsave(
      file.path(dir_path, paste0(plot_name, "_600dpi.png")),
      plot = g, width = width, height = height, dpi = 600, device = "png")
  }
  
  save_one(g_labeled,   fig_labeled)
  save_one(g_unlabeled, fig_unlabeled)
  
  cat(sprintf("  [SAVED]  %s -> labeled/ + unlabeled/ (PDF + 300 + 600 dpi)\n\n",
              plot_name))
  invisible(NULL)
}


# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

# Convert correlation matrix to long format for ggplot
mat_to_long <- function(mat) {
  df        <- as.data.frame(mat)
  df$Var1   <- rownames(df)
  tidyr::pivot_longer(df, -Var1, names_to = "Var2", values_to = "r")
}

# VIF: manual OLS method, no car package needed
# Belsley et al. (1980): VIF_j = 1 / (1 - R2_j)
hesapla_vif <- function(df_model) {
  x_vars <- setdiff(names(df_model), "ln_yield")
  x_vars <- x_vars[sapply(x_vars, function(v) {
    x <- df_model[[v]]
    !all(is.na(x)) && sd(x, na.rm = TRUE) > 0
  })]
  if (length(x_vars) < 2) return(NULL)
  
  vif_vals <- sapply(x_vars, function(v) {
    others <- setdiff(x_vars, v)
    if (length(others) == 0) return(1)
    fm  <- as.formula(paste(v, "~", paste(others, collapse = "+")))
    fit <- tryCatch(lm(fm, data = df_model), error = function(e) NULL)
    if (is.null(fit)) return(NA_real_)
    r2 <- summary(fit)$r.squared
    if (is.na(r2) || r2 >= 1) return(Inf)
    1 / (1 - r2)
  })
  round(sort(vif_vals, decreasing = TRUE), 2)
}

# Significance stars from r and n
sig_r <- function(r, n) {
  result <- rep("", length(r))
  ok     <- !is.na(r) & !is.na(n) & n >= 4 & abs(r) < 1
  if (any(ok)) {
    t_val      <- r[ok] * sqrt(n[ok] - 2) / sqrt(1 - r[ok]^2)
    pv         <- 2 * pt(-abs(t_val), df = n[ok] - 2)
    result[ok] <- ifelse(pv < 0.01,  "***",
                         ifelse(pv < 0.05,  "**",
                                ifelse(pv < 0.10,  "*", "")))
  }
  result
}

# Safe lookup in correlation matrix by original variable name
get_r <- function(mat, v1, v2, name_map) {
  s1 <- name_map[v1]; s2 <- name_map[v2]
  if (all(c(s1, s2) %in% rownames(mat))) mat[s1, s2] else NA_real_
}


# ==============================================================================
# MAIN LOOP -- read + compute correlations for all crops
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("STEP 02 -- CORRELATION ANALYSIS (all crops)\n")
cat(strrep("=", 65), "\n\n")

ozet_listesi    <- list()
rapor_listesi   <- list()
vif_listesi     <- list()
ham_long_all    <- list()
within_long_all <- list()

for (urun in urun_listesi) {
  dosya <- file.path(ana_yol, paste0("pan7", urun, ".xlsx"))
  if (!file.exists(dosya)) {
    cat(sprintf("[MISSING] pan7%s.xlsx\n", urun)); next
  }
  
  cat(sprintf("\n%s\n%s\n%s\n",
              strrep("=", 60), toupper(urun), strrep("=", 60)))
  
  df_raw <- as.data.frame(readxl::read_excel(dosya))
  
  # Fix decimal comma in character columns
  for (col in names(df_raw)) {
    if (is.character(df_raw[[col]])) {
      d <- suppressWarnings(as.numeric(gsub(",", ".", df_raw[[col]])))
      if (mean(!is.na(d)) > 0.80) df_raw[[col]] <- d
    }
  }
  df_raw$Yil <- as.integer(as.character(df_raw$Yil))
  
  # Derived variables
  df_raw$ln_yield <- suppressWarnings(log(df_raw$yield_hg_per_ha))
  df_raw$ln_yield[!is.finite(df_raw$ln_yield)] <- NA
  
  df_raw$ln_gdp <- suppressWarnings(log(df_raw$GDP_per_Capita))
  df_raw$ln_gdp[!is.finite(df_raw$ln_gdp)] <- NA
  
  if ("PRE_grainfillsum" %in% names(df_raw))
    df_raw$PRE_sq <- df_raw$PRE_grainfillsum^2
  
  if (!"irr_ratio" %in% names(df_raw) && "IRR" %in% names(df_raw))
    df_raw$irr_ratio <- df_raw$IRR
  
  # Select available model variables
  mev       <- intersect(MODEL_DEGISKENLER, names(df_raw))
  df_model  <- df_raw[, c("Country_ISO", "Yil", mev), drop = FALSE]
  
  # Remove duplicates: keep first row per country-year
  df_model <- df_model[!duplicated(df_model[, c("Country_ISO", "Yil")]), ]
  df_model <- df_model[complete.cases(df_model[, mev]), ]
  
  cat(sprintf("Complete observations: %d | Countries: %d | Years: %d-%d\n",
              nrow(df_model),
              length(unique(df_model$Country_ISO)),
              min(df_model$Yil, na.rm = TRUE),
              max(df_model$Yil, na.rm = TRUE)))
  
  if (nrow(df_model) < 30) {
    cat("  [SKIP] Too few complete observations\n"); next
  }
  
  # ── A) RAW CORRELATION ─────────────────────────────────────────────────────
  cor_ham          <- cor(df_model[, mev], use = "complete.obs")
  short_names      <- kisa_isim[rownames(cor_ham)]
  rownames(cor_ham) <- short_names
  colnames(cor_ham) <- short_names
  
  cat("\n[Raw correlation -- |r| > 0.4]\n")
  for (i in seq_len(nrow(cor_ham) - 1)) {
    for (j in (i + 1):ncol(cor_ham)) {
      r <- cor_ham[i, j]
      if (abs(r) >= 0.40) {
        flag <- ifelse(abs(r) >= 0.70, "*** HIGH",
                       ifelse(abs(r) >= 0.50, "**  MEDIUM", "*   NOTE"))
        cat(sprintf("  %-12s -- %-12s : %+.3f  %s\n",
                    rownames(cor_ham)[i], colnames(cor_ham)[j], r, flag))
      }
    }
  }
  
  # ── B) WITHIN-COUNTRY CORRELATION ──────────────────────────────────────────
  # Dell et al. (2014): FE identification uses within-country variation
  df_within <- df_model %>%
    dplyr::group_by(Country_ISO) %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(mev),
      ~ . - mean(., na.rm = TRUE))) %>%
    dplyr::ungroup()
  
  cor_within           <- cor(df_within[, mev], use = "complete.obs")
  rownames(cor_within) <- kisa_isim[rownames(cor_within)]
  colnames(cor_within) <- kisa_isim[colnames(cor_within)]
  
  cat("\n[Within-country correlation -- |r| > 0.4]\n")
  for (i in seq_len(nrow(cor_within) - 1)) {
    for (j in (i + 1):ncol(cor_within)) {
      r <- cor_within[i, j]
      if (abs(r) >= 0.40) {
        flag <- ifelse(abs(r) >= 0.70, "*** HIGH",
                       ifelse(abs(r) >= 0.50, "**  MEDIUM", "*   NOTE"))
        cat(sprintf("  %-12s -- %-12s : %+.3f  %s\n",
                    rownames(cor_within)[i], colnames(cor_within)[j], r, flag))
      }
    }
  }
  
  # ── C) YEAR-TREND CONFOUNDING CHECK ────────────────────────────────────────
  cat("\n[Year-trend confounding check]\n")
  trend_vars <- intersect(c("CO2_ppm", "ln_gdp"), mev)
  for (tv in trend_vars) {
    tv_s   <- kisa_isim[tv]
    r_tmx  <- if (all(c(tv_s, "TMX") %in% rownames(cor_within)))
      cor_within[tv_s, "TMX"] else NA_real_
    r_lny  <- if (all(c(tv_s, "ln(Yield)") %in% rownames(cor_within)))
      cor_within[tv_s, "ln(Yield)"] else NA_real_
    cat(sprintf("  %s -- TMX within r = %+.3f | %s -- ln(Yield) within r = %+.3f\n",
                tv_s, r_tmx, tv_s, r_lny))
  }
  cat("  NOTE: High CO2-TMX within-r -> CO2 may confound TMX coefficient\n")
  cat("        Year FE absorbs common time trend\n")
  
  # ── D) VIF ──────────────────────────────────────────────────────────────────
  cat("\n[VIF -- full variable set]\n")
  vif_v <- hesapla_vif(df_model[, mev])
  if (!is.null(vif_v)) {
    print(vif_v)
    hi_vif <- vif_v[!is.na(vif_v) & vif_v > 10]
    if (length(hi_vif) > 0) {
      cat(sprintf("  [WARNING] VIF > 10: %s\n",
                  paste(names(hi_vif), collapse = ", ")))
      cat("  -> Expected: CO2 + year FE inflate time-trending variables\n")
    } else {
      cat("  [OK] All VIF < 10\n")
    }
    vif_listesi[[urun]] <- data.frame(
      crop     = urun,
      variable = names(vif_v),
      VIF      = as.numeric(vif_v),
      hi_vif   = as.numeric(vif_v) > 10,
      stringsAsFactors = FALSE)
  }
  
  # ── E) Store for summary and plots ─────────────────────────────────────────
  ozet_listesi[[urun]] <- data.frame(
    crop           = urun,
    n_obs          = nrow(df_model),
    TMX_r_raw      = round(get_r(cor_ham,   "ln_yield", "TMX_floweringmean",
                                 kisa_isim), 3),
    PRE_r_raw      = round(get_r(cor_ham,   "ln_yield", "PRE_grainfillsum",
                                 kisa_isim), 3),
    TMX_r_within   = round(get_r(cor_within,"ln_yield", "TMX_floweringmean",
                                 kisa_isim), 3),
    PRE_r_within   = round(get_r(cor_within,"ln_yield", "PRE_grainfillsum",
                                 kisa_isim), 3),
    CO2_TMX_within = round(get_r(cor_within,"CO2_ppm",  "TMX_floweringmean",
                                 kisa_isim), 3),
    GDP_IRR_within = round(get_r(cor_within,"ln_gdp",   "irr_ratio",
                                 kisa_isim), 3),
    stringsAsFactors = FALSE)
  
  ham_long_all[[urun]]    <- mat_to_long(cor_ham)    %>%
    dplyr::mutate(crop = urun, type = "raw")
  within_long_all[[urun]] <- mat_to_long(cor_within) %>%
    dplyr::mutate(crop = urun, type = "within")
  
  rapor_listesi[[paste0(urun, "_raw")]] <-
    as.data.frame(cor_ham) %>%
    dplyr::mutate(variable = rownames(.), crop = urun, type = "raw")
  rapor_listesi[[paste0(urun, "_within")]] <-
    as.data.frame(cor_within) %>%
    dplyr::mutate(variable = rownames(.), crop = urun, type = "within")
}


# ==============================================================================
# SUMMARY OBJECTS
# ==============================================================================

ozet_df       <- dplyr::bind_rows(ozet_listesi)
vif_df        <- dplyr::bind_rows(vif_listesi)
ham_all_df    <- dplyr::bind_rows(ham_long_all)
within_all_df <- dplyr::bind_rows(within_long_all)


# ==============================================================================
# CONSOLE SUMMARY TABLE
# ==============================================================================

cat("\n\n", strrep("=", 75), "\n")
cat("CORRELATION SUMMARY\n")
cat(strrep("=", 75), "\n\n")
cat(sprintf("%-10s  %8s  %8s  %8s  %8s  %8s  %8s\n",
            "Crop", "TMX_raw", "TMX_wthn",
            "PRE_raw", "PRE_wthn", "CO2-TMX", "GDP-IRR"))
cat(strrep("-", 68), "\n")
for (i in seq_len(nrow(ozet_df))) {
  r <- ozet_df[i, ]
  cat(sprintf("%-10s  %+8.3f  %+8.3f  %+8.3f  %+8.3f  %+8.3f  %+8.3f\n",
              r$crop, r$TMX_r_raw, r$TMX_r_within,
              r$PRE_r_raw, r$PRE_r_within,
              r$CO2_TMX_within, r$GDP_IRR_within))
}
cat("\nNOTE: CO2_TMX_within high -> Zhao (2017) confounding risk\n")
cat("      GDP_IRR_within high -> M4 multicollinearity concern\n\n")


# ==============================================================================
# PLOTS -- G_K1 to G_K6
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("GENERATING PLOTS (G_K1 to G_K6)\n")
cat("Each plot: screen print x2 (labeled + unlabeled), saved x6\n")
cat(strrep("=", 65), "\n")


# ------------------------------------------------------------------------------
# G_K1 -- RAW CORRELATION HEATMAP: ALL 8 CROPS FACET
# x-axis at bottom, consistent variable order, ASCII labels
# ------------------------------------------------------------------------------

cat("\n[G_K1] Raw correlation heatmap all crops...\n")

if (nrow(ham_all_df) > 0) {
  
  ham_plot <- ham_all_df %>%
    dplyr::filter(Var1 != Var2) %>%
    dplyr::mutate(
      crop = factor(crop, levels = urun_listesi),
      Var1 = factor(Var1, levels = rev(intersect(VAR_ORDER, unique(Var1)))),
      Var2 = factor(Var2, levels = intersect(VAR_ORDER, unique(Var2))))
  
  g_k1 <- ggplot(ham_plot, aes(x = Var2, y = Var1, fill = r)) +
    geom_tile(color = "white", linewidth = 0.3) +
    geom_text(aes(label = sprintf("%.2f", r)),
              size = 1.9, color = "grey20", show.legend = FALSE) +
    facet_wrap(~crop, nrow = 2) +
    scale_fill_gradient2(
      low      = "#C0392B",
      mid      = "white",
      high     = "#2471A3",
      midpoint = 0,
      limits   = c(-1, 1),
      name     = "r") +
    labs(
      title    = "G_K1: Raw Correlation Matrix -- All 8 Crops",
      subtitle = "Red = negative | Blue = positive | Includes time trends (CO2, GDP)",
      caption  = paste0("Complete-case observations | Before within-country demeaning\n",
                        "Dell et al. (2014, AER)"),
      x = NULL, y = NULL) +
    theme_bw(base_size = 8) +
    theme(
      panel.grid      = element_blank(),
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 6),
      axis.text.y     = element_text(size = 6),
      strip.text      = element_text(face = "bold", size = 8),
      strip.background= element_rect(fill = "grey95"),
      legend.position = "bottom",
      plot.title      = element_text(face = "bold", size = 11),
      plot.subtitle   = element_text(size = 7, color = "grey40"),
      plot.caption    = element_text(size = 7, color = "grey50"),
      plot.margin     = margin(5, 5, 5, 5))
  
  save_plot(g_k1, "G_K1_raw_corr_heatmap", width = 14, height = 9)
  cat("[G_K1] DONE\n")
} else {
  cat("[G_K1] SKIPPED -- no data\n")
}


# ------------------------------------------------------------------------------
# G_K2 -- WITHIN-COUNTRY CORRELATION HEATMAP: ALL 8 CROPS FACET
# ------------------------------------------------------------------------------

cat("\n[G_K2] Within-country correlation heatmap all crops...\n")

if (nrow(within_all_df) > 0) {
  
  within_plot <- within_all_df %>%
    dplyr::filter(Var1 != Var2) %>%
    dplyr::mutate(
      crop = factor(crop, levels = urun_listesi),
      Var1 = factor(Var1, levels = rev(intersect(VAR_ORDER, unique(Var1)))),
      Var2 = factor(Var2, levels = intersect(VAR_ORDER, unique(Var2))))
  
  g_k2 <- ggplot(within_plot, aes(x = Var2, y = Var1, fill = r)) +
    geom_tile(color = "white", linewidth = 0.3) +
    geom_text(aes(label = sprintf("%.2f", r)),
              size = 1.9, color = "grey20", show.legend = FALSE) +
    facet_wrap(~crop, nrow = 2) +
    scale_fill_gradient2(
      low      = "#C0392B",
      mid      = "white",
      high     = "#2471A3",
      midpoint = 0,
      limits   = c(-1, 1),
      name     = "r") +
    labs(
      title    = "G_K2: Within-Country Correlation Matrix -- All 8 Crops",
      subtitle = paste0("After removing country means | ",
                        "Dell et al. (2014): FE identification uses within variation"),
      caption  = paste0("Within-country demeaned variables | ",
                        "CO2/GDP time trends largely removed by demeaning"),
      x = NULL, y = NULL) +
    theme_bw(base_size = 8) +
    theme(
      panel.grid       = element_blank(),
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 6),
      axis.text.y      = element_text(size = 6),
      strip.text       = element_text(face = "bold", size = 8),
      strip.background = element_rect(fill = "grey95"),
      legend.position  = "bottom",
      plot.title       = element_text(face = "bold", size = 11),
      plot.subtitle    = element_text(size = 7, color = "grey40"),
      plot.caption     = element_text(size = 7, color = "grey50"),
      plot.margin      = margin(5, 5, 5, 5))
  
  save_plot(g_k2, "G_K2_within_corr_heatmap", width = 14, height = 9)
  cat("[G_K2] DONE\n")
} else {
  cat("[G_K2] SKIPPED -- no data\n")
}


# ------------------------------------------------------------------------------
# G_K3 -- TMX WITHIN-r BAR CHART: 8 CROPS
# Ref: Lobell et al. (2011) -- TMX-yield signal
# ------------------------------------------------------------------------------

cat("\n[G_K3] TMX within-r bar chart...\n")

if (nrow(ozet_df) > 0) {
  
  g_k3 <- ggplot(ozet_df,
                 aes(x = reorder(crop, TMX_r_within),
                     y = TMX_r_within,
                     fill = TMX_r_within)) +
    geom_col(color = "white", alpha = 0.85) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_text(
      aes(label = sprintf("%+.3f%s",
                          TMX_r_within,
                          sig_r(TMX_r_within, n_obs)),
          y = TMX_r_within + ifelse(TMX_r_within >= 0, 0.015, -0.015)),
      size = 3.5, hjust = 0.5) +
    scale_fill_gradient2(
      low      = "#C0392B",
      mid      = "white",
      high     = "#2471A3",
      midpoint = 0,
      guide    = "none") +
    coord_flip() +
    labs(
      title    = "G_K3: TMX -- ln(Yield) Within-Country Correlation -- 8 Crops",
      subtitle = paste0("After country mean removal (FE analogue) | Negative expected\n",
                        "Lobell et al. (2011): wheat r ~ -0.15 to -0.25"),
      caption  = paste0("Dell et al. (2014, AER) | Stars = significance level\n",
                        "*** p<0.01  ** p<0.05  * p<0.10"),
      x = NULL, y = "Within-country r") +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.y      = element_text(size = 11, face = "bold"),
      plot.title       = element_text(face = "bold", size = 12),
      plot.subtitle    = element_text(size = 8, color = "grey40"),
      plot.caption     = element_text(size = 7.5, color = "grey50"),
      plot.margin      = margin(8, 12, 8, 8))
  
  save_plot(g_k3, "G_K3_tmx_within_r", width = 9, height = 6)
  cat("[G_K3] DONE\n")
} else {
  cat("[G_K3] SKIPPED -- no summary data\n")
}


# ------------------------------------------------------------------------------
# G_K4 -- PRE WITHIN-r BAR CHART: 8 CROPS
# Ref: Blanc & Schlenker (2017, REEP)
# ------------------------------------------------------------------------------

cat("\n[G_K4] PRE within-r bar chart...\n")

if (nrow(ozet_df) > 0) {
  
  g_k4 <- ggplot(ozet_df,
                 aes(x = reorder(crop, PRE_r_within),
                     y = PRE_r_within,
                     fill = PRE_r_within)) +
    geom_col(color = "white", alpha = 0.85) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_text(
      aes(label = sprintf("%+.3f%s",
                          PRE_r_within,
                          sig_r(PRE_r_within, n_obs)),
          y = PRE_r_within + ifelse(PRE_r_within >= 0, 0.015, -0.015)),
      size = 3.5, hjust = 0.5) +
    scale_fill_gradient2(
      low      = "#C0392B",
      mid      = "white",
      high     = "#2471A3",
      midpoint = 0,
      guide    = "none") +
    coord_flip() +
    labs(
      title    = "G_K4: PRE (Grain Fill) -- ln(Yield) Within-Country Correlation -- 8 Crops",
      subtitle = paste0("After country mean removal | Positive expected\n",
                        "Caution: irrigation confounding may weaken signal in arid regions"),
      caption  = paste0("Blanc & Schlenker (2017, REEP) -- PRE identification challenge\n",
                        "*** p<0.01  ** p<0.05  * p<0.10"),
      x = NULL, y = "Within-country r") +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.y      = element_text(size = 11, face = "bold"),
      plot.title       = element_text(face = "bold", size = 12),
      plot.subtitle    = element_text(size = 8, color = "grey40"),
      plot.caption     = element_text(size = 7.5, color = "grey50"),
      plot.margin      = margin(8, 12, 8, 8))
  
  save_plot(g_k4, "G_K4_pre_within_r", width = 9, height = 6)
  cat("[G_K4] DONE\n")
} else {
  cat("[G_K4] SKIPPED -- no summary data\n")
}


# ------------------------------------------------------------------------------
# G_K5 -- VIF DOT PLOT: ALL VARIABLES x ALL CROPS
# Ref: Belsley et al. (1980) -- VIF > 10 = serious multicollinearity
# ------------------------------------------------------------------------------

cat("\n[G_K5] VIF dot plot...\n")

if (nrow(vif_df) > 0) {
  
  # Median VIF per variable across crops (for y-axis ordering)
  var_vif_order <- vif_df %>%
    dplyr::filter(!is.na(VIF) & !is.infinite(VIF)) %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(med = median(VIF, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(med) %>%
    dplyr::pull(variable)
  
  vif_plot <- vif_df %>%
    dplyr::filter(!is.na(VIF) & !is.infinite(VIF)) %>%
    dplyr::mutate(
      crop    = factor(crop, levels = urun_listesi),
      variable = factor(variable, levels = var_vif_order),
      vif_cat = dplyr::case_when(
        VIF > 10 ~ ">10 (problematic)",
        VIF >  5 ~ "5-10 (moderate)",
        TRUE     ~ "<5 (acceptable)"),
      vif_cat = factor(vif_cat,
                       levels = c(">10 (problematic)",
                                  "5-10 (moderate)",
                                  "<5 (acceptable)")))
  
  vif_cols <- c(">10 (problematic)" = "#C0392B",
                "5-10 (moderate)"   = "#E67E22",
                "<5 (acceptable)"   = "#27AE60")
  
  x_lim <- max(max(vif_plot$VIF, na.rm = TRUE) * 1.25, 13)
  
  g_k5 <- ggplot(vif_plot,
                 aes(x = VIF, y = variable, fill = vif_cat)) +
    geom_col(alpha = 0.80, width = 0.65) +
    geom_vline(xintercept =  5, linetype = "dashed",
               color = "#E67E22", linewidth = 0.7) +
    geom_vline(xintercept = 10, linetype = "dashed",
               color = "#C0392B", linewidth = 0.7) +
    geom_text(aes(label = sprintf("%.1f", VIF)),
              hjust = -0.15, size = 2.5, color = "grey20") +
    facet_wrap(~crop, nrow = 2) +
    scale_fill_manual(values = vif_cols, name = "VIF Level") +
    scale_x_continuous(
      limits = c(0, x_lim),
      breaks = c(0, 2, 5, 10),
      expand = expansion(mult = c(0, 0))) +
    labs(
      title    = "G_K5: Variance Inflation Factor (VIF) -- All Variables x All Crops",
      subtitle = paste0("VIF > 10 = serious multicollinearity (red dashed) | ",
                        "VIF > 5 = moderate (orange dashed)\n",
                        "PRE + PRE2 structural multicollinearity expected"),
      caption  = paste0("Belsley et al. (1980) | Manual OLS: VIF = 1/(1 - R2_j)\n",
                        "Year FE absorbs CO2/GDP common trend in final models"),
      x = "VIF", y = NULL) +
    theme_bw(base_size = 9) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.text         = element_text(face = "bold", size = 9),
      strip.background   = element_rect(fill = "grey95"),
      legend.position    = "bottom",
      plot.title         = element_text(face = "bold", size = 11),
      plot.subtitle      = element_text(size = 7, color = "grey40"),
      plot.caption       = element_text(size = 7, color = "grey50"),
      plot.margin        = margin(5, 10, 5, 5))
  
  save_plot(g_k5, "G_K5_vif", width = 14, height = 9)
  cat("[G_K5] DONE\n")
} else {
  cat("[G_K5] SKIPPED -- no VIF data\n")
}


# ------------------------------------------------------------------------------
# G_K6 -- CO2/GDP CONFOUNDING CHECK
# Ref: Zhao et al. (2017, PNAS); Blanc & Schlenker (2017, REEP)
# ASCII fix: "--" not em dash, no Unicode characters in labels
# ------------------------------------------------------------------------------

cat("\n[G_K6] CO2/GDP confounding check...\n")

if (nrow(ozet_df) > 0 &&
    "CO2_TMX_within" %in% names(ozet_df) &&
    "GDP_IRR_within" %in% names(ozet_df)) {
  
  conf_long <- ozet_df %>%
    dplyr::select(crop, CO2_TMX_within, GDP_IRR_within) %>%
    tidyr::pivot_longer(
      cols      = c(CO2_TMX_within, GDP_IRR_within),
      names_to  = "pair",
      values_to = "r_within") %>%
    dplyr::mutate(
      crop = factor(crop, levels = rev(urun_listesi)),
      # ASCII only labels -- no special characters
      pair = dplyr::recode(pair,
                           "CO2_TMX_within" = "CO2 -- TMX (Zhao 2017)",
                           "GDP_IRR_within" = "ln(GDP) -- IRR (multicollinearity)"))
  
  g_k6 <- ggplot(conf_long,
                 aes(x = r_within, y = crop,
                     color = pair, shape = pair)) +
    geom_vline(xintercept =  0,   linetype = "dashed",
               color = "grey50", linewidth = 0.6) +
    geom_vline(xintercept =  0.5, linetype = "dotted",
               color = "grey70", linewidth = 0.5) +
    geom_vline(xintercept = -0.5, linetype = "dotted",
               color = "grey70", linewidth = 0.5) +
    geom_point(size = 3.5, alpha = 0.85) +
    scale_color_manual(
      values = c(
        "CO2 -- TMX (Zhao 2017)"          = "#C0392B",
        "ln(GDP) -- IRR (multicollinearity)" = "#8E44AD"),
      name = "Variable Pair") +
    scale_shape_manual(
      values = c(
        "CO2 -- TMX (Zhao 2017)"          = 16,
        "ln(GDP) -- IRR (multicollinearity)" = 17),
      name = "Variable Pair") +
    annotate("text", x = 0.52, y = 0.7,
             label = "r = 0.5 threshold",
             color = "grey55", size = 2.8, hjust = 0) +
    labs(
      title    = "G_K6: Confounding Variable Within-Country Correlations",
      subtitle = paste0("CO2-TMX: high within-r = CO2 may confound TMX coefficient ",
                        "(Zhao et al. 2017)\n",
                        "GDP-IRR: high within-r = multicollinearity between income ",
                        "and irrigation"),
      caption  = "Zhao et al. (2017, PNAS); Blanc & Schlenker (2017, REEP)",
      x = "Within-country r", y = NULL) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      axis.text.y      = element_text(size = 10, face = "bold"),
      plot.title       = element_text(face = "bold", size = 12),
      plot.subtitle    = element_text(size = 8, color = "grey40"),
      plot.caption     = element_text(size = 7.5, color = "grey50"),
      plot.margin      = margin(8, 12, 8, 8))
  
  save_plot(g_k6, "G_K6_confounding_check", width = 10, height = 7)
  cat("[G_K6] DONE\n")
} else {
  cat("[G_K6] SKIPPED -- missing summary columns\n")
}


# ==============================================================================
# SAVE RESULTS TO EXCEL
# ==============================================================================

cat("\nSaving adim2_korelasyon_raporu.xlsx...\n")

rapor_wide_df <- dplyr::bind_rows(rapor_listesi)

writexl::write_xlsx(
  list(
    Summary         = ozet_df,
    VIF_AllCrops    = vif_df,
    RawCorr_Long    = ham_all_df,
    WithinCorr_Long = within_all_df,
    RawCorr_Wide    = rapor_wide_df %>% dplyr::filter(type == "raw"),
    WithinCorr_Wide = rapor_wide_df %>% dplyr::filter(type == "within")),
  file.path(ana_yol, "adim2_korelasyon_raporu.xlsx"))

cat("[SAVED] adim2_korelasyon_raporu.xlsx\n")


# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n", strrep("=", 65), "\n")
cat("STEP 02 COMPLETE\n")
cat(strrep("=", 65), "\n\n")

cat("COMPLETED:\n")
cat("  A) Raw correlation matrix -- all crops\n")
cat("  B) Within-country correlation -- Dell et al. (2014)\n")
cat("  C) Year-trend confounding check -- CO2/GDP vs TMX\n")
cat("  D) VIF -- Belsley et al. (1980)\n\n")

cat("PLOTS (each printed labeled + unlabeled on screen):\n")
cat("  G_K1 : Raw correlation heatmap -- 8 crops facet\n")
cat("  G_K2 : Within-country correlation heatmap -- 8 crops facet\n")
cat("  G_K3 : TMX within-r bar chart\n")
cat("  G_K4 : PRE within-r bar chart\n")
cat("  G_K5 : VIF dot plot -- all variables x all crops\n")
cat("  G_K6 : CO2/GDP confounding check\n\n")

cat("FILES SAVED (6 per plot = 36 total):\n")
cat("  figures/labeled/   -- PDF + 300dpi PNG + 600dpi PNG\n")
cat("  figures/unlabeled/ -- PDF + 300dpi PNG + 600dpi PNG\n\n")

cat("OUTPUT: adim2_korelasyon_raporu.xlsx\n")
cat("  Sheets: Summary, VIF_AllCrops, RawCorr_Long,\n")
cat("          WithinCorr_Long, RawCorr_Wide, WithinCorr_Wide\n\n")

cat("NEXT STEP: paper1_step03_models.R\n")
cat("  -- TMX within-r negative and significant? -> model can be estimated\n")
cat("  -- CO2_TMX_within high? -> CO2 control is important\n")
cat("  -- VIF > 10? -> remove that variable from M1/M2\n")
cat(strrep("=", 65), "\n")

















# ==============================================================================
# PAPER 1 -- STEP 03: TWO-WAY FE MODELS
# Temperature, VPD, and Global Crop Yields: Evidence from a Multi-Crop Panel
#
# 8 crops x 8 models x 2 weights = 128 estimates
# Plus: G14 (FE comparison), G15 (PRE robustness), G16 (CO2 robustness)
#
# Models:
#   M1: ln_yield ~ TMX + PRE + PRE_sq                            | iso + year
#   M2: ln_yield ~ TMX + PRE + PRE_sq + IRR                      | iso + year
#   M3: ln_yield ~ TMX + PRE + PRE_sq + ln_gdp                   | iso + year (GDP subsample)
#   M4: ln_yield ~ TMX + PRE + PRE_sq + IRR + ln_gdp             | iso + year (GDP subsample)
#   M5: ln_yield ~ TMX * IRR + PRE + PRE_sq                      | iso + year
#   M6: ln_yield ~ TMX * is_arid + PRE + PRE_sq + IRR            | iso + year
#   M7: ln_yield ~ TMX + PRE + PRE_sq + IRR + ln_gdp + TMX_lngdp | iso + year (GDP subsample)
#   M8: ln_yield ~ TMX * PRE + PRE_sq + IRR                      | iso + year
#
# Plots: G1-G16, each saved as PDF + PNG 300dpi + PNG 600dpi x labeled + unlabeled
#
# UTF-8 NOTE: All special characters replaced with ASCII
#   degree sign  -> degC
#   superscripts -> PRE2 not PRE^2
#   em dash      -> --
#   multiply     -> x or omitted
#
# References:
#   Lobell et al. (2011, Science)    -- benchmark TMX coefficient
#   Dell et al. (2014, AER)          -- two-way FE identification
#   Burke & Emerick (2016, AEJ)      -- panel design
#   Schlenker & Roberts (2009, PNAS) -- nonlinear PRE response
#   Zhao et al. (2017, PNAS)         -- CO2 robustness
# ==============================================================================


# ==============================================================================
# 0. SETUP
# ==============================================================================

library(dplyr)
library(readxl)
library(fixest)
library(ggplot2)
library(writexl)
library(tidyr)

ana_yol      <- "C:/Users/Alper.Tosun/Desktop/cru130"
urun_listesi <- c("wheat","barley","oats","rye","rice","maize","soybean","sorghum")

# Output directories
fig_labeled   <- file.path(ana_yol, "figures", "labeled")
fig_unlabeled <- file.path(ana_yol, "figures", "unlabeled")
dir.create(fig_labeled,   recursive = TRUE, showWarnings = FALSE)
dir.create(fig_unlabeled, recursive = TRUE, showWarnings = FALSE)

# Model and crop colors
renk8m <- c(M1 = "#95A5A6", M2 = "#E74C3C", M3 = "#3498DB",
            M4 = "#8E44AD", M5 = "#27AE60", M6 = "#F39C12",
            M7 = "#1A5276", M8 = "#E91E63")

renk_crop <- c(wheat   = "#1A5276", barley  = "#2E86C1",
               oats    = "#27AE60", rye     = "#1E8449",
               rice    = "#F39C12", maize   = "#E67E22",
               soybean = "#8E44AD", sorghum = "#C0392B")


# ==============================================================================
# SAVE_PLOT FUNCTION
# Prints labeled + unlabeled to R screen, saves 6 files per plot
# ==============================================================================

save_plot <- function(g_labeled, plot_name, width = 10, height = 7) {
  
  g_unlabeled <- g_labeled +
    labs(title    = NULL,
         subtitle = NULL,
         caption  = NULL,
         x        = NULL,
         y        = NULL) +
    theme(legend.title = element_blank(),
          plot.margin  = margin(4, 4, 4, 4))
  
  cat(sprintf("  [SCREEN] %s -- LABELED\n",   plot_name)); print(g_labeled)
  cat(sprintf("  [SCREEN] %s -- UNLABELED\n", plot_name)); print(g_unlabeled)
  
  save_one <- function(g, dir_path) {
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, ".pdf")),
                    plot = g, width = width, height = height, device = "pdf")
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, "_300dpi.png")),
                    plot = g, width = width, height = height,
                    dpi = 300, device = "png")
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, "_600dpi.png")),
                    plot = g, width = width, height = height,
                    dpi = 600, device = "png")
  }
  save_one(g_labeled,   fig_labeled)
  save_one(g_unlabeled, fig_unlabeled)
  
  cat(sprintf("  [SAVED]  %s -> labeled/ + unlabeled/ (PDF + 300 + 600 dpi)\n\n",
              plot_name))
  invisible(NULL)
}


# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

# Significance stars
sig_s <- function(p)
  ifelse(is.na(p), "",
         ifelse(p < 0.01, "***",
                ifelse(p < 0.05, "**",
                       ifelse(p < 0.10, "*", ""))))

# Fit model with weights and clustered SE
fit_model <- function(formula, data, wt_var, lbl) {
  w <- data[[wt_var]]
  w[is.na(w) | w <= 0] <- NA
  m <- tryCatch(
    fixest::feols(formula, data = data,
                  weights = w, cluster = ~Country_ISO),
    error = function(e) {
      cat(sprintf("  [ERROR %-14s]: %s\n", lbl, e$message)); NULL
    })
  if (!is.null(m)) {
    ar2 <- tryCatch(as.numeric(fixest::r2(m, "ar2")), error = function(e) NA_real_)
    n   <- tryCatch(stats::nobs(m),                   error = function(e) NA_integer_)
    cat(sprintf("  [%-16s]  AdjR2 = %+.4f  N = %d\n", lbl,
                ifelse(is.na(ar2), -99, ar2),
                ifelse(is.na(n),    0L,  n)))
  }
  m
}

# Extract coefficient table from a fitted model
coef_tablo_full <- function(m, nm, urun) {
  if (is.null(m)) return(NULL)
  cf <- tryCatch(coef(m), error = function(e) NULL)
  vc <- tryCatch(vcov(m), error = function(e) NULL)
  if (is.null(cf) || is.null(vc)) return(NULL)
  se <- sqrt(diag(vc))
  pv <- 2 * pnorm(-abs(cf / se))
  data.frame(
    crop        = urun,
    model       = nm,
    model_short = sub("_(AW|PW)$", "", nm),
    weight      = ifelse(grepl("_AW$", nm), "Area-Weighted", "Production-Weighted"),
    variable    = names(cf),
    coef        = as.numeric(cf),
    se          = as.numeric(se),
    pval        = as.numeric(pv),
    sig         = sig_s(pv),
    lo95        = as.numeric(cf - 1.96 * se),
    hi95        = as.numeric(cf + 1.96 * se),
    row.names   = NULL,
    stringsAsFactors = FALSE)
}

# Extract TMX coefficient as a one-row data frame
get_tmx <- function(m) {
  if (is.null(m)) return(data.frame(coef=NA, se=NA, pval=NA, sig="",
                                    lo95=NA, hi95=NA))
  cf <- tryCatch(coef(m), error = function(e) NULL)
  vc <- tryCatch(vcov(m), error = function(e) NULL)
  if (is.null(cf) || is.null(vc) || !"TMX" %in% names(cf))
    return(data.frame(coef=NA, se=NA, pval=NA, sig="", lo95=NA, hi95=NA))
  b  <- as.numeric(cf["TMX"])
  se <- as.numeric(sqrt(vc["TMX", "TMX"]))
  pv <- 2 * pnorm(-abs(b / se))
  data.frame(coef = b, se = se, pval = pv, sig = sig_s(pv),
             lo95 = b - 1.96 * se, hi95 = b + 1.96 * se)
}

# Marginal TMX effect at specified interaction values (delta method)
marginal_tmx <- function(m, model_short, df_data = NULL) {
  if (is.null(m)) return(NULL)
  cf <- tryCatch(coef(m), error = function(e) NULL)
  vc <- tryCatch(vcov(m), error = function(e) NULL)
  if (is.null(cf) || is.null(vc)) return(NULL)
  
  # Delta method: dE/dTMX = b_TMX + b_int * val
  delta_mg <- function(b1_nm, b2_nm, val) {
    b1  <- if (b1_nm %in% names(cf)) cf[[b1_nm]] else 0
    b2  <- if (b2_nm %in% names(cf)) cf[[b2_nm]] else 0
    mg  <- b1 + b2 * val
    v1  <- if (b1_nm %in% rownames(vc)) vc[b1_nm, b1_nm] else 0
    v2  <- if (b2_nm %in% rownames(vc)) vc[b2_nm, b2_nm] else 0
    c12 <- if (b1_nm %in% rownames(vc) && b2_nm %in% rownames(vc))
      vc[b1_nm, b2_nm] else 0
    se  <- sqrt(max(0, v1 + val^2 * v2 + 2 * val * c12))
    pv  <- 2 * pnorm(-abs(mg / se))
    data.frame(marginal_coef = mg, se = se, pval = pv, sig = sig_s(pv),
               lo95 = mg - 1.96 * se, hi95 = mg + 1.96 * se)
  }
  
  if (model_short == "M5") {
    int_nm <- intersect(c("TMX:IRR", "IRR:TMX"), names(cf))
    if (length(int_nm) == 0) return(NULL)
    vals <- c("Rainfed (IRR=0)" = 0, "Mixed (IRR=0.5)" = 0.5,
              "Irrigated (IRR=1)" = 1)
    dplyr::bind_rows(lapply(names(vals), function(lbl)
      cbind(data.frame(sentetik = lbl, stringsAsFactors = FALSE),
            delta_mg("TMX", int_nm[1], vals[[lbl]]))))
    
  } else if (model_short == "M6") {
    int_nm <- intersect(c("TMX:is_arid", "is_arid:TMX"), names(cf))
    if (length(int_nm) == 0) return(NULL)
    vals <- c("Humid (is_arid=0)" = 0, "Arid (is_arid=1)" = 1)
    dplyr::bind_rows(lapply(names(vals), function(lbl)
      cbind(data.frame(sentetik = lbl, stringsAsFactors = FALSE),
            delta_mg("TMX", int_nm[1], vals[[lbl]]))))
    
  } else if (model_short == "M7") {
    if (!"TMX_lngdp" %in% names(cf)) return(NULL)
    # Use empirical quartiles from estimation sample
    if (!is.null(df_data) && "ln_gdp" %in% names(df_data)) {
      q_vals <- quantile(df_data$ln_gdp, probs = c(0.25, 0.50, 0.75),
                         na.rm = TRUE)
      gdp_vals <- setNames(
        as.numeric(q_vals),
        c(sprintf("P25 (ln_gdp=%.2f, ~$%.0f/cap)", q_vals[1], exp(q_vals[1])),
          sprintf("P50 (ln_gdp=%.2f, ~$%.0f/cap)", q_vals[2], exp(q_vals[2])),
          sprintf("P75 (ln_gdp=%.2f, ~$%.0f/cap)", q_vals[3], exp(q_vals[3]))))
    } else {
      gdp_vals <- c("P25 (ln_gdp=7.3, ~$1500/cap)"  = 7.3,
                    "P50 (ln_gdp=8.4, ~$4600/cap)"  = 8.4,
                    "P75 (ln_gdp=9.8, ~$18000/cap)" = 9.8)
    }
    dplyr::bind_rows(lapply(names(gdp_vals), function(lbl)
      cbind(data.frame(sentetik = lbl, stringsAsFactors = FALSE),
            delta_mg("TMX", "TMX_lngdp", gdp_vals[[lbl]]))))
    
  } else if (model_short == "M8") {
    int_nm <- intersect(c("TMX:PRE", "PRE:TMX"), names(cf))
    if (length(int_nm) == 0 || is.null(df_data)) return(NULL)
    pre_q <- quantile(df_data$PRE, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
    pre_vals <- list("Q25 (dry)"    = pre_q[["25%"]],
                     "Q50 (median)" = pre_q[["50%"]],
                     "Q75 (wet)"    = pre_q[["75%"]])
    dplyr::bind_rows(lapply(names(pre_vals), function(lbl)
      cbind(data.frame(sentetik = lbl, pre_val = pre_vals[[lbl]],
                       stringsAsFactors = FALSE),
            delta_mg("TMX", int_nm[1], pre_vals[[lbl]]))))
  } else NULL
}


# ==============================================================================
# OUTPUT CONTAINERS
# ==============================================================================

tum_sonuclar <- list()
tum_modeller <- list()
tum_marginal <- list()
adjr2_rows   <- list()   # AdjR2 extracted immediately inside loop


# ==============================================================================
# MAIN LOOP -- estimate all models for all crops
# ==============================================================================

for (urun in urun_listesi) {
  
  dosya <- file.path(ana_yol, paste0("pan7", urun, ".xlsx"))
  if (!file.exists(dosya)) { cat("[MISSING]", urun, "\n"); next }
  
  cat("\n", strrep("=", 65), "\n")
  cat(toupper(urun), "\n")
  cat(strrep("=", 65), "\n")
  
  df_raw <- as.data.frame(readxl::read_excel(dosya))
  
  # Fix decimal comma in character columns
  for (col in names(df_raw)) {
    if (is.character(df_raw[[col]])) {
      d <- suppressWarnings(as.numeric(gsub(",", ".", df_raw[[col]])))
      if (mean(!is.na(d)) > 0.8) df_raw[[col]] <- d
    }
  }
  df_raw$Yil <- as.integer(as.character(df_raw$Yil))
  
  # Build analysis data frame
  df <- df_raw %>%
    dplyr::mutate(
      ln_yield    = log(yield_hg_per_ha),
      ln_gdp      = log(GDP_per_Capita),
      TMX         = TMX_floweringmean,
      PRE         = PRE_grainfillsum,
      PRE_sq      = PRE_grainfillsum^2,
      IRR         = pmin(pmax(irr_ratio, 0), 1),
      Country_ISO = as.character(Country_ISO)) %>%
    dplyr::filter(
      !is.na(ln_yield),  is.finite(ln_yield),
      !is.na(TMX),       is.finite(TMX),
      !is.na(PRE),       is.finite(PRE),
      !is.na(IRR),
      !is.na(area_harvested_ha), area_harvested_ha > 0,
      !is.na(production_tonnes), production_tonnes > 0)
  
  # is_arid: UNESCO/IPCC semi-arid threshold = annual precip < 400 mm
  # Uses PRE_ann (annual total from CRU), NOT grain-fill PRE
  if ("PRE_ann" %in% names(df)) {
    df$is_arid <- as.numeric(df$PRE_ann < 400)
    df <- df %>% dplyr::filter(!is.na(is_arid))
  } else {
    cat(sprintf("  [WARNING] PRE_ann missing -- M6 (is_arid) skipped for %s\n", urun))
    df$is_arid <- NA_real_
    df <- df %>% dplyr::filter(!is.na(is_arid))
  }
  
  # GDP subsample: M3, M4, M7 only
  df_gdp       <- df %>% dplyr::filter(!is.na(ln_gdp), is.finite(ln_gdp))
  df$TMX_lngdp     <- df$TMX     * df$ln_gdp
  df_gdp$TMX_lngdp <- df_gdp$TMX * df_gdp$ln_gdp
  
  cat(sprintf("Full N = %d | Countries = %d | %d-%d\n",
              nrow(df), dplyr::n_distinct(df$Country_ISO),
              min(df$Yil, na.rm = TRUE), max(df$Yil, na.rm = TRUE)))
  cat(sprintf("GDP subsample N = %d\n", nrow(df_gdp)))
  
  # Model formulas
  fm <- list(
    M1 = ln_yield ~ TMX + PRE + PRE_sq                              | Country_ISO + Yil,
    M2 = ln_yield ~ TMX + PRE + PRE_sq + IRR                        | Country_ISO + Yil,
    M3 = ln_yield ~ TMX + PRE + PRE_sq + ln_gdp                     | Country_ISO + Yil,
    M4 = ln_yield ~ TMX + PRE + PRE_sq + IRR + ln_gdp               | Country_ISO + Yil,
    M5 = ln_yield ~ TMX * IRR + PRE + PRE_sq                        | Country_ISO + Yil,
    M6 = ln_yield ~ TMX * is_arid + PRE + PRE_sq + IRR              | Country_ISO + Yil,
    M7 = ln_yield ~ TMX + PRE + PRE_sq + IRR + ln_gdp + TMX_lngdp   | Country_ISO + Yil,
    M8 = ln_yield ~ TMX * PRE + PRE_sq + IRR                        | Country_ISO + Yil)
  
  # Area-weighted
  cat("\n-- Area-Weighted --\n")
  mods_aw <- list()
  for (nm in c("M1","M2","M5","M6","M8"))
    mods_aw[[paste0(nm, "_AW")]] <-
    fit_model(fm[[nm]], df,     "area_harvested_ha", paste0(nm, "_AW"))
  for (nm in c("M3","M4","M7"))
    mods_aw[[paste0(nm, "_AW")]] <-
    fit_model(fm[[nm]], df_gdp, "area_harvested_ha", paste0(nm, "_AW"))
  
  # Production-weighted
  cat("\n-- Production-Weighted --\n")
  mods_pw <- list()
  for (nm in c("M1","M2","M5","M6","M8"))
    mods_pw[[paste0(nm, "_PW")]] <-
    fit_model(fm[[nm]], df,     "production_tonnes", paste0(nm, "_PW"))
  for (nm in c("M3","M4","M7"))
    mods_pw[[paste0(nm, "_PW")]] <-
    fit_model(fm[[nm]], df_gdp, "production_tonnes", paste0(nm, "_PW"))
  
  all_mods <- c(Filter(Negate(is.null), mods_aw),
                Filter(Negate(is.null), mods_pw))
  cat(sprintf("Successful models: %d / 16\n", length(all_mods)))
  
  # Coefficient table
  ct <- dplyr::bind_rows(lapply(names(all_mods),
                                function(nm) coef_tablo_full(all_mods[[nm]], nm, urun)))
  tum_sonuclar[[urun]] <- ct
  
  # AdjR2 -- extracted immediately to avoid deferred extraction errors
  for (nm in names(all_mods)) {
    m   <- all_mods[[nm]]
    ar2 <- tryCatch(as.numeric(fixest::r2(m, "ar2")), error = function(e) NA_real_)
    n   <- tryCatch(as.integer(stats::nobs(m)),        error = function(e) NA_integer_)
    adjr2_rows[[paste0(urun, "_", nm)]] <- data.frame(
      crop        = urun,
      model_nm    = nm,
      model_short = sub("_(AW|PW)$", "", nm),
      weight      = ifelse(grepl("_AW$", nm), "Area-Weighted", "Production-Weighted"),
      adjr2       = ar2,
      N           = n,
      stringsAsFactors = FALSE)
  }
  
  # TMX console summary
  cat(sprintf("\n%-12s  %9s  %6s  %-3s\n", "Model", "TMX coef", "p", "sig"))
  cat(strrep("-", 36), "\n")
  for (ms in c("M1","M2","M3","M4")) {
    r <- ct[ct$variable == "TMX" &
              ct$weight   == "Production-Weighted" &
              ct$model_short == ms, ]
    if (nrow(r) > 0)
      cat(sprintf("%-12s  %+9.4f  %6.3f  %-3s\n",
                  ms, r$coef[1], r$pval[1], r$sig[1]))
  }
  
  # Marginal effects for M5-M8
  for (ms in c("M5","M6","M7","M8")) {
    key <- paste0(ms, "_PW")
    df_arg <- if (ms == "M7") df_gdp else if (ms == "M8") df else NULL
    mg  <- marginal_tmx(all_mods[[key]], ms, df_data = df_arg)
    if (!is.null(mg)) {
      mg$urun <- urun
      tum_marginal[[paste0(urun, "_", ms)]] <- mg
    }
  }
  
  tum_modeller[[urun]] <- list(all_mods = all_mods, df = df, df_gdp = df_gdp)
  rm(df, df_gdp, df_raw); gc()
}


# ==============================================================================
# BIND ALL RESULTS
# ==============================================================================

sonuc_df <- dplyr::bind_rows(tum_sonuclar)
adjr2_df <- dplyr::bind_rows(adjr2_rows)

cat("\n--- Sanity check ---\n")
cat(sprintf("sonuc_df rows       : %d\n", nrow(sonuc_df)))
cat(sprintf("adjr2_df rows       : %d\n", nrow(adjr2_df)))
cat(sprintf("adjr2 non-NA        : %d / %d\n",
            sum(!is.na(adjr2_df$adjr2)), nrow(adjr2_df)))

# Console summary table
cat("\n", strrep("=", 80), "\n")
cat("MAIN RESULTS -- TMX Coefficient M1-M4 | Production-Weighted\n")
cat(strrep("=", 80), "\n")
cat(sprintf("%-10s  %9s  %9s  %9s  %9s\n",
            "Crop", "M1", "M2", "M3", "M4"))
cat(strrep("-", 50), "\n")
for (urun in urun_listesi) {
  vals <- sapply(c("M1","M2","M3","M4"), function(ms) {
    r <- sonuc_df[sonuc_df$crop == urun & sonuc_df$variable == "TMX" &
                    sonuc_df$model_short == ms & sonuc_df$weight == "Production-Weighted", ]
    if (nrow(r) == 0) return("   --   ")
    sprintf("%+.3f%s", r$coef[1], r$sig[1])
  })
  cat(sprintf("%-10s  %9s  %9s  %9s  %9s\n",
              urun, vals[1], vals[2], vals[3], vals[4]))
}
cat(strrep("=", 80), "\n\n")


# ==============================================================================
# PLOTS -- G1 to G16
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("GENERATING PLOTS (G1 to G16)\n")
cat("Each plot: screen print x2 (labeled + unlabeled), saved x6\n")
cat(strrep("=", 65), "\n")

# Shared theme additions
theme_model <- theme_bw(base_size = 11) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "bold", size = 12),
        plot.subtitle    = element_text(size = 8.5, color = "grey40"),
        plot.caption     = element_text(size = 7.5, color = "grey50"),
        plot.margin      = margin(8, 12, 8, 8))


# ------------------------------------------------------------------------------
# G1 -- TMX M1-M4 FOREST PLOT
# ------------------------------------------------------------------------------

cat("\n[G1] TMX M1-M4 forest plot...\n")

g1_df <- sonuc_df %>%
  dplyr::filter(variable == "TMX",
                weight   == "Production-Weighted",
                model_short %in% c("M1","M2","M3","M4")) %>%
  dplyr::mutate(
    model_short = factor(model_short, levels = c("M1","M2","M3","M4")),
    crop        = factor(crop, levels = rev(urun_listesi)))

if (nrow(g1_df) > 0) {
  g1 <- ggplot(g1_df,
               aes(x = coef, y = crop,
                   color = model_short, shape = model_short)) +
    geom_vline(xintercept =  0,     linetype = "dashed",  color = "grey50") +
    geom_vline(xintercept = -0.021, linetype = "dotted",
               color = "#E74C3C", linewidth = 0.8) +
    geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                   height = 0.2, linewidth = 0.7,
                   position = position_dodge(width = 0.7)) +
    geom_point(size = 3, position = position_dodge(width = 0.7)) +
    annotate("text", x = -0.024, y = 0.5,
             label = "Lobell 2011\nbenchmark (-0.021)",
             color = "#E74C3C", size = 2.5, hjust = 1.05) +
    scale_color_manual(values = renk8m[c("M1","M2","M3","M4")], name = "Model") +
    scale_shape_manual(values = c(M1=16, M2=17, M3=15, M4=18),  name = "Model") +
    labs(
      title    = "G1: TMX Coefficient (M1-M4) | Production-Weighted",
      subtitle = "95% CI | Dotted = Lobell et al. (2011) wheat benchmark (-0.021/degC)",
      caption  = "Two-Way FE | Clustered SE by country | *** p<0.01 ** p<0.05 * p<0.10",
      x = "TMX Coefficient [delta ln(Yield) per 1 degC]", y = NULL) +
    theme_model
  save_plot(g1, "G1_TMX_M1M4", width = 10, height = 7)
  cat("[G1] DONE\n")
}


# ------------------------------------------------------------------------------
# G2 -- PRE M1-M4 FOREST PLOT
# ------------------------------------------------------------------------------

cat("\n[G2] PRE M1-M4 forest plot...\n")

g2_df <- sonuc_df %>%
  dplyr::filter(variable == "PRE",
                weight   == "Production-Weighted",
                model_short %in% c("M1","M2","M3","M4")) %>%
  dplyr::mutate(
    model_short = factor(model_short, levels = c("M1","M2","M3","M4")),
    crop        = factor(crop, levels = rev(urun_listesi)))

if (nrow(g2_df) > 0) {
  g2 <- ggplot(g2_df,
               aes(x = coef, y = crop,
                   color = model_short, shape = model_short)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                   height = 0.2, linewidth = 0.7,
                   position = position_dodge(width = 0.7)) +
    geom_point(size = 3, position = position_dodge(width = 0.7)) +
    scale_color_manual(values = renk8m[c("M1","M2","M3","M4")], name = "Model") +
    scale_shape_manual(values = c(M1=16, M2=17, M3=15, M4=18),  name = "Model") +
    labs(
      title    = "G2: PRE Coefficient (Grain Fill Precipitation, M1-M4) | Production-Weighted",
      subtitle = "95% CI | Positive expected (more rain -> higher yield)",
      caption  = "Two-Way FE | Clustered SE by country",
      x = "PRE Coefficient [delta ln(Yield) per 1 mm]", y = NULL) +
    theme_model
  save_plot(g2, "G2_PRE_M1M4", width = 10, height = 7)
  cat("[G2] DONE\n")
}


# ------------------------------------------------------------------------------
# G3 -- IRR M2/M4/M5/M6 FOREST PLOT
# ------------------------------------------------------------------------------

cat("\n[G3] IRR coefficient forest plot...\n")

g3_df <- sonuc_df %>%
  dplyr::filter(variable == "IRR",
                weight   == "Production-Weighted",
                model_short %in% c("M2","M4","M5","M6")) %>%
  dplyr::mutate(
    model_short = factor(model_short, levels = c("M2","M4","M5","M6")),
    crop        = factor(crop, levels = rev(urun_listesi)))

if (nrow(g3_df) > 0) {
  g3 <- ggplot(g3_df,
               aes(x = coef, y = crop,
                   color = model_short, shape = model_short)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                   height = 0.2, linewidth = 0.7,
                   position = position_dodge(width = 0.7)) +
    geom_point(size = 3, position = position_dodge(width = 0.7)) +
    scale_color_manual(values = renk8m[c("M2","M4","M5","M6")], name = "Model") +
    scale_shape_manual(values = c(M2=17, M4=18, M5=15, M6=16),  name = "Model") +
    labs(
      title    = "G3: IRR Coefficient (Irrigation Ratio, M2/M4/M5/M6) | Production-Weighted",
      subtitle = "95% CI | Positive expected | Blanc & Strobl (2013)",
      caption  = "Two-Way FE | Clustered SE by country",
      x = "IRR Coefficient [delta ln(Yield) per unit irrigation increase]", y = NULL) +
    theme_model
  save_plot(g3, "G3_IRR", width = 10, height = 7)
  cat("[G3] DONE\n")
}


# ------------------------------------------------------------------------------
# G4 -- ln_gdp M3/M4/M7 FOREST PLOT
# ------------------------------------------------------------------------------

cat("\n[G4] ln_gdp coefficient forest plot...\n")

g4_df <- sonuc_df %>%
  dplyr::filter(variable == "ln_gdp",
                weight   == "Production-Weighted",
                model_short %in% c("M3","M4","M7")) %>%
  dplyr::mutate(
    model_short = factor(model_short, levels = c("M3","M4","M7")),
    crop        = factor(crop, levels = rev(urun_listesi)))

if (nrow(g4_df) > 0) {
  g4 <- ggplot(g4_df,
               aes(x = coef, y = crop,
                   color = model_short, shape = model_short)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                   height = 0.2, linewidth = 0.7,
                   position = position_dodge(width = 0.7)) +
    geom_point(size = 3, position = position_dodge(width = 0.7)) +
    scale_color_manual(values = renk8m[c("M3","M4","M7")], name = "Model") +
    scale_shape_manual(values = c(M3=15, M4=18, M7=16),    name = "Model") +
    labs(
      title    = "G4: ln(GDP) Coefficient (Income Control, M3/M4/M7) | Production-Weighted",
      subtitle = "95% CI | GDP subsample | Lobell & Field (2007)",
      caption  = "Two-Way FE | Clustered SE by country",
      x = "ln(GDP) Coefficient [delta ln(Yield) per ln(GDP) unit]", y = NULL) +
    theme_model
  save_plot(g4, "G4_lnGDP", width = 10, height = 7)
  cat("[G4] DONE\n")
}


# ------------------------------------------------------------------------------
# G5 -- M5 IRR BUFFERING: MARGINAL TMX BY IRRIGATION LEVEL
# ------------------------------------------------------------------------------

cat("\n[G5] M5 IRR buffering...\n")

mg_m5_df <- dplyr::bind_rows(tum_marginal[grepl("_M5$", names(tum_marginal))])

if (nrow(mg_m5_df) > 0) {
  mg_m5_df <- mg_m5_df %>%
    dplyr::mutate(
      urun     = factor(urun, levels = urun_listesi),
      sentetik = factor(sentetik,
                        levels = c("Rainfed (IRR=0)",
                                   "Mixed (IRR=0.5)",
                                   "Irrigated (IRR=1)")))
  irr_cols <- c("Rainfed (IRR=0)"    = "#E74C3C",
                "Mixed (IRR=0.5)"    = "#F39C12",
                "Irrigated (IRR=1)"  = "#27AE60")
  g5 <- ggplot(mg_m5_df,
               aes(x = marginal_coef, y = sentetik, color = sentetik)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                   height = 0.25, linewidth = 0.9) +
    geom_point(size = 3.2) +
    geom_text(aes(label = sig, x = hi95 + 0.003),
              size = 3.5, hjust = 0, show.legend = FALSE) +
    facet_wrap(~urun, nrow = 2) +
    scale_color_manual(values = irr_cols, guide = "none") +
    labs(
      title    = "G5: M5 -- Marginal TMX Effect by Irrigation Level | Production-Weighted",
      subtitle = "Does irrigation buffer heat damage? | 95% CI | Delta method",
      caption  = "Blanc & Strobl (2013) | *** p<0.01 ** p<0.05 * p<0.10",
      x = "Marginal TMX Coefficient [delta ln(Yield) per 1 degC]", y = NULL) +
    theme_bw(base_size = 10) +
    theme(panel.grid.minor  = element_blank(),
          strip.text        = element_text(face = "bold"),
          strip.background  = element_rect(fill = "grey95"),
          plot.title        = element_text(face = "bold", size = 12),
          plot.subtitle     = element_text(size = 8, color = "grey40"),
          plot.caption      = element_text(size = 7.5, color = "grey50"))
  save_plot(g5, "G5_M5_IRR_marginal", width = 13, height = 8)
  cat("[G5] DONE\n")
}


# ------------------------------------------------------------------------------
# G6 -- M6 ARIDITY ASYMMETRY
# ------------------------------------------------------------------------------

cat("\n[G6] M6 aridity asymmetry...\n")

mg_m6_df <- dplyr::bind_rows(tum_marginal[grepl("_M6$", names(tum_marginal))])

if (nrow(mg_m6_df) > 0) {
  mg_m6_df <- mg_m6_df %>%
    dplyr::mutate(
      urun     = factor(urun, levels = urun_listesi),
      sentetik = factor(sentetik,
                        levels = c("Humid (is_arid=0)", "Arid (is_arid=1)")))
  g6 <- ggplot(mg_m6_df,
               aes(x = marginal_coef, y = sentetik, color = sentetik)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                   height = 0.25, linewidth = 0.9) +
    geom_point(size = 3.2) +
    geom_text(aes(label = sig, x = hi95 + 0.003),
              size = 3.5, hjust = 0, show.legend = FALSE) +
    facet_wrap(~urun, nrow = 2) +
    scale_color_manual(
      values = c("Humid (is_arid=0)" = "#2471A3",
                 "Arid (is_arid=1)"  = "#C0392B"),
      guide = "none") +
    labs(
      title    = "G6: M6 -- Marginal TMX Effect in Arid vs Humid Regions | Production-Weighted",
      subtitle = "Aridity = PRE_ann < 400 mm (UNESCO/IPCC) | 95% CI | Dell et al. (2014)",
      caption  = "Two-Way FE | Clustered SE by country | *** p<0.01 ** p<0.05 * p<0.10",
      x = "Marginal TMX Coefficient [delta ln(Yield) per 1 degC]", y = NULL) +
    theme_bw(base_size = 10) +
    theme(panel.grid.minor = element_blank(),
          strip.text       = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey95"),
          plot.title       = element_text(face = "bold", size = 12),
          plot.subtitle    = element_text(size = 8, color = "grey40"),
          plot.caption     = element_text(size = 7.5, color = "grey50"))
  save_plot(g6, "G6_M6_aridity_marginal", width = 13, height = 8)
  cat("[G6] DONE\n")
}


# ------------------------------------------------------------------------------
# G7 -- M7 ADAPTATION: MARGINAL TMX BY INCOME QUARTILE
# ------------------------------------------------------------------------------

cat("\n[G7] M7 income adaptation...\n")

mg_m7_df <- dplyr::bind_rows(tum_marginal[grepl("_M7$", names(tum_marginal))])

if (nrow(mg_m7_df) > 0) {
  mg_m7_df <- mg_m7_df %>%
    dplyr::mutate(
      urun        = factor(urun, levels = urun_listesi),
      income_rank = dplyr::case_when(
        grepl("^P25", sentetik) ~ 1L,
        grepl("^P50", sentetik) ~ 2L,
        grepl("^P75", sentetik) ~ 3L, TRUE ~ 2L),
      income_lbl  = dplyr::case_when(
        grepl("^P25", sentetik) ~ "P25 (low income)",
        grepl("^P50", sentetik) ~ "P50 (median income)",
        grepl("^P75", sentetik) ~ "P75 (high income)",
        TRUE ~ sentetik),
      income_lbl  = factor(income_lbl,
                           levels = c("P25 (low income)",
                                      "P50 (median income)",
                                      "P75 (high income)")))
  g7 <- ggplot(mg_m7_df,
               aes(x = marginal_coef, y = income_lbl, color = income_lbl)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                   height = 0.25, linewidth = 0.9) +
    geom_point(size = 3.2) +
    geom_text(aes(label = sig, x = hi95 + 0.003),
              size = 3.5, hjust = 0, show.legend = FALSE) +
    facet_wrap(~urun, nrow = 2) +
    scale_color_manual(
      values = c("P25 (low income)"    = "#E74C3C",
                 "P50 (median income)" = "#F39C12",
                 "P75 (high income)"   = "#27AE60"),
      name = "Income Quartile") +
    labs(
      title    = "G7: M7 -- Marginal TMX Effect by Income Level | Production-Weighted",
      subtitle = "Empirical ln(GDP) quartiles from each crop's estimation sample | Lobell & Field (2007)",
      caption  = "Two-Way FE | Clustered SE by country | *** p<0.01 ** p<0.05 * p<0.10",
      x = "Marginal TMX Coefficient [delta ln(Yield) per 1 degC]", y = NULL) +
    theme_bw(base_size = 10) +
    theme(panel.grid.minor = element_blank(),
          strip.text       = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey95"),
          legend.position  = "bottom",
          plot.title       = element_text(face = "bold", size = 12),
          plot.subtitle    = element_text(size = 8, color = "grey40"),
          plot.caption     = element_text(size = 7.5, color = "grey50"))
  save_plot(g7, "G7_M7_income_marginal", width = 13, height = 8)
  cat("[G7] DONE\n")
}


# ------------------------------------------------------------------------------
# G8 -- M8 HEAT-WATER INTERACTION
# ------------------------------------------------------------------------------

cat("\n[G8] M8 heat-water interaction...\n")

mg_m8_df <- dplyr::bind_rows(tum_marginal[grepl("_M8$", names(tum_marginal))])

if (nrow(mg_m8_df) > 0) {
  mg_m8_df <- mg_m8_df %>%
    dplyr::mutate(
      urun     = factor(urun, levels = urun_listesi),
      sentetik = factor(sentetik,
                        levels = c("Q25 (dry)", "Q50 (median)", "Q75 (wet)")))
  g8 <- ggplot(mg_m8_df,
               aes(x = marginal_coef, y = sentetik, color = sentetik)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                   height = 0.25, linewidth = 0.9) +
    geom_point(size = 3.2) +
    geom_text(aes(label = sig, x = hi95 + 0.003),
              size = 3.5, hjust = 0, show.legend = FALSE) +
    facet_wrap(~urun, nrow = 2) +
    scale_color_manual(
      values = c("Q25 (dry)"    = "#E74C3C",
                 "Q50 (median)" = "#F39C12",
                 "Q75 (wet)"    = "#2471A3"),
      name = "Precipitation Level") +
    labs(
      title    = "G8: M8 -- Marginal TMX Effect by Precipitation Level | Production-Weighted",
      subtitle = "TMX x PRE interaction | Q25/Q50/Q75 grain-fill precipitation | 95% CI",
      caption  = "Matiu et al. (2017) | Schauberger et al. (2017) | *** p<0.01 ** p<0.05 * p<0.10",
      x = "Marginal TMX Coefficient [delta ln(Yield) per 1 degC]", y = NULL) +
    theme_bw(base_size = 10) +
    theme(panel.grid.minor = element_blank(),
          strip.text       = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey95"),
          legend.position  = "bottom",
          plot.title       = element_text(face = "bold", size = 12),
          plot.subtitle    = element_text(size = 8, color = "grey40"),
          plot.caption     = element_text(size = 7.5, color = "grey50"))
  save_plot(g8, "G8_M8_precip_marginal", width = 13, height = 8)
  cat("[G8] DONE\n")
}


# ------------------------------------------------------------------------------
# G9 -- ADJUSTED R2: ALL MODELS x ALL CROPS
# ------------------------------------------------------------------------------

cat("\n[G9] AdjR2 comparison...\n")

g9_data <- adjr2_df %>%
  dplyr::filter(weight == "Production-Weighted",
                !is.na(adjr2),
                model_short %in% paste0("M", 1:8)) %>%
  dplyr::mutate(
    crop        = factor(crop,        levels = urun_listesi),
    model_short = factor(model_short, levels = paste0("M", 1:8)),
    model_num   = as.integer(model_short))

cat(sprintf("  G9 rows: %d | Crops: %s | Models: %s\n",
            nrow(g9_data),
            paste(levels(droplevels(g9_data$crop)),        collapse = ", "),
            paste(levels(droplevels(g9_data$model_short)), collapse = ", ")))

if (nrow(g9_data) >= 2) {
  end_lbl <- g9_data %>%
    dplyr::group_by(crop) %>%
    dplyr::filter(model_num == max(model_num)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  g9 <- ggplot(g9_data,
               aes(x = model_short, y = adjr2,
                   color = crop, group = crop)) +
    geom_line(linewidth = 1.0, alpha = 0.85) +
    geom_point(size = 3.0) +
    ggplot2::geom_text(
      data    = end_lbl,
      mapping = ggplot2::aes(x = model_short, y = adjr2,
                             label = crop, color = crop),
      nudge_x = 0.30, hjust = 0, size = 2.8, show.legend = FALSE) +
    scale_color_manual(values = renk_crop, name = "Crop") +
    scale_y_continuous(labels = function(x) sprintf("%.2f", x),
                       breaks = pretty(g9_data$adjr2, n = 6)) +
    coord_cartesian(clip = "off") +
    labs(
      title    = "G9: Adjusted R2 -- All Models M1-M8 x All Crops | Production-Weighted",
      subtitle = "M3/M4/M7 use GDP subsample -- direct comparison requires caution",
      caption  = "Two-Way FE | Within-R2 reported",
      x = "Model", y = "Adjusted R2") +
    theme_model +
    theme(legend.position = "right",
          plot.margin     = margin(5, 70, 5, 5))
  save_plot(g9, "G9_adjR2", width = 11, height = 7)
  cat("[G9] DONE\n")
} else {
  cat("[G9] SKIPPED -- insufficient data\n")
}


# ------------------------------------------------------------------------------
# G10 -- TMX STABILITY M1 -> M4
# ------------------------------------------------------------------------------

cat("\n[G10] TMX stability path...\n")

stab_df <- sonuc_df %>%
  dplyr::filter(variable == "TMX",
                weight   == "Production-Weighted",
                model_short %in% c("M1","M2","M3","M4")) %>%
  dplyr::mutate(
    model_short = factor(model_short, levels = c("M1","M2","M3","M4")),
    crop        = factor(crop, levels = urun_listesi))

if (nrow(stab_df) > 0) {
  g10 <- ggplot(stab_df,
                aes(x = model_short, y = coef,
                    color = crop, group = crop)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_line(linewidth = 0.9, alpha = 0.85) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = lo95, ymax = hi95),
                  width = 0.12, linewidth = 0.5, alpha = 0.45) +
    scale_color_manual(values = renk_crop, name = "Crop") +
    labs(
      title    = "G10: TMX Coefficient Stability M1 to M4 | Production-Weighted",
      subtitle = "Flat lines = adding controls does not change TMX (robust identification)",
      caption  = "Two-Way FE | 95% CI | Clustered SE by country",
      x = "Model", y = "TMX Coefficient [delta ln(Yield) per 1 degC]") +
    theme_model +
    theme(legend.position = "right")
  save_plot(g10, "G10_TMX_stability", width = 10, height = 7)
  cat("[G10] DONE\n")
}


# ------------------------------------------------------------------------------
# G11 -- COEFFICIENT HEATMAP: MODEL x VARIABLE x CROP
# ------------------------------------------------------------------------------

cat("\n[G11] Coefficient heatmap...\n")

var_clean <- c(
  "TMX"         = "TMX",
  "PRE"         = "PRE",
  "PRE_sq"      = "PRE2",
  "IRR"         = "IRR",
  "ln_gdp"      = "ln(GDP)",
  "is_arid"     = "Arid",
  "TMX:IRR"     = "TMXxIRR",
  "IRR:TMX"     = "TMXxIRR",
  "TMX:is_arid" = "TMXxArid",
  "is_arid:TMX" = "TMXxArid",
  "TMX_lngdp"   = "TMXxGDP",
  "TMX:PRE"     = "TMXxPRE",
  "PRE:TMX"     = "TMXxPRE")

var_order <- c("TMX","PRE","PRE2","IRR","ln(GDP)","Arid",
               "TMXxIRR","TMXxArid","TMXxGDP","TMXxPRE")

heat_df <- sonuc_df %>%
  dplyr::filter(variable %in% names(var_clean),
                weight   == "Production-Weighted",
                model_short %in% paste0("M", 1:8)) %>%
  dplyr::mutate(
    var_lbl     = dplyr::recode(variable, !!!var_clean),
    model_short = factor(model_short, levels = paste0("M", 1:8)),
    crop        = factor(crop, levels = urun_listesi),
    t_stat      = coef / se,
    sig_lbl     = dplyr::case_when(
      pval < 0.01 ~ "***", pval < 0.05 ~ "**",
      pval < 0.10 ~ "*",   TRUE ~ "")) %>%
  dplyr::group_by(crop, model_short, var_lbl) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(var_lbl = factor(var_lbl, levels = rev(var_order)))

if (nrow(heat_df) > 0) {
  g11 <- ggplot(heat_df,
                aes(x = model_short, y = var_lbl, fill = t_stat)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sig_lbl), size = 2.5, color = "grey20") +
    facet_wrap(~crop, nrow = 2) +
    scale_fill_gradient2(
      low      = "#C0392B",
      mid      = "white",
      high     = "#2471A3",
      midpoint = 0,
      limits   = c(-8, 8),
      oob      = scales::squish,
      name     = "t-stat") +
    labs(
      title    = "G11: Coefficient Sign and Magnitude -- All Models x All Crops | Production-Weighted",
      subtitle = "Blue = positive | Red = negative | Stars = significance | Colour = t-statistic",
      caption  = "Two-Way FE | Squished at t = +/-8",
      x = "Model", y = "Variable") +
    theme_bw(base_size = 9) +
    theme(panel.grid       = element_blank(),
          strip.text       = element_text(face = "bold", size = 9),
          strip.background = element_rect(fill = "grey95"),
          legend.position  = "right",
          plot.title       = element_text(face = "bold", size = 11),
          plot.subtitle    = element_text(size = 7, color = "grey40"),
          plot.caption     = element_text(size = 7, color = "grey50"))
  save_plot(g11, "G11_coef_heatmap", width = 14, height = 9)
  cat("[G11] DONE\n")
}


# ------------------------------------------------------------------------------
# G12 -- AW vs PW TMX ALL 8 MODELS
# ------------------------------------------------------------------------------

cat("\n[G12] AW vs PW TMX all 8 models...\n")

aw_pw_df <- sonuc_df %>%
  dplyr::filter(variable == "TMX") %>%
  dplyr::mutate(
    crop        = factor(crop, levels = urun_listesi),
    model_short = factor(model_short, levels = paste0("M", 1:8)),
    weight      = factor(weight,
                         levels = c("Area-Weighted", "Production-Weighted")))

if (nrow(aw_pw_df) > 0) {
  g12 <- ggplot(aw_pw_df,
                aes(x = coef, y = crop,
                    color = weight, shape = weight)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                   height = 0.18, linewidth = 0.65,
                   position = position_dodge(width = 0.55)) +
    geom_point(size = 2.5,
               position = position_dodge(width = 0.55)) +
    facet_wrap(~model_short, nrow = 2) +
    scale_color_manual(
      values = c("Area-Weighted"        = "#E74C3C",
                 "Production-Weighted"  = "#1A5276"),
      name = "Weighting") +
    scale_shape_manual(
      values = c("Area-Weighted"       = 16,
                 "Production-Weighted" = 17),
      name = "Weighting") +
    labs(
      title    = "G12: Area-Weighted vs Production-Weighted TMX Coefficient -- All 8 Models",
      subtitle = "Overlapping CIs = results not sensitive to weighting | 95% CI",
      caption  = "Two-Way FE | Clustered SE by country",
      x = "TMX Coefficient [delta ln(Yield) per 1 degC]", y = NULL) +
    theme_bw(base_size = 9) +
    theme(panel.grid.minor = element_blank(),
          strip.text       = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey95"),
          legend.position  = "bottom",
          plot.title       = element_text(face = "bold", size = 11),
          plot.subtitle    = element_text(size = 7.5, color = "grey40"),
          plot.caption     = element_text(size = 7, color = "grey50"))
  save_plot(g12, "G12_AW_vs_PW", width = 13, height = 9)
  cat("[G12] DONE\n")
}


# ------------------------------------------------------------------------------
# G13 -- PRE_sq NONLINEAR RESPONSE (coefficients x 1e6 for readability)
# ------------------------------------------------------------------------------

cat("\n[G13] PRE_sq forest plot...\n")

g13_df <- sonuc_df %>%
  dplyr::filter(variable == "PRE_sq",
                weight   == "Production-Weighted",
                model_short %in% c("M1","M2","M3","M4")) %>%
  dplyr::mutate(
    model_short = factor(model_short, levels = c("M1","M2","M3","M4")),
    crop        = factor(crop, levels = rev(urun_listesi)),
    coef_s      = coef * 1e6,
    lo95_s      = lo95 * 1e6,
    hi95_s      = hi95 * 1e6)

if (nrow(g13_df) > 0) {
  g13 <- ggplot(g13_df,
                aes(x = coef_s, y = crop,
                    color = model_short, shape = model_short)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = lo95_s, xmax = hi95_s),
                   height = 0.2, linewidth = 0.7,
                   position = position_dodge(width = 0.7)) +
    geom_point(size = 3, position = position_dodge(width = 0.7)) +
    scale_color_manual(values = renk8m[c("M1","M2","M3","M4")], name = "Model") +
    scale_shape_manual(values = c(M1=16, M2=17, M3=15, M4=18),  name = "Model") +
    labs(
      title    = "G13: PRE2 Coefficient (x1e-6) -- M1-M4 | Production-Weighted",
      subtitle = "Negative = concave response (optimal precipitation exists)",
      caption  = "Schlenker & Roberts (2009, PNAS) | Coefficients scaled x1e6 for readability",
      x = "PRE2 Coefficient [x1e-6]", y = NULL) +
    theme_model
  save_plot(g13, "G13_PRE2", width = 10, height = 7)
  cat("[G13] DONE\n")
}


# ------------------------------------------------------------------------------
# G14 -- FE COMPARISON: OLS vs COUNTRY FE vs TWO-WAY FE
# ------------------------------------------------------------------------------

cat("\n[G14] FE comparison...\n")

fe_list <- list()

for (urun in urun_listesi) {
  df <- tum_modeller[[urun]]$df
  if (is.null(df)) next
  
  m_ols <- tryCatch(
    fixest::feols(ln_yield ~ TMX + PRE + PRE_sq,
                  data = df, weights = ~production_tonnes,
                  cluster = ~Country_ISO),
    error = function(e) NULL)
  
  m_cfe <- tryCatch(
    fixest::feols(ln_yield ~ TMX + PRE + PRE_sq | Country_ISO,
                  data = df, weights = ~production_tonnes,
                  cluster = ~Country_ISO),
    error = function(e) NULL)
  
  m_twfe <- tum_modeller[[urun]]$all_mods[["M1_PW"]]
  
  r_ols  <- get_tmx(m_ols);  r_ols$spec  <- "OLS (no FE)"
  r_cfe  <- get_tmx(m_cfe);  r_cfe$spec  <- "Country FE only"
  r_twfe <- get_tmx(m_twfe); r_twfe$spec <- "Two-Way FE (Country + Year)"
  
  cat(sprintf("[%s]  OLS=%+.4f%s  CountryFE=%+.4f%s  TWFE=%+.4f%s\n",
              urun, r_ols$coef, r_ols$sig, r_cfe$coef, r_cfe$sig,
              r_twfe$coef, r_twfe$sig))
  
  fe_list[[urun]] <- dplyr::bind_rows(r_ols, r_cfe, r_twfe) %>%
    dplyr::mutate(crop = urun)
}

fe_df <- dplyr::bind_rows(fe_list) %>%
  dplyr::mutate(
    crop = factor(crop, levels = rev(urun_listesi)),
    spec = factor(spec, levels = c("OLS (no FE)",
                                   "Country FE only",
                                   "Two-Way FE (Country + Year)")))

if (nrow(fe_df) > 0) {
  g14 <- ggplot(fe_df,
                aes(x = coef, y = crop, color = spec, shape = spec)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                   height = 0.22, linewidth = 0.75,
                   position = position_dodge(width = 0.65)) +
    geom_point(size = 3.2, position = position_dodge(width = 0.65)) +
    geom_text(aes(label = sig, x = hi95 + 0.003),
              size = 3.2, hjust = 0,
              position = position_dodge(width = 0.65),
              show.legend = FALSE) +
    scale_color_manual(
      values = c("OLS (no FE)"                  = "#E74C3C",
                 "Country FE only"              = "#E67E22",
                 "Two-Way FE (Country + Year)"  = "#1A5276"),
      name = "Specification") +
    scale_shape_manual(
      values = c("OLS (no FE)"=16, "Country FE only"=17,
                 "Two-Way FE (Country + Year)"=15),
      name = "Specification") +
    labs(
      title    = "G14: TMX Coefficient -- OLS vs Country FE vs Two-Way FE",
      subtitle = paste0("Production-Weighted | 95% CI | Clustered SE by country\n",
                        "OLS often positive (warm countries more productive) | ",
                        "Year FE removes common time trends"),
      caption  = "Dell et al. (2014, J.Econ.Lit.) | Lobell et al. (2011, Science)",
      x = "TMX Coefficient [delta ln(Yield) per 1 degC]", y = NULL) +
    theme_model +
    theme(axis.text.y = element_text(size = 10, face = "bold"))
  save_plot(g14, "G14_FE_comparison", width = 10, height = 7)
  cat("[G14] DONE\n")
}


# ------------------------------------------------------------------------------
# G15 -- PRE ROBUSTNESS: TMX WITH vs WITHOUT PRECIPITATION CONTROL
# ------------------------------------------------------------------------------

cat("\n[G15] PRE robustness...\n")

pre_rob_list <- list()

for (urun in urun_listesi) {
  df <- tum_modeller[[urun]]$df
  if (is.null(df)) next
  
  r_full  <- get_tmx(tum_modeller[[urun]]$all_mods[["M2_PW"]])
  
  m_nopre <- tryCatch(
    fixest::feols(ln_yield ~ TMX + IRR | Country_ISO + Yil,
                  data = df, weights = ~production_tonnes,
                  cluster = ~Country_ISO),
    error = function(e) NULL)
  r_nopre <- get_tmx(m_nopre)
  
  pct_chg <- (r_nopre$coef - r_full$coef) / abs(r_full$coef) * 100
  
  pre_rob_list[[urun]] <- dplyr::bind_rows(
    r_full  %>% dplyr::mutate(crop = urun,
                              spec = "M2 Full (TMX + PRE + PRE2 + IRR)"),
    r_nopre %>% dplyr::mutate(crop = urun,
                              spec       = "No PRE (TMX + IRR only)",
                              pct_change = pct_chg))
  
  cat(sprintf("[%s]  Full=%+.4f%s  NoPRE=%+.4f%s  chg=%.1f%%\n",
              urun, r_full$coef, r_full$sig,
              r_nopre$coef, r_nopre$sig, pct_chg))
}

pre_rob_df <- dplyr::bind_rows(pre_rob_list) %>%
  dplyr::mutate(
    crop = factor(crop, levels = rev(urun_listesi)),
    spec = factor(spec, levels = c("M2 Full (TMX + PRE + PRE2 + IRR)",
                                   "No PRE (TMX + IRR only)")))

if (nrow(pre_rob_df) > 0) {
  g15 <- ggplot(pre_rob_df,
                aes(x = coef, y = crop, color = spec, shape = spec)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                   height = 0.22, linewidth = 0.75,
                   position = position_dodge(width = 0.6)) +
    geom_point(size = 3.5, position = position_dodge(width = 0.6)) +
    geom_text(aes(label = sig, x = hi95 + 0.002),
              size = 3.5, hjust = 0,
              position = position_dodge(width = 0.6),
              show.legend = FALSE) +
    scale_color_manual(
      values = c("M2 Full (TMX + PRE + PRE2 + IRR)" = "#1A5276",
                 "No PRE (TMX + IRR only)"           = "#C0392B"),
      name = "Specification") +
    scale_shape_manual(
      values = c("M2 Full (TMX + PRE + PRE2 + IRR)" = 16,
                 "No PRE (TMX + IRR only)"           = 17),
      name = "Specification") +
    labs(
      title    = "G15: PRE Robustness -- TMX With vs Without Precipitation Control",
      subtitle = "Production-Weighted Two-Way FE | 95% CI | Clustered SE by country",
      caption  = "Blanc & Schlenker (2017, Rev.Environ.Econ.Policy) | Lobell et al. (2011, Science)",
      x = "TMX Coefficient [delta ln(Yield) per 1 degC]", y = NULL) +
    theme_model +
    theme(axis.text.y = element_text(size = 10, face = "bold"))
  save_plot(g15, "G15_PRE_robustness", width = 10, height = 7)
  cat("[G15] DONE\n")
}


# ------------------------------------------------------------------------------
# G16 -- CO2 ROBUSTNESS: TMX WITH vs WITHOUT CO2 CONTROL
# ------------------------------------------------------------------------------

cat("\n[G16] CO2 robustness...\n")

co2_list  <- list()
n_co2_ok  <- 0L

for (urun in urun_listesi) {
  df <- tum_modeller[[urun]]$df
  if (is.null(df)) next
  
  has_co2 <- "CO2_ppm" %in% names(df) && sum(!is.na(df$CO2_ppm)) > 100
  if (!has_co2) { cat(sprintf("[%s] CO2_ppm not found -- skip\n", urun)); next }
  
  r_base <- get_tmx(tum_modeller[[urun]]$all_mods[["M2_PW"]])
  
  m_co2 <- tryCatch(
    fixest::feols(ln_yield ~ TMX + PRE + PRE_sq + IRR + CO2_ppm |
                    Country_ISO + Yil,
                  data = df, weights = ~production_tonnes,
                  cluster = ~Country_ISO),
    error = function(e) NULL)
  r_co2 <- get_tmx(m_co2)
  
  pct_chg <- (r_co2$coef - r_base$coef) / abs(r_base$coef) * 100
  cat(sprintf("[%s]  Base=%+.4f%s  +CO2=%+.4f%s  chg=%.1f%%\n",
              urun, r_base$coef, r_base$sig,
              r_co2$coef, r_co2$sig, pct_chg))
  n_co2_ok <- n_co2_ok + 1L
  
  co2_list[[urun]] <- dplyr::bind_rows(
    r_base %>% dplyr::mutate(crop = urun, spec = "M2 Baseline"),
    r_co2  %>% dplyr::mutate(crop = urun, spec = "+CO2_ppm control"))
}

if (n_co2_ok > 0) {
  co2_df <- dplyr::bind_rows(co2_list) %>%
    dplyr::mutate(
      crop = factor(crop, levels = rev(urun_listesi)),
      spec = factor(spec, levels = c("M2 Baseline", "+CO2_ppm control")))
  
  g16 <- ggplot(co2_df,
                aes(x = coef, y = crop, color = spec, shape = spec)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                   height = 0.22, linewidth = 0.75,
                   position = position_dodge(width = 0.55)) +
    geom_point(size = 3.2, position = position_dodge(width = 0.55)) +
    geom_text(aes(label = sig, x = hi95 + 0.002),
              size = 3.2, hjust = 0,
              position = position_dodge(width = 0.55),
              show.legend = FALSE) +
    scale_color_manual(
      values = c("M2 Baseline"      = "#1A5276",
                 "+CO2_ppm control" = "#27AE60"),
      name = "Specification") +
    scale_shape_manual(
      values = c("M2 Baseline"=16, "+CO2_ppm control"=17),
      name = "Specification") +
    labs(
      title    = "G16: CO2 Robustness -- TMX With vs Without CO2 Control (Appendix)",
      subtitle = paste0("Production-Weighted | 95% CI | Clustered SE\n",
                        "Year FE absorbs global CO2 trend -- adding CO2_ppm should minimally change TMX"),
      caption  = "Zhao et al. (2017, PNAS) | Lobell et al. (2011, Science)",
      x = "TMX Coefficient [delta ln(Yield) per 1 degC]", y = NULL) +
    theme_model +
    theme(axis.text.y = element_text(size = 10, face = "bold"))
  save_plot(g16, "G16_CO2_robustness", width = 10, height = 7)
  cat("[G16] DONE\n")
} else {
  cat("[G16] SKIPPED -- CO2_ppm not available in any crop dataset\n")
}


# ==============================================================================
# SAVE RESULTS TO EXCEL AND RDS
# ==============================================================================

cat("\nSaving adim3_sonuc.xlsx...\n")

ozet_tablo <- sonuc_df %>%
  dplyr::filter(variable %in% c("TMX","PRE","PRE_sq","IRR","ln_gdp",
                                "TMX:IRR","IRR:TMX","TMX:PRE","PRE:TMX",
                                "is_arid","TMX:is_arid","is_arid:TMX",
                                "TMX_lngdp")) %>%
  dplyr::select(crop, model_short, weight, variable,
                coef, se, pval, sig, lo95, hi95) %>%
  dplyr::arrange(crop, model_short, weight, variable)

adjr2_wide <- adjr2_df %>%
  dplyr::filter(weight == "Production-Weighted", !is.na(adjr2)) %>%
  dplyr::select(crop, model_short, adjr2, N) %>%
  tidyr::pivot_wider(names_from  = model_short,
                     values_from = c(adjr2, N),
                     names_sep   = "_")

writexl::write_xlsx(
  list(
    KeyCoefficients = ozet_tablo,
    Marginal_M5_M8  = dplyr::bind_rows(tum_marginal),
    AdjR2_Long      = adjr2_df,
    AdjR2_Wide      = adjr2_wide,
    AllCoefficients = sonuc_df),
  file.path(ana_yol, "adim3_sonuc.xlsx"))
cat("[SAVED] adim3_sonuc.xlsx\n")

saveRDS(tum_modeller, file.path(ana_yol, "modeller_v4_objects.rds"))
cat("[SAVED] modeller_v4_objects.rds\n")


# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n", strrep("=", 65), "\n")
cat("STEP 03 COMPLETE\n")
cat(strrep("=", 65), "\n\n")

cat("COMPLETED:\n")
cat("  8 crops x 8 models x 2 weights = 128 estimates\n")
cat("  AdjR2 extracted immediately (no deferred extraction)\n")
cat("  Marginal effects for M5-M8 (delta method)\n\n")

cat("PLOTS (each printed labeled + unlabeled on screen):\n")
cat("  G1-G4   : TMX/PRE/IRR/ln(GDP) forest plots\n")
cat("  G5-G8   : Marginal TMX effects (M5/M6/M7/M8)\n")
cat("  G9      : Adjusted R2 comparison\n")
cat("  G10     : TMX stability M1->M4\n")
cat("  G11     : Coefficient heatmap\n")
cat("  G12     : AW vs PW comparison\n")
cat("  G13     : PRE2 nonlinear response\n")
cat("  G14     : OLS vs Country FE vs Two-Way FE\n")
cat("  G15     : PRE robustness\n")
cat("  G16     : CO2 robustness\n\n")

cat("FILES SAVED (6 per plot = 96 total):\n")
cat("  figures/labeled/   -- PDF + 300dpi PNG + 600dpi PNG\n")
cat("  figures/unlabeled/ -- PDF + 300dpi PNG + 600dpi PNG\n\n")

cat("OUTPUT:\n")
cat("  adim3_sonuc.xlsx        -- 5 sheets\n")
cat("  modeller_v4_objects.rds -- model objects\n\n")

cat("NEXT STEP: paper1_step04_projections.R\n")
cat(strrep("=", 65), "\n")

















# ==============================================================================
# PAPER 1 -- STEP 04: PRE RESPONSE ANALYSIS (APPENDIX)
# Temperature, VPD, and Global Crop Yields: Evidence from a Multi-Crop Panel
#
# Reads: modeller_v4_objects.rds (from step03)
#
# Contents:
#   Step 1 : Compute optimal PRE for M1-M4, all crops
#   Plots  : APX-G1 to APX-G6
#            -- R screen: labeled then unlabeled
#            -- Saved: PDF + PNG 300dpi + PNG 600dpi x labeled + unlabeled
#
# Plots:
#   APX-G1 : PRE response curve (M2 PW, all crops)
#   APX-G2 : Optimal PRE vs mean PRE (M2 PW)
#   APX-G3 : Monte Carlo 2,000 vs 10,000 CI comparison
#   APX-G4 : Optimal PRE -- M1/M2/M3/M4 comparison (all crops)
#   APX-G5 : PRE coefficient M1-M4 forest plot (all crops)
#   APX-G6 : Concave/convex response matrix (crop x model)
#
# UTF-8 NOTE: All special characters replaced with ASCII
#   degree sign  -> degC
#   superscripts -> plain text (b_PRE_sq not b_PRE^2)
#   x (multiply) -> x or omitted
#   em dash      -> --
#
# References:
#   Schlenker & Roberts (2009, PNAS) -- PRE_sq justification
#   Lobell & Burke (2010)            -- nonlinear PRE control
#   Blanc & Strobl (2013)            -- irrigation x PRE
# ==============================================================================


# ==============================================================================
# 0. SETUP
# ==============================================================================

library(dplyr)
library(ggplot2)
library(writexl)
library(MASS)
library(tidyr)

ana_yol      <- "C:/Users/Alper.Tosun/Desktop/cru130"
urun_listesi <- c("wheat","barley","oats","rye","rice","maize","soybean","sorghum")

# Output directories
fig_labeled   <- file.path(ana_yol, "figures", "labeled")
fig_unlabeled <- file.path(ana_yol, "figures", "unlabeled")
dir.create(fig_labeled,   recursive = TRUE, showWarnings = FALSE)
dir.create(fig_unlabeled, recursive = TRUE, showWarnings = FALSE)

# Colors
renk8 <- c(wheat   = "#1A5276", barley  = "#2E86C1",
           oats    = "#27AE60", rye     = "#1E8449",
           rice    = "#F39C12", maize   = "#E67E22",
           soybean = "#8E44AD", sorghum = "#C0392B")

renk4m <- c(M1 = "#95A5A6", M2 = "#E74C3C",
            M3 = "#3498DB", M4 = "#8E44AD")


# ==============================================================================
# SAVE_PLOT FUNCTION
# Prints labeled + unlabeled to R screen, saves 6 files per plot
# ==============================================================================

save_plot <- function(g_labeled, plot_name, width = 10, height = 7) {
  
  g_unlabeled <- g_labeled +
    labs(title    = NULL,
         subtitle = NULL,
         caption  = NULL,
         x        = NULL,
         y        = NULL) +
    theme(legend.title = element_blank(),
          plot.margin  = margin(4, 4, 4, 4))
  
  cat(sprintf("  [SCREEN] %s -- LABELED\n",   plot_name)); print(g_labeled)
  cat(sprintf("  [SCREEN] %s -- UNLABELED\n", plot_name)); print(g_unlabeled)
  
  save_one <- function(g, dir_path) {
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, ".pdf")),
                    plot = g, width = width, height = height, device = "pdf")
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, "_300dpi.png")),
                    plot = g, width = width, height = height,
                    dpi = 300, device = "png")
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, "_600dpi.png")),
                    plot = g, width = width, height = height,
                    dpi = 600, device = "png")
  }
  save_one(g_labeled,   fig_labeled)
  save_one(g_unlabeled, fig_unlabeled)
  
  cat(sprintf("  [SAVED]  %s -> labeled/ + unlabeled/ (PDF + 300 + 600 dpi)\n\n",
              plot_name))
  invisible(NULL)
}


# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

sig_s <- function(p)
  ifelse(is.na(p), "",
         ifelse(p < 0.01, "***",
                ifelse(p < 0.05, "**",
                       ifelse(p < 0.10, "*", ""))))

# Extract b_PRE, b_PRE_sq, compute optimal PRE via delta method
extract_pre_stats <- function(m, df_data, urun, model_nm) {
  if (is.null(m) || is.null(df_data)) return(NULL)
  cf <- tryCatch(coef(m), error = function(e) NULL)
  vc <- tryCatch(vcov(m), error = function(e) NULL)
  if (is.null(cf) || is.null(vc)) return(NULL)
  if (!all(c("PRE", "PRE_sq") %in% names(cf))) return(NULL)
  
  b1 <- as.numeric(cf["PRE"])
  b3 <- as.numeric(cf["PRE_sq"])
  if (is.na(b1) || is.na(b3) || b3 == 0) return(NULL)
  
  pre_opt <- -b1 / (2 * b3)
  
  # Delta method SE for optimal PRE: g = gradient of -b1/(2*b3)
  v1  <- vc["PRE",    "PRE"]
  v3  <- vc["PRE_sq", "PRE_sq"]
  c13 <- vc["PRE",    "PRE_sq"]
  g1  <- -1 / (2 * b3)
  g3  <-  b1 / (2 * b3^2)
  se_opt <- sqrt(max(0, g1^2 * v1 + g3^2 * v3 + 2 * g1 * g3 * c13))
  
  pre_mean <- mean(df_data$PRE, na.rm = TRUE)
  se_b1    <- sqrt(v1); pv_b1 <- 2 * pnorm(-abs(b1 / se_b1))
  se_b3    <- sqrt(v3); pv_b3 <- 2 * pnorm(-abs(b3 / se_b3))
  
  data.frame(
    crop        = urun,
    model       = model_nm,
    b_PRE       = b1,       se_PRE     = se_b1,    pval_PRE     = pv_b1,
    sig_PRE     = sig_s(pv_b1),
    b_PRE_sq    = b3,       se_PRE_sq  = se_b3,    pval_PRE_sq  = pv_b3,
    sig_PRE_sq  = sig_s(pv_b3),
    is_concave  = (b3 < 0),
    pre_opt     = pre_opt,  se_opt     = se_opt,
    opt_lo95    = pre_opt - 1.96 * se_opt,
    opt_hi95    = pre_opt + 1.96 * se_opt,
    pre_mean    = pre_mean,
    lo95_PRE    = b1 - 1.96 * se_b1,
    hi95_PRE    = b1 + 1.96 * se_b1,
    lo95_PRE_sq = b3 - 1.96 * se_b3,
    hi95_PRE_sq = b3 + 1.96 * se_b3,
    stringsAsFactors = FALSE)
}


# ==============================================================================
# LOAD RDS
# ==============================================================================

rds_v4 <- file.path(ana_yol, "modeller_v4_objects.rds")
rds_v3 <- file.path(ana_yol, "modeller_v3_objects.rds")

if (file.exists(rds_v4)) {
  tum_modeller <- readRDS(rds_v4)
  cat("[LOADED] modeller_v4_objects.rds\n")
} else if (file.exists(rds_v3)) {
  tum_modeller <- readRDS(rds_v3)
  cat("[LOADED] modeller_v3_objects.rds (v4 not found, fallback)\n")
} else {
  stop("[ERROR] No RDS file found. Run paper1_step03_models.R first.")
}

cat(sprintf("Crops in RDS: %s\n\n", paste(names(tum_modeller), collapse = ", ")))


# ==============================================================================
# STEP 1 -- COMPUTE OPTIMAL PRE FOR M1-M4, ALL CROPS
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("STEP 1 -- Optimal PRE | M1/M2/M3/M4 | All crops\n")
cat(strrep("=", 65), "\n\n")

optim_list_all <- list()   # all models
curve_list     <- list()   # M2 PW response curves only

for (urun in urun_listesi) {
  if (!urun %in% names(tum_modeller)) next
  obj     <- tum_modeller[[urun]]
  df_full <- obj$df
  df_gdp  <- obj$df_gdp
  
  cat(sprintf("[%s]\n", toupper(urun)))
  
  for (ms in c("M1","M2","M3","M4")) {
    # M3/M4 use GDP subsample; M1/M2 use full sample
    df_use <- if (ms %in% c("M3","M4")) df_gdp else df_full
    m_pw   <- obj$all_mods[[paste0(ms, "_PW")]]
    stats  <- extract_pre_stats(m_pw, df_use, urun, ms)
    if (!is.null(stats)) {
      optim_list_all[[paste0(urun, "_", ms)]] <- stats
      cat(sprintf("  [%s] b_PRE=%+.5f%s  b_PRE_sq=%+.8f%s  OPT=%.1fmm  concave=%s\n",
                  ms,
                  stats$b_PRE,    stats$sig_PRE,
                  stats$b_PRE_sq, stats$sig_PRE_sq,
                  stats$pre_opt,
                  ifelse(stats$is_concave, "YES", "no")))
    }
  }
  
  # Build M2 PW response curve for APX-G1
  m2pw <- obj$all_mods[["M2_PW"]]
  if (!is.null(m2pw) && !is.null(df_full)) {
    cf <- tryCatch(coef(m2pw), error = function(e) NULL)
    vc <- tryCatch(vcov(m2pw), error = function(e) NULL)
    if (!is.null(cf) && !is.null(vc) &&
        all(c("PRE","PRE_sq") %in% names(cf))) {
      b1 <- as.numeric(cf["PRE"])
      b3 <- as.numeric(cf["PRE_sq"])
      pre_mean <- mean(df_full$PRE, na.rm = TRUE)
      if (!is.na(b3) && b3 != 0) {
        pre_opt <- -b1 / (2 * b3)
        pre_max <- min(max(3 * pre_mean, 2 * abs(pre_opt), na.rm = TRUE),
                       pre_mean * 4)
        pre_seq <- seq(0, pre_max, length.out = 300)
        dly     <- b1 * (pre_seq - pre_mean) +
          b3 * (pre_seq^2 - pre_mean^2)
        
        # Monte Carlo confidence band (1,000 draws)
        draws   <- MASS::mvrnorm(1000,
                                 mu    = c(b1, b3),
                                 Sigma = vc[c("PRE","PRE_sq"), c("PRE","PRE_sq")])
        sim_mat <- outer(draws[, 1], pre_seq - pre_mean) +
          outer(draws[, 2], pre_seq^2 - pre_mean^2)
        
        curve_list[[urun]] <- data.frame(
          crop       = urun,
          PRE        = pre_seq,
          PRE_pct    = (pre_seq / pre_mean - 1) * 100,
          pct_change = (exp(dly) - 1) * 100,
          lo95_pct   = (exp(apply(sim_mat, 2, quantile, 0.025)) - 1) * 100,
          hi95_pct   = (exp(apply(sim_mat, 2, quantile, 0.975)) - 1) * 100,
          pre_mean   = pre_mean,
          pre_opt    = pre_opt,
          is_concave = (b3 < 0),
          stringsAsFactors = FALSE)
      }
    }
  }
  cat("\n")
}

optim_df    <- dplyr::bind_rows(optim_list_all)
optim_df_m2 <- optim_df %>% dplyr::filter(model == "M2")
curve_df    <- dplyr::bind_rows(curve_list)

cat(sprintf("Optimal PRE rows (all models): %d\n", nrow(optim_df)))
cat(sprintf("Response curve rows          : %d\n", nrow(curve_df)))
cat(sprintf("Concave crops (M2)           : %d / %d\n",
            sum(optim_df_m2$is_concave, na.rm = TRUE), nrow(optim_df_m2)))
cat("\n")


# ==============================================================================
# PLOTS -- APX-G1 to APX-G6
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("GENERATING PLOTS (APX-G1 to APX-G6)\n")
cat("Each plot: screen print x2 (labeled + unlabeled), saved x6\n")
cat(strrep("=", 65), "\n")

# Shared theme
theme_apx <- theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        strip.text       = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey95"),
        legend.position  = "bottom",
        plot.title       = element_text(face = "bold", size = 12),
        plot.subtitle    = element_text(size = 8, color = "grey40"),
        plot.caption     = element_text(size = 7.5, color = "grey50"),
        plot.margin      = margin(8, 12, 8, 8))


# ------------------------------------------------------------------------------
# APX-G1 -- PRE RESPONSE CURVE (M2 PW)
# Shows yield % change vs precipitation level; diamond = optimal PRE
# ------------------------------------------------------------------------------

cat("\n[APX-G1] PRE response curve (M2 PW)...\n")

if (nrow(curve_df) > 0) {
  
  # Optimal PRE point on each curve (for diamond overlay)
  opt_pts <- data.frame()
  for (urun in unique(curve_df$crop)) {
    cd <- curve_df[curve_df$crop == urun, ]
    od <- optim_df_m2[optim_df_m2$crop == urun, ]
    if (nrow(od) == 0 || nrow(cd) == 0) next
    ov  <- od$pre_opt[1]
    if (is.na(ov) || ov < 0 || ov > max(cd$PRE) * 1.1) next
    idx <- which.min(abs(cd$PRE - ov))
    opt_pts <- dplyr::bind_rows(
      opt_pts,
      data.frame(crop = urun, PRE = cd$PRE[idx],
                 pct_change = cd$pct_change[idx],
                 stringsAsFactors = FALSE))
  }
  
  curve_df2 <- curve_df %>%
    dplyr::mutate(crop = factor(crop, levels = urun_listesi))
  
  opt_pts_f <- opt_pts %>%
    dplyr::mutate(crop = factor(crop, levels = urun_listesi))
  
  optim_df_m2_f <- optim_df_m2 %>%
    dplyr::mutate(crop = factor(crop, levels = urun_listesi))
  
  apx_g1 <- ggplot(curve_df2,
                   aes(x = PRE, y = pct_change, color = crop)) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.5) +
    geom_ribbon(aes(ymin = lo95_pct, ymax = hi95_pct, fill = crop),
                alpha = 0.12, color = NA) +
    geom_line(linewidth = 0.9) +
    geom_vline(data    = optim_df_m2_f,
               aes(xintercept = pre_mean),
               linetype = "dotted", color = "grey55", linewidth = 0.6) +
    geom_point(data  = opt_pts_f,
               aes(x = PRE, y = pct_change, color = crop),
               shape = 18, size = 4, show.legend = FALSE) +
    facet_wrap(~crop, scales = "free_x", nrow = 2) +
    scale_color_manual(values = renk8, guide = "none") +
    scale_fill_manual( values = renk8, guide = "none") +
    labs(
      title    = "APX-G1: PRE Response Curve -- Yield Change vs Precipitation Level",
      subtitle = paste0("Diamond = estimated optimal PRE | Dotted = sample mean | ",
                        "Shaded = MC 95% CI (n=1,000)\n",
                        "Inverted-U valid only for concave crops (b_PRE_sq < 0)"),
      caption  = paste0("M2 Production-Weighted | ",
                        "Schlenker & Roberts (2009, PNAS)"),
      x = "Grain Fill Precipitation (mm)",
      y = "Predicted Yield Change from Current Mean (%)") +
    theme_apx
  save_plot(apx_g1, "APX_G1_PRE_response_curve", width = 13, height = 8)
  cat("[APX-G1] DONE\n")
} else {
  cat("[APX-G1] SKIPPED -- no curve data\n")
}


# ------------------------------------------------------------------------------
# APX-G2 -- OPTIMAL PRE vs MEAN PRE (M2 PW)
# Red diamond = optimal; blue dot = observed mean; faded = CI too wide
# ------------------------------------------------------------------------------

cat("\n[APX-G2] Optimal PRE vs mean PRE (M2 PW)...\n")

if (nrow(optim_df_m2) > 0) {
  
  optim_plot <- optim_df_m2 %>%
    dplyr::mutate(
      crop          = factor(crop, levels = rev(urun_listesi)),
      ci_width      = opt_hi95 - opt_lo95,
      # Interpretable: positive estimate, CI width < 1,500mm
      interpretable = (pre_opt > 0 & pre_opt < 1500 & ci_width < 1500),
      alpha_val     = ifelse(interpretable, 1.0, 0.30),
      concave_lbl   = ifelse(is_concave, "[concave]", "[convex]"))
  
  apx_g2 <- ggplot(optim_plot) +
    geom_errorbarh(
      aes(y     = crop,
          xmin  = pmax(opt_lo95, 0),
          xmax  = pmin(opt_hi95, 1500),
          alpha = alpha_val),
      height = 0.25, linewidth = 0.9, color = "#C0392B") +
    geom_point(aes(x = pre_opt, y = crop, alpha = alpha_val),
               color = "#C0392B", shape = 18, size = 4) +
    geom_point(aes(x = pre_mean, y = crop),
               color = "#1A5276", shape = 16, size = 3.5) +
    geom_text(
      data = optim_plot %>% dplyr::filter(!interpretable),
      aes(x = 750, y = crop, label = "CI too wide"),
      size = 2.8, color = "grey50", hjust = 0.5, fontface = "italic") +
    geom_text(
      data = optim_plot %>% dplyr::filter(is_concave == TRUE),
      aes(x = 1490, y = crop, label = concave_lbl),
      size = 2.6, color = "#27AE60", hjust = 1) +
    geom_text(
      data = optim_plot %>% dplyr::filter(is_concave == FALSE),
      aes(x = 1490, y = crop, label = concave_lbl),
      size = 2.6, color = "grey60", hjust = 1) +
    scale_alpha_identity() +
    labs(
      title    = "APX-G2: Estimated Optimal PRE vs Current Mean PRE (M2, Production-Weighted)",
      subtitle = paste0("Red diamond = optimal PRE + 95% CI | Blue dot = sample mean\n",
                        "Faded = CI too wide (>1,500mm) -- not interpretable"),
      caption  = paste0("Optimal PRE = -b_PRE / (2 x b_PRE_sq) | Delta method SE\n",
                        "Schlenker & Roberts (2009, PNAS)"),
      x = "Grain Fill Precipitation (mm)", y = NULL) +
    theme_bw(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          plot.title       = element_text(face = "bold", size = 12),
          plot.subtitle    = element_text(size = 8, color = "grey40"),
          plot.caption     = element_text(size = 7.5, color = "grey50"),
          plot.margin      = margin(8, 12, 8, 8))
  save_plot(apx_g2, "APX_G2_optimal_PRE_M2", width = 10, height = 7)
  cat("[APX-G2] DONE\n")
} else {
  cat("[APX-G2] SKIPPED\n")
}


# ------------------------------------------------------------------------------
# APX-G3 -- MONTE CARLO CI COMPARISON: 2,000 vs 10,000 DRAWS
# Validates that 2,000 draws are sufficient
# ------------------------------------------------------------------------------

cat("\n[APX-G3] MC 2,000 vs 10,000 CI comparison...\n")

set.seed(42)
mc_compare <- list()

for (urun in urun_listesi) {
  if (!urun %in% names(tum_modeller)) next
  m2pw <- tum_modeller[[urun]]$all_mods[["M2_PW"]]
  if (is.null(m2pw)) next
  cf <- tryCatch(coef(m2pw), error = function(e) NULL)
  vc <- tryCatch(vcov(m2pw), error = function(e) NULL)
  if (is.null(cf) || is.null(vc) || !"TMX" %in% names(cf)) next
  
  for (n_mc in c(2000L, 10000L)) {
    draws <- MASS::mvrnorm(n_mc,
                           mu    = cf["TMX"],
                           Sigma = matrix(vc["TMX", "TMX"]))
    sims  <- draws * 2   # +2 degC scenario
    
    mc_compare[[paste0(urun, "_", n_mc)]] <- data.frame(
      crop     = urun,
      n_mc     = n_mc,
      n_lbl    = paste0("MC n=", formatC(n_mc, big.mark = ",", format = "d")),
      pct_mean = (exp(mean(sims)) - 1) * 100,
      pct_lo95 = (exp(quantile(sims, 0.025)) - 1) * 100,
      pct_hi95 = (exp(quantile(sims, 0.975)) - 1) * 100,
      ci_width = abs((exp(quantile(sims, 0.975)) -
                        exp(quantile(sims, 0.025))) * 100),
      stringsAsFactors = FALSE)
  }
}

mc_df <- dplyr::bind_rows(mc_compare) %>%
  dplyr::mutate(
    crop  = factor(crop,  levels = urun_listesi),
    n_lbl = factor(n_lbl, levels = c("MC n=2,000", "MC n=10,000")))

# Console table
cat("\n--- CI width (pp): +2 degC, M2 PW ---\n")
cat(sprintf("%-10s  %8s  %8s  %8s\n", "Crop", "n=2,000", "n=10,000", "Diff"))
cat(strrep("-", 42), "\n")
for (urun in urun_listesi) {
  r2  <- mc_df[mc_df$crop == urun & mc_df$n_mc == 2000,  ]
  r10 <- mc_df[mc_df$crop == urun & mc_df$n_mc == 10000, ]
  if (nrow(r2) == 0 || nrow(r10) == 0) next
  cat(sprintf("%-10s  %8.3f  %8.3f  %8.4f\n",
              urun, r2$ci_width, r10$ci_width,
              r2$ci_width - r10$ci_width))
}
cat("Note: diff < 0.1pp => 2,000 draws sufficient\n\n")

if (nrow(mc_df) > 0) {
  apx_g3 <- ggplot(mc_df,
                   aes(x = pct_mean, y = crop,
                       color = n_lbl, shape = n_lbl)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = pct_lo95, xmax = pct_hi95),
                   height   = 0.25, linewidth = 0.9,
                   position = position_dodge(width = 0.6)) +
    geom_point(size     = 3.2,
               position = position_dodge(width = 0.6)) +
    scale_color_manual(
      values = c("MC n=2,000"  = "#E74C3C",
                 "MC n=10,000" = "#1A5276"),
      name = "Simulation size") +
    scale_shape_manual(
      values = c("MC n=2,000" = 16, "MC n=10,000" = 17),
      name = "Simulation size") +
    labs(
      title    = "APX-G3: Monte Carlo CI Comparison -- 2,000 vs 10,000 Simulations",
      subtitle = paste0("+2 degC scenario, M2 Production-Weighted | ",
                        "Validates MC sample size choice\n",
                        "If CI widths near-identical: 2,000 draws sufficient"),
      caption  = "MASS::mvrnorm | seed=42 | Both use same vcov matrix",
      x = "Projected Yield Change at +2 degC (%)", y = NULL) +
    theme_bw(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          legend.position  = "bottom",
          plot.title       = element_text(face = "bold", size = 12),
          plot.subtitle    = element_text(size = 8, color = "grey40"),
          plot.caption     = element_text(size = 7.5, color = "grey50"),
          plot.margin      = margin(8, 12, 8, 8))
  save_plot(apx_g3, "APX_G3_MC_comparison", width = 10, height = 7)
  cat("[APX-G3] DONE\n")
}


# ------------------------------------------------------------------------------
# APX-G4 -- OPTIMAL PRE: M1/M2/M3/M4 COMPARISON, ALL CROPS
# Shows whether adding IRR or GDP changes the estimated optimal PRE
# ------------------------------------------------------------------------------

cat("\n[APX-G4] Optimal PRE comparison M1-M4 all crops...\n")

g4_data <- optim_df %>%
  dplyr::filter(model %in% c("M1","M2","M3","M4")) %>%
  dplyr::mutate(
    crop          = factor(crop,  levels = rev(urun_listesi)),
    model         = factor(model, levels = c("M1","M2","M3","M4")),
    ci_width      = opt_hi95 - opt_lo95,
    opt_lo95_cap  = pmax(opt_lo95, 0),
    opt_hi95_cap  = pmin(opt_hi95, 1500),
    # Interpretable: non-negative, CI width < 1,000mm
    interpretable = (pre_opt >= 0 & ci_width < 1000))

if (nrow(g4_data) > 0) {
  
  mean_ref <- g4_data %>% dplyr::distinct(crop, pre_mean)
  
  apx_g4 <- ggplot(g4_data,
                   aes(x = pre_opt, y = crop,
                       color = model, shape = model,
                       alpha = interpretable)) +
    geom_point(data = mean_ref,
               aes(x = pre_mean, y = crop),
               color = "grey30", shape = 4, size = 3,
               inherit.aes = FALSE) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    geom_errorbarh(aes(xmin = opt_lo95_cap, xmax = opt_hi95_cap),
                   height   = 0.2, linewidth = 0.7,
                   position = position_dodge(width = 0.65)) +
    geom_point(size     = 3,
               position = position_dodge(width = 0.65)) +
    scale_color_manual(values = renk4m, name = "Model") +
    scale_shape_manual(values = c(M1=16, M2=17, M3=15, M4=18), name = "Model") +
    scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.25),
                       guide  = "none") +
    coord_cartesian(xlim = c(-200, 1500)) +
    labs(
      title    = "APX-G4: Estimated Optimal PRE -- M1/M2/M3/M4 Comparison | Production-Weighted",
      subtitle = paste0("Diamond = optimal PRE per model | Cross = sample mean PRE | ",
                        "Faded = CI > 1,500mm (unreliable)\n",
                        "M3/M4 use GDP subsample"),
      caption  = paste0("Optimal PRE = -b_PRE / (2 x b_PRE_sq) | Delta method 95% CI\n",
                        "Schlenker & Roberts (2009, PNAS)"),
      x = "Estimated Optimal Grain Fill Precipitation (mm)",
      y = NULL) +
    theme_bw(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          legend.position  = "bottom",
          plot.title       = element_text(face = "bold", size = 12),
          plot.subtitle    = element_text(size = 8, color = "grey40"),
          plot.caption     = element_text(size = 7.5, color = "grey50"),
          plot.margin      = margin(8, 12, 8, 8))
  save_plot(apx_g4, "APX_G4_optimal_PRE_M1M4", width = 11, height = 7)
  cat("[APX-G4] DONE\n")
} else {
  cat("[APX-G4] SKIPPED -- no data\n")
}


# ------------------------------------------------------------------------------
# APX-G5 -- PRE COEFFICIENT M1-M4 FOREST PLOT, ALL CROPS
# Does adding IRR (M2) or income (M3/M4) change the PRE coefficient?
# ------------------------------------------------------------------------------

cat("\n[APX-G5] PRE coefficient M1-M4 forest plot...\n")

g5_data <- optim_df %>%
  dplyr::filter(model %in% c("M1","M2","M3","M4")) %>%
  dplyr::mutate(
    crop  = factor(crop,  levels = rev(urun_listesi)),
    model = factor(model, levels = c("M1","M2","M3","M4")))

if (nrow(g5_data) > 0) {
  apx_g5 <- ggplot(g5_data,
                   aes(x = b_PRE, y = crop,
                       color = model, shape = model)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = lo95_PRE, xmax = hi95_PRE),
                   height   = 0.2, linewidth = 0.7,
                   position = position_dodge(width = 0.7)) +
    geom_point(size     = 3,
               position = position_dodge(width = 0.7)) +
    scale_color_manual(values = renk4m, name = "Model") +
    scale_shape_manual(values = c(M1=16, M2=17, M3=15, M4=18), name = "Model") +
    labs(
      title    = "APX-G5: PRE Coefficient (b_PRE) -- M1/M2/M3/M4 x All Crops | Production-Weighted",
      subtitle = paste0("Does adding irrigation (M2) or income (M3/M4) ",
                        "change the precipitation effect?\n",
                        "95% CI | Positive = more rain raises yield"),
      caption  = paste0("M3/M4 use GDP subsample | Lobell & Burke (2010)"),
      x = "PRE Coefficient [delta ln(Yield) per 1 mm]",
      y = NULL) +
    theme_bw(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          legend.position  = "bottom",
          plot.title       = element_text(face = "bold", size = 12),
          plot.subtitle    = element_text(size = 8, color = "grey40"),
          plot.caption     = element_text(size = 7.5, color = "grey50"),
          plot.margin      = margin(8, 12, 8, 8))
  save_plot(apx_g5, "APX_G5_PRE_coef_M1M4", width = 10, height = 7)
  cat("[APX-G5] DONE\n")
} else {
  cat("[APX-G5] SKIPPED\n")
}


# ------------------------------------------------------------------------------
# APX-G6 -- CONCAVE/CONVEX RESPONSE MATRIX: CROP x MODEL
# Tile: red = concave, blue = convex; cell text = b_PRE_sq x 1e6
# ------------------------------------------------------------------------------

cat("\n[APX-G6] Concave/convex summary matrix...\n")

concave_summary <- optim_df %>%
  dplyr::filter(model %in% c("M1","M2","M3","M4")) %>%
  dplyr::group_by(model) %>%
  dplyr::summarise(
    n_crops       = dplyr::n(),
    n_concave     = sum(is_concave, na.rm = TRUE),
    n_convex      = n_crops - n_concave,
    n_sig_concave = sum(is_concave & pval_PRE_sq < 0.05, na.rm = TRUE),
    pct_concave   = n_concave / n_crops * 100,
    .groups = "drop") %>%
  dplyr::mutate(model = factor(model, levels = c("M1","M2","M3","M4")))

cat("\n--- Concave summary by model ---\n")
print(concave_summary, row.names = FALSE)

concave_detail <- optim_df %>%
  dplyr::filter(model %in% c("M1","M2","M3","M4")) %>%
  dplyr::mutate(
    response_type = dplyr::case_when(
      is_concave  & pval_PRE_sq < 0.05  ~ "Concave (sig.)",
      is_concave  & pval_PRE_sq >= 0.05 ~ "Concave (ns)",
      !is_concave & pval_PRE_sq < 0.05  ~ "Convex (sig.)",
      TRUE                               ~ "Convex (ns)"),
    crop  = factor(crop,  levels = urun_listesi),
    model = factor(model, levels = c("M1","M2","M3","M4")))

if (nrow(concave_detail) > 0) {
  
  type_cols <- c(
    "Concave (sig.)" = "#C0392B",
    "Concave (ns)"   = "#E74C3C",
    "Convex (sig.)"  = "#2471A3",
    "Convex (ns)"    = "#AED6F1")
  
  apx_g6 <- ggplot(concave_detail,
                   aes(x = model, y = crop, fill = response_type)) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = sprintf("%.3f", b_PRE_sq * 1e6)),
              size = 2.3, color = "grey20") +
    scale_fill_manual(values = type_cols, name = "PRE Response") +
    labs(
      title    = "APX-G6: PRE_sq Response Type -- All Crops x M1-M4 | Production-Weighted",
      subtitle = paste0("Red = concave (inverted-U, optimal PRE exists) | Blue = convex\n",
                        "Numbers = b_PRE_sq x 1e6 | sig. = p < 0.05"),
      caption  = paste0("Significant concave response (rye) is the key interpretable case\n",
                        "Schlenker & Roberts (2009, PNAS)"),
      x = "Model", y = NULL) +
    theme_bw(base_size = 11) +
    theme(panel.grid      = element_blank(),
          legend.position = "bottom",
          plot.title      = element_text(face = "bold", size = 12),
          plot.subtitle   = element_text(size = 8, color = "grey40"),
          plot.caption    = element_text(size = 7.5, color = "grey50"),
          plot.margin     = margin(8, 12, 8, 8))
  save_plot(apx_g6, "APX_G6_concave_matrix", width = 9, height = 7)
  cat("[APX-G6] DONE\n")
} else {
  cat("[APX-G6] SKIPPED\n")
}


# ==============================================================================
# CONSOLE SUMMARY TABLE
# ==============================================================================

cat("\n", strrep("=", 75), "\n")
cat("SUMMARY: b_PRE and b_PRE_sq by Model and Crop | Production-Weighted\n")
cat(strrep("=", 75), "\n")
cat(sprintf("%-10s  %-4s  %10s  %5s  %12s  %5s  %8s  %s\n",
            "Crop","Mdl","b_PRE","sig","b_PRE_sq(1e6)","sig","OPT(mm)","Concave"))
cat(strrep("-", 75), "\n")

for (urun in urun_listesi) {
  for (ms in c("M1","M2","M3","M4")) {
    r <- optim_df[optim_df$crop == urun & optim_df$model == ms, ]
    if (nrow(r) == 0) next
    cat(sprintf("%-10s  %-4s  %+10.5f  %-5s  %+12.4f  %-5s  %8.1f  %s\n",
                urun, ms,
                r$b_PRE[1],    r$sig_PRE[1],
                r$b_PRE_sq[1] * 1e6, r$sig_PRE_sq[1],
                r$pre_opt[1],
                ifelse(r$is_concave[1], "YES", "no")))
  }
}
cat(strrep("=", 75), "\n\n")


# ==============================================================================
# SAVE RESULTS TO EXCEL
# ==============================================================================

cat("Saving adim4_appendix.xlsx...\n")

optim_wide <- optim_df %>%
  dplyr::select(crop, model, b_PRE, sig_PRE, b_PRE_sq, sig_PRE_sq,
                is_concave, pre_opt, se_opt, opt_lo95, opt_hi95, pre_mean) %>%
  dplyr::arrange(crop, model)

writexl::write_xlsx(
  list(
    OptimalPRE_AllModels = optim_wide,
    OptimalPRE_M2        = optim_df_m2,
    ResponseCurve_M2     = curve_df %>%
      dplyr::select(crop, PRE, PRE_pct, pct_change,
                    lo95_pct, hi95_pct, pre_mean, pre_opt, is_concave),
    MC_Comparison        = mc_df,
    ConcaveSummary       = concave_summary,
    ConcaveDetail        = concave_detail %>%
      dplyr::select(crop, model, b_PRE, sig_PRE, b_PRE_sq,
                    sig_PRE_sq, is_concave, response_type, pre_opt)),
  file.path(ana_yol, "adim4_appendix.xlsx"))

cat("[SAVED] adim4_appendix.xlsx\n")


# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n", strrep("=", 65), "\n")
cat("STEP 04 COMPLETE\n")
cat(strrep("=", 65), "\n\n")

cat("COMPLETED:\n")
cat("  Optimal PRE computed for M1-M4, all crops (delta method)\n")
cat("  MC confidence bands validated (2,000 vs 10,000 draws)\n")
cat("  Concave/convex classification for all model x crop combinations\n\n")

cat("PLOTS (each printed labeled + unlabeled on screen):\n")
cat("  APX-G1 : PRE response curve (M2 PW, all crops)\n")
cat("  APX-G2 : Optimal PRE vs mean PRE (M2 PW)\n")
cat("  APX-G3 : MC 2,000 vs 10,000 CI comparison\n")
cat("  APX-G4 : Optimal PRE -- M1/M2/M3/M4 comparison\n")
cat("  APX-G5 : PRE coefficient M1-M4 forest plot\n")
cat("  APX-G6 : Concave/convex response matrix\n\n")

cat("FILES SAVED (6 per plot = 36 total):\n")
cat("  figures/labeled/   -- PDF + 300dpi PNG + 600dpi PNG\n")
cat("  figures/unlabeled/ -- PDF + 300dpi PNG + 600dpi PNG\n\n")

cat("OUTPUT: adim4_appendix.xlsx\n")
cat("  Sheets: OptimalPRE_AllModels, OptimalPRE_M2,\n")
cat("          ResponseCurve_M2, MC_Comparison,\n")
cat("          ConcaveSummary, ConcaveDetail\n\n")

cat("NEXT STEP: paper1_step05_projections.R\n")
cat(strrep("=", 65), "\n")



















# ==============================================================================
# PAPER 1 -- STEP 05: SUMMARY TABLES AND VISUALIZATIONS
# Temperature, VPD, and Global Crop Yields: Evidence from a Multi-Crop Panel
#
# Reads:
#   modeller_v4_objects.rds  (from step03)
#   adim9_senaryo_TMX.xlsx   (from step09, optional -- G6 skipped if absent)
#
# Contents:
#   Step 1 : Extract all key coefficients (M1-M8, all crops, AW + PW)
#   Step 2 : Literature comparison (Lobell 2011, Zhao 2017, Asseng 2015)
#   Step 3 : AW vs PW consistency (filter + inner_join, no pivot_wider)
#   Step 4 : Full coefficient summary
#   Plots  : G1 to G6
#            -- R screen: labeled then unlabeled
#            -- Saved: PDF + PNG 300dpi + PNG 600dpi x labeled + unlabeled
#
# Plots:
#   G1 : TMX M1-M4 vs literature benchmarks
#   G2 : AW vs PW TMX -- all 8 models, all crops
#   G3 : AW/PW consistency heatmap
#   G4 : All key coefficients facet by variable (PW)
#   G5 : Crop ranking by TMX damage severity (M2 PW)
#   G6 : Scenario projections +1/+2/+3/+4 degC (if adim9 available)
#
# UTF-8 NOTE: All special characters replaced with ASCII
#   degree sign  -> degC
#   superscripts -> sq or plain
#   multiply     -> x
#   em dash      -> --
#
# References:
#   Lobell et al. (2011, Science)         -- benchmark TMX
#   Zhao et al. (2017, PNAS)              -- meta-analysis
#   Asseng et al. (2015, Nat. Clim. Ch.)  -- process model
#   Dell et al. (2014, JEL)               -- two-way FE
# ==============================================================================


# ==============================================================================
# 0. SETUP
# ==============================================================================

library(dplyr)
library(ggplot2)
library(writexl)
library(readxl)
library(fixest)

# tidyr only needed for Excel output -- proceed without it if missing
has_tidyr <- requireNamespace("tidyr", quietly = TRUE)
if (has_tidyr) library(tidyr)

ana_yol      <- "C:/Users/Alper.Tosun/Desktop/cru130"
urun_listesi <- c("wheat","barley","oats","rye","rice","maize","soybean","sorghum")

# Output directories
fig_labeled   <- file.path(ana_yol, "figures", "labeled")
fig_unlabeled <- file.path(ana_yol, "figures", "unlabeled")
dir.create(fig_labeled,   recursive = TRUE, showWarnings = FALSE)
dir.create(fig_unlabeled, recursive = TRUE, showWarnings = FALSE)

# Colors
renk8m <- c(M1 = "#95A5A6", M2 = "#E74C3C", M3 = "#3498DB",
            M4 = "#8E44AD", M5 = "#27AE60", M6 = "#F39C12",
            M7 = "#1A5276", M8 = "#E91E63")

renk_crop <- c(wheat   = "#1A5276", barley  = "#2E86C1",
               oats    = "#27AE60", rye     = "#1E8449",
               rice    = "#F39C12", maize   = "#E67E22",
               soybean = "#8E44AD", sorghum = "#C0392B")


# ==============================================================================
# SAVE_PLOT FUNCTION
# Prints labeled + unlabeled to R screen, saves 6 files per plot
# ==============================================================================

save_plot <- function(g_labeled, plot_name, width = 10, height = 7) {
  
  g_unlabeled <- g_labeled +
    labs(title    = NULL,
         subtitle = NULL,
         caption  = NULL,
         x        = NULL,
         y        = NULL) +
    theme(legend.title = element_blank(),
          plot.margin  = margin(4, 4, 4, 4))
  
  cat(sprintf("  [SCREEN] %s -- LABELED\n",   plot_name)); print(g_labeled)
  cat(sprintf("  [SCREEN] %s -- UNLABELED\n", plot_name)); print(g_unlabeled)
  
  save_one <- function(g, dir_path) {
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, ".pdf")),
                    plot = g, width = width, height = height, device = "pdf")
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, "_300dpi.png")),
                    plot = g, width = width, height = height,
                    dpi = 300, device = "png")
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, "_600dpi.png")),
                    plot = g, width = width, height = height,
                    dpi = 600, device = "png")
  }
  save_one(g_labeled,   fig_labeled)
  save_one(g_unlabeled, fig_unlabeled)
  
  cat(sprintf("  [SAVED]  %s -> labeled/ + unlabeled/ (PDF + 300 + 600 dpi)\n\n",
              plot_name))
  invisible(NULL)
}


# ==============================================================================
# HELPER
# ==============================================================================

sig_s <- function(p)
  ifelse(is.na(p), "",
         ifelse(p < 0.01, "***",
                ifelse(p < 0.05, "**",
                       ifelse(p < 0.10, "*", ""))))


# ==============================================================================
# LOAD RDS AND OPTIONAL SCENARIO FILE
# ==============================================================================

rds_v4 <- file.path(ana_yol, "modeller_v4_objects.rds")
rds_v3 <- file.path(ana_yol, "modeller_v3_objects.rds")

if (file.exists(rds_v4)) {
  tum_modeller <- readRDS(rds_v4)
  cat("[LOADED] modeller_v4_objects.rds\n")
} else if (file.exists(rds_v3)) {
  tum_modeller <- readRDS(rds_v3)
  cat("[LOADED] modeller_v3_objects.rds (v4 not found, fallback)\n")
} else {
  stop("[ERROR] No RDS file found. Run paper1_step03_models.R first.")
}

# Optional: scenario projections from step09
scen_path <- file.path(ana_yol, "adim9_senaryo_TMX.xlsx")
has_scen  <- file.exists(scen_path)
if (has_scen) {
  scen_df <- as.data.frame(readxl::read_excel(scen_path,
                                              sheet = "ScenarioResults"))
  cat("[LOADED] adim9_senaryo_TMX.xlsx\n")
} else {
  cat("[NOTE] adim9_senaryo_TMX.xlsx not found -- G6 will be skipped\n")
}
cat("\n")


# ==============================================================================
# STEP 1 -- EXTRACT ALL KEY COEFFICIENTS: M1-M8, ALL CROPS, AW + PW
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("STEP 1 -- Extract all coefficients\n")
cat(strrep("=", 65), "\n\n")

key_vars <- c("TMX","PRE","PRE_sq","IRR","ln_gdp",
              "TMX:IRR","IRR:TMX",
              "TMX:PRE","PRE:TMX",
              "TMX:is_arid","is_arid:TMX",
              "TMX_lngdp")

coef_rows <- list()

for (urun in urun_listesi) {
  if (!urun %in% names(tum_modeller)) next
  obj <- tum_modeller[[urun]]
  
  for (ms in paste0("M", 1:8)) {
    for (wt_sfx in c("AW","PW")) {
      nm <- paste0(ms, "_", wt_sfx)
      m  <- obj$all_mods[[nm]]
      if (is.null(m)) next
      
      cf_all <- tryCatch(coef(m), error = function(e) NULL)
      vc_all <- tryCatch(vcov(m), error = function(e) NULL)
      if (is.null(cf_all) || is.null(vc_all)) next
      
      vars_present <- intersect(key_vars, names(cf_all))
      n_obs <- tryCatch(as.integer(stats::nobs(m)),        error = function(e) NA_integer_)
      ar2   <- tryCatch(as.numeric(fixest::r2(m, "ar2")),  error = function(e) NA_real_)
      
      for (v in vars_present) {
        b  <- as.numeric(cf_all[v])
        se <- as.numeric(sqrt(vc_all[v, v]))
        pv <- 2 * pnorm(-abs(b / se))
        coef_rows[[paste0(urun, "_", nm, "_", v)]] <- data.frame(
          crop        = urun,
          model_short = ms,
          weight      = ifelse(wt_sfx == "AW",
                               "Area-Weighted", "Production-Weighted"),
          variable    = v,
          coef        = b,
          se          = se,
          pval        = pv,
          sig         = sig_s(pv),
          lo95        = b - 1.96 * se,
          hi95        = b + 1.96 * se,
          N           = n_obs,
          adjr2       = ar2,
          stringsAsFactors = FALSE)
      }
    }
  }
}

all_coef_df <- dplyr::bind_rows(coef_rows)
cat(sprintf("Total coefficient records : %d\n", nrow(all_coef_df)))

# TMX subset -- used by most plots
tmx_df <- all_coef_df %>%
  dplyr::filter(variable == "TMX") %>%
  dplyr::mutate(
    crop        = factor(crop,        levels = urun_listesi),
    model_short = factor(model_short, levels = paste0("M", 1:8)))

cat(sprintf("TMX records               : %d\n\n", nrow(tmx_df)))


# ==============================================================================
# STEP 2 -- LITERATURE COMPARISON
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("STEP 2 -- Literature Comparison\n")
cat(strrep("=", 65), "\n\n")

# Published benchmark coefficients (semi-elasticities)
lit_ref <- data.frame(
  crop     = c("wheat","wheat","wheat",
               "rice","rice",
               "maize","maize",
               "soybean",
               "barley","oats","rye","sorghum"),
  source   = c("Lobell et al. (2011, Science)",
               "Zhao et al. (2017, PNAS)",
               "Asseng et al. (2015, Nat.Clim.Ch.)",
               "Zhao et al. (2017, PNAS)",
               "Tashiro & Wardlaw (1990) [process]",
               "Lobell et al. (2011, Science)",
               "Zhao et al. (2017, PNAS)",
               "Zhao et al. (2017, PNAS)",
               "No direct benchmark",
               "No direct benchmark",
               "No direct benchmark",
               "No direct benchmark"),
  lit_coef = c(-0.021, -0.019, -0.060,
               -0.032,  NA,
               -0.062, -0.047,
               -0.082,
               NA, NA, NA, NA),
  lit_note = c("Global panel PW, 1961-2008",
               "Meta-analysis central estimate",
               "Process model -- not directly comparable",
               "Meta-analysis central estimate",
               "Process model threshold",
               "Global panel PW, 1961-2008",
               "Meta-analysis central estimate",
               "Meta-analysis central estimate",
               "No crop-specific benchmark available",
               "No crop-specific benchmark available",
               "No crop-specific benchmark available",
               "No crop-specific benchmark available"),
  stringsAsFactors = FALSE)

# My M2 PW coefficients
my_m2_coefs <- tmx_df %>%
  dplyr::filter(model_short == "M2",
                weight      == "Production-Weighted") %>%
  dplyr::select(crop, my_coef = coef, my_se = se,
                my_pval = pval, my_sig = sig,
                my_lo95 = lo95, my_hi95 = hi95, N) %>%
  dplyr::mutate(crop = as.character(crop))

lit_compare <- lit_ref %>%
  dplyr::left_join(my_m2_coefs, by = "crop") %>%
  dplyr::mutate(
    diff_from_lit  = ifelse(!is.na(lit_coef), my_coef - lit_coef, NA),
    ratio_to_lit   = ifelse(!is.na(lit_coef) & lit_coef != 0,
                            my_coef / lit_coef, NA),
    direction_note = dplyr::case_when(
      is.na(lit_coef)         ~ "No benchmark available",
      diff_from_lit >  0.005  ~ "Less negative than literature",
      diff_from_lit < -0.005  ~ "More negative than literature",
      TRUE                    ~ "Consistent with literature"))

cat("--- Literature Comparison (M2 PW) ---\n")
cat(sprintf("%-10s  %-35s  %8s  %9s  %-5s  %s\n",
            "Crop","Source","Lit","Mine","Sig","Direction"))
cat(strrep("-", 85), "\n")
for (i in seq_len(nrow(lit_compare))) {
  r <- lit_compare[i, ]
  if (is.na(r$lit_coef)) next
  cat(sprintf("%-10s  %-35s  %+8.3f  %+9.4f  %-5s  %s\n",
              r$crop, r$source, r$lit_coef,
              r$my_coef, r$my_sig, r$direction_note))
}

cat("\n--- TMX Summary (M1-M4, Production-Weighted) ---\n")
cat(sprintf("%-10s  %9s  %9s  %9s  %9s\n",
            "Crop","M1","M2","M3","M4"))
cat(strrep("-", 52), "\n")
for (urun in urun_listesi) {
  vals <- sapply(c("M1","M2","M3","M4"), function(ms) {
    r <- tmx_df[tmx_df$crop == urun &
                  tmx_df$model_short == ms &
                  tmx_df$weight == "Production-Weighted", ]
    if (nrow(r) == 0) return("   --   ")
    sprintf("%+.4f%s", r$coef[1], r$sig[1])
  })
  cat(sprintf("%-10s  %9s  %9s  %9s  %9s\n",
              urun, vals[1], vals[2], vals[3], vals[4]))
}
cat("\n")


# ==============================================================================
# STEP 3 -- AW vs PW CONSISTENCY
# Uses filter() + inner_join() -- no pivot_wider, no tidyr dependency
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("STEP 3 -- AW vs PW Consistency\n")
cat(strrep("=", 65), "\n\n")

tmx_aw <- tmx_df %>%
  dplyr::filter(weight == "Area-Weighted") %>%
  dplyr::select(crop, model_short,
                coef_AW  = coef,  se_AW    = se,
                pval_AW  = pval,  sig_AW   = sig,
                lo95_AW  = lo95,  hi95_AW  = hi95,
                N_AW     = N,     adjr2_AW = adjr2) %>%
  dplyr::mutate(crop        = as.character(crop),
                model_short = as.character(model_short))

tmx_pw <- tmx_df %>%
  dplyr::filter(weight == "Production-Weighted") %>%
  dplyr::select(crop, model_short,
                coef_PW  = coef,  se_PW    = se,
                pval_PW  = pval,  sig_PW   = sig,
                lo95_PW  = lo95,  hi95_PW  = hi95,
                N_PW     = N,     adjr2_PW = adjr2) %>%
  dplyr::mutate(crop        = as.character(crop),
                model_short = as.character(model_short))

aw_pw_compare <- dplyr::inner_join(tmx_aw, tmx_pw,
                                   by = c("crop","model_short")) %>%
  dplyr::mutate(
    diff       = coef_AW - coef_PW,
    abs_diff   = abs(diff),
    consistent = abs_diff < 0.010)

cat("--- AW vs PW: M2, |diff| < 0.010 = robust ---\n")
cat(sprintf("%-10s  %9s  %9s  %9s  %s\n",
            "Crop","AW","PW","Diff","Robust?"))
cat(strrep("-", 50), "\n")
for (urun in urun_listesi) {
  r <- aw_pw_compare[aw_pw_compare$crop == urun &
                       aw_pw_compare$model_short == "M2", ]
  if (nrow(r) == 0) next
  cat(sprintf("%-10s  %+9.4f  %+9.4f  %+9.4f  %s\n",
              urun, r$coef_AW[1], r$coef_PW[1], r$diff[1],
              ifelse(isTRUE(r$consistent[1]), "YES [robust]", "CHECK")))
}
cat("\n")


# ==============================================================================
# STEP 4 -- FULL COEFFICIENT SUMMARY
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("STEP 4 -- Full coefficient summary\n")
cat(strrep("=", 65), "\n\n")

coef_summary <- all_coef_df %>%
  dplyr::arrange(crop, model_short, weight, variable)
cat(sprintf("Total records: %d\n\n", nrow(coef_summary)))


# ==============================================================================
# PLOTS -- G1 to G6
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("GENERATING PLOTS (G1 to G6)\n")
cat("Each plot: screen print x2 (labeled + unlabeled), saved x6\n")
cat(strrep("=", 65), "\n")

# Shared theme
theme_sum <- theme_bw(base_size = 11) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "bold", size = 12),
        plot.subtitle    = element_text(size = 8.5, color = "grey40"),
        plot.caption     = element_text(size = 7.5, color = "grey50"),
        plot.margin      = margin(8, 12, 8, 8))


# ------------------------------------------------------------------------------
# G1 -- TMX M1-M4 vs LITERATURE BENCHMARKS
# Coloured = this study; black diamonds = published benchmarks
# ------------------------------------------------------------------------------

cat("\n[G1] TMX M1-M4 vs literature benchmarks...\n")

g1_my <- tmx_df %>%
  dplyr::filter(model_short %in% c("M1","M2","M3","M4"),
                weight == "Production-Weighted") %>%
  dplyr::mutate(
    model_short = as.character(model_short),
    crop        = factor(crop, levels = rev(urun_listesi)))

g1_lit <- lit_ref %>%
  dplyr::filter(!is.na(lit_coef)) %>%
  dplyr::mutate(crop = factor(crop, levels = rev(urun_listesi)),
                coef = lit_coef)

if (nrow(g1_my) > 0) {
  g1 <- ggplot(g1_my,
               aes(x = coef, y = crop,
                   color = model_short, shape = model_short)) +
    geom_vline(xintercept =  0,     linetype = "dashed",  color = "grey60") +
    geom_vline(xintercept = -0.021, linetype = "dotted",
               color = "#E74C3C", linewidth = 0.7) +
    geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                   height   = 0.18, linewidth = 0.65,
                   position = position_dodge(width = 0.65)) +
    geom_point(size     = 2.8,
               position = position_dodge(width = 0.65)) +
    geom_point(data        = g1_lit,
               aes(x = coef, y = crop),
               shape       = 18, size = 4.5, color = "black",
               inherit.aes = FALSE) +
    annotate("text", x = -0.023, y = 0.5,
             label = "Lobell 2011\nwheat (-0.021)",
             color = "#E74C3C", size = 2.3, hjust = 1.02) +
    annotate("text", x = -0.087, y = 0.5,
             label = "Black diamond\n= lit. benchmark",
             color = "grey30", size = 2.2, hjust = 0) +
    scale_color_manual(values = renk8m[c("M1","M2","M3","M4")],
                       name = "Model") +
    scale_shape_manual(values = c(M1=16, M2=17, M3=15, M4=18),
                       name = "Model") +
    labs(
      title    = "G1: TMX Coefficient (M1-M4) vs Literature Benchmarks | Production-Weighted",
      subtitle = "Coloured points = this study (95% CI) | Black diamonds = published benchmarks",
      caption  = paste0("Lobell et al. (2011, Science) | Zhao et al. (2017, PNAS) | ",
                        "Asseng et al. (2015, Nat.Clim.Ch.)"),
      x = "TMX Coefficient [delta ln(Yield) per 1 degC]",
      y = NULL) +
    theme_sum
  save_plot(g1, "G1_TMX_vs_literature", width = 11, height = 7)
  cat("[G1] DONE\n")
} else {
  cat("[G1] SKIPPED -- no data\n")
}


# ------------------------------------------------------------------------------
# G2 -- AW vs PW TMX: ALL 8 MODELS, ALL CROPS
# ------------------------------------------------------------------------------

cat("\n[G2] AW vs PW all 8 models...\n")

g2_df <- tmx_df %>%
  dplyr::mutate(
    crop   = factor(crop, levels = urun_listesi),
    weight = factor(weight,
                    levels = c("Area-Weighted","Production-Weighted")))

if (nrow(g2_df) > 0) {
  g2 <- ggplot(g2_df,
               aes(x = coef, y = crop,
                   color = weight, shape = weight)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                   height   = 0.18, linewidth = 0.65,
                   position = position_dodge(width = 0.55)) +
    geom_point(size     = 2.5,
               position = position_dodge(width = 0.55)) +
    facet_wrap(~model_short, nrow = 2) +
    scale_color_manual(
      values = c("Area-Weighted"       = "#E74C3C",
                 "Production-Weighted" = "#1A5276"),
      name = "Weighting") +
    scale_shape_manual(
      values = c("Area-Weighted"       = 16,
                 "Production-Weighted" = 17),
      name = "Weighting") +
    labs(
      title    = "G2: TMX Coefficient -- Area-Weighted vs Production-Weighted (All 8 Models)",
      subtitle = "Overlapping CIs = results not sensitive to weighting choice | 95% CI",
      caption  = "Two-Way FE | Clustered SE by country",
      x = "TMX Coefficient [delta ln(Yield) per 1 degC]",
      y = NULL) +
    theme_bw(base_size = 9) +
    theme(panel.grid.minor = element_blank(),
          strip.text       = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey95"),
          legend.position  = "bottom",
          plot.title       = element_text(face = "bold", size = 11),
          plot.subtitle    = element_text(size = 7.5, color = "grey40"),
          plot.caption     = element_text(size = 7, color = "grey50"))
  save_plot(g2, "G2_AW_vs_PW_all_models", width = 13, height = 9)
  cat("[G2] DONE\n")
}


# ------------------------------------------------------------------------------
# G3 -- AW/PW CONSISTENCY HEATMAP: ALL MODELS x ALL CROPS
# Green = robust, red = sensitive to weighting choice
# ------------------------------------------------------------------------------

cat("\n[G3] AW/PW consistency heatmap...\n")

g3_df <- aw_pw_compare %>%
  dplyr::filter(!is.na(abs_diff)) %>%
  dplyr::mutate(
    crop        = factor(crop,        levels = rev(urun_listesi)),
    model_short = factor(model_short, levels = paste0("M", 1:8)),
    diff_cat    = dplyr::case_when(
      abs_diff < 0.005  ~ "< 0.005 (very robust)",
      abs_diff < 0.010  ~ "0.005-0.010 (robust)",
      abs_diff < 0.020  ~ "0.010-0.020 (check)",
      TRUE              ~ "> 0.020 (sensitive)"),
    diff_cat    = factor(diff_cat,
                         levels = c("< 0.005 (very robust)",
                                    "0.005-0.010 (robust)",
                                    "0.010-0.020 (check)",
                                    "> 0.020 (sensitive)")))

if (nrow(g3_df) > 0) {
  consist_cols <- c(
    "< 0.005 (very robust)" = "#27AE60",
    "0.005-0.010 (robust)"  = "#82E0AA",
    "0.010-0.020 (check)"   = "#F39C12",
    "> 0.020 (sensitive)"   = "#C0392B")
  
  g3 <- ggplot(g3_df,
               aes(x = model_short, y = crop, fill = diff_cat)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(aes(label = sprintf("%.3f", abs_diff)),
              size = 2.2, color = "grey20") +
    scale_fill_manual(values = consist_cols,
                      name   = "|AW - PW| Difference") +
    labs(
      title    = "G3: AW vs PW Consistency -- |AW - PW| for TMX (All Models x All Crops)",
      subtitle = "Green = robust to weighting | Red = sensitive | Numbers = absolute difference",
      caption  = "Threshold: < 0.010 considered robust",
      x = "Model", y = NULL) +
    theme_bw(base_size = 10) +
    theme(panel.grid      = element_blank(),
          legend.position = "bottom",
          plot.title      = element_text(face = "bold", size = 11),
          plot.subtitle   = element_text(size = 8, color = "grey40"),
          plot.caption    = element_text(size = 7.5, color = "grey50"),
          plot.margin     = margin(8, 12, 8, 8))
  save_plot(g3, "G3_AW_PW_consistency", width = 11, height = 7)
  cat("[G3] DONE\n")
} else {
  cat("[G3] SKIPPED\n")
}


# ------------------------------------------------------------------------------
# G4 -- ALL KEY COEFFICIENTS FACET BY VARIABLE (PW)
# One panel per variable; all models shown per crop
# ASCII fix: "x" not Unicode multiply sign in interaction labels
# ------------------------------------------------------------------------------

cat("\n[G4] All key coefficients facet by variable...\n")

# ASCII-only variable display labels
var_clean <- c(
  "TMX"          = "TMX",
  "PRE"          = "PRE",
  "PRE_sq"       = "PRE-sq",
  "IRR"          = "IRR",
  "ln_gdp"       = "ln(GDP)",
  "TMX:IRR"      = "TMX x IRR",
  "IRR:TMX"      = "TMX x IRR",
  "TMX:is_arid"  = "TMX x Arid",
  "is_arid:TMX"  = "TMX x Arid",
  "TMX_lngdp"    = "TMX x ln(GDP)",
  "TMX:PRE"      = "TMX x PRE",
  "PRE:TMX"      = "TMX x PRE")

g4_df <- all_coef_df %>%
  dplyr::filter(weight      == "Production-Weighted",
                variable    %in% names(var_clean),
                model_short %in% paste0("M", 1:8)) %>%
  dplyr::mutate(
    var_lbl     = dplyr::recode(variable, !!!var_clean),
    crop        = factor(crop,        levels = rev(urun_listesi)),
    model_short = factor(model_short, levels = paste0("M", 1:8))) %>%
  dplyr::group_by(crop, model_short, var_lbl) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    # Scale PRE-sq x 1e6 for readability
    coef_plot = ifelse(var_lbl == "PRE-sq", coef * 1e6, coef),
    lo95_plot = ifelse(var_lbl == "PRE-sq", lo95 * 1e6, lo95),
    hi95_plot = ifelse(var_lbl == "PRE-sq", hi95 * 1e6, hi95),
    var_lbl   = ifelse(var_lbl == "PRE-sq", "PRE-sq (x1e-6)", var_lbl))

main_vars <- c("TMX","PRE","IRR","ln(GDP)",
               "TMX x IRR","TMX x Arid","TMX x ln(GDP)","TMX x PRE")
g4_df_main <- g4_df %>% dplyr::filter(var_lbl %in% main_vars)

if (nrow(g4_df_main) > 0) {
  g4 <- ggplot(g4_df_main,
               aes(x = coef_plot, y = crop,
                   color = model_short, shape = model_short)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = lo95_plot, xmax = hi95_plot),
                   height   = 0.18, linewidth = 0.55,
                   position = position_dodge(width = 0.65)) +
    geom_point(size     = 2.0,
               position = position_dodge(width = 0.65)) +
    facet_wrap(~var_lbl, scales = "free_x", nrow = 2) +
    scale_color_manual(values = renk8m, name = "Model") +
    scale_shape_manual(
      values = c(M1=16, M2=17, M3=15, M4=18,
                 M5=8,  M6=7,  M7=10, M8=12),
      name = "Model") +
    labs(
      title    = "G4: All Key Coefficients by Variable -- Production-Weighted",
      subtitle = "Each panel = one variable | Colour = model | 95% CI",
      caption  = "Interaction terms shown where model includes them",
      x = "Coefficient estimate", y = NULL) +
    theme_bw(base_size = 9) +
    theme(panel.grid.minor = element_blank(),
          strip.text       = element_text(face = "bold", size = 8),
          strip.background = element_rect(fill = "grey95"),
          legend.position  = "bottom",
          plot.title       = element_text(face = "bold", size = 11),
          plot.subtitle    = element_text(size = 7.5, color = "grey40"),
          plot.caption     = element_text(size = 7, color = "grey50"))
  save_plot(g4, "G4_all_coef_by_variable", width = 14, height = 9)
  cat("[G4] DONE\n")
} else {
  cat("[G4] SKIPPED\n")
}


# ------------------------------------------------------------------------------
# G5 -- CROP RANKING BY TMX DAMAGE SEVERITY (M2 PW)
# Bar = TMX coef; right label = implied % yield loss at +2 degC
# ------------------------------------------------------------------------------

cat("\n[G5] Crop ranking by TMX damage severity...\n")

g5_df <- tmx_df %>%
  dplyr::filter(model_short == "M2",
                weight      == "Production-Weighted") %>%
  dplyr::mutate(
    crop     = as.character(crop),
    crop_ord = reorder(crop, coef, decreasing = FALSE),
    pct_2c   = (exp(coef  * 2) - 1) * 100,
    pct_lo   = (exp(lo95  * 2) - 1) * 100,
    pct_hi   = (exp(hi95  * 2) - 1) * 100,
    dmg_grp  = dplyr::case_when(
      coef > -0.030 ~ "Low (<3%/degC)",
      coef > -0.045 ~ "Moderate (3-4.5%/degC)",
      TRUE          ~ "High (>4.5%/degC)"))

if (nrow(g5_df) > 0) {
  dmg_cols <- c(
    "Low (<3%/degC)"         = "#27AE60",
    "Moderate (3-4.5%/degC)" = "#F39C12",
    "High (>4.5%/degC)"      = "#C0392B")
  
  g5 <- ggplot(g5_df,
               aes(x = coef, y = crop_ord, fill = dmg_grp)) +
    geom_col(alpha = 0.85, width = 0.65) +
    geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                   height = 0.28, linewidth = 0.9, color = "grey20") +
    geom_vline(xintercept =  0,     linetype = "dashed",  color = "grey40") +
    geom_vline(xintercept = -0.021, linetype = "dotted",
               color = "grey30", linewidth = 0.7) +
    geom_text(aes(label = sprintf("%.3f%s", coef, sig),
                  x     = lo95 - 0.001),
              hjust = 1.05, size = 3.0, fontface = "bold") +
    geom_text(aes(label = sprintf("%.1f%%", pct_2c),
                  x     = hi95 + 0.001),
              hjust = -0.1, size = 2.8, color = "grey40") +
    scale_fill_manual(values = dmg_cols, name = "Damage Level") +
    annotate("text", x = -0.022, y = 0.4,
             label = "Lobell 2011\nwheat ref.",
             size = 2.2, color = "grey40", hjust = 1) +
    expand_limits(x = c(-0.09, 0.005)) +
    labs(
      title    = "G5: Crop Ranking by TMX Heat Damage -- M2, Production-Weighted",
      subtitle = paste0("Bar = TMX coefficient | Error bar = 95% CI | ",
                        "Right label = implied yield loss at +2 degC"),
      caption  = "Lobell et al. (2011, Science) | Zhao et al. (2017, PNAS)",
      x = "TMX Coefficient [delta ln(Yield) per 1 degC]",
      y = NULL) +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor   = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position    = "bottom",
          axis.text.y        = element_text(size = 11, face = "bold"),
          plot.title         = element_text(face = "bold", size = 12),
          plot.subtitle      = element_text(size = 8.5, color = "grey40"),
          plot.caption       = element_text(size = 7.5, color = "grey50"),
          plot.margin        = margin(8, 50, 8, 8))
  save_plot(g5, "G5_crop_damage_ranking", width = 11, height = 7)
  cat("[G5] DONE\n")
}


# ------------------------------------------------------------------------------
# G6 -- SCENARIO PROJECTIONS +1/+2/+3/+4 degC (requires adim9 output)
# ASCII fix: "degC" not degree symbol
# ------------------------------------------------------------------------------

cat("\n[G6] Scenario projections...\n")

if (has_scen && exists("scen_df") && nrow(scen_df) > 0) {
  
  scen_plot <- scen_df %>%
    dplyr::mutate(
      crop     = factor(crop,    levels = urun_listesi),
      scen_id  = factor(scen_id, levels = paste0("T+", c(1,2,3,4), "C")),
      crop_rev = factor(as.character(crop), levels = rev(urun_listesi)))
  
  scen_cols <- c("T+1C" = "#2471A3", "T+2C" = "#E67E22",
                 "T+3C" = "#C0392B", "T+4C" = "#6C3483")
  
  g6 <- ggplot(scen_plot,
               aes(x = point_pct, y = crop_rev,
                   color = scen_id, shape = scen_id)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = lo95_pct, xmax = hi95_pct),
                   height   = 0.22, linewidth = 0.75,
                   position = position_dodge(width = 0.72)) +
    geom_point(size     = 3.2,
               position = position_dodge(width = 0.72)) +
    scale_color_manual(values = scen_cols, name = "Warming Scenario") +
    scale_shape_manual(
      values = c("T+1C"=16, "T+2C"=17, "T+3C"=15, "T+4C"=18),
      name   = "Warming Scenario") +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title    = "G6: Projected Yield Change Under Temperature Scenarios (M2 PW, +1 to +4 degC)",
      subtitle = "Point estimate + 95% parameter uncertainty CI | 10,000 MC bootstrap",
      caption  = "Lobell et al. (2011, Science) | Zhao et al. (2017, PNAS)",
      x = "Projected Yield Change (%)", y = NULL) +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor   = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position    = "right",
          axis.text.y        = element_text(size = 11, face = "bold"),
          plot.title         = element_text(face = "bold", size = 12),
          plot.subtitle      = element_text(size = 8, color = "grey40"),
          plot.caption       = element_text(size = 7.5, color = "grey50"),
          plot.margin        = margin(8, 12, 8, 8))
  save_plot(g6, "G6_scenario_projections", width = 11, height = 7)
  cat("[G6] DONE\n")
} else {
  cat("[G6] SKIPPED -- adim9_senaryo_TMX.xlsx not found\n")
}


# ==============================================================================
# SAVE RESULTS TO EXCEL
# ==============================================================================

cat("\nSaving adim5_ozet_tablo.xlsx...\n")

# AdjR2 long format (no pivot_wider needed)
adjr2_long <- all_coef_df %>%
  dplyr::filter(variable == "TMX",
                weight   == "Production-Weighted") %>%
  dplyr::select(crop, model_short, adjr2) %>%
  dplyr::distinct() %>%
  dplyr::arrange(crop, model_short)

# Optional wide format if tidyr available
if (has_tidyr) {
  adjr2_out <- adjr2_long %>%
    tidyr::pivot_wider(names_from   = model_short,
                       values_from  = adjr2,
                       names_prefix = "AdjR2_")
} else {
  adjr2_out <- adjr2_long
}

key_coef_pw <- all_coef_df %>%
  dplyr::filter(weight == "Production-Weighted") %>%
  dplyr::select(crop, model_short, variable,
                coef, se, pval, sig, lo95, hi95, N) %>%
  dplyr::arrange(crop, model_short, variable)

sheets_out <- list(
  LiteratureComparison = lit_compare,
  TMX_AllModels_PW     = tmx_df %>%
    dplyr::filter(weight == "Production-Weighted") %>%
    dplyr::select(crop, model_short, coef, se, pval, sig,
                  lo95, hi95, N, adjr2) %>%
    dplyr::mutate(crop        = as.character(crop),
                  model_short = as.character(model_short)) %>%
    dplyr::arrange(crop, model_short),
  TMX_AllModels_AW     = tmx_df %>%
    dplyr::filter(weight == "Area-Weighted") %>%
    dplyr::select(crop, model_short, coef, se, pval, sig,
                  lo95, hi95, N, adjr2) %>%
    dplyr::mutate(crop        = as.character(crop),
                  model_short = as.character(model_short)) %>%
    dplyr::arrange(crop, model_short),
  AW_vs_PW_Consistency = aw_pw_compare %>%
    dplyr::select(crop, model_short,
                  coef_AW, sig_AW, coef_PW, sig_PW,
                  diff, abs_diff, consistent) %>%
    dplyr::arrange(crop, model_short),
  KeyCoefficients_PW   = key_coef_pw,
  AdjR2_Summary        = adjr2_out)

if (has_scen && exists("scen_df"))
  sheets_out[["ScenarioProjections"]] <- scen_df

writexl::write_xlsx(sheets_out,
                    file.path(ana_yol, "adim5_ozet_tablo.xlsx"))
cat("[SAVED] adim5_ozet_tablo.xlsx\n")


# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n", strrep("=", 65), "\n")
cat("STEP 05 COMPLETE\n")
cat(strrep("=", 65), "\n\n")

cat("COMPLETED:\n")
cat("  Step 1: All M1-M8 coefficients extracted (AW + PW)\n")
cat("  Step 2: Literature comparison (Lobell 2011, Zhao 2017, Asseng 2015)\n")
cat("  Step 3: AW vs PW consistency (filter + inner_join)\n")
cat("  Step 4: Full coefficient summary table\n\n")

cat("PLOTS (each printed labeled + unlabeled on screen):\n")
cat("  G1 : TMX M1-M4 vs literature benchmarks\n")
cat("  G2 : AW vs PW -- all 8 models, all crops\n")
cat("  G3 : AW/PW consistency heatmap\n")
cat("  G4 : All key coefficients facet by variable (PW)\n")
cat("  G5 : Crop ranking by TMX damage severity (M2 PW)\n")
cat("  G6 : Scenario projections (skipped if adim9 absent)\n\n")

cat("FILES SAVED (6 per plot = up to 36 total):\n")
cat("  figures/labeled/   -- PDF + 300dpi PNG + 600dpi PNG\n")
cat("  figures/unlabeled/ -- PDF + 300dpi PNG + 600dpi PNG\n\n")

cat("OUTPUT: adim5_ozet_tablo.xlsx\n")
cat("  Sheets: LiteratureComparison, TMX_AllModels_PW/AW,\n")
cat("          AW_vs_PW_Consistency, KeyCoefficients_PW,\n")
cat("          AdjR2_Summary, ScenarioProjections (if available)\n\n")

cat("NEXT STEP: paper1_step06_robustness.R\n")
cat(strrep("=", 65), "\n")















# ==============================================================================
# PAPER 1 -- STEP 06: DIAGNOSTIC TESTS
# Temperature, VPD, and Global Crop Yields: Evidence from a Multi-Crop Panel
#
# Reads: modeller_v4_objects.rds (from step03)
#
# Contents:
#   Step 1 : Structural break (M1-M4 x all crops, break=1990)
#   Step 2 : Break year sensitivity (1980/1985/1990/1995/2000 x M2)
#   Step 3 : Pesaran CD test (cross-sectional dependence)
#   Step 4 : Hausman test (FE vs RE)
#   Plots  : G_SB1, G_SB2, G_SB3, G_SB4, G_CD, G_HAU
#            -- R screen: labeled then unlabeled
#            -- Saved: PDF + PNG 300dpi + PNG 600dpi x labeled + unlabeled
#
# KEY FIX vs original:
#   update(formula, . ~ . + TMX:post_dummy) does NOT work with fixest |FE syntax.
#   Solution: write each formula explicitly inside fit_sb_one() using switch().
#   No update() or reformulate() anywhere in this script.
#
# UTF-8 NOTE: All special characters replaced with ASCII
#   degree sign  -> degC
#   multiply     -> x
#   em dash      -> --
#
# References:
#   Pesaran (2004)              -- CD test
#   Wooldridge (2010)           -- Hausman test
#   Grassini et al. (2013)      -- structural break
#   Lobell & Field (2007, GRL)  -- adaptation signal
# ==============================================================================


# ==============================================================================
# 0. SETUP
# ==============================================================================

library(dplyr)
library(ggplot2)
library(writexl)
library(fixest)
library(plm)

ana_yol      <- "C:/Users/Alper.Tosun/Desktop/cru130"
urun_listesi <- c("wheat","barley","oats","rye","rice","maize","soybean","sorghum")

# Output directories
fig_labeled   <- file.path(ana_yol, "figures", "labeled")
fig_unlabeled <- file.path(ana_yol, "figures", "unlabeled")
dir.create(fig_labeled,   recursive = TRUE, showWarnings = FALSE)
dir.create(fig_unlabeled, recursive = TRUE, showWarnings = FALSE)

# Colors
renk_crop <- c(wheat   = "#1A5276", barley  = "#2E86C1",
               oats    = "#27AE60", rye     = "#1E8449",
               rice    = "#F39C12", maize   = "#E67E22",
               soybean = "#8E44AD", sorghum = "#C0392B")

break_years <- c(1980L, 1985L, 1990L, 1995L, 2000L)


# ==============================================================================
# SAVE_PLOT FUNCTION
# Prints labeled + unlabeled to R screen, saves 6 files per plot
# ==============================================================================

save_plot <- function(g_labeled, plot_name, width = 10, height = 7) {
  
  g_unlabeled <- g_labeled +
    labs(title    = NULL,
         subtitle = NULL,
         caption  = NULL,
         x        = NULL,
         y        = NULL) +
    theme(legend.title = element_blank(),
          plot.margin  = margin(4, 4, 4, 4))
  
  cat(sprintf("  [SCREEN] %s -- LABELED\n",   plot_name)); print(g_labeled)
  cat(sprintf("  [SCREEN] %s -- UNLABELED\n", plot_name)); print(g_unlabeled)
  
  save_one <- function(g, dir_path) {
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, ".pdf")),
                    plot = g, width = width, height = height, device = "pdf")
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, "_300dpi.png")),
                    plot = g, width = width, height = height,
                    dpi = 300, device = "png")
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, "_600dpi.png")),
                    plot = g, width = width, height = height,
                    dpi = 600, device = "png")
  }
  save_one(g_labeled,   fig_labeled)
  save_one(g_unlabeled, fig_unlabeled)
  
  cat(sprintf("  [SAVED]  %s -> labeled/ + unlabeled/ (PDF + 300 + 600 dpi)\n\n",
              plot_name))
  invisible(NULL)
}


# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

sig_s <- function(p)
  ifelse(is.na(p), "",
         ifelse(p < 0.01, "***",
                ifelse(p < 0.05, "**",
                       ifelse(p < 0.10, "*", ""))))

# Fit ONE structural break model
# FIX: explicit formula per model_id via switch()
# fixest feols formulas with | FE cannot be modified by update()
fit_sb_one <- function(df, model_id, break_yr,
                       wt_var = "production_tonnes") {
  if (is.null(df) || nrow(df) < 30) return(NULL)
  
  df$post_dummy <- as.integer(df$Yil >= break_yr)
  
  w <- df[[wt_var]]
  w[is.na(w) | w <= 0] <- NA
  
  # Explicit complete formula for each model -- no update(), no reformulate()
  fm <- switch(model_id,
               M1 = ln_yield ~ TMX + TMX:post_dummy + PRE + PRE_sq                | Country_ISO + Yil,
               M2 = ln_yield ~ TMX + TMX:post_dummy + PRE + PRE_sq + IRR          | Country_ISO + Yil,
               M3 = ln_yield ~ TMX + TMX:post_dummy + PRE + PRE_sq + ln_gdp       | Country_ISO + Yil,
               M4 = ln_yield ~ TMX + TMX:post_dummy + PRE + PRE_sq + IRR + ln_gdp | Country_ISO + Yil,
               NULL)
  
  if (is.null(fm)) return(NULL)
  
  m <- tryCatch(
    fixest::feols(fm, data = df, weights = w, cluster = ~Country_ISO),
    error = function(e) {
      cat(sprintf("    feols error [%s break=%d]: %s\n",
                  model_id, break_yr, conditionMessage(e)))
      NULL })
  if (is.null(m)) return(NULL)
  
  cf <- tryCatch(coef(m), error = function(e) NULL)
  vc <- tryCatch(vcov(m), error = function(e) NULL)
  if (is.null(cf) || is.null(vc) || !"TMX" %in% names(cf)) return(NULL)
  
  # Interaction term name -- fixest may order either way
  int_nm <- intersect(c("TMX:post_dummy", "post_dummy:TMX"), names(cf))
  if (length(int_nm) == 0) {
    cat(sprintf("    [%s] interaction term not found in: %s\n",
                model_id, paste(names(cf), collapse = ", ")))
    return(NULL)
  }
  
  b_tmx <- as.numeric(cf["TMX"])
  b_int <- as.numeric(cf[int_nm[1]])
  se_tmx <- sqrt(vc["TMX",     "TMX"])
  se_int <- sqrt(vc[int_nm[1], int_nm[1]])
  cov_ab <- vc["TMX",          int_nm[1]]
  
  pv_tmx <- 2 * pnorm(-abs(b_tmx / se_tmx))
  pv_int <- 2 * pnorm(-abs(b_int / se_int))
  
  # Post-break total effect: b_tmx + b_int (delta method SE)
  b_post  <- b_tmx + b_int
  se_post <- sqrt(se_tmx^2 + se_int^2 + 2 * cov_ab)
  pv_post <- 2 * pnorm(-abs(b_post / se_post))
  
  wr <- tryCatch(fixest::wald(m, keep = int_nm[1]),
                 error = function(e) NULL)
  
  list(b_tmx   = b_tmx,   se_tmx  = se_tmx,   pv_tmx  = pv_tmx,
       b_int   = b_int,   se_int  = se_int,    pv_int  = pv_int,
       b_post  = b_post,  se_post = se_post,   pv_post = pv_post,
       wald_F  = if (!is.null(wr)) as.numeric(wr$stat) else NA_real_,
       wald_p  = if (!is.null(wr)) as.numeric(wr$p)    else NA_real_,
       n_pre   = sum(df$Yil <  break_yr, na.rm = TRUE),
       n_post  = sum(df$Yil >= break_yr, na.rm = TRUE))
}


# ==============================================================================
# LOAD RDS
# ==============================================================================

rds_v4 <- file.path(ana_yol, "modeller_v4_objects.rds")
rds_v3 <- file.path(ana_yol, "modeller_v3_objects.rds")

if (file.exists(rds_v4)) {
  tum_modeller <- readRDS(rds_v4)
  cat("[LOADED] modeller_v4_objects.rds\n\n")
} else if (file.exists(rds_v3)) {
  tum_modeller <- readRDS(rds_v3)
  cat("[LOADED] modeller_v3_objects.rds (fallback)\n\n")
} else {
  stop("[ERROR] No RDS file found. Run paper1_step03_models.R first.")
}


# ==============================================================================
# STEP 1 -- STRUCTURAL BREAK (M1-M4 x ALL CROPS, BREAK=1990)
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("STEP 1 -- Structural Break (M1-M4, break=1990)\n")
cat(strrep("=", 65), "\n\n")

sb_list   <- list()
chow_list <- list()

for (urun in urun_listesi) {
  if (!urun %in% names(tum_modeller)) next
  obj     <- tum_modeller[[urun]]
  df_full <- obj$df
  df_gdp  <- obj$df_gdp
  if (is.null(df_full)) next
  
  cat(sprintf("[%s]\n", toupper(urun)))
  
  for (ms in c("M1","M2","M3","M4")) {
    # M3/M4 use GDP subsample
    df_use <- if (ms %in% c("M3","M4")) df_gdp else df_full
    if (is.null(df_use) || nrow(df_use) < 50) next
    
    res <- fit_sb_one(df_use, ms, break_yr = 1990L)
    if (is.null(res)) { cat(sprintf("  [%s] FAILED\n", ms)); next }
    
    dir_val <- dplyr::case_when(
      is.na(res$b_int)                    ~ "NA",
      res$b_int < 0 & res$pv_int < 0.10  ~ "Damage INCREASED post-1990",
      res$b_int > 0 & res$pv_int < 0.10  ~ "Adaptation post-1990",
      TRUE                                ~ "No significant break")
    
    key <- paste0(urun, "_", ms)
    
    sb_list[[key]] <- data.frame(
      crop          = urun,
      model_col     = ms,
      break_year    = 1990L,
      b_TMX_pre     = res$b_tmx,
      se_pre        = res$se_tmx,
      pval_pre      = res$pv_tmx,
      sig_pre       = sig_s(res$pv_tmx),
      lo95_pre      = res$b_tmx - 1.96 * res$se_tmx,
      hi95_pre      = res$b_tmx + 1.96 * res$se_tmx,
      b_interaction = res$b_int,
      se_int        = res$se_int,
      pval_int      = res$pv_int,
      sig_int       = sig_s(res$pv_int),
      lo95_int      = res$b_int - 1.96 * res$se_int,
      hi95_int      = res$b_int + 1.96 * res$se_int,
      b_TMX_post    = res$b_post,
      se_post       = res$se_post,
      pval_post     = res$pv_post,
      sig_post      = sig_s(res$pv_post),
      lo95_post     = res$b_post - 1.96 * res$se_post,
      hi95_post     = res$b_post + 1.96 * res$se_post,
      direction     = dir_val,
      N_pre         = res$n_pre,
      N_post        = res$n_post,
      stringsAsFactors = FALSE)
    
    chow_list[[key]] <- data.frame(
      crop              = urun,
      model_col         = ms,
      break_year        = 1990L,
      Wald_F            = res$wald_F,
      Wald_p            = res$wald_p,
      sig               = sig_s(res$wald_p),
      break_significant = (!is.na(res$wald_p) & res$wald_p < 0.10),
      stringsAsFactors  = FALSE)
    
    cat(sprintf("  [%s] pre=%+.4f%s | int=%+.4f%s | post=%+.4f%s | %s\n",
                ms,
                res$b_tmx,  sig_s(res$pv_tmx),
                res$b_int,  sig_s(res$pv_int),
                res$b_post, sig_s(res$pv_post),
                dir_val))
  }
  cat("\n")
}

sb_df   <- dplyr::bind_rows(sb_list)
chow_df <- dplyr::bind_rows(chow_list)

cat(sprintf("sb_df rows: %d | models: %s | crops: %s\n\n",
            nrow(sb_df),
            paste(sort(unique(sb_df$model_col)), collapse = ", "),
            paste(sort(unique(sb_df$crop)),      collapse = ", ")))

# Summary
cat("--- Structural Break Summary (break=1990) ---\n")
for (ms in c("M1","M2","M3","M4")) {
  sub_c <- chow_df[chow_df$model_col == ms, ]
  sub_s <- sb_df[sb_df$model_col == ms, ]
  cat(sprintf("  [%s] sig: %d/8 | Damage UP: %-25s | Adaptation: %s\n",
              ms,
              sum(sub_c$break_significant, na.rm = TRUE),
              paste(sub_s$crop[sub_s$direction ==
                                 "Damage INCREASED post-1990"], collapse = ", "),
              paste(sub_s$crop[sub_s$direction ==
                                 "Adaptation post-1990"], collapse = ", ")))
}
cat("\n")


# ==============================================================================
# STEP 2 -- BREAK YEAR SENSITIVITY (M2, break years 1980-2000)
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("STEP 2 -- Break Year Sensitivity (M2, 1980-2000)\n")
cat(strrep("=", 65), "\n\n")

sens_list <- list()

for (urun in urun_listesi) {
  if (!urun %in% names(tum_modeller)) next
  df_full <- tum_modeller[[urun]]$df
  if (is.null(df_full)) next
  
  cat(sprintf("[%s] ", urun))
  
  for (by in break_years) {
    res <- fit_sb_one(df_full, "M2", break_yr = by)
    if (is.null(res)) { cat(sprintf("%d:fail ", by)); next }
    
    sens_list[[paste0(urun, "_", by)]] <- data.frame(
      crop       = urun,
      break_year = by,
      b_int      = res$b_int,
      se_int     = res$se_int,
      pval_int   = res$pv_int,
      sig_int    = sig_s(res$pv_int),
      lo95_int   = res$b_int - 1.96 * res$se_int,
      hi95_int   = res$b_int + 1.96 * res$se_int,
      wald_p     = res$wald_p,
      sig_wald   = sig_s(res$wald_p),
      stringsAsFactors = FALSE)
    
    cat(sprintf("%d:%+.3f%s ", by, res$b_int, sig_s(res$pv_int)))
  }
  cat("\n")
}

sens_df <- dplyr::bind_rows(sens_list)
cat(sprintf("\nSensitivity rows: %d\n\n", nrow(sens_df)))


# ==============================================================================
# STEP 3 -- PESARAN CD TEST (cross-sectional dependence in M2 residuals)
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("STEP 3 -- Pesaran CD Test\n")
cat(strrep("=", 65), "\n\n")

cd_list <- list()

for (urun in urun_listesi) {
  if (!urun %in% names(tum_modeller)) next
  df <- tum_modeller[[urun]]$df
  if (is.null(df)) next
  
  pdf <- tryCatch(
    plm::pdata.frame(df, index = c("Country_ISO","Yil"),
                     drop.index = FALSE),
    error = function(e) NULL)
  if (is.null(pdf)) { cat(sprintf("  [%s] pdata.frame failed\n", urun)); next }
  
  m_plm <- tryCatch(
    plm::plm(ln_yield ~ TMX + PRE + PRE_sq + IRR,
             data = pdf, model = "within", effect = "twoways"),
    error = function(e) {
      cat(sprintf("  [%s] plm error\n", urun)); NULL })
  if (is.null(m_plm)) next
  
  cd_res <- tryCatch(plm::pcdtest(m_plm, test = "cd"),
                     error = function(e) NULL)
  if (is.null(cd_res)) { cat(sprintf("  [%s] CD test failed\n", urun)); next }
  
  cd_stat <- as.numeric(cd_res$statistic)
  cd_pval <- as.numeric(cd_res$p.value)
  
  cd_list[[urun]] <- data.frame(
    crop           = urun,
    CD_statistic   = cd_stat,
    CD_pvalue      = cd_pval,
    sig            = sig_s(cd_pval),
    has_CD         = (cd_pval < 0.05),
    recommendation = dplyr::case_when(
      cd_pval < 0.01 ~ "Strong CD -- Add DK SE to appendix",
      cd_pval < 0.05 ~ "Moderate CD -- Consider DK SE",
      cd_pval < 0.10 ~ "Weak CD -- Clustered SE likely sufficient",
      TRUE           ~ "No CD -- Clustered SE sufficient"),
    stringsAsFactors = FALSE)
  
  cat(sprintf("  [%-8s] CD = %+6.3f  p = %.4f%s  --> %s\n",
              urun, cd_stat, cd_pval, sig_s(cd_pval),
              ifelse(cd_pval < 0.05, "CD DETECTED", "OK")))
}

cd_df <- dplyr::bind_rows(cd_list)
cat(sprintf("\nCD detected: %d / %d\n\n",
            sum(cd_df$has_CD, na.rm = TRUE), nrow(cd_df)))


# ==============================================================================
# STEP 4 -- HAUSMAN TEST (FE vs RE)
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("STEP 4 -- Hausman Test (FE vs RE)\n")
cat(strrep("=", 65), "\n\n")

hausman_list <- list()

for (urun in urun_listesi) {
  if (!urun %in% names(tum_modeller)) next
  df <- tum_modeller[[urun]]$df
  if (is.null(df)) next
  
  pdf <- tryCatch(
    plm::pdata.frame(df, index = c("Country_ISO","Yil"),
                     drop.index = FALSE),
    error = function(e) NULL)
  if (is.null(pdf)) next
  
  m_fe <- tryCatch(
    plm::plm(ln_yield ~ TMX + PRE + PRE_sq + IRR,
             data = pdf, model = "within", effect = "twoways"),
    error = function(e) NULL)
  
  m_re <- tryCatch(
    plm::plm(ln_yield ~ TMX + PRE + PRE_sq + IRR,
             data = pdf, model = "random", effect = "twoways"),
    error = function(e) NULL)
  
  if (is.null(m_fe) || is.null(m_re)) {
    cat(sprintf("  [%s] model failed\n", urun)); next
  }
  
  ht <- tryCatch(plm::phtest(m_fe, m_re), error = function(e) NULL)
  if (is.null(ht)) { cat(sprintf("  [%s] Hausman failed\n", urun)); next }
  
  h_stat <- as.numeric(ht$statistic)
  h_pval <- as.numeric(ht$p.value)
  h_df   <- as.integer(ht$parameter)
  
  hausman_list[[urun]] <- data.frame(
    crop         = urun,
    H_statistic  = h_stat,
    df_param     = h_df,
    H_pvalue     = h_pval,
    sig          = sig_s(h_pval),
    FE_preferred = (h_pval < 0.05),
    conclusion   = ifelse(h_pval < 0.05,
                          "FE preferred (H0 rejected)",
                          "RE not rejected"),
    stringsAsFactors = FALSE)
  
  cat(sprintf("  [%-8s] H = %+6.2f  df = %d  p = %.4f%s  --> %s\n",
              urun, h_stat, h_df, h_pval, sig_s(h_pval),
              ifelse(h_pval < 0.05, "FE CONFIRMED", "check RE")))
}

hausman_df <- dplyr::bind_rows(hausman_list)
cat(sprintf("\nFE confirmed: %d / %d\n\n",
            sum(hausman_df$FE_preferred, na.rm = TRUE), nrow(hausman_df)))


# ==============================================================================
# PLOTS -- G_SB1 to G_HAU
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("GENERATING PLOTS (G_SB1, G_SB2, G_SB3, G_SB4, G_CD, G_HAU)\n")
cat("Each plot: screen print x2 (labeled + unlabeled), saved x6\n")
cat(strrep("=", 65), "\n")

# Shared theme
theme_diag <- theme_bw(base_size = 11) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "bold", size = 12),
        plot.subtitle    = element_text(size = 8.5, color = "grey40"),
        plot.caption     = element_text(size = 7.5, color = "grey50"),
        plot.margin      = margin(8, 12, 8, 8))

sb_m2 <- sb_df[sb_df$model_col == "M2", ]


# ------------------------------------------------------------------------------
# G_SB1 -- PRE vs POST-1990 TMX COMPARISON (M2 PW)
# ------------------------------------------------------------------------------

cat("\n[G_SB1] Pre vs post-1990 TMX (M2)...\n")

if (nrow(sb_m2) > 0) {
  
  sb_long <- dplyr::bind_rows(
    sb_m2 %>% dplyr::mutate(period = "Pre-1990",
                            coef = b_TMX_pre,
                            lo95 = lo95_pre, hi95 = hi95_pre,
                            sig  = sig_pre),
    sb_m2 %>% dplyr::mutate(period = "Post-1990",
                            coef = b_TMX_post,
                            lo95 = lo95_post, hi95 = hi95_post,
                            sig  = sig_post)) %>%
    dplyr::mutate(
      crop   = factor(crop,   levels = rev(urun_listesi)),
      period = factor(period, levels = c("Pre-1990","Post-1990")))
  
  sig_lbl <- sb_m2 %>%
    dplyr::filter(sig_int %in% c("*","**","***")) %>%
    dplyr::mutate(crop = factor(crop, levels = rev(urun_listesi)))
  
  g_sb1 <- ggplot(sb_long,
                  aes(x = coef, y = crop,
                      color = period, shape = period)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                   height   = 0.2, linewidth = 0.8,
                   position = position_dodge(width = 0.55)) +
    geom_point(size     = 3,
               position = position_dodge(width = 0.55)) +
    { if (nrow(sig_lbl) > 0)
      geom_text(data        = sig_lbl,
                aes(x = hi95_post + 0.002, y = crop,
                    label = paste0("int:", sig_int)),
                color = "#E74C3C", size = 2.8, hjust = 0,
                inherit.aes = FALSE) } +
    scale_color_manual(
      values = c("Pre-1990"  = "#7F8C8D",
                 "Post-1990" = "#C0392B"),
      name = "Period") +
    scale_shape_manual(
      values = c("Pre-1990"  = 16,
                 "Post-1990" = 17),
      name = "Period") +
    labs(
      title    = "G_SB1: TMX Effect -- Pre-1990 vs Post-1990 (M2, Production-Weighted)",
      subtitle = "Post-1990 = total effect (b_TMX + b_interaction) | 95% CI",
      caption  = "Grassini et al. (2013, Nat. Comm.) | Lobell & Field (2007, GRL)",
      x = "TMX Coefficient [delta ln(Yield) per 1 degC]",
      y = NULL) +
    theme_diag
  save_plot(g_sb1, "G_SB1_pre_post_1990", width = 10, height = 7)
  cat("[G_SB1] DONE\n")
} else {
  cat("[G_SB1] SKIPPED -- no M2 structural break data\n")
}


# ------------------------------------------------------------------------------
# G_SB2 -- INTERACTION COEFFICIENT FOREST PLOT (M2 PW)
# Positive = adaptation; negative = damage intensified
# ------------------------------------------------------------------------------

cat("\n[G_SB2] Interaction coefficient forest plot (M2)...\n")

if (nrow(sb_m2) > 0) {
  
  sb_int <- sb_m2 %>%
    dplyr::mutate(
      crop      = factor(crop, levels = rev(urun_listesi)),
      sig_color = dplyr::case_when(
        sig_int == "***" ~ "p<0.01",
        sig_int == "**"  ~ "p<0.05",
        sig_int == "*"   ~ "p<0.10",
        TRUE             ~ "ns"))
  
  g_sb2 <- ggplot(sb_int,
                  aes(x = b_interaction, y = crop, color = sig_color)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
    geom_errorbarh(aes(xmin = lo95_int, xmax = hi95_int),
                   height = 0.2, linewidth = 0.8) +
    geom_point(size = 3.5, shape = 18) +
    scale_color_manual(
      values = c("p<0.01" = "#C0392B", "p<0.05" = "#E74C3C",
                 "p<0.10" = "#E67E22", "ns"      = "grey60"),
      name = "Significance") +
    annotate("rect",
             xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
             fill = "#E74C3C", alpha = 0.04) +
    annotate("text", x = -0.001, y = 0.6,
             label = "Damage increased post-1990",
             hjust = 1, size = 2.8, color = "#C0392B", fontface = "italic") +
    annotate("text", x = 0.001, y = 0.6,
             label = "Adaptation post-1990",
             hjust = 0, size = 2.8, color = "#27AE60", fontface = "italic") +
    labs(
      title    = "G_SB2: Structural Break -- TMX x post1990 Interaction (M2, Production-Weighted)",
      subtitle = "Negative = damage intensified | Positive = adaptation signal | 95% CI",
      caption  = "Grassini et al. (2013, Nat. Comm.) | Lobell & Field (2007, GRL)",
      x = "Interaction Coefficient [TMX x post1990]",
      y = NULL) +
    theme_diag +
    theme(legend.position = "right")
  save_plot(g_sb2, "G_SB2_interaction_coef", width = 10, height = 7)
  cat("[G_SB2] DONE\n")
}


# ------------------------------------------------------------------------------
# G_SB3 -- BREAK YEAR SENSITIVITY: 1980-2000 x M2
# Stable lines across break years = finding robust to year choice
# ------------------------------------------------------------------------------

cat("\n[G_SB3] Break year sensitivity...\n")

if (nrow(sens_df) > 0) {
  
  sens_plot <- sens_df %>%
    dplyr::mutate(
      crop       = factor(crop, levels = urun_listesi),
      break_year = factor(break_year))
  
  sig_pts <- sens_df %>%
    dplyr::filter(!is.na(pval_int) & pval_int < 0.05) %>%
    dplyr::mutate(crop       = factor(crop, levels = urun_listesi),
                  break_year = factor(break_year))
  
  g_sb3 <- ggplot(sens_plot,
                  aes(x = break_year, y = b_int,
                      color = crop, group = crop)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin = lo95_int, ymax = hi95_int, fill = crop),
                alpha = 0.10, color = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.5) +
    { if (nrow(sig_pts) > 0)
      geom_text(data = sig_pts,
                aes(label = sig_int, y = hi95_int + 0.004),
                size = 2.8, hjust = 0.5, show.legend = FALSE) } +
    facet_wrap(~crop, nrow = 2, scales = "free_y") +
    scale_color_manual(values = renk_crop, guide = "none") +
    scale_fill_manual( values = renk_crop, guide = "none") +
    labs(
      title    = "G_SB3: Break Year Sensitivity -- TMX x post-break Interaction (M2, PW)",
      subtitle = "Stable lines = finding robust to year choice | Stars = p < 0.05",
      caption  = "Break years: 1980, 1985, 1990, 1995, 2000 | Shaded = 95% CI",
      x = "Break Year",
      y = "Interaction Coefficient [TMX x post-break]") +
    theme_bw(base_size = 10) +
    theme(panel.grid.minor = element_blank(),
          strip.text       = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey95"),
          plot.title       = element_text(face = "bold", size = 12),
          plot.subtitle    = element_text(size = 8, color = "grey40"),
          plot.caption     = element_text(size = 7.5, color = "grey50"))
  save_plot(g_sb3, "G_SB3_break_year_sensitivity", width = 13, height = 8)
  cat("[G_SB3] DONE\n")
} else {
  cat("[G_SB3] SKIPPED -- no sensitivity data\n")
}


# ------------------------------------------------------------------------------
# G_SB4 -- STRUCTURAL BREAK DIRECTION HEATMAP: MODEL x CROP
# Red = damage increased; green = adaptation; grey = no break
# ------------------------------------------------------------------------------

cat("\n[G_SB4] Structural break direction heatmap...\n")

if (nrow(sb_df) > 0) {
  
  hm_df <- sb_df %>%
    dplyr::mutate(
      crop      = factor(crop,      levels = rev(urun_listesi)),
      model_col = factor(model_col, levels = c("M1","M2","M3","M4")),
      dir_short = dplyr::case_when(
        direction == "Damage INCREASED post-1990" ~ "Damage UP",
        direction == "Adaptation post-1990"       ~ "Adaptation",
        direction == "No significant break"       ~ "No break",
        TRUE                                      ~ "NA"),
      dir_short = factor(dir_short,
                         levels = c("Damage UP","No break","Adaptation","NA")),
      cell_lbl  = sprintf("%+.3f%s", b_interaction, sig_int))
  
  dir_cols <- c("Damage UP"  = "#C0392B",
                "No break"   = "#BDC3C7",
                "Adaptation" = "#27AE60",
                "NA"         = "white")
  
  g_sb4 <- ggplot(hm_df,
                  aes(x = model_col, y = crop, fill = dir_short)) +
    geom_tile(color = "white", linewidth = 0.7) +
    geom_text(aes(label = cell_lbl), size = 2.5, color = "grey20") +
    scale_fill_manual(values = dir_cols, name = "Direction (p<0.10)") +
    labs(
      title    = "G_SB4: Structural Break Direction -- Model x Crop (break=1990, PW)",
      subtitle = paste0("Red = damage increased post-1990 | Green = adaptation | ",
                        "Grey = no break\n",
                        "Numbers = b_interaction with significance stars"),
      caption  = "Wald test at 10% level | M3/M4 use GDP subsample",
      x = "Model", y = NULL) +
    theme_bw(base_size = 10) +
    theme(panel.grid      = element_blank(),
          legend.position = "bottom",
          plot.title      = element_text(face = "bold", size = 12),
          plot.subtitle   = element_text(size = 8, color = "grey40"),
          plot.caption    = element_text(size = 7.5, color = "grey50"),
          plot.margin     = margin(8, 12, 8, 8))
  save_plot(g_sb4, "G_SB4_direction_heatmap", width = 9, height = 7)
  cat("[G_SB4] DONE\n")
} else {
  cat("[G_SB4] SKIPPED\n")
}


# ------------------------------------------------------------------------------
# G_CD -- PESARAN CD STATISTIC BAR CHART
# Outside +/-1.96 = cross-sectional dependence detected
# ------------------------------------------------------------------------------

cat("\n[G_CD] Pesaran CD bar chart...\n")

if (nrow(cd_df) > 0) {
  
  cd_plot <- cd_df %>%
    dplyr::mutate(
      crop     = factor(crop, levels = urun_listesi),
      cd_color = dplyr::case_when(
        CD_pvalue < 0.01 ~ "Strong CD (p<0.01)",
        CD_pvalue < 0.05 ~ "Moderate CD (p<0.05)",
        CD_pvalue < 0.10 ~ "Weak CD (p<0.10)",
        TRUE             ~ "No CD"),
      cd_color = factor(cd_color,
                        levels = c("Strong CD (p<0.01)",
                                   "Moderate CD (p<0.05)",
                                   "Weak CD (p<0.10)",
                                   "No CD")))
  
  cd_cols <- c("Strong CD (p<0.01)"   = "#C0392B",
               "Moderate CD (p<0.05)" = "#E67E22",
               "Weak CD (p<0.10)"     = "#F39C12",
               "No CD"                = "#27AE60")
  
  g_cd <- ggplot(cd_plot,
                 aes(x = reorder(crop, CD_statistic),
                     y = CD_statistic, fill = cd_color)) +
    geom_col(alpha = 0.85, width = 0.65) +
    geom_hline(yintercept =  1.96, linetype = "dashed",
               color = "#E74C3C", linewidth = 0.8) +
    geom_hline(yintercept = -1.96, linetype = "dashed",
               color = "#E74C3C", linewidth = 0.8) +
    geom_text(aes(label = sprintf("%.2f%s", CD_statistic, sig)),
              hjust = ifelse(cd_plot$CD_statistic >= 0, -0.15, 1.15),
              size = 3.2) +
    coord_flip() +
    scale_fill_manual(values = cd_cols, name = "CD Strength") +
    annotate("text", x = 0.5, y = 2.2,
             label = "+/-1.96 (p=0.05)",
             color = "#E74C3C", size = 2.5, hjust = 0) +
    labs(
      title    = "G_CD: Pesaran CD Statistic -- Cross-Sectional Dependence Test",
      subtitle = "Outside +/-1.96 = CD detected | M2 two-way FE residuals",
      caption  = "Pesaran (2004) | plm::pcdtest() | H0: cross-sectional independence",
      x = NULL,
      y = "CD Statistic (asymptotically N(0,1))") +
    theme_bw(base_size = 11) +
    theme(panel.grid.minor   = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position    = "bottom",
          plot.title         = element_text(face = "bold", size = 12),
          plot.subtitle      = element_text(size = 8, color = "grey40"),
          plot.caption       = element_text(size = 7.5, color = "grey50"),
          plot.margin        = margin(8, 12, 8, 8))
  save_plot(g_cd, "G_CD_pesaran", width = 10, height = 7)
  cat("[G_CD] DONE\n")
} else {
  cat("[G_CD] SKIPPED\n")
}


# ------------------------------------------------------------------------------
# G_HAU -- HAUSMAN TEST (FE vs RE)
# Above critical value = FE preferred
# ------------------------------------------------------------------------------

cat("\n[G_HAU] Hausman test...\n")

if (nrow(hausman_df) > 0) {
  
  crit_val <- qchisq(0.95, df = 4)   # chi-sq critical value, df=4
  
  hau_plot <- hausman_df %>%
    dplyr::mutate(
      crop     = factor(crop, levels = urun_listesi),
      fe_color = ifelse(FE_preferred,
                        "FE preferred (p<0.05)",
                        "RE not rejected"))
  
  hau_cols <- c("FE preferred (p<0.05)" = "#C0392B",
                "RE not rejected"       = "#27AE60")
  
  g_hau <- ggplot(hau_plot,
                  aes(x = reorder(crop, H_statistic),
                      y = H_statistic, fill = fe_color)) +
    geom_col(alpha = 0.85, width = 0.65) +
    geom_hline(yintercept = crit_val, linetype = "dashed",
               color = "#E74C3C", linewidth = 0.8) +
    geom_text(aes(label = sprintf("%.1f%s", H_statistic, sig)),
              hjust = -0.1, size = 3.2) +
    coord_flip() +
    scale_fill_manual(values = hau_cols, name = "Hausman Result") +
    annotate("text", x = 0.5, y = crit_val + 0.3,
             label = sprintf("chi-sq crit\n(df=4) = %.1f", crit_val),
             color = "#E74C3C", size = 2.3, hjust = 0) +
    expand_limits(y = max(hausman_df$H_statistic, na.rm = TRUE) * 1.3) +
    labs(
      title    = "G_HAU: Hausman Test -- Fixed vs Random Effects",
      subtitle = "Above dashed = FE preferred at 5% | H0: RE consistent",
      caption  = "plm::phtest() | Two-way FE vs RE | Wooldridge (2010)",
      x = NULL,
      y = "Hausman Chi-squared Statistic") +
    theme_bw(base_size = 11) +
    theme(panel.grid.minor   = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position    = "bottom",
          plot.title         = element_text(face = "bold", size = 12),
          plot.subtitle      = element_text(size = 8, color = "grey40"),
          plot.caption       = element_text(size = 7.5, color = "grey50"),
          plot.margin        = margin(8, 12, 8, 8))
  save_plot(g_hau, "G_HAU_hausman", width = 10, height = 7)
  cat("[G_HAU] DONE\n")
} else {
  cat("[G_HAU] SKIPPED\n")
}


# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

cat("\n", strrep("=", 75), "\n")
cat("DIAGNOSTIC SUMMARY\n")
cat(strrep("=", 75), "\n\n")

if (nrow(sb_df) > 0) {
  cat("--- Structural Break (break=1990) ---\n")
  cat(sprintf("%-10s %-4s %9s %-5s %-32s\n",
              "Crop","Mdl","b_int","sig","Direction"))
  cat(strrep("-", 65), "\n")
  for (urun in urun_listesi) {
    for (ms in c("M1","M2","M3","M4")) {
      r <- sb_df[sb_df$crop == urun & sb_df$model_col == ms, ]
      if (nrow(r) == 0) next
      cat(sprintf("%-10s %-4s %+9.4f %-5s %-32s\n",
                  urun, ms, r$b_interaction[1],
                  r$sig_int[1], r$direction[1]))
    }
  }
}

cat("\n--- Pesaran CD ---\n")
if (nrow(cd_df) > 0)
  print(cd_df[, c("crop","CD_statistic","CD_pvalue","sig","has_CD")],
        row.names = FALSE)

cat("\n--- Hausman ---\n")
if (nrow(hausman_df) > 0)
  print(hausman_df[, c("crop","H_statistic","H_pvalue","sig",
                       "FE_preferred","conclusion")],
        row.names = FALSE)


# ==============================================================================
# SAVE RESULTS TO EXCEL
# ==============================================================================

cat("\nSaving adim6_diagnostic.xlsx...\n")

writexl::write_xlsx(
  list(
    StructuralBreak_M1M4  = sb_df,
    StructuralBreak_Chow  = chow_df,
    BreakYear_Sensitivity = sens_df,
    PesaranCD             = cd_df,
    Hausman               = hausman_df),
  file.path(ana_yol, "adim6_diagnostic.xlsx"))

cat("[SAVED] adim6_diagnostic.xlsx\n")


# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n", strrep("=", 65), "\n")
cat("STEP 06 COMPLETE\n")
cat(strrep("=", 65), "\n\n")

cat("KEY FIX vs original:\n")
cat("  update(formula, .~.+TMX:post_dummy) does NOT work with fixest | FE.\n")
cat("  Solution: explicit formula per model in fit_sb_one() via switch().\n\n")

cat("COMPLETED:\n")
cat("  Step 1: Structural break M1-M4 x all crops (break=1990)\n")
cat("  Step 2: Break year sensitivity 1980-2000 (M2)\n")
cat("  Step 3: Pesaran CD test (M2 two-way FE residuals)\n")
cat("  Step 4: Hausman test (FE vs RE)\n\n")

cat("PLOTS (each printed labeled + unlabeled on screen):\n")
cat("  G_SB1 : Pre vs post-1990 TMX (M2)\n")
cat("  G_SB2 : Interaction coefficient forest plot (M2)\n")
cat("  G_SB3 : Break year sensitivity 1980-2000\n")
cat("  G_SB4 : Structural break direction heatmap\n")
cat("  G_CD  : Pesaran CD statistic bar chart\n")
cat("  G_HAU : Hausman test bar chart\n\n")

cat("FILES SAVED (6 per plot = 36 total):\n")
cat("  figures/labeled/   -- PDF + 300dpi PNG + 600dpi PNG\n")
cat("  figures/unlabeled/ -- PDF + 300dpi PNG + 600dpi PNG\n\n")

cat("OUTPUT: adim6_diagnostic.xlsx\n")
cat("  Sheets: StructuralBreak_M1M4, StructuralBreak_Chow,\n")
cat("          BreakYear_Sensitivity, PesaranCD, Hausman\n\n")

cat("NEXT STEP: paper1_step07_projections.R\n")
cat(strrep("=", 65), "\n")





















# ==============================================================================
# PAPER 1 -- STEP 07: OUTLIER ANALYSIS AND ROBUSTNESS
# Temperature, VPD, and Global Crop Yields: Evidence from a Multi-Crop Panel
#
# Reads: modeller_v4_objects.rds (from step03)
#
# Contents:
#   Step 1 : LOO country analysis (M2 PW, all crops)
#   Step 2 : LOO across model specifications (M1/M2/M4)
#   Step 3 : Major producer exclusion (CHN/USA/IND/BRA)
#   Step 4 : Regional exclusion (Asia/Americas/Europe/Africa)
#   Step 5 : Sorghum post-1990 LOO stability
#   Step 6 : Winsorizing stability
#   Step 7 : Post-Soviet country exclusion (wheat/barley/oats/rye)
#   Step 8 : Rice Asia-within robustness
#   Plots  : G_OL1 to G_OL7 + G_SOVIET
#            -- R screen: labeled then unlabeled
#            -- Saved: PDF + PNG 300dpi + PNG 600dpi x labeled + unlabeled
#
# UTF-8 NOTE: All special characters replaced with ASCII
#   degree sign  -> degC
#   multiply     -> x
#   em dash      -> --
#
# References:
#   Cook (1977, Technometrics)   -- Cook's distance / LOO
#   Burke et al. (2015, Nature)  -- LOO country robustness standard
#   Huber (1964, Ann.Math.Stat.) -- winsorizing
#   Zhao et al. (2017, PNAS)     -- regional robustness
# ==============================================================================


# ==============================================================================
# 0. SETUP
# ==============================================================================

library(dplyr)
library(ggplot2)
library(writexl)
library(fixest)
library(tidyr)

ana_yol      <- "C:/Users/Alper.Tosun/Desktop/cru130"
urun_listesi <- c("wheat","barley","oats","rye","rice","maize","soybean","sorghum")

# Output directories
fig_labeled   <- file.path(ana_yol, "figures", "labeled")
fig_unlabeled <- file.path(ana_yol, "figures", "unlabeled")
dir.create(fig_labeled,   recursive = TRUE, showWarnings = FALSE)
dir.create(fig_unlabeled, recursive = TRUE, showWarnings = FALSE)

# Colors
renk_crop <- c(wheat   = "#1A5276", barley  = "#2E86C1",
               oats    = "#27AE60", rye     = "#1E8449",
               rice    = "#F39C12", maize   = "#E67E22",
               soybean = "#8E44AD", sorghum = "#C0392B")

# Major producers most likely to drive global results
major_producers <- c("CHN","USA","IND","BRA","RUS","ARG","AUS","FRA","DEU","CAN")

# Regional groupings
region_map <- list(
  "Asia"     = c("CHN","IND","IDN","THA","VNM","BGD","PAK","MMR","JPN","KOR",
                 "MYS","PHL","KHM","LAO","NPL","LKA","IRN","IRQ","SAU","TUR",
                 "UZB","KAZ","AFG","SYR","ISR","JOR","LBN","AZE","GEO","ARM"),
  "Americas" = c("USA","BRA","ARG","MEX","CAN","COL","PER","CHL","ECU","VEN",
                 "BOL","PRY","URY","GTM","HND","SLV","NIC","CRI","PAN","CUB"),
  "Europe"   = c("FRA","DEU","GBR","ITA","ESP","POL","ROU","UKR","HUN","CZE",
                 "SVK","AUT","BEL","NLD","SWE","FIN","DNK","NOR","PRT","GRC",
                 "BGR","SRB","HRV","BLR","LTU","LVA","EST","MDA","ALB","MKD"),
  "Africa"   = c("DZA","EGY","LBY","MAR","SDN","TUN","ETH","NGA","BFA","MLI",
                 "NER","TCD","SEN","GHA","CMR","MOZ","UGA","TZA","KEN","ZAF",
                 "ZWE","ZMB","MWI","SOM","CIV","GIN","CAF","COG","COD","AGO"))

# Post-Soviet ISO codes (independent after 1991)
post_soviet <- c("RUS","UKR","BLR",
                 "LTU","LVA","EST",
                 "MDA",
                 "AZE","GEO","ARM",
                 "KAZ","UZB","TJK","TKM","KGZ")

# ASEAN countries
asean_iso <- c("IDN","THA","VNM","MYS","PHL","KHM","LAO","MMR","SGP","BRN")


# ==============================================================================
# SAVE_PLOT FUNCTION
# ==============================================================================

save_plot <- function(g_labeled, plot_name, width = 10, height = 7) {
  
  g_unlabeled <- g_labeled +
    labs(title    = NULL,
         subtitle = NULL,
         caption  = NULL,
         x        = NULL,
         y        = NULL) +
    theme(legend.title = element_blank(),
          plot.margin  = margin(4, 4, 4, 4))
  
  cat(sprintf("  [SCREEN] %s -- LABELED\n",   plot_name)); print(g_labeled)
  cat(sprintf("  [SCREEN] %s -- UNLABELED\n", plot_name)); print(g_unlabeled)
  
  save_one <- function(g, dir_path) {
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, ".pdf")),
                    plot = g, width = width, height = height, device = "pdf")
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, "_300dpi.png")),
                    plot = g, width = width, height = height,
                    dpi = 300, device = "png")
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, "_600dpi.png")),
                    plot = g, width = width, height = height,
                    dpi = 600, device = "png")
  }
  save_one(g_labeled,   fig_labeled)
  save_one(g_unlabeled, fig_unlabeled)
  
  cat(sprintf("  [SAVED]  %s -> labeled/ + unlabeled/ (PDF + 300 + 600 dpi)\n\n",
              plot_name))
  invisible(NULL)
}


# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

sig_s <- function(p)
  ifelse(is.na(p), "",
         ifelse(p < 0.01, "***",
                ifelse(p < 0.05, "**",
                       ifelse(p < 0.10, "*", ""))))

# Extract TMX coefficient safely -- always returns scalars
get_tmx <- function(m) {
  if (is.null(m))
    return(list(coef = NA_real_, se = NA_real_,
                pval = NA_real_, N  = NA_integer_))
  cf <- tryCatch(coef(m), error = function(e) NULL)
  vc <- tryCatch(vcov(m), error = function(e) NULL)
  if (is.null(cf) || is.null(vc) || !"TMX" %in% names(cf))
    return(list(coef = NA_real_, se = NA_real_,
                pval = NA_real_, N  = NA_integer_))
  b  <- as.numeric(cf["TMX"])[1]
  se <- as.numeric(sqrt(vc["TMX","TMX"]))[1]
  pv <- as.numeric(2 * pnorm(-abs(b / se)))[1]
  n  <- tryCatch(as.integer(stats::nobs(m))[1],
                 error = function(e) NA_integer_)
  list(coef = b, se = se, pval = pv, N = n)
}

# Model fitting helpers
fit_m1pw <- function(df) tryCatch(
  fixest::feols(ln_yield ~ TMX + PRE + PRE_sq | Country_ISO + Yil,
                data = df, weights = ~production_tonnes,
                cluster = ~Country_ISO),
  error = function(e) NULL)

fit_m2pw <- function(df) tryCatch(
  fixest::feols(ln_yield ~ TMX + PRE + PRE_sq + IRR | Country_ISO + Yil,
                data = df, weights = ~production_tonnes,
                cluster = ~Country_ISO),
  error = function(e) NULL)

fit_m4pw <- function(df) tryCatch(
  fixest::feols(ln_yield ~ TMX + PRE + PRE_sq + IRR + ln_gdp | Country_ISO + Yil,
                data = df, weights = ~production_tonnes,
                cluster = ~Country_ISO),
  error = function(e) NULL)

winsor <- function(x, lo, hi)
  pmax(pmin(x, quantile(x, hi, na.rm = TRUE)),
       quantile(x, lo, na.rm = TRUE))


# ==============================================================================
# LOAD RDS
# ==============================================================================

rds_v4 <- file.path(ana_yol, "modeller_v4_objects.rds")
rds_v3 <- file.path(ana_yol, "modeller_v3_objects.rds")

if (file.exists(rds_v4)) {
  tum_modeller <- readRDS(rds_v4)
  cat("[LOADED] modeller_v4_objects.rds\n\n")
} else if (file.exists(rds_v3)) {
  tum_modeller <- readRDS(rds_v3)
  cat("[LOADED] modeller_v3_objects.rds (fallback)\n\n")
} else {
  stop("[ERROR] No RDS file found. Run paper1_step03_models.R first.")
}


# ==============================================================================
# STEP 1 -- LOO COUNTRY ANALYSIS (M2 PW, all crops)
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("STEP 1 -- LOO Country Analysis (M2 PW)\n")
cat(strrep("=", 65), "\n\n")

loo_list   <- list()
cooks_list <- list()

for (urun in urun_listesi) {
  if (!urun %in% names(tum_modeller)) next
  df     <- tum_modeller[[urun]]$df
  m_full <- tum_modeller[[urun]]$all_mods[["M2_PW"]]
  if (is.null(df) || is.null(m_full)) next
  
  full_res <- get_tmx(m_full)
  b_full   <- full_res$coef
  se_full  <- full_res$se
  if (is.na(b_full)) next
  
  ulkeler <- sort(unique(df$Country_ISO))
  cat(sprintf("[%s] Full TMX=%+.4f%s | LOO: %d countries...\n",
              urun, b_full, sig_s(full_res$pval), length(ulkeler)))
  
  rows <- vector("list", length(ulkeler))
  for (k in seq_along(ulkeler)) {
    iso    <- ulkeler[k]
    df_loo <- df[df$Country_ISO != iso, ]
    if (dplyr::n_distinct(df_loo$Country_ISO) < 5) next
    m_loo  <- fit_m2pw(df_loo)
    res    <- get_tmx(m_loo)
    if (is.na(res$coef)) next
    
    rows[[k]] <- data.frame(
      crop         = urun,
      excl_country = iso,
      is_major     = iso %in% major_producers,
      region       = dplyr::case_when(
        iso %in% region_map[["Asia"]]     ~ "Asia",
        iso %in% region_map[["Americas"]] ~ "Americas",
        iso %in% region_map[["Europe"]]   ~ "Europe",
        iso %in% region_map[["Africa"]]   ~ "Africa",
        TRUE                              ~ "Other"),
      b_full       = b_full,
      se_full      = se_full,
      b_loo        = res$coef,
      se_loo       = res$se,
      pval_loo     = res$pval,
      sig_loo      = sig_s(res$pval),
      N_loo        = res$N,
      delta_b      = res$coef - b_full,
      std_delta    = abs(res$coef - b_full) / se_full,
      pct_change   = (res$coef - b_full) / abs(b_full) * 100,
      stringsAsFactors = FALSE)
  }
  
  loo_results <- dplyr::bind_rows(rows)
  if (nrow(loo_results) == 0) next
  
  top5 <- loo_results %>%
    dplyr::arrange(dplyr::desc(std_delta)) %>%
    dplyr::slice_head(n = 5)
  
  cat("  Top influential:\n")
  for (i in seq_len(nrow(top5)))
    cat(sprintf("    %-4s: b_loo=%+.4f  delta=%+.4f  std_d=%.2f  major=%s\n",
                top5$excl_country[i], top5$b_loo[i],
                top5$delta_b[i],     top5$std_delta[i],
                ifelse(top5$is_major[i], "YES","no")))
  
  n_inf <- sum(loo_results$std_delta > 2, na.rm = TRUE)
  cat(sprintf("  Influential (std_delta>2): %d / %d\n\n",
              n_inf, nrow(loo_results)))
  
  loo_list[[urun]]   <- loo_results
  cooks_list[[urun]] <- top5 %>% dplyr::mutate(influential = std_delta > 2)
}

loo_df   <- dplyr::bind_rows(loo_list)
cooks_df <- dplyr::bind_rows(cooks_list)
cat(sprintf("LOO total rows: %d\n\n", nrow(loo_df)))


# ==============================================================================
# STEP 2 -- LOO ACROSS MODEL SPECIFICATIONS (M1/M2/M4)
# Tests whether the same country is influential across all 3 specifications
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("STEP 2 -- LOO Across Models (M1/M2/M4)\n")
cat(strrep("=", 65), "\n\n")

loo_models_list <- list()

for (urun in urun_listesi) {
  if (!urun %in% names(tum_modeller)) next
  df_full <- tum_modeller[[urun]]$df
  df_gdp  <- tum_modeller[[urun]]$df_gdp
  if (is.null(df_full)) next
  
  b_m1  <- get_tmx(tum_modeller[[urun]]$all_mods[["M1_PW"]])$coef
  b_m2  <- get_tmx(tum_modeller[[urun]]$all_mods[["M2_PW"]])$coef
  b_m4  <- get_tmx(tum_modeller[[urun]]$all_mods[["M4_PW"]])$coef
  se_m2 <- get_tmx(tum_modeller[[urun]]$all_mods[["M2_PW"]])$se
  if (is.na(b_m2) || is.na(se_m2)) next
  
  # Test only top 10 most influential countries from Step 1 M2 LOO
  top_countries <- loo_df[loo_df$crop == urun, ] %>%
    dplyr::arrange(dplyr::desc(std_delta)) %>%
    dplyr::slice_head(n = 10) %>%
    dplyr::pull(excl_country)
  if (length(top_countries) == 0) next
  
  cat(sprintf("[%s] %d top countries across M1/M2/M4...\n",
              urun, length(top_countries)))
  
  for (iso in top_countries) {
    df_loo     <- df_full[df_full$Country_ISO != iso, ]
    df_loo_gdp <- if (!is.null(df_gdp))
      df_gdp[df_gdp$Country_ISO != iso, ] else NULL
    if (dplyr::n_distinct(df_loo$Country_ISO) < 5) next
    
    r_m1 <- get_tmx(fit_m1pw(df_loo))
    r_m2 <- get_tmx(fit_m2pw(df_loo))
    r_m4 <- if (!is.null(df_loo_gdp) && nrow(df_loo_gdp) > 50)
      get_tmx(fit_m4pw(df_loo_gdp))
    else
      list(coef = NA_real_, se = NA_real_, pval = NA_real_, N = NA_integer_)
    
    loo_models_list[[paste0(urun, "_", iso)]] <- data.frame(
      crop         = urun,
      excl_country = iso,
      b_full_M2    = b_m2,
      b_loo_M1     = r_m1$coef,
      std_d_M1     = abs(r_m1$coef - b_m1) / se_m2,
      b_loo_M2     = r_m2$coef,
      std_d_M2     = abs(r_m2$coef - b_m2) / se_m2,
      b_loo_M4     = r_m4$coef,
      std_d_M4     = abs(r_m4$coef - b_m4) / se_m2,
      consistent   = (sign(r_m1$coef - b_m1) == sign(r_m2$coef - b_m2)),
      stringsAsFactors = FALSE)
  }
}

loo_models_df <- dplyr::bind_rows(loo_models_list)
cat(sprintf("\nLOO across models rows: %d\n\n", nrow(loo_models_df)))


# ==============================================================================
# STEP 3 -- MAJOR PRODUCER EXCLUSION (CHN/USA/IND/BRA, all crops)
# These 4 dominate global production -- result must hold without them
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("STEP 3 -- Major Producer Exclusion\n")
cat(strrep("=", 65), "\n\n")

major_excl_list <- list()
major_test      <- c("CHN","USA","IND","BRA")

cat(sprintf("%-10s  %-9s  %-9s  %-9s  %-9s  %-9s  %-9s\n",
            "Crop","Full","ex-CHN","ex-USA","ex-IND","ex-BRA","ex-ALL4"))
cat(strrep("-", 70), "\n")

for (urun in urun_listesi) {
  if (!urun %in% names(tum_modeller)) next
  df_full <- tum_modeller[[urun]]$df
  m_full  <- tum_modeller[[urun]]$all_mods[["M2_PW"]]
  if (is.null(df_full) || is.null(m_full)) next
  b_full <- get_tmx(m_full)$coef
  if (is.na(b_full)) next
  
  val_vec <- sprintf("%+.4f", b_full)
  
  # Exclude each major producer individually, then all four together
  test_groups <- c(as.list(major_test), list(major_test))
  test_names  <- c(paste0("ex_", major_test), "ex_ALL4")
  
  for (j in seq_along(test_groups)) {
    excl    <- test_groups[[j]]
    df_excl <- df_full[!df_full$Country_ISO %in% excl, ]
    if (dplyr::n_distinct(df_excl$Country_ISO) < 5) {
      val_vec <- c(val_vec, "  --  ")
      next
    }
    b_excl <- get_tmx(fit_m2pw(df_excl))$coef
    val_vec <- c(val_vec, sprintf("%+.4f", b_excl))
    
    major_excl_list[[paste0(urun, "_", test_names[j])]] <- data.frame(
      crop       = urun,
      excl_group = test_names[j],
      n_excl     = length(excl),
      b_full     = b_full,
      b_excl     = b_excl,
      delta_b    = b_excl - b_full,
      pct_change = (b_excl - b_full) / abs(b_full) * 100,
      stable     = abs((b_excl - b_full) / b_full) < 0.10,
      stringsAsFactors = FALSE)
  }
  cat(sprintf("%-10s  %s\n", urun, paste(val_vec, collapse = "  ")))
}

major_excl_df <- dplyr::bind_rows(major_excl_list)
cat(sprintf("\nMajor exclusion rows: %d\n\n", nrow(major_excl_df)))


# ==============================================================================
# STEP 4 -- REGIONAL EXCLUSION (all crops)
# If results hold when excluding Asia: not CHN/IND driven
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("STEP 4 -- Regional Exclusion\n")
cat(strrep("=", 65), "\n\n")

region_excl_list <- list()

cat(sprintf("%-10s  %-9s  %-9s  %-9s  %-9s  %-9s\n",
            "Crop","Full","ex-Asia","ex-Amer","ex-Eur","ex-Afr"))
cat(strrep("-", 62), "\n")

for (urun in urun_listesi) {
  if (!urun %in% names(tum_modeller)) next
  df_full <- tum_modeller[[urun]]$df
  m_full  <- tum_modeller[[urun]]$all_mods[["M2_PW"]]
  if (is.null(df_full) || is.null(m_full)) next
  b_full <- get_tmx(m_full)$coef
  if (is.na(b_full)) next
  
  val_vec <- sprintf("%+.4f", b_full)
  
  for (reg in names(region_map)) {
    df_excl  <- df_full[!df_full$Country_ISO %in% region_map[[reg]], ]
    n_remain <- dplyr::n_distinct(df_excl$Country_ISO)
    if (n_remain < 5) { val_vec <- c(val_vec, "  --  "); next }
    
    b_excl  <- get_tmx(fit_m2pw(df_excl))$coef
    val_vec <- c(val_vec, sprintf("%+.4f", b_excl))
    
    region_excl_list[[paste0(urun, "_", reg)]] <- data.frame(
      crop        = urun,
      excl_region = reg,
      n_remain    = n_remain,
      b_full      = b_full,
      b_excl      = b_excl,
      delta_b     = b_excl - b_full,
      pct_change  = (b_excl - b_full) / abs(b_full) * 100,
      stable      = abs((b_excl - b_full) / b_full) < 0.10,
      stringsAsFactors = FALSE)
  }
  cat(sprintf("%-10s  %s\n", urun, paste(val_vec, collapse = "  ")))
}

region_excl_df <- dplyr::bind_rows(region_excl_list)
cat(sprintf("\nRegional exclusion rows: %d\n\n", nrow(region_excl_df)))


# ==============================================================================
# STEP 5 -- SORGHUM POST-1990 LOO STABILITY
# Does the vulnerability increase finding depend on any single country?
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("STEP 5 -- Sorghum Post-1990 LOO\n")
cat(strrep("=", 65), "\n\n")

sorg_sb_df <- data.frame()
df_sorg    <- tum_modeller[["sorghum"]]$df

if (!is.null(df_sorg)) {
  df_sorg$post1990 <- as.integer(df_sorg$Yil >= 1990)
  afrika_iso       <- region_map[["Africa"]]
  
  fit_sb_sorg <- function(dat) tryCatch(
    fixest::feols(
      ln_yield ~ TMX + TMX:post1990 + PRE + PRE_sq + IRR | Country_ISO + Yil,
      data = dat, weights = ~production_tonnes,
      cluster = ~Country_ISO),
    error = function(e) NULL)
  
  sorg_samples <- list(
    "Full sample"  = df_sorg,
    "Excl. Africa" = df_sorg[!df_sorg$Country_ISO %in% afrika_iso, ],
    "Africa only"  = df_sorg[ df_sorg$Country_ISO %in% afrika_iso, ])
  
  cat("--- Sorghum Post-1990: 3 Sample Comparison ---\n")
  cat(sprintf("  %-20s  %9s  %9s  %-5s  %6s\n",
              "Sample","b_TMX","b_int","sig","N"))
  cat(strrep("-", 58), "\n")
  
  for (nm in names(sorg_samples)) {
    dat <- sorg_samples[[nm]]
    if (dplyr::n_distinct(dat$Country_ISO) < 5) {
      cat(sprintf("  %-20s  [N too small]\n", nm)); next }
    m  <- fit_sb_sorg(dat)
    if (is.null(m)) { cat(sprintf("  %-20s  [failed]\n", nm)); next }
    cf <- tryCatch(coef(m), error = function(e) NULL)
    vc <- tryCatch(vcov(m), error = function(e) NULL)
    if (is.null(cf) || is.null(vc)) next
    b1  <- as.numeric(cf["TMX"])
    inm <- intersect(c("TMX:post1990","post1990:TMX"), names(cf))
    bi  <- if (length(inm) > 0) as.numeric(cf[inm[1]])            else NA_real_
    sei <- if (length(inm) > 0) as.numeric(sqrt(vc[inm[1],inm[1]])) else NA_real_
    pvi <- if (!is.na(sei)) 2 * pnorm(-abs(bi / sei)) else NA_real_
    nn  <- tryCatch(stats::nobs(m), error = function(e) NA)
    cat(sprintf("  %-20s  %+9.4f  %+9.4f  %-5s  %6d\n",
                nm, b1, bi, sig_s(pvi), nn))
  }
  
  # LOO for sorghum structural break
  ulkeler_sorg <- sort(unique(df_sorg$Country_ISO))
  cat(sprintf("\n  Sorghum LOO (%d countries)...\n", length(ulkeler_sorg)))
  
  sorg_rows <- vector("list", length(ulkeler_sorg))
  for (k in seq_along(ulkeler_sorg)) {
    iso    <- ulkeler_sorg[k]
    df_loo <- df_sorg[df_sorg$Country_ISO != iso, ]
    if (dplyr::n_distinct(df_loo$Country_ISO) < 5) next
    m_loo  <- fit_sb_sorg(df_loo)
    if (is.null(m_loo)) next
    cf2 <- tryCatch(coef(m_loo), error = function(e) NULL)
    vc2 <- tryCatch(vcov(m_loo), error = function(e) NULL)
    if (is.null(cf2) || is.null(vc2)) next
    inm2 <- intersect(c("TMX:post1990","post1990:TMX"), names(cf2))
    if (length(inm2) == 0) next
    bi  <- as.numeric(cf2[inm2[1]])
    sei <- as.numeric(sqrt(vc2[inm2[1], inm2[1]]))
    pvi <- 2 * pnorm(-abs(bi / sei))
    sorg_rows[[k]] <- data.frame(
      excl_country = iso,
      is_africa    = iso %in% afrika_iso,
      b_int_loo    = bi,
      se_int_loo   = sei,
      pval_int_loo = pvi,
      sig_int      = sig_s(pvi),
      lo95         = bi - 1.96 * sei,
      hi95         = bi + 1.96 * sei,
      stringsAsFactors = FALSE)
  }
  
  sorg_sb_df <- dplyr::bind_rows(sorg_rows) %>%
    dplyr::arrange(b_int_loo)
  
  cat(sprintf("  b_int LOO range: %.4f to %.4f\n",
              min(sorg_sb_df$b_int_loo, na.rm = TRUE),
              max(sorg_sb_df$b_int_loo, na.rm = TRUE)))
  cat(sprintf("  All negative   : %s\n",
              all(sorg_sb_df$b_int_loo < 0, na.rm = TRUE)))
  cat(sprintf("  pct sig (p<0.05): %.1f%%\n",
              mean(sorg_sb_df$pval_int_loo < 0.05, na.rm = TRUE) * 100))
}


# ==============================================================================
# STEP 6 -- WINSORIZING STABILITY
# Apply Huber winsorizing to ln_yield and TMX -- does it change results?
# ==============================================================================

cat("\n", strrep("=", 65), "\n")
cat("STEP 6 -- Winsorizing Stability\n")
cat(strrep("=", 65), "\n\n")

wins_list <- list()

for (urun in urun_listesi) {
  if (!urun %in% names(tum_modeller)) next
  df     <- tum_modeller[[urun]]$df
  m_full <- tum_modeller[[urun]]$all_mods[["M2_PW"]]
  if (is.null(df) || is.null(m_full)) next
  b_full <- get_tmx(m_full)$coef
  if (is.na(b_full)) next
  
  for (bound in list(c(0.01, 0.99), c(0.05, 0.95))) {
    lo_p <- bound[1]; hi_p <- bound[2]
    lbl  <- paste0(lo_p * 100, "pct-", hi_p * 100, "pct")
    df_w          <- df
    df_w$ln_yield <- winsor(df_w$ln_yield, lo_p, hi_p)
    df_w$TMX      <- winsor(df_w$TMX,      lo_p, hi_p)
    res_w <- get_tmx(fit_m2pw(df_w))
    
    wins_list[[paste0(urun, "_", lbl)]] <- data.frame(
      crop         = urun,
      winsor_bound = lbl,
      b_full       = b_full,
      b_wins       = res_w$coef,
      se_wins      = res_w$se,
      pval_wins    = res_w$pval,
      sig_wins     = sig_s(res_w$pval),
      delta_b      = res_w$coef - b_full,
      pct_change   = (res_w$coef - b_full) / abs(b_full) * 100,
      stable       = abs((res_w$coef - b_full) / b_full) < 0.10,
      stringsAsFactors = FALSE)
  }
  cat(sprintf("[%s] full=%+.4f | 1pct=%+.4f | 5pct=%+.4f\n",
              urun, b_full,
              wins_list[[paste0(urun,"_1pct-99pct")]]$b_wins,
              wins_list[[paste0(urun,"_5pct-95pct")]]$b_wins))
}

wins_df <- dplyr::bind_rows(wins_list)

cat("\n--- Winsorizing (<10%% change = STABLE) ---\n")
for (urun in urun_listesi) {
  r1 <- wins_df[wins_df$crop == urun & wins_df$winsor_bound == "1pct-99pct", ]
  r5 <- wins_df[wins_df$crop == urun & wins_df$winsor_bound == "5pct-95pct", ]
  if (nrow(r1) == 0 || nrow(r5) == 0) next
  cat(sprintf("  %-10s  1pct=%s  5pct=%s\n",
              urun,
              ifelse(isTRUE(r1$stable), "STABLE","CHECK"),
              ifelse(isTRUE(r5$stable), "STABLE","CHECK")))
}


# ==============================================================================
# STEP 7 -- POST-SOVIET EXCLUSION (wheat/barley/oats/rye)
# ==============================================================================

cat("\n", strrep("=", 65), "\n")
cat("STEP 7 -- Post-Soviet Country Exclusion (wheat/barley/oats/rye)\n")
cat(strrep("=", 65), "\n\n")

hedef_urunler  <- c("wheat","barley","oats","rye")
soviet_results <- list()

for (urun in hedef_urunler) {
  if (!urun %in% names(tum_modeller)) next
  obj    <- tum_modeller[[urun]]
  df     <- obj$df
  if (is.null(df)) next
  
  present_soviet <- intersect(post_soviet, unique(df$Country_ISO))
  cat(sprintf("[%s] Post-Soviet present: %s\n",
              urun, paste(present_soviet, collapse = ", ")))
  
  m_full <- obj$all_mods[["M2_PW"]]
  r_full <- get_tmx(m_full)
  b_full <- r_full$coef
  
  add_row <- function(sample_nm, type, b, se, pv, n, n_excl) {
    data.frame(
      crop     = urun,
      sample   = sample_nm,
      sample_type = type,
      b        = b,
      se       = se,
      pval     = pv,
      sig      = sig_s(pv),
      lo95     = b - 1.96 * se,
      hi95     = b + 1.96 * se,
      N        = n,
      n_excl   = n_excl,
      stringsAsFactors = FALSE)
  }
  
  soviet_results[[paste0(urun,"_full")]] <-
    add_row("Full sample","full", r_full$coef, r_full$se,
            r_full$pval, r_full$N, 0L)
  
  # Exclude all post-Soviet
  df_excl <- df[!df$Country_ISO %in% post_soviet, ]
  r_excl  <- get_tmx(fit_m2pw(df_excl))
  soviet_results[[paste0(urun,"_excl_all")]] <-
    add_row(sprintf("Excl. all post-Soviet (-%d)", length(present_soviet)),
            "excl_all", r_excl$coef, r_excl$se,
            r_excl$pval, r_excl$N, length(present_soviet))
  
  # Exclude Russia only
  if ("RUS" %in% unique(df$Country_ISO)) {
    df_norus <- df[df$Country_ISO != "RUS", ]
    r_norus  <- get_tmx(fit_m2pw(df_norus))
    soviet_results[[paste0(urun,"_excl_rus")]] <-
      add_row("Excl. Russia only","excl_rus",
              r_norus$coef, r_norus$se, r_norus$pval, r_norus$N, 1L)
  }
  
  # Exclude RUS + UKR + KAZ (dominant grain bloc)
  big3_present <- intersect(c("RUS","UKR","KAZ"), unique(df$Country_ISO))
  if (length(big3_present) > 0) {
    df_nobig3 <- df[!df$Country_ISO %in% c("RUS","UKR","KAZ"), ]
    r_nobig3  <- get_tmx(fit_m2pw(df_nobig3))
    soviet_results[[paste0(urun,"_excl_big3")]] <-
      add_row("Excl. RUS+UKR+KAZ","excl_big3",
              r_nobig3$coef, r_nobig3$se,
              r_nobig3$pval, r_nobig3$N,
              length(big3_present))
  }
  
  cat(sprintf("  full=%+.4f%s | excl_all=%+.4f%s | excl_RUS=%s\n",
              b_full, sig_s(r_full$pval),
              r_excl$coef, sig_s(r_excl$pval),
              if ("RUS" %in% unique(df$Country_ISO))
                sprintf("%+.4f%s", get_tmx(fit_m2pw(df[df$Country_ISO!="RUS",]))$coef,
                        sig_s(get_tmx(fit_m2pw(df[df$Country_ISO!="RUS",]))$pval))
              else "--"))
  cat("\n")
}

soviet_df <- dplyr::bind_rows(soviet_results)


# ==============================================================================
# STEP 8 -- RICE ASIA-WITHIN ROBUSTNESS
# Which Asian subgroup drives the rice heat damage signal?
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("STEP 8 -- Rice Asia-Within Robustness\n")
cat(strrep("=", 65), "\n\n")

df_rice <- tum_modeller[["rice"]]$df

if (!is.null(df_rice)) {
  asya_gruplari <- list(
    "Full sample"          = character(0),
    "Excl. China"          = "CHN",
    "Excl. India"          = "IND",
    "Excl. Japan"          = "JPN",
    "Excl. ASEAN"          = asean_iso,
    "Excl. CHN+IND"        = c("CHN","IND"),
    "Excl. CHN+IND+ASEAN"  = c("CHN","IND", asean_iso),
    "Excl. all Asia"       = region_map[["Asia"]])
  
  rice_asia_list <- list()
  b_rice_full    <- NA_real_
  
  cat(sprintf("  %-28s  %9s  %-5s  %6s  %8s\n",
              "Sample","b_TMX","sig","N","pct_chg"))
  cat(strrep("-", 65), "\n")
  
  for (grp_nm in names(asya_gruplari)) {
    excl_iso    <- asya_gruplari[[grp_nm]]
    df_sub      <- if (length(excl_iso) == 0) df_rice else
      df_rice[!df_rice$Country_ISO %in% excl_iso, ]
    n_countries <- dplyr::n_distinct(df_sub$Country_ISO)
    if (n_countries < 5) {
      cat(sprintf("  %-28s  [too few: %d]\n", grp_nm, n_countries)); next }
    
    r_sub <- get_tmx(fit_m2pw(df_sub))
    if (grp_nm == "Full sample") b_rice_full <- r_sub$coef
    pct_chg <- if (!is.na(b_rice_full) && grp_nm != "Full sample")
      (r_sub$coef - b_rice_full) / abs(b_rice_full) * 100 else NA_real_
    
    cat(sprintf("  %-28s  %+9.4f  %-5s  %6d  %s\n",
                grp_nm,
                ifelse(is.na(r_sub$coef), 0, r_sub$coef),
                sig_s(r_sub$pval), n_countries,
                ifelse(is.na(pct_chg), "  ref  ",
                       sprintf("%+.1f%%", pct_chg))))
    
    rice_asia_list[[grp_nm]] <- data.frame(
      sample      = as.character(grp_nm),
      n_excl      = as.integer(length(excl_iso)),
      n_countries = as.integer(n_countries),
      b           = as.numeric(r_sub$coef),
      se          = as.numeric(r_sub$se),
      pval        = as.numeric(r_sub$pval),
      sig         = as.character(sig_s(r_sub$pval)),
      lo95        = as.numeric(r_sub$coef - 1.96 * r_sub$se),
      hi95        = as.numeric(r_sub$coef + 1.96 * r_sub$se),
      pct_change  = as.numeric(pct_chg),
      stringsAsFactors = FALSE)
  }
  
  rice_asia_df <- dplyr::bind_rows(rice_asia_list) %>%
    dplyr::mutate(
      sample    = factor(sample, levels = names(asya_gruplari)),
      stability = dplyr::case_when(
        is.na(pct_change)        ~ "Reference",
        abs(pct_change) < 10    ~ "Stable (<10%)",
        abs(pct_change) < 30    ~ "Moderate (10-30%)",
        TRUE                    ~ "Large (>30%)"),
      stability = factor(stability,
                         levels = c("Reference","Stable (<10%)",
                                    "Moderate (10-30%)","Large (>30%)")))
} else {
  cat("[SKIP] rice df not found\n")
  rice_asia_df <- data.frame()
  b_rice_full  <- NA_real_
}


# ==============================================================================
# PLOTS -- G_OL1 to G_OL7 + G_SOVIET
# ==============================================================================

cat("\n", strrep("=", 65), "\n")
cat("GENERATING PLOTS\n")
cat("Each plot: screen print x2 (labeled + unlabeled), saved x6\n")
cat(strrep("=", 65), "\n")

theme_rob <- theme_bw(base_size = 11) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "bold", size = 12),
        plot.subtitle    = element_text(size = 8.5, color = "grey40"),
        plot.caption     = element_text(size = 7.5, color = "grey50"),
        plot.margin      = margin(8, 12, 8, 8))


# ------------------------------------------------------------------------------
# G_OL1 -- LOO BUBBLE PLOT (top 8 influential per crop)
# ------------------------------------------------------------------------------

cat("\n[G_OL1] LOO bubble plot...\n")

if (nrow(loo_df) > 0) {
  top_loo <- loo_df %>%
    dplyr::group_by(crop) %>%
    dplyr::slice_max(order_by = std_delta, n = 8) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      crop        = factor(crop, levels = urun_listesi),
      influential = std_delta > 2,
      lbl         = ifelse(std_delta > 1.5, excl_country, ""))
  
  full_ref <- loo_df %>%
    dplyr::distinct(crop, b_full) %>%
    dplyr::mutate(crop = factor(crop, levels = urun_listesi))
  
  g_ol1 <- ggplot(top_loo,
                  aes(x = b_loo, y = reorder(excl_country, std_delta),
                      size = std_delta, color = influential)) +
    geom_vline(data        = full_ref,
               aes(xintercept = b_full),
               linetype    = "dotted", color = "grey50",
               linewidth   = 0.7, inherit.aes = FALSE) +
    geom_point(alpha = 0.75) +
    geom_text(aes(label = lbl), size = 2.0, vjust = -0.8) +
    facet_wrap(~crop, scales = "free", nrow = 2) +
    scale_color_manual(
      values = c("TRUE" = "#C0392B", "FALSE" = "#7F8C8D"),
      labels = c("TRUE" = "Influential (>2 SE)", "FALSE" = "Normal"),
      name   = "") +
    scale_size_continuous(range = c(1.5, 6), name = "Std delta") +
    labs(
      title    = "G_OL1: LOO Country Analysis -- Influential Observations (M2 PW)",
      subtitle = "Dotted = full-sample TMX coef | Red = std_delta > 2 SE | Top 8 per crop",
      caption  = "Cook (1977, Technometrics) | Burke et al. (2015, Nature)",
      x = "TMX Coef When Country Excluded", y = NULL) +
    theme_bw(base_size = 9) +
    theme(panel.grid.minor = element_blank(),
          strip.text       = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey95"),
          legend.position  = "bottom",
          plot.title       = element_text(face = "bold", size = 11),
          plot.subtitle    = element_text(size = 7, color = "grey40"),
          plot.caption     = element_text(size = 7, color = "grey50"))
  save_plot(g_ol1, "G_OL1_LOO_bubble", width = 14, height = 9)
  cat("[G_OL1] DONE\n")
}


# ------------------------------------------------------------------------------
# G_OL2 -- SORGHUM POST-1990 LOO STABILITY
# ------------------------------------------------------------------------------

cat("\n[G_OL2] Sorghum post-1990 LOO stability...\n")

if (nrow(sorg_sb_df) > 0) {
  sorg_sb_df2 <- sorg_sb_df %>%
    dplyr::mutate(
      rank   = dplyr::row_number(),
      region = ifelse(is_africa, "Africa", "Non-Africa"),
      lbl    = ifelse(rank <= 4 | rank >= (max(rank) - 3),
                      excl_country, ""))
  
  g_ol2 <- ggplot(sorg_sb_df2,
                  aes(x = rank, y = b_int_loo, color = region)) +
    geom_hline(yintercept = 0,       linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = -0.0538, linetype = "dotted",
               color = "#C0392B", linewidth = 0.9) +
    geom_errorbar(aes(ymin = lo95, ymax = hi95),
                  width = 0.4, linewidth = 0.4, alpha = 0.45) +
    geom_point(size = 2.0, alpha = 0.85) +
    geom_text(aes(label = lbl), size = 2.1, vjust = -0.9) +
    annotate("text", x = 2, y = -0.053,
             label = "Full model int = -0.054***",
             hjust = 0, vjust = -0.5, size = 2.6, color = "#C0392B") +
    scale_color_manual(
      values = c("Africa" = "#E74C3C", "Non-Africa" = "#1A5276"),
      name = "Region") +
    labs(
      title    = "G_OL2: Sorghum Post-1990 Interaction -- LOO Country Stability",
      subtitle = paste0("Each point = TMX:post1990 when that country excluded | Red = Africa\n",
                        "Africa and Non-Africa cluster together = break not Africa-driven"),
      caption  = "Dotted = full-sample estimate (-0.054***)",
      x = "Countries ranked by LOO interaction estimate",
      y = "TMX:post1990 Coefficient") +
    theme_rob
  save_plot(g_ol2, "G_OL2_sorghum_LOO", width = 10, height = 7)
  cat("[G_OL2] DONE\n")
}


# ------------------------------------------------------------------------------
# G_OL3 -- WINSORIZING STABILITY
# ------------------------------------------------------------------------------

cat("\n[G_OL3] Winsorizing stability...\n")

if (nrow(wins_df) > 0 && nrow(loo_df) > 0) {
  orig_df <- loo_df %>%
    dplyr::distinct(crop, b_full) %>%
    dplyr::rename(b_wins = b_full) %>%
    dplyr::mutate(winsor_bound = "Original")
  
  wins_plot <- dplyr::bind_rows(
    orig_df,
    wins_df %>% dplyr::select(crop, winsor_bound, b_wins) %>%
      dplyr::mutate(winsor_bound = dplyr::recode(
        winsor_bound,
        "1pct-99pct" = "1%-99%",
        "5pct-95pct" = "5%-95%"))) %>%
    dplyr::mutate(
      crop         = factor(crop, levels = urun_listesi),
      winsor_bound = factor(winsor_bound,
                            levels = c("Original","1%-99%","5%-95%")))
  
  g_ol3 <- ggplot(wins_plot,
                  aes(x = b_wins, y = crop,
                      color = winsor_bound, shape = winsor_bound)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(size = 3, position = position_dodge(width = 0.55)) +
    scale_color_manual(
      values = c("Original" = "#1A5276",
                 "1%-99%"   = "#E67E22",
                 "5%-95%"   = "#C0392B"),
      name = "Winsorization") +
    scale_shape_manual(
      values = c("Original" = 16, "1%-99%" = 17, "5%-95%" = 15),
      name = "Winsorization") +
    labs(
      title    = "G_OL3: TMX Coefficient Stability Under Winsorization (M2 PW)",
      subtitle = "Overlapping points = extreme values do not drive results\nApplied to ln_yield and TMX simultaneously",
      caption  = "Huber (1964, Ann. Math. Stat.)",
      x = "TMX Coefficient", y = NULL) +
    theme_rob
  save_plot(g_ol3, "G_OL3_winsorizing", width = 10, height = 7)
  cat("[G_OL3] DONE\n")
}


# ------------------------------------------------------------------------------
# G_OL4 -- LOO ACROSS MODEL SPECIFICATIONS (M1/M2/M4)
# ------------------------------------------------------------------------------

cat("\n[G_OL4] LOO across model specifications...\n")

if (nrow(loo_models_df) > 0 && !all(is.na(loo_models_df$b_loo_M2))) {
  
  loo_mod_long <- loo_models_df %>%
    dplyr::select(crop, excl_country, b_full_M2,
                  b_loo_M1, b_loo_M2, b_loo_M4) %>%
    tidyr::pivot_longer(
      cols      = c(b_loo_M1, b_loo_M2, b_loo_M4),
      names_to  = "model_spec",
      values_to = "b_loo") %>%
    dplyr::mutate(
      model_spec = dplyr::recode(model_spec,
                                 "b_loo_M1" = "M1",
                                 "b_loo_M2" = "M2",
                                 "b_loo_M4" = "M4"),
      crop = factor(crop, levels = urun_listesi)) %>%
    dplyr::filter(!is.na(b_loo))
  
  g_ol4 <- ggplot(loo_mod_long,
                  aes(x = b_loo,
                      y = reorder(excl_country, b_loo),
                      color = model_spec, shape = model_spec)) +
    geom_vline(aes(xintercept = b_full_M2),
               linetype = "dotted", color = "grey50", linewidth = 0.6) +
    geom_point(size     = 2.2, alpha = 0.85,
               position = position_dodge(width = 0.5)) +
    facet_wrap(~crop, scales = "free", nrow = 2) +
    scale_color_manual(
      values = c("M1" = "#95A5A6", "M2" = "#E74C3C", "M4" = "#8E44AD"),
      name = "Model") +
    scale_shape_manual(
      values = c("M1" = 16, "M2" = 17, "M4" = 18),
      name = "Model") +
    labs(
      title    = "G_OL4: LOO Stability Across Model Specifications (M1/M2/M4)",
      subtitle = "Top 10 influential countries per crop | Dotted = M2 full-sample estimate\nIf M1/M2/M4 align: influential country finding is model-robust",
      caption  = "Cook (1977, Technometrics) | Burke et al. (2015, Nature)",
      x = "TMX Coef When Country Excluded", y = NULL) +
    theme_bw(base_size = 9) +
    theme(panel.grid.minor = element_blank(),
          strip.text       = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey95"),
          legend.position  = "bottom",
          plot.title       = element_text(face = "bold", size = 11),
          plot.subtitle    = element_text(size = 7, color = "grey40"),
          plot.caption     = element_text(size = 7, color = "grey50"))
  save_plot(g_ol4, "G_OL4_LOO_models", width = 14, height = 9)
  cat("[G_OL4] DONE\n")
} else {
  cat("[G_OL4] SKIPPED -- insufficient data\n")
}


# ------------------------------------------------------------------------------
# G_OL5 -- MAJOR PRODUCER EXCLUSION HEATMAP
# ------------------------------------------------------------------------------

cat("\n[G_OL5] Major producer exclusion heatmap...\n")

if (nrow(major_excl_df) > 0) {
  excl_plot <- major_excl_df %>%
    dplyr::mutate(
      crop       = factor(crop, levels = urun_listesi),
      excl_group = factor(excl_group,
                          levels = c("ex_CHN","ex_USA","ex_IND",
                                     "ex_BRA","ex_ALL4")),
      stable_col = dplyr::case_when(
        is.na(pct_change)       ~ "N/A",
        abs(pct_change) < 5    ~ "< 5%",
        abs(pct_change) < 10   ~ "5-10%",
        abs(pct_change) < 20   ~ "10-20%",
        TRUE                   ~ "> 20%"),
      stable_col = factor(stable_col,
                          levels = c("< 5%","5-10%","10-20%","> 20%","N/A")),
      cell_lbl   = ifelse(is.na(b_excl), "--",
                          sprintf("%+.3f", b_excl)))
  
  stab_cols <- c("< 5%"  = "#27AE60", "5-10%" = "#82E0AA",
                 "10-20%"= "#F39C12", "> 20%" = "#C0392B",
                 "N/A"   = "grey90")
  
  g_ol5 <- ggplot(excl_plot,
                  aes(x = excl_group, y = crop, fill = stable_col)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(aes(label = cell_lbl), size = 2.4, color = "grey20") +
    scale_fill_manual(values = stab_cols, name = "TMX Change vs Full") +
    labs(
      title    = "G_OL5: Major Producer Exclusion -- TMX Coefficient (M2 PW)",
      subtitle = "Each cell = TMX coef when that country/group excluded\nGreen = stable (<10% change from full sample)",
      caption  = "Burke et al. (2015, Nature) | Top 4 producers + all four together",
      x = "Excluded Country/Group", y = NULL) +
    theme_bw(base_size = 10) +
    theme(panel.grid      = element_blank(),
          legend.position = "bottom",
          plot.title      = element_text(face = "bold", size = 12),
          plot.subtitle   = element_text(size = 8, color = "grey40"),
          plot.caption    = element_text(size = 7.5, color = "grey50"),
          plot.margin     = margin(8, 12, 8, 8))
  save_plot(g_ol5, "G_OL5_major_producer", width = 10, height = 7)
  cat("[G_OL5] DONE\n")
}


# ------------------------------------------------------------------------------
# G_OL6 -- REGIONAL EXCLUSION HEATMAP
# ------------------------------------------------------------------------------

cat("\n[G_OL6] Regional exclusion heatmap...\n")

if (nrow(region_excl_df) > 0) {
  reg_plot <- region_excl_df %>%
    dplyr::mutate(
      crop        = factor(crop, levels = urun_listesi),
      excl_region = factor(excl_region,
                           levels = c("Asia","Americas","Europe","Africa")),
      stable_col  = dplyr::case_when(
        is.na(pct_change)       ~ "N/A",
        abs(pct_change) < 5    ~ "< 5%",
        abs(pct_change) < 10   ~ "5-10%",
        abs(pct_change) < 20   ~ "10-20%",
        TRUE                   ~ "> 20%"),
      stable_col  = factor(stable_col,
                           levels = c("< 5%","5-10%","10-20%","> 20%","N/A")),
      cell_lbl    = sprintf("%+.3f\n(%+.0f%%)", b_excl, pct_change))
  
  stab_cols2 <- c("< 5%"  = "#27AE60", "5-10%" = "#82E0AA",
                  "10-20%"= "#F39C12", "> 20%" = "#C0392B",
                  "N/A"   = "grey90")
  
  g_ol6 <- ggplot(reg_plot,
                  aes(x = excl_region, y = crop, fill = stable_col)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(aes(label = cell_lbl),
              size = 2.2, color = "grey20", lineheight = 0.9) +
    scale_fill_manual(values = stab_cols2, name = "TMX Change vs Full") +
    labs(
      title    = "G_OL6: Regional Exclusion -- TMX Coefficient Stability (M2 PW)",
      subtitle = "Each cell = TMX coef + % change when entire region excluded\nGreen = stable regardless of region included",
      caption  = "Region definitions: Asia / Americas / Europe / Africa",
      x = "Excluded Region", y = NULL) +
    theme_bw(base_size = 10) +
    theme(panel.grid      = element_blank(),
          legend.position = "bottom",
          plot.title      = element_text(face = "bold", size = 12),
          plot.subtitle   = element_text(size = 8, color = "grey40"),
          plot.caption    = element_text(size = 7.5, color = "grey50"),
          plot.margin     = margin(8, 12, 8, 8))
  save_plot(g_ol6, "G_OL6_regional_exclusion", width = 10, height = 7)
  cat("[G_OL6] DONE\n")
}


# ------------------------------------------------------------------------------
# G_OL7 -- RICE ASIA-WITHIN ROBUSTNESS
# ------------------------------------------------------------------------------

cat("\n[G_OL7] Rice Asia-within robustness...\n")

if (nrow(rice_asia_df) > 0 && !is.na(b_rice_full)) {
  
  stab_cols_rice <- c("Reference"         = "#2C3E50",
                      "Stable (<10%)"     = "#27AE60",
                      "Moderate (10-30%)" = "#E67E22",
                      "Large (>30%)"      = "#C0392B")
  
  g_ol7 <- ggplot(rice_asia_df,
                  aes(x = b,
                      y = forcats::fct_rev(sample),
                      color = stability, shape = stability)) +
    geom_vline(xintercept = 0,
               linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = b_rice_full,
               linetype = "dotted", color = "#2C3E50", linewidth = 0.8) +
    geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                   height = 0.25, linewidth = 0.8) +
    geom_point(size = 3.5) +
    geom_text(aes(label = sig, x = hi95 + 0.002),
              size = 3.5, hjust = 0, show.legend = FALSE) +
    geom_text(
      data = rice_asia_df %>% dplyr::filter(!is.na(pct_change)),
      aes(x = lo95 - 0.003, y = forcats::fct_rev(sample),
          label = sprintf("%+.0f%%", pct_change)),
      hjust = 1, size = 3.0, color = "grey40", inherit.aes = FALSE) +
    scale_color_manual(values = stab_cols_rice, name = "Stability") +
    scale_shape_manual(
      values = c("Reference" = 16, "Stable (<10%)" = 16,
                 "Moderate (10-30%)" = 17, "Large (>30%)" = 15),
      name = "Stability") +
    labs(
      title    = "G_OL7: Rice TMX Coefficient -- Asia-Within Robustness",
      subtitle = paste0("M2 Production-Weighted | 95% CI | Clustered SE by country\n",
                        "Dotted = full-sample estimate | %% label = change from full sample"),
      caption  = paste0("ASEAN: IDN THA VNM MYS PHL KHM LAO MMR | ",
                        "Burke et al. (2015, Nature) | Zhao et al. (2017, PNAS)"),
      x = "TMX Coefficient [delta ln(Yield) per 1 degC]",
      y = NULL) +
    theme_rob
  save_plot(g_ol7, "G_OL7_rice_asia", width = 10, height = 7)
  cat("[G_OL7] DONE\n")
} else {
  cat("[G_OL7] SKIPPED -- no rice data\n")
}


# ------------------------------------------------------------------------------
# G_SOVIET -- POST-SOVIET EXCLUSION (wheat/barley/oats/rye)
# ------------------------------------------------------------------------------

cat("\n[G_SOVIET] Post-Soviet exclusion forest plot...\n")

if (nrow(soviet_df) > 0) {
  
  sample_cols <- c("full"      = "#2C3E50", "excl_rus"  = "#E67E22",
                   "excl_big3" = "#E74C3C", "excl_all"  = "#8E44AD")
  sample_shapes <- c("full" = 16, "excl_rus" = 17,
                     "excl_big3" = 15, "excl_all" = 18)
  sample_labels <- c("full"      = "Full sample",
                     "excl_rus"  = "Excl. Russia",
                     "excl_big3" = "Excl. RUS+UKR+KAZ",
                     "excl_all"  = "Excl. all post-Soviet")
  
  soviet_plot <- soviet_df %>%
    dplyr::mutate(crop = factor(crop, levels = rev(hedef_urunler)))
  
  g_soviet <- ggplot(soviet_plot,
                     aes(x = b, y = crop,
                         color = sample_type, shape = sample_type)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                   height   = 0.22, linewidth = 0.75,
                   position = position_dodge(width = 0.65)) +
    geom_point(size     = 3.2,
               position = position_dodge(width = 0.65)) +
    geom_text(aes(label = sig, x = hi95 + 0.002),
              size        = 3.5, hjust = 0,
              position    = position_dodge(width = 0.65),
              show.legend = FALSE) +
    scale_color_manual(values = sample_cols,
                       labels = sample_labels, name = "Sample") +
    scale_shape_manual(values = sample_shapes,
                       labels = sample_labels, name = "Sample") +
    labs(
      title    = "G_SOVIET: Post-Soviet Country Exclusion -- TMX Coefficient (M2 PW)",
      subtitle = paste0("Wheat/barley/oats/rye | 95% CI | Clustered SE by country\n",
                        "Post-Soviet: 15 countries independent after 1991"),
      caption  = paste0("Post-Soviet: ",
                        paste(sort(post_soviet), collapse = ", ")),
      x = "TMX Coefficient [delta ln(Yield) per 1 degC]",
      y = NULL) +
    theme_rob +
    theme(axis.text.y = element_text(size = 12, face = "bold"))
  save_plot(g_soviet, "G_SOVIET_post_soviet", width = 11, height = 7)
  cat("[G_SOVIET] DONE\n")
}


# ==============================================================================
# SAVE RESULTS TO EXCEL
# ==============================================================================

cat("\nSaving adim7_outlier.xlsx...\n")

sheets_out <- list(
  LOO_Country        = loo_df %>%
    dplyr::arrange(crop, dplyr::desc(std_delta)) %>%
    dplyr::select(crop, excl_country, region, is_major,
                  b_full, b_loo, delta_b, std_delta,
                  pct_change, sig_loo, N_loo),
  LOO_Across_Models  = loo_models_df,
  CooksTop5          = cooks_df,
  MajorProducerExcl  = major_excl_df,
  RegionalExcl       = region_excl_df,
  Winsorizing        = wins_df,
  PostSoviet         = soviet_df,
  RiceAsia           = if (nrow(rice_asia_df) > 0) rice_asia_df else data.frame())

if (nrow(sorg_sb_df) > 0)
  sheets_out[["Sorghum_PostBreak_LOO"]] <- sorg_sb_df

writexl::write_xlsx(sheets_out,
                    file.path(ana_yol, "adim7_outlier.xlsx"))
cat("[SAVED] adim7_outlier.xlsx\n")


# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n", strrep("=", 65), "\n")
cat("STEP 07 COMPLETE\n")
cat(strrep("=", 65), "\n\n")

cat("COMPLETED:\n")
cat("  Step 1: LOO country analysis (M2 PW, all crops)\n")
cat("  Step 2: LOO across M1/M2/M4 specifications\n")
cat("  Step 3: Major producer exclusion (CHN/USA/IND/BRA)\n")
cat("  Step 4: Regional exclusion (Asia/Americas/Europe/Africa)\n")
cat("  Step 5: Sorghum post-1990 LOO stability\n")
cat("  Step 6: Winsorizing stability\n")
cat("  Step 7: Post-Soviet exclusion (wheat/barley/oats/rye)\n")
cat("  Step 8: Rice Asia-within robustness\n\n")

cat("PLOTS (each printed labeled + unlabeled on screen):\n")
cat("  G_OL1   : LOO bubble plot (top 8 per crop)\n")
cat("  G_OL2   : Sorghum post-1990 LOO stability\n")
cat("  G_OL3   : Winsorizing stability\n")
cat("  G_OL4   : LOO across M1/M2/M4 specifications\n")
cat("  G_OL5   : Major producer exclusion heatmap\n")
cat("  G_OL6   : Regional exclusion heatmap\n")
cat("  G_OL7   : Rice Asia-within robustness\n")
cat("  G_SOVIET: Post-Soviet exclusion (wheat/barley/oats/rye)\n\n")

cat("FILES SAVED (6 per plot = 48 total):\n")
cat("  figures/labeled/   -- PDF + 300dpi PNG + 600dpi PNG\n")
cat("  figures/unlabeled/ -- PDF + 300dpi PNG + 600dpi PNG\n\n")

cat("OUTPUT: adim7_outlier.xlsx\n")
cat("  Sheets: LOO_Country, LOO_Across_Models, CooksTop5,\n")
cat("          MajorProducerExcl, RegionalExcl, Winsorizing,\n")
cat("          PostSoviet, RiceAsia, Sorghum_PostBreak_LOO\n\n")

if (nrow(loo_df) > 0) {
  n_inf <- sum(loo_df$std_delta > 2, na.rm = TRUE)
  cat(sprintf("LOO: %d influential countries (std_delta>2) / %d total\n",
              n_inf, nrow(loo_df)))
}
if (nrow(region_excl_df) > 0) {
  n_stable <- sum(region_excl_df$stable, na.rm = TRUE)
  cat(sprintf("Regional: %d / %d combinations stable (pct<10%%)\n",
              n_stable, nrow(region_excl_df)))
}

cat("\nNEXT STEP: paper1_step08_projections.R\n")
cat(strrep("=", 65), "\n")















# ==============================================================================
# PAPER 1 -- STEP 08: DRISCOLL-KRAAY STANDARD ERRORS
# Temperature, VPD, and Global Crop Yields: Evidence from a Multi-Crop Panel
#
# Reads: modeller_v4_objects.rds (from step03)
#
# Method:
#   fixest::feols(..., weights=~production_tonnes,
#                panel.id=~Country_ISO+Yil, vcov="DK")
#   Both clustered and DK estimates are production-weighted.
#   No plm::vcovSCC() -- that runs unweighted, biasing the comparison.
#
# Contents:
#   Step 1 : M2 PW -- Clustered SE vs DK SE, all crops
#   Step 2 : M1-M8 -- Full DK SE comparison, all crops
#   Plots  : G_DK1 to G_DK5
#            -- R screen: labeled then unlabeled
#            -- Saved: PDF + PNG 300dpi + PNG 600dpi x labeled + unlabeled
#
# UTF-8 NOTE: All special characters replaced with ASCII
#   degree sign  -> degC
#   em dash      -> --
#
# References:
#   Driscoll & Kraay (1998, Rev.Econ.Stat.)
#   Hoechle (2007, Stata J.)
#   Pesaran (2004) -- CD test motivation
# ==============================================================================


# ==============================================================================
# 0. SETUP
# ==============================================================================

library(dplyr)
library(ggplot2)
library(writexl)
library(fixest)

ana_yol      <- "C:/Users/Alper.Tosun/Desktop/cru130"
urun_listesi <- c("wheat","barley","oats","rye","rice","maize","soybean","sorghum")

# Output directories
fig_labeled   <- file.path(ana_yol, "figures", "labeled")
fig_unlabeled <- file.path(ana_yol, "figures", "unlabeled")
dir.create(fig_labeled,   recursive = TRUE, showWarnings = FALSE)
dir.create(fig_unlabeled, recursive = TRUE, showWarnings = FALSE)

# Model colors
renk8m <- c(M1 = "#95A5A6", M2 = "#E74C3C", M3 = "#3498DB",
            M4 = "#8E44AD", M5 = "#27AE60", M6 = "#F39C12",
            M7 = "#1A5276", M8 = "#E91E63")


# ==============================================================================
# SAVE_PLOT FUNCTION
# ==============================================================================

save_plot <- function(g_labeled, plot_name, width = 10, height = 7) {
  
  g_unlabeled <- g_labeled +
    labs(title    = NULL,
         subtitle = NULL,
         caption  = NULL,
         x        = NULL,
         y        = NULL) +
    theme(legend.title = element_blank(),
          plot.margin  = margin(4, 4, 4, 4))
  
  cat(sprintf("  [SCREEN] %s -- LABELED\n",   plot_name)); print(g_labeled)
  cat(sprintf("  [SCREEN] %s -- UNLABELED\n", plot_name)); print(g_unlabeled)
  
  save_one <- function(g, dir_path) {
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, ".pdf")),
                    plot = g, width = width, height = height, device = "pdf")
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, "_300dpi.png")),
                    plot = g, width = width, height = height,
                    dpi = 300, device = "png")
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, "_600dpi.png")),
                    plot = g, width = width, height = height,
                    dpi = 600, device = "png")
  }
  save_one(g_labeled,   fig_labeled)
  save_one(g_unlabeled, fig_unlabeled)
  
  cat(sprintf("  [SAVED]  %s -> labeled/ + unlabeled/ (PDF + 300 + 600 dpi)\n\n",
              plot_name))
  invisible(NULL)
}


# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

sig_s <- function(p)
  ifelse(is.na(p), "",
         ifelse(p < 0.01, "***",
                ifelse(p < 0.05, "**",
                       ifelse(p < 0.10, "*", ""))))

# Fit feols with DK SE -- production-weighted, panel.id required
# panel.id tells fixest which variable is the time index for bandwidth selection
fit_dk_fixest <- function(df, fm, wt_var = "production_tonnes") {
  w <- df[[wt_var]]
  w[is.na(w) | w <= 0] <- NA
  tryCatch(
    fixest::feols(fm,
                  data     = df,
                  weights  = w,
                  panel.id = ~Country_ISO + Yil,
                  vcov     = "DK"),
    error = function(e) {
      cat(sprintf("    DK feols error: %s\n", conditionMessage(e)))
      NULL })
}

# Extract TMX coefficient from any feols model (clustered or DK)
get_tmx_feols <- function(m) {
  if (is.null(m)) return(NULL)
  cf <- tryCatch(coef(m), error = function(e) NULL)
  vc <- tryCatch(vcov(m), error = function(e) NULL)
  if (is.null(cf) || is.null(vc) || !"TMX" %in% names(cf)) return(NULL)
  b  <- as.numeric(cf["TMX"])
  se <- as.numeric(sqrt(vc["TMX","TMX"]))
  pv <- 2 * pnorm(-abs(b / se))
  list(coef = b, se = se, pval = pv,
       lo95 = b - 1.96 * se,
       hi95 = b + 1.96 * se)
}

# Model formulas -- same as step03, explicit for DK re-estimation
formul_fe <- list(
  M1 = ln_yield ~ TMX + PRE + PRE_sq                             | Country_ISO + Yil,
  M2 = ln_yield ~ TMX + PRE + PRE_sq + IRR                       | Country_ISO + Yil,
  M3 = ln_yield ~ TMX + PRE + PRE_sq + ln_gdp                    | Country_ISO + Yil,
  M4 = ln_yield ~ TMX + PRE + PRE_sq + IRR + ln_gdp              | Country_ISO + Yil,
  M5 = ln_yield ~ TMX * IRR + PRE + PRE_sq                       | Country_ISO + Yil,
  M6 = ln_yield ~ TMX * is_arid + PRE + PRE_sq + IRR             | Country_ISO + Yil,
  M7 = ln_yield ~ TMX + PRE + PRE_sq + IRR + ln_gdp + TMX_lngdp  | Country_ISO + Yil,
  M8 = ln_yield ~ TMX * PRE + PRE_sq + IRR                       | Country_ISO + Yil)


# ==============================================================================
# LOAD RDS
# ==============================================================================

rds_v4 <- file.path(ana_yol, "modeller_v4_objects.rds")
rds_v3 <- file.path(ana_yol, "modeller_v3_objects.rds")

if (file.exists(rds_v4)) {
  tum_modeller <- readRDS(rds_v4)
  cat("[LOADED] modeller_v4_objects.rds\n\n")
} else if (file.exists(rds_v3)) {
  tum_modeller <- readRDS(rds_v3)
  cat("[LOADED] modeller_v3_objects.rds (fallback)\n\n")
} else {
  stop("[ERROR] No RDS file found. Run paper1_step03_models.R first.")
}


# ==============================================================================
# STEP 1 -- M2 PW: CLUSTERED SE vs DK SE (all crops)
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("STEP 1 -- M2 PW: Clustered SE vs DK SE (all crops)\n")
cat(strrep("=", 65), "\n\n")

m2_list <- list()

for (urun in urun_listesi) {
  if (!urun %in% names(tum_modeller)) next
  df <- tum_modeller[[urun]]$df
  if (is.null(df)) next
  
  # Ensure interaction column exists
  if (!"TMX_lngdp" %in% names(df))
    df$TMX_lngdp <- df$TMX * df$ln_gdp
  
  # Clustered SE: already estimated in step03
  m_cl <- tum_modeller[[urun]]$all_mods[["M2_PW"]]
  r_cl <- get_tmx_feols(m_cl)
  if (is.null(r_cl)) { cat(sprintf("[%s] Clustered failed\n", urun)); next }
  
  # DK SE: re-estimate with panel.id + vcov="DK"
  cat(sprintf("[%s] Computing DK SE...\n", urun))
  m_dk <- fit_dk_fixest(df, formul_fe[["M2"]])
  r_dk <- get_tmx_feols(m_dk)
  if (is.null(r_dk)) { cat(sprintf("[%s] DK failed\n", urun)); next }
  
  se_ratio <- r_dk$se / r_cl$se
  
  m2_list[[urun]] <- data.frame(
    crop                 = urun,
    N                    = tryCatch(as.integer(stats::nobs(m_cl)),
                                    error = function(e) NA_integer_),
    b_cl                 = r_cl$coef,
    se_cl                = r_cl$se,
    pval_cl              = r_cl$pval,
    sig_cl               = sig_s(r_cl$pval),
    lo95_cl              = r_cl$lo95,
    hi95_cl              = r_cl$hi95,
    b_dk                 = r_dk$coef,
    se_dk                = r_dk$se,
    pval_dk              = r_dk$pval,
    sig_dk               = sig_s(r_dk$pval),
    lo95_dk              = r_dk$lo95,
    hi95_dk              = r_dk$hi95,
    se_ratio             = se_ratio,
    sig_changed          = sig_s(r_cl$pval) != sig_s(r_dk$pval),
    dk_more_conservative = se_ratio > 1,
    stringsAsFactors = FALSE)
  
  cat(sprintf(
    "  Clustered: %+.4f (SE=%.4f)%s | DK: %+.4f (SE=%.4f)%s | ratio=%.3f%s\n",
    r_cl$coef, r_cl$se, sig_s(r_cl$pval),
    r_dk$coef, r_dk$se, sig_s(r_dk$pval),
    se_ratio,
    ifelse(se_ratio > 1.1, " <- DK WIDER",
           ifelse(se_ratio < 0.9, " <- DK NARROWER", ""))))
}

m2_dk_df <- dplyr::bind_rows(m2_list)

cat(sprintf("\n--- M2 Summary ---\n"))
cat(sprintf("  Mean SE ratio (DK/Clustered) : %.3f\n",
            mean(m2_dk_df$se_ratio, na.rm = TRUE)))
cat(sprintf("  Significance changed         : %d / %d\n",
            sum(m2_dk_df$sig_changed, na.rm = TRUE), nrow(m2_dk_df)))
cat(sprintf("  DK wider (ratio > 1)         : %d / %d\n\n",
            sum(m2_dk_df$dk_more_conservative, na.rm = TRUE), nrow(m2_dk_df)))


# ==============================================================================
# STEP 2 -- M1-M8: FULL DK SE COMPARISON (all crops, both PW)
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("STEP 2 -- M1-M8: Full DK SE comparison (all crops)\n")
cat(strrep("=", 65), "\n\n")

all_list <- list()

for (urun in urun_listesi) {
  if (!urun %in% names(tum_modeller)) next
  df_full <- tum_modeller[[urun]]$df
  df_gdp  <- tum_modeller[[urun]]$df_gdp
  if (is.null(df_full)) next
  
  # Ensure derived columns present
  if (!"TMX_lngdp" %in% names(df_full))
    df_full$TMX_lngdp <- df_full$TMX * df_full$ln_gdp
  
  # is_arid: 400mm threshold (UNESCO/IPCC), uses PRE_ann -- same as step03
  if (!"is_arid" %in% names(df_full)) {
    df_full$is_arid <- if ("PRE_ann" %in% names(df_full))
      as.numeric(df_full$PRE_ann < 400) else NA_real_
  }
  
  if (!is.null(df_gdp)) {
    if (!"TMX_lngdp" %in% names(df_gdp))
      df_gdp$TMX_lngdp <- df_gdp$TMX * df_gdp$ln_gdp
    if (!"is_arid" %in% names(df_gdp)) {
      df_gdp$is_arid <- if ("PRE_ann" %in% names(df_gdp))
        as.numeric(df_gdp$PRE_ann < 400) else NA_real_
    }
  }
  
  for (ms in paste0("M", 1:8)) {
    # M3/M4/M7 use GDP subsample
    dat <- if (ms %in% c("M3","M4","M7") && !is.null(df_gdp)) df_gdp else df_full
    
    # Skip M6 if is_arid not available
    if (ms == "M6" && (!"is_arid" %in% names(dat) || all(is.na(dat$is_arid)))) {
      cat(sprintf("  [%s %s] is_arid not available -- skip\n", urun, ms)); next
    }
    
    m_cl <- tum_modeller[[urun]]$all_mods[[paste0(ms, "_PW")]]
    r_cl <- get_tmx_feols(m_cl)
    if (is.null(r_cl)) next
    
    fm <- formul_fe[[ms]]
    if (is.null(fm)) next
    
    m_dk <- fit_dk_fixest(dat, fm)
    r_dk <- get_tmx_feols(m_dk)
    if (is.null(r_dk)) next
    
    se_ratio <- r_dk$se / r_cl$se
    
    all_list[[paste0(urun, "_", ms)]] <- data.frame(
      crop        = urun,
      model       = ms,
      b_cl        = r_cl$coef,
      se_cl       = r_cl$se,
      pval_cl     = r_cl$pval,
      sig_cl      = sig_s(r_cl$pval),
      b_dk        = r_dk$coef,
      se_dk       = r_dk$se,
      pval_dk     = r_dk$pval,
      sig_dk      = sig_s(r_dk$pval),
      se_ratio    = se_ratio,
      sig_changed = sig_s(r_cl$pval) != sig_s(r_dk$pval),
      lo95_cl     = r_cl$lo95,
      hi95_cl     = r_cl$hi95,
      lo95_dk     = r_dk$lo95,
      hi95_dk     = r_dk$hi95,
      stringsAsFactors = FALSE)
  }
  cat(sprintf("[%s] M1-M8 DK SE done\n", urun))
}

all_dk_df <- dplyr::bind_rows(all_list)
cat(sprintf("\nTotal DK comparison rows: %d\n\n", nrow(all_dk_df)))

# Console SE ratio table
cat("--- SE Ratio (DK/Clustered) | ! = significance changed ---\n")
cat(sprintf("%-10s  %7s  %7s  %7s  %7s  %7s  %7s  %7s  %7s\n",
            "Crop","M1","M2","M3","M4","M5","M6","M7","M8"))
cat(strrep("-", 74), "\n")

for (urun in urun_listesi) {
  vals <- sapply(paste0("M", 1:8), function(ms) {
    r <- all_dk_df[all_dk_df$crop == urun & all_dk_df$model == ms, ]
    if (nrow(r) == 0) return("  --  ")
    sprintf("%.3f%s", r$se_ratio, ifelse(isTRUE(r$sig_changed), "!", " "))
  })
  cat(sprintf("%-10s  %7s  %7s  %7s  %7s  %7s  %7s  %7s  %7s\n",
              urun, vals[1], vals[2], vals[3], vals[4],
              vals[5], vals[6], vals[7], vals[8]))
}
cat("\n")


# ==============================================================================
# PLOTS -- G_DK1 to G_DK5
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("GENERATING PLOTS (G_DK1 to G_DK5)\n")
cat("Each plot: screen print x2 (labeled + unlabeled), saved x6\n")
cat(strrep("=", 65), "\n")

# Shared theme
theme_dk <- theme_bw(base_size = 11) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "bold", size = 12),
        plot.subtitle    = element_text(size = 8.5, color = "grey40"),
        plot.caption     = element_text(size = 7.5, color = "grey50"),
        plot.margin      = margin(8, 12, 8, 8))


# ------------------------------------------------------------------------------
# G_DK1 -- CLUSTERED vs DK CI (M2 PW)
# Point estimates are identical; only the CI width differs
# ------------------------------------------------------------------------------

cat("\n[G_DK1] Clustered vs DK SE (M2)...\n")

if (nrow(m2_dk_df) > 0) {
  
  dk_long <- dplyr::bind_rows(
    m2_dk_df %>% dplyr::transmute(
      crop    = factor(crop, levels = rev(urun_listesi)),
      se_type = "Clustered SE",
      coef    = b_cl, lo95 = lo95_cl, hi95 = hi95_cl, sig = sig_cl),
    m2_dk_df %>% dplyr::transmute(
      crop    = factor(crop, levels = rev(urun_listesi)),
      se_type = "Driscoll-Kraay SE",
      coef    = b_dk, lo95 = lo95_dk, hi95 = hi95_dk, sig = sig_dk)) %>%
    dplyr::mutate(se_type = factor(se_type,
                                   levels = c("Clustered SE",
                                              "Driscoll-Kraay SE")))
  
  lbl_dk <- dk_long %>%
    dplyr::filter(se_type == "Driscoll-Kraay SE") %>%
    dplyr::mutate(x_lbl = lo95 - 0.002)
  
  g_dk1 <- ggplot(dk_long,
                  aes(x = coef, y = crop,
                      color = se_type, shape = se_type)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                   height   = 0.2, linewidth = 0.8,
                   position = position_dodge(width = 0.55)) +
    geom_point(size = 3, position = position_dodge(width = 0.55)) +
    geom_text(data        = lbl_dk,
              aes(x = x_lbl, y = crop, label = sig),
              size        = 2.8, hjust = 1, color = "#C0392B",
              inherit.aes = FALSE) +
    scale_color_manual(
      values = c("Clustered SE"      = "#1A5276",
                 "Driscoll-Kraay SE" = "#C0392B"),
      name = "SE Type") +
    scale_shape_manual(
      values = c("Clustered SE"      = 16,
                 "Driscoll-Kraay SE" = 17),
      name = "SE Type") +
    labs(
      title    = "G_DK1: TMX Coefficient -- Clustered SE vs Driscoll-Kraay SE (M2 PW)",
      subtitle = paste0("Both production-weighted | panel.id = Country_ISO + Yil | ",
                        "95% CI\nPoint estimates identical -- only CI width differs"),
      caption  = paste0("Driscoll & Kraay (1998, Rev.Econ.Stat.) | ",
                        "fixest::feols() vcov='DK'"),
      x = "TMX Coefficient [delta ln(Yield) per 1 degC]",
      y = NULL) +
    theme_dk
  save_plot(g_dk1, "G_DK1_clustered_vs_DK_M2", width = 10, height = 7)
  cat("[G_DK1] DONE\n")
} else {
  cat("[G_DK1] SKIPPED\n")
}


# ------------------------------------------------------------------------------
# G_DK2 -- SE RATIO DOT PLOT: M1-M4, ALL CROPS
# Ratio > 1 means DK is more conservative (wider CI)
# ------------------------------------------------------------------------------

cat("\n[G_DK2] SE ratio dot plot M1-M4...\n")

se_ratio_m14 <- all_dk_df %>%
  dplyr::filter(model %in% c("M1","M2","M3","M4")) %>%
  dplyr::mutate(
    crop  = factor(crop,  levels = urun_listesi),
    model = factor(model, levels = c("M1","M2","M3","M4")))

if (nrow(se_ratio_m14) > 0) {
  
  lbl_ratio <- se_ratio_m14 %>%
    dplyr::filter(se_ratio > 1.15 | se_ratio < 0.85)
  
  g_dk2 <- ggplot(se_ratio_m14,
                  aes(x = se_ratio, y = crop,
                      color = model, shape = model)) +
    annotate("rect", xmin = 0.9, xmax = 1.1,
             ymin = -Inf, ymax = Inf,
             fill = "grey80", alpha = 0.30) +
    geom_vline(xintercept = 1.0, linetype = "dashed",
               color = "grey30", linewidth = 0.9) +
    geom_vline(xintercept = c(0.9, 1.1), linetype = "dotted",
               color = "grey60", linewidth = 0.5) +
    geom_point(size = 3, position = position_dodge(width = 0.55)) +
    geom_text(data = lbl_ratio,
              aes(label = sprintf("%.2f", se_ratio)),
              size = 2.9, vjust = -0.9,
              position = position_dodge(width = 0.55)) +
    annotate("text", x = 1.0, y = 0.6,
             label = "within 10%",
             hjust = 0.5, size = 2.3, color = "grey50") +
    scale_color_manual(
      values = c("M1" = "#1A5276", "M2" = "#2E86C1",
                 "M3" = "#E67E22", "M4" = "#C0392B"),
      name = "Model") +
    scale_shape_manual(
      values = c("M1" = 16, "M2" = 17, "M3" = 15, "M4" = 18),
      name = "Model") +
    labs(
      title    = "G_DK2: SE Ratio (DK / Clustered) -- M1-M4, All Crops (both PW)",
      subtitle = "Ratio > 1: DK more conservative | Shaded = within 10% of clustered",
      caption  = paste0("Driscoll & Kraay (1998) | fixest vcov='DK' | ",
                        "panel.id = Country_ISO + Yil"),
      x = "SE Ratio (DK SE / Clustered SE)", y = NULL) +
    theme_dk
  save_plot(g_dk2, "G_DK2_SE_ratio_M1M4", width = 10, height = 7)
  cat("[G_DK2] DONE\n")
} else {
  cat("[G_DK2] SKIPPED\n")
}


# ------------------------------------------------------------------------------
# G_DK3 -- SE RATIO HEATMAP: ALL MODELS M1-M8, ALL CROPS
# Full picture of where DK diverges from clustered
# ------------------------------------------------------------------------------

cat("\n[G_DK3] SE ratio heatmap M1-M8...\n")

if (nrow(all_dk_df) > 0) {
  
  hm_dk <- all_dk_df %>%
    dplyr::mutate(
      crop      = factor(crop,  levels = rev(urun_listesi)),
      model     = factor(model, levels = paste0("M", 1:8)),
      ratio_cat = dplyr::case_when(
        se_ratio > 1.20 ~ "> 1.20 (DK much wider)",
        se_ratio > 1.10 ~ "1.10-1.20 (DK wider)",
        se_ratio > 0.90 ~ "0.90-1.10 (similar)",
        se_ratio > 0.80 ~ "0.80-0.90 (DK narrower)",
        TRUE            ~ "< 0.80 (DK much narrower)"),
      ratio_cat = factor(ratio_cat,
                         levels = c("> 1.20 (DK much wider)",
                                    "1.10-1.20 (DK wider)",
                                    "0.90-1.10 (similar)",
                                    "0.80-0.90 (DK narrower)",
                                    "< 0.80 (DK much narrower)")),
      cell_lbl  = sprintf("%.2f%s", se_ratio,
                          ifelse(isTRUE(sig_changed), "!", "")))
  
  ratio_cols <- c("> 1.20 (DK much wider)"    = "#C0392B",
                  "1.10-1.20 (DK wider)"       = "#E67E22",
                  "0.90-1.10 (similar)"        = "#BDC3C7",
                  "0.80-0.90 (DK narrower)"    = "#82E0AA",
                  "< 0.80 (DK much narrower)"  = "#27AE60")
  
  g_dk3 <- ggplot(hm_dk,
                  aes(x = model, y = crop, fill = ratio_cat)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = cell_lbl), size = 2.3, color = "grey20") +
    scale_fill_manual(values = ratio_cols, name = "SE Ratio Category") +
    labs(
      title    = "G_DK3: SE Ratio (DK/Clustered) Heatmap -- All Models M1-M8 x All Crops",
      subtitle = paste0("Green = DK narrower | Red = DK wider | ",
                        "! = significance changed\n",
                        "Both estimates production-weighted"),
      caption  = "Driscoll & Kraay (1998) | fixest vcov='DK'",
      x = "Model", y = NULL) +
    theme_bw(base_size = 10) +
    theme(panel.grid      = element_blank(),
          legend.position = "bottom",
          plot.title      = element_text(face = "bold", size = 12),
          plot.subtitle   = element_text(size = 8, color = "grey40"),
          plot.caption    = element_text(size = 7.5, color = "grey50"),
          plot.margin     = margin(8, 12, 8, 8))
  save_plot(g_dk3, "G_DK3_SE_ratio_heatmap", width = 12, height = 7)
  cat("[G_DK3] DONE\n")
} else {
  cat("[G_DK3] SKIPPED\n")
}


# ------------------------------------------------------------------------------
# G_DK4 -- SIGNIFICANCE CHANGE MAP
# Which crop-model combinations change significance under DK SE?
# ------------------------------------------------------------------------------

cat("\n[G_DK4] Significance change map...\n")

if (nrow(all_dk_df) > 0) {
  
  sig_map <- all_dk_df %>%
    dplyr::mutate(
      crop  = factor(crop,  levels = rev(urun_listesi)),
      model = factor(model, levels = paste0("M", 1:8)),
      sig_status = dplyr::case_when(
        sig_cl %in% c("***","**","*") & sig_dk %in% c("***","**","*") ~
          "Significant in both",
        sig_cl %in% c("***","**","*") & sig_dk == "" ~
          "Lost under DK",
        sig_cl == "" & sig_dk %in% c("***","**","*") ~
          "Gained under DK",
        TRUE ~ "Not significant in either"),
      sig_status = factor(sig_status,
                          levels = c("Significant in both",
                                     "Lost under DK",
                                     "Gained under DK",
                                     "Not significant in either")),
      lbl_cl   = ifelse(sig_cl == "", "ns", sig_cl),
      lbl_dk   = ifelse(sig_dk == "", "ns", sig_dk),
      cell_lbl = paste0("Cl:", lbl_cl, " DK:", lbl_dk))
  
  sig_cols <- c("Significant in both"      = "#27AE60",
                "Lost under DK"            = "#C0392B",
                "Gained under DK"          = "#2471A3",
                "Not significant in either"= "#BDC3C7")
  
  g_dk4 <- ggplot(sig_map,
                  aes(x = model, y = crop, fill = sig_status)) +
    geom_tile(color = "white", linewidth = 0.7) +
    geom_text(aes(label = cell_lbl),
              size = 2.8, color = "grey10") +
    scale_fill_manual(values = sig_cols, name = "") +
    labs(
      title    = "G_DK4: Significance Change -- Clustered vs DK SE (All Models x All Crops)",
      subtitle = paste0("Green = significant under both | Red = lost under DK\n",
                        "Both production-weighted | Cl = Clustered | DK = Driscoll-Kraay"),
      caption  = "TMX coefficient | Production-Weighted two-way FE | ns = not significant",
      x = "Model", y = NULL) +
    theme_bw(base_size = 11) +
    theme(panel.grid      = element_blank(),
          legend.position = "bottom",
          legend.text     = element_text(size = 9),
          plot.title      = element_text(face = "bold", size = 12),
          plot.subtitle   = element_text(size = 8, color = "grey40"),
          plot.caption    = element_text(size = 7.5, color = "grey50"),
          axis.text.y     = element_text(size = 10),
          axis.text.x     = element_text(size = 10, face = "bold"),
          plot.margin     = margin(8, 12, 8, 8))
  save_plot(g_dk4, "G_DK4_significance_change", width = 12, height = 7)
  cat("[G_DK4] DONE\n")
} else {
  cat("[G_DK4] SKIPPED\n")
}


# ------------------------------------------------------------------------------
# G_DK5 -- DK vs CLUSTERED CI: ALL 8 MODELS, FACET
# Complete comparison across all specifications
# ------------------------------------------------------------------------------

cat("\n[G_DK5] DK vs Clustered all 8 models (facet)...\n")

if (nrow(all_dk_df) > 0) {
  
  models_in_data <- sort(unique(all_dk_df$model))
  cat(sprintf("  Models with DK data: %s\n",
              paste(models_in_data, collapse = ", ")))
  missing_models <- setdiff(paste0("M", 1:8), models_in_data)
  if (length(missing_models) > 0)
    cat(sprintf("  NOTE: Missing models: %s\n",
                paste(missing_models, collapse = ", ")))
  
  all_long <- dplyr::bind_rows(
    all_dk_df %>% dplyr::transmute(
      crop    = factor(crop,  levels = rev(urun_listesi)),
      model   = factor(model, levels = paste0("M", 1:8)),
      se_type = "Clustered SE",
      coef    = b_cl, lo95 = lo95_cl, hi95 = hi95_cl, sig = sig_cl),
    all_dk_df %>% dplyr::transmute(
      crop    = factor(crop,  levels = rev(urun_listesi)),
      model   = factor(model, levels = paste0("M", 1:8)),
      se_type = "Driscoll-Kraay SE",
      coef    = b_dk, lo95 = lo95_dk, hi95 = hi95_dk, sig = sig_dk)) %>%
    dplyr::mutate(se_type = factor(se_type,
                                   levels = c("Clustered SE",
                                              "Driscoll-Kraay SE"))) %>%
    dplyr::filter(!is.na(coef))
  
  # Label changed-significance cases
  lbl_changed <- all_dk_df %>%
    dplyr::filter(isTRUE(sig_changed)) %>%
    dplyr::mutate(
      crop  = factor(crop,  levels = rev(urun_listesi)),
      model = factor(model, levels = paste0("M", 1:8)),
      lbl   = paste0("DK:", ifelse(sig_dk == "", "ns", sig_dk)))
  
  g_dk5 <- ggplot(all_long,
                  aes(x = coef, y = crop,
                      color = se_type, shape = se_type)) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.5) +
    geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                   height   = 0.18, linewidth = 0.65,
                   position = position_dodge(width = 0.55)) +
    geom_point(size     = 2.2,
               position = position_dodge(width = 0.55)) +
    { if (nrow(lbl_changed) > 0)
      geom_text(data        = lbl_changed,
                aes(x = lo95_dk - 0.002, y = crop, label = lbl),
                color       = "#C0392B", size = 2.5, hjust = 1,
                inherit.aes = FALSE) } +
    facet_wrap(~model, nrow = 2) +
    scale_color_manual(
      values = c("Clustered SE"      = "#1A5276",
                 "Driscoll-Kraay SE" = "#C0392B"),
      name = "SE Type") +
    scale_shape_manual(
      values = c("Clustered SE"      = 16,
                 "Driscoll-Kraay SE" = 17),
      name = "SE Type") +
    labs(
      title    = "G_DK5: Clustered vs Driscoll-Kraay SE -- All 8 Models (Production-Weighted)",
      subtitle = paste0("Both estimates production-weighted | 95% CI\n",
                        "Red label = significance changed under DK SE"),
      caption  = paste0("Driscoll & Kraay (1998) | ",
                        "fixest::feols() vcov='DK' | ",
                        "panel.id = Country_ISO + Yil"),
      x = "TMX Coefficient [delta ln(Yield) per 1 degC]",
      y = NULL) +
    theme_bw(base_size = 10) +
    theme(panel.grid.minor = element_blank(),
          strip.text       = element_text(face = "bold", size = 10),
          strip.background = element_rect(fill = "grey95"),
          legend.position  = "bottom",
          plot.title       = element_text(face = "bold", size = 12),
          plot.subtitle    = element_text(size = 8, color = "grey40"),
          plot.caption     = element_text(size = 7.5, color = "grey50"))
  save_plot(g_dk5, "G_DK5_all_models_facet", width = 14, height = 9)
  cat("[G_DK5] DONE\n")
} else {
  cat("[G_DK5] SKIPPED\n")
}


# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

cat("\n", strrep("=", 75), "\n")
cat("DK SE SUMMARY (fixest, Production-Weighted)\n")
cat(strrep("=", 75), "\n\n")

if (nrow(all_dk_df) > 0) {
  cat(sprintf("Total comparisons            : %d\n", nrow(all_dk_df)))
  cat(sprintf("Significance changed (any)   : %d / %d\n",
              sum(all_dk_df$sig_changed, na.rm = TRUE), nrow(all_dk_df)))
  cat(sprintf("DK wider (ratio > 1)         : %d / %d\n",
              sum(all_dk_df$se_ratio > 1, na.rm = TRUE), nrow(all_dk_df)))
  cat(sprintf("DK within 10%% of clustered  : %d / %d\n\n",
              sum(abs(all_dk_df$se_ratio - 1) < 0.10, na.rm = TRUE),
              nrow(all_dk_df)))
  
  cat("--- Mean SE ratio by model ---\n")
  for (ms in paste0("M", 1:8)) {
    sub <- all_dk_df[all_dk_df$model == ms, ]
    if (nrow(sub) == 0) next
    cat(sprintf("  [%s] mean ratio=%.3f | sig changed: %d/%d\n",
                ms,
                mean(sub$se_ratio,   na.rm = TRUE),
                sum(sub$sig_changed, na.rm = TRUE),
                nrow(sub)))
  }
  
  cat("\n--- Decision rule ---\n")
  n_sig <- sum(all_dk_df$sig_changed, na.rm = TRUE)
  mr    <- mean(all_dk_df$se_ratio,   na.rm = TRUE)
  if (mr < 1.1 && n_sig == 0) {
    cat("  --> CLUSTERED SE SUFFICIENT\n")
  } else if (n_sig > 0) {
    cat(sprintf("  --> %d significance changes -- see G_DK4 for details\n",
                n_sig))
  } else {
    cat(sprintf("  --> Mean ratio = %.3f -- review G_DK3\n", mr))
  }
}


# ==============================================================================
# SAVE RESULTS TO EXCEL
# ==============================================================================

cat("\nSaving adim8_dk_se.xlsx...\n")

writexl::write_xlsx(
  list(
    DK_vs_Clustered_M2 = m2_dk_df,
    DK_AllModels       = all_dk_df,
    SE_Ratio_Summary   = all_dk_df %>%
      dplyr::select(crop, model, b_cl, se_cl, sig_cl,
                    b_dk, se_dk, sig_dk,
                    se_ratio, sig_changed) %>%
      dplyr::arrange(crop, model)),
  file.path(ana_yol, "adim8_dk_se.xlsx"))

cat("[SAVED] adim8_dk_se.xlsx\n")


# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n", strrep("=", 65), "\n")
cat("STEP 08 COMPLETE\n")
cat(strrep("=", 65), "\n\n")

cat("KEY FIX vs original:\n")
cat("  plm::vcovSCC() removed -- it runs unweighted, not comparable to PW.\n")
cat("  fixest vcov='DK' + panel.id used -- consistent with PW models.\n")
cat("  is_arid fallback corrected: PRE_ann < 400mm (not 200mm).\n\n")

cat("COMPLETED:\n")
cat("  Step 1: M2 PW -- Clustered vs DK SE, all crops\n")
cat("  Step 2: M1-M8 -- Full DK SE comparison, all crops\n\n")

cat("PLOTS (each printed labeled + unlabeled on screen):\n")
cat("  G_DK1 : Clustered vs DK CI (M2 PW)\n")
cat("  G_DK2 : SE ratio dot plot (M1-M4)\n")
cat("  G_DK3 : SE ratio heatmap (all models M1-M8)\n")
cat("  G_DK4 : Significance change map\n")
cat("  G_DK5 : DK vs Clustered CI -- all 8 models facet\n\n")

cat("FILES SAVED (6 per plot = 30 total):\n")
cat("  figures/labeled/   -- PDF + 300dpi PNG + 600dpi PNG\n")
cat("  figures/unlabeled/ -- PDF + 300dpi PNG + 600dpi PNG\n\n")

cat("OUTPUT: adim8_dk_se.xlsx\n")
cat("  Sheets: DK_vs_Clustered_M2, DK_AllModels, SE_Ratio_Summary\n\n")

cat("NEXT STEP: paper1_step09_projections.R\n")
cat(strrep("=", 65), "\n")


















# ==============================================================================
# PAPER 1 -- STEP 09: TEMPERATURE SCENARIOS + TMX NONLINEAR ANALYSIS
# Temperature, VPD, and Global Crop Yields: Evidence from a Multi-Crop Panel
#
# Reads: modeller_v4_objects.rds (from step03)
#
# Contents:
#   Part A -- Temperature Scenarios
#     Step 1 : Collect b_TMX for M1-M4 x PW/AW x all crops
#     Step 2 : Run MC scenarios (+1/+2/+3/+4 degC)
#     Plots  : G_S1 to G_S6
#
#   Part B -- TMX Squared Nonlinear Analysis
#     Step 3 : Fit TMX + TMX_sq model, compute optimal TMX
#     Plot   : G_TMX2
#
# Scenario formula:
#   delta_lnyield = b_TMX * dT
#   Percent change = (exp(delta_lnyield) - 1) * 100
#   MC: sim_TMX ~ N(b_TMX, se_TMX^2), CI = quantile(sim_pct, 0.025/0.975)
#
# Why only temperature scenarios (no PRE):
#   b_PRE is confounded by irrigation and agroclimatic heterogeneity
#   in global panels (Blanc & Schlenker 2017; Lobell et al. 2011).
#   b_TMX is consistent, significant, and literature-aligned.
#
# UTF-8 NOTE: All special characters replaced with ASCII
#   degree sign  -> degC
#   superscripts -> sq or plain
#   em dash      -> --
#
# References:
#   Lobell et al. (2011, Science)        -- benchmark scenarios
#   Zhao et al. (2017, PNAS)             -- meta-analysis benchmarks
#   Blanc & Schlenker (2017, REEP)       -- PRE identification
#   Schlenker & Roberts (2009, PNAS)     -- nonlinear temperature
# ==============================================================================


# ==============================================================================
# 0. SETUP
# ==============================================================================

library(dplyr)
library(ggplot2)
library(writexl)
library(tidyr)
library(fixest)
library(MASS)

ana_yol      <- "C:/Users/Alper.Tosun/Desktop/cru130"
urun_listesi <- c("wheat","barley","oats","rye","rice","maize","soybean","sorghum")

# Output directories
fig_labeled   <- file.path(ana_yol, "figures", "labeled")
fig_unlabeled <- file.path(ana_yol, "figures", "unlabeled")
dir.create(fig_labeled,   recursive = TRUE, showWarnings = FALSE)
dir.create(fig_unlabeled, recursive = TRUE, showWarnings = FALSE)

set.seed(2025)
N_MC         <- 10000L
delta_T_vals <- c(1, 2, 3, 4)

# Colors
pal_scen  <- c("T+1C" = "#2471A3", "T+2C" = "#E67E22",
               "T+3C" = "#C0392B", "T+4C" = "#6C3483")
pal_model <- c("M1"   = "#95A5A6", "M2"   = "#E74C3C",
               "M3"   = "#3498DB", "M4"   = "#8E44AD")
renk_crop <- c(wheat   = "#1A5276", barley  = "#2E86C1",
               oats    = "#27AE60", rye     = "#1E8449",
               rice    = "#F39C12", maize   = "#E67E22",
               soybean = "#8E44AD", sorghum = "#C0392B")

# Literature benchmarks (per +1 degC warming)
lit_bench <- data.frame(
  crop   = c("wheat","wheat","rice","maize","soybean"),
  source = c("Lobell 2011","Zhao 2017","Zhao 2017",
             "Lobell 2011","Zhao 2017"),
  pct_1C = c(-4.1, -6.0, -3.2, -7.4, -3.1),
  stringsAsFactors = FALSE)


# ==============================================================================
# SAVE_PLOT FUNCTION
# ==============================================================================

save_plot <- function(g_labeled, plot_name, width = 10, height = 7) {
  
  g_unlabeled <- g_labeled +
    labs(title    = NULL,
         subtitle = NULL,
         caption  = NULL,
         x        = NULL,
         y        = NULL) +
    theme(legend.title = element_blank(),
          plot.margin  = margin(4, 4, 4, 4))
  
  cat(sprintf("  [SCREEN] %s -- LABELED\n",   plot_name)); print(g_labeled)
  cat(sprintf("  [SCREEN] %s -- UNLABELED\n", plot_name)); print(g_unlabeled)
  
  save_one <- function(g, dir_path) {
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, ".pdf")),
                    plot = g, width = width, height = height, device = "pdf")
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, "_300dpi.png")),
                    plot = g, width = width, height = height,
                    dpi = 300, device = "png")
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, "_600dpi.png")),
                    plot = g, width = width, height = height,
                    dpi = 600, device = "png")
  }
  save_one(g_labeled,   fig_labeled)
  save_one(g_unlabeled, fig_unlabeled)
  
  cat(sprintf("  [SAVED]  %s -> labeled/ + unlabeled/ (PDF + 300 + 600 dpi)\n\n",
              plot_name))
  invisible(NULL)
}


# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

sig_s <- function(p)
  ifelse(is.na(p), "",
         ifelse(p < 0.001, "***",
                ifelse(p < 0.01,  "**",
                       ifelse(p < 0.05,  "*", ""))))

# Extract b_TMX + SE from any feols model
get_tmx_coef <- function(m) {
  if (is.null(m))
    return(list(b = NA_real_, se = NA_real_, pval = NA_real_))
  cf <- tryCatch(coef(m), error = function(e) NULL)
  vc <- tryCatch(vcov(m), error = function(e) NULL)
  if (is.null(cf) || is.null(vc) || !"TMX" %in% names(cf))
    return(list(b = NA_real_, se = NA_real_, pval = NA_real_))
  b  <- as.numeric(cf["TMX"])
  se <- as.numeric(sqrt(vc["TMX","TMX"]))
  pv <- 2 * pt(abs(b / se), df = Inf, lower.tail = FALSE)
  list(b = b, se = se, pval = pv)
}

# MC scenario for one (b, se, dT) combination
run_mc <- function(b_tmx, se_tmx, dT, n_mc = N_MC) {
  if (is.na(b_tmx) || is.na(se_tmx))
    return(list(point = NA_real_, lo95 = NA_real_,
                hi95  = NA_real_, sd   = NA_real_))
  sim_tmx <- rnorm(n_mc, mean = b_tmx, sd = se_tmx)
  sim_pct <- (exp(sim_tmx * dT) - 1) * 100
  pt_pct  <- (exp(b_tmx   * dT) - 1) * 100
  list(point = pt_pct,
       lo95  = as.numeric(quantile(sim_pct, 0.025)),
       hi95  = as.numeric(quantile(sim_pct, 0.975)),
       sd    = sd(sim_pct))
}


# ==============================================================================
# LOAD RDS
# ==============================================================================

rds_v4 <- file.path(ana_yol, "modeller_v4_objects.rds")
rds_v3 <- file.path(ana_yol, "modeller_v3_objects.rds")

if (file.exists(rds_v4)) {
  tum_modeller <- readRDS(rds_v4)
  cat("[LOADED] modeller_v4_objects.rds\n\n")
} else if (file.exists(rds_v3)) {
  tum_modeller <- readRDS(rds_v3)
  cat("[LOADED] modeller_v3_objects.rds (fallback)\n\n")
} else {
  stop("[ERROR] No RDS file found. Run paper1_step03_models.R first.")
}


# ==============================================================================
# PART A -- TEMPERATURE SCENARIOS
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("PART A -- TEMPERATURE SCENARIOS\n")
cat(strrep("=", 65), "\n\n")


# ==============================================================================
# STEP 1 -- COLLECT b_TMX FOR ALL MODEL x WEIGHT x CROP COMBINATIONS
# ==============================================================================

cat("STEP 1 -- Collecting b_TMX coefficients\n\n")

model_weights <- c("M1_PW","M1_AW","M2_PW","M2_AW",
                   "M3_PW","M3_AW","M4_PW","M4_AW")

coef_all <- list()

for (urun in urun_listesi) {
  if (!urun %in% names(tum_modeller)) next
  for (mw in model_weights) {
    m   <- tum_modeller[[urun]]$all_mods[[mw]]
    res <- get_tmx_coef(m)
    if (is.na(res$b)) next
    parts <- strsplit(mw, "_")[[1]]
    coef_all[[paste0(urun, "_", mw)]] <- data.frame(
      crop   = urun,
      model  = parts[1],
      weight = parts[2],
      mw_id  = mw,
      b_TMX  = res$b,
      se_TMX = res$se,
      pval   = res$pval,
      sig    = sig_s(res$pval),
      stringsAsFactors = FALSE)
  }
}

coef_df <- dplyr::bind_rows(coef_all)
cat(sprintf("Coefficients collected: %d rows\n\n", nrow(coef_df)))

# Console: M2 PW summary
cat("--- M2 PW b_TMX summary ---\n")
cat(sprintf("%-10s  %9s  %9s  %7s  %s\n",
            "Crop","b_TMX","SE","t","sig"))
cat(strrep("-", 52), "\n")
m2pw_coef <- coef_df[coef_df$mw_id == "M2_PW", ]
for (i in seq_len(nrow(m2pw_coef)))
  cat(sprintf("%-10s  %9.5f  %9.5f  %7.2f  %s\n",
              m2pw_coef$crop[i], m2pw_coef$b_TMX[i],
              m2pw_coef$se_TMX[i],
              m2pw_coef$b_TMX[i] / m2pw_coef$se_TMX[i],
              m2pw_coef$sig[i]))
cat("\n")


# ==============================================================================
# STEP 2 -- RUN MC SCENARIOS FOR ALL COMBINATIONS
# ==============================================================================

cat("STEP 2 -- Running MC scenarios (N =", N_MC, ")\n\n")

results_list <- list()

for (i in seq_len(nrow(coef_df))) {
  urun   <- coef_df$crop[i]
  mw_id  <- coef_df$mw_id[i]
  b_tmx  <- coef_df$b_TMX[i]
  se_tmx <- coef_df$se_TMX[i]
  
  for (dT in delta_T_vals) {
    mc <- run_mc(b_tmx, se_tmx, dT)
    results_list[[paste0(urun, "_", mw_id, "_T+", dT, "C")]] <- data.frame(
      crop      = urun,
      model     = coef_df$model[i],
      weight    = coef_df$weight[i],
      mw_id     = mw_id,
      scen_id   = paste0("T+", dT, "C"),
      delta_T   = dT,
      b_TMX     = b_tmx,
      se_TMX    = se_tmx,
      point_pct = mc$point,
      lo95_pct  = mc$lo95,
      hi95_pct  = mc$hi95,
      sd_pct    = mc$sd,
      stringsAsFactors = FALSE)
  }
}

results_df <- dplyr::bind_rows(results_list) %>%
  dplyr::mutate(
    crop    = factor(crop,    levels = urun_listesi),
    scen_id = factor(scen_id, levels = paste0("T+", delta_T_vals, "C")),
    model   = factor(model,   levels = paste0("M", 1:4)),
    weight  = factor(weight,  levels = c("PW","AW")))

cat(sprintf("Scenario results: %d rows\n\n", nrow(results_df)))

# Console: +2 degC point estimates (M2 PW)
cat("--- Point Estimates at +2 degC (M2 PW) ---\n")
cat(sprintf("%-10s  %8s  %14s\n", "Crop","Pct.Ch","95%% CI"))
cat(strrep("-", 40), "\n")
m2pw_2c <- results_df[results_df$mw_id == "M2_PW" &
                        results_df$delta_T == 2, ]
for (i in seq_len(nrow(m2pw_2c)))
  cat(sprintf("%-10s  %7.2f%%  [%+.1f, %+.1f]\n",
              as.character(m2pw_2c$crop[i]),
              m2pw_2c$point_pct[i],
              m2pw_2c$lo95_pct[i],
              m2pw_2c$hi95_pct[i]))
cat("\n")

# Console: all T values (M2 PW)
cat("--- All Scenarios M2 PW ---\n")
cat(sprintf("%-10s  %7s  %7s  %7s  %7s\n",
            "Crop","T+1C","T+2C","T+3C","T+4C"))
cat(strrep("-", 50), "\n")
for (urun in urun_listesi) {
  sub <- results_df[results_df$mw_id == "M2_PW" &
                      as.character(results_df$crop) == urun, ]
  sub <- sub[order(sub$delta_T), ]
  if (nrow(sub) < 4) next
  cat(sprintf("%-10s  %7.2f  %7.2f  %7.2f  %7.2f\n",
              urun, sub$point_pct[1], sub$point_pct[2],
              sub$point_pct[3], sub$point_pct[4]))
}
cat("\n")

# Wide tables for Excel
m2pw_results <- results_df[results_df$mw_id == "M2_PW", ] %>%
  dplyr::mutate(crop    = as.character(crop),
                scen_id = as.character(scen_id))

wide_point <- m2pw_results %>%
  dplyr::select(crop, scen_id, point_pct) %>%
  tidyr::pivot_wider(names_from = scen_id, values_from = point_pct)

wide_lo <- m2pw_results %>%
  dplyr::select(crop, scen_id, lo95_pct) %>%
  tidyr::pivot_wider(names_from = scen_id, values_from = lo95_pct)

wide_hi <- m2pw_results %>%
  dplyr::select(crop, scen_id, hi95_pct) %>%
  tidyr::pivot_wider(names_from = scen_id, values_from = hi95_pct)


# ==============================================================================
# PLOTS -- G_S1 to G_S6
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("GENERATING SCENARIO PLOTS (G_S1 to G_S6)\n")
cat("Each plot: screen print x2 (labeled + unlabeled), saved x6\n")
cat(strrep("=", 65), "\n")

theme_scen <- theme_bw(base_size = 12) +
  theme(legend.position    = "right",
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title         = element_text(face = "bold", size = 12),
        plot.subtitle      = element_text(size = 8.5, color = "grey40"),
        plot.caption       = element_text(size = 7.5, color = "grey50"),
        plot.margin        = margin(8, 12, 8, 8))


# ------------------------------------------------------------------------------
# G_S1 -- FOREST PLOT: 4 scenarios x 8 crops (M2 PW)
# ------------------------------------------------------------------------------

cat("\n[G_S1] Forest plot 4 scenarios x 8 crops...\n")

forest_df <- results_df %>%
  dplyr::filter(mw_id == "M2_PW") %>%
  dplyr::mutate(
    crop_rev = factor(crop, levels = rev(urun_listesi)),
    dT_lbl   = factor(paste0("T+", delta_T, "C"),
                      levels = paste0("T+", delta_T_vals, "C")))

g_s1 <- ggplot(forest_df,
               aes(x = point_pct, y = crop_rev,
                   color = dT_lbl, shape = dT_lbl)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = lo95_pct, xmax = hi95_pct),
                 height   = 0.22, linewidth = 0.75,
                 position = position_dodge(width = 0.72)) +
  geom_point(size = 3.2, position = position_dodge(width = 0.72)) +
  scale_color_manual(values = pal_scen,
                     labels = c("T+1C" = "+1 degC", "T+2C" = "+2 degC",
                                "T+3C" = "+3 degC", "T+4C" = "+4 degC"),
                     name = "Warming Scenario") +
  scale_shape_manual(values = c("T+1C"=16,"T+2C"=17,"T+3C"=15,"T+4C"=18),
                     labels = c("T+1C" = "+1 degC", "T+2C" = "+2 degC",
                                "T+3C" = "+3 degC", "T+4C" = "+4 degC"),
                     name = "Warming Scenario") +
  scale_x_continuous(breaks = seq(-25, 5, by = 5),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "G_S1: Projected Yield Change Under Temperature Scenarios (M2 PW)",
    subtitle = "M2 Production-Weighted FE | 95%% parameter uncertainty CI | 10,000 MC bootstrap",
    caption  = "Lobell et al. (2011, Science) | Zhao et al. (2017, PNAS)",
    x = "Projected Yield Change (%)", y = NULL) +
  theme_scen +
  theme(axis.text.y = element_text(size = 11, face = "bold"))
save_plot(g_s1, "G_S1_scenarios_forest", width = 11, height = 7)
cat("[G_S1] DONE\n")


# ------------------------------------------------------------------------------
# G_S2 -- BAR CHART: +2 degC point estimate + literature benchmarks
# ------------------------------------------------------------------------------

cat("\n[G_S2] +2 degC bar chart...\n")

bar_df <- results_df %>%
  dplyr::filter(mw_id == "M2_PW", delta_T == 2) %>%
  dplyr::mutate(crop_ord = reorder(crop, point_pct, decreasing = FALSE))

lit_2c <- lit_bench %>%
  dplyr::mutate(pct_2C   = pct_1C * 2,
                crop_ord = factor(crop, levels = levels(bar_df$crop_ord)))

g_s2 <- ggplot(bar_df,
               aes(x = point_pct, y = crop_ord)) +
  geom_col(fill = "#C0392B", alpha = 0.80, width = 0.65) +
  geom_errorbarh(aes(xmin = lo95_pct, xmax = hi95_pct),
                 height = 0.30, linewidth = 0.9, color = "grey20") +
  geom_point(data        = lit_2c,
             aes(x = pct_2C, y = crop_ord,
                 shape = source, fill = source),
             size        = 3.5, color = "grey20",
             inherit.aes = FALSE) +
  scale_shape_manual(
    values = c("Lobell 2011" = 23, "Zhao 2017" = 24),
    name   = "Literature Benchmark") +
  scale_fill_manual(
    values = c("Lobell 2011" = "#F4D03F", "Zhao 2017" = "#A9CCE3"),
    name   = "Literature Benchmark") +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey40", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", point_pct),
                x     = point_pct - 0.3),
            hjust = 1.05, size = 3.2, fontface = "bold",
            color = "white") +
  scale_x_continuous(limits = c(-13, 1),
                     breaks = seq(-12, 0, by = 2),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "G_S2: Projected Yield Change at +2 degC Warming (M2 PW)",
    subtitle = "Error bars = 95%% CI | Diamonds/triangles = literature benchmarks at +2 degC",
    caption  = "Lobell et al. (2011, Science) | Zhao et al. (2017, PNAS) | 10,000 MC bootstrap",
    x = "Projected Yield Change (%)", y = NULL) +
  theme_scen +
  theme(axis.text.y    = element_text(size = 11, face = "bold"),
        legend.position = "bottom")
save_plot(g_s2, "G_S2_plus2C_bar", width = 11, height = 7)
cat("[G_S2] DONE\n")


# ------------------------------------------------------------------------------
# G_S3 -- HEATMAP: scenario x crop
# ------------------------------------------------------------------------------

cat("\n[G_S3] Heatmap scenario x crop...\n")

heat_df <- results_df %>%
  dplyr::filter(mw_id == "M2_PW") %>%
  dplyr::mutate(
    dT_lbl = factor(paste0("+", delta_T, " degC"),
                    levels = paste0("+", delta_T_vals, " degC")),
    crop   = factor(crop, levels = urun_listesi))

g_s3 <- ggplot(heat_df,
               aes(x = crop, y = dT_lbl, fill = point_pct)) +
  geom_tile(color = "white", linewidth = 0.7) +
  geom_text(aes(label = sprintf("%.1f\n[%.1f,%.1f]",
                                point_pct, lo95_pct, hi95_pct)),
            size = 2.5, lineheight = 0.9) +
  scale_fill_gradient2(
    low      = "#C0392B",
    mid      = "#F9EBEA",
    high     = "white",
    midpoint = -10,
    limits   = c(-22, 0),
    oob      = scales::squish,
    name     = "Yield Change (%)") +
  scale_x_discrete(position = "top") +
  labs(
    title    = "G_S3: Projected Yield Change -- Temperature Scenarios (M2 PW)",
    subtitle = "Point estimate [95%% CI] | Rows: warming level | Cols: crop",
    caption  = "10,000 MC | Parameter uncertainty only",
    x = NULL, y = "Warming Scenario") +
  theme_bw(base_size = 10) +
  theme(panel.grid      = element_blank(),
        axis.text.x     = element_text(face = "bold", size = 10),
        legend.position = "right",
        plot.title      = element_text(face = "bold", size = 12),
        plot.subtitle   = element_text(size = 7, color = "grey40"),
        plot.caption    = element_text(size = 7.5, color = "grey50"),
        plot.margin     = margin(8, 12, 8, 8))
save_plot(g_s3, "G_S3_heatmap", width = 12, height = 6)
cat("[G_S3] DONE\n")


# ------------------------------------------------------------------------------
# G_S4 -- MODEL ROBUSTNESS: M1/M2/M3/M4 at +2 degC (PW)
# Do scenario estimates change across model specifications?
# ------------------------------------------------------------------------------

cat("\n[G_S4] Model robustness at +2 degC...\n")

rob_df <- results_df %>%
  dplyr::filter(weight == "PW", delta_T == 2) %>%
  dplyr::mutate(
    crop_rev  = factor(crop, levels = rev(urun_listesi)),
    model_lbl = factor(model, levels = paste0("M", 1:4)))

if (nrow(rob_df) > 0) {
  g_s4 <- ggplot(rob_df,
                 aes(x = point_pct, y = crop_rev,
                     color = model_lbl, shape = model_lbl)) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.5) +
    geom_errorbarh(aes(xmin = lo95_pct, xmax = hi95_pct),
                   height   = 0.20, linewidth = 0.70,
                   position = position_dodge(width = 0.70)) +
    geom_point(size = 2.8, position = position_dodge(width = 0.70)) +
    scale_color_manual(values = pal_model, name = "Model") +
    scale_shape_manual(
      values = c("M1"=16,"M2"=17,"M3"=15,"M4"=18),
      name   = "Model") +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title    = "G_S4: Model Robustness -- Projected Yield Change at +2 degC (M1-M4, PW)",
      subtitle = "M1 = baseline | M2 = +IRR | M3 = +GDP | M4 = +IRR+GDP\nOverlapping CIs = scenario estimate is model-robust",
      caption  = "95%% CI from 10,000 MC bootstrap | Production-Weighted FE",
      x = "Projected Yield Change (%)", y = NULL) +
    theme_scen +
    theme(axis.text.y = element_text(size = 10, face = "bold"))
  save_plot(g_s4, "G_S4_model_robustness", width = 11, height = 7)
  cat("[G_S4] DONE\n")
} else {
  cat("[G_S4] SKIPPED\n")
}


# ------------------------------------------------------------------------------
# G_S5 -- AW vs PW SCENARIO COMPARISON AT +2 degC (M2)
# ------------------------------------------------------------------------------

cat("\n[G_S5] AW vs PW comparison at +2 degC...\n")

aw_pw_df <- results_df %>%
  dplyr::filter(model == "M2", delta_T == 2) %>%
  dplyr::mutate(
    crop_rev = factor(crop, levels = rev(urun_listesi)),
    wt_lbl   = factor(weight, levels = c("PW","AW")))

if (nrow(aw_pw_df) > 0 && length(unique(aw_pw_df$weight)) > 1) {
  g_s5 <- ggplot(aw_pw_df,
                 aes(x = point_pct, y = crop_rev,
                     color = wt_lbl, shape = wt_lbl)) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.5) +
    geom_errorbarh(aes(xmin = lo95_pct, xmax = hi95_pct),
                   height   = 0.20, linewidth = 0.75,
                   position = position_dodge(width = 0.55)) +
    geom_point(size = 3.0, position = position_dodge(width = 0.55)) +
    scale_color_manual(
      values = c("PW" = "#1A5276", "AW" = "#C0392B"),
      labels = c("PW" = "Production-Weighted",
                 "AW" = "Area-Weighted"),
      name = "Weighting") +
    scale_shape_manual(
      values = c("PW" = 16, "AW" = 17),
      labels = c("PW" = "Production-Weighted",
                 "AW" = "Area-Weighted"),
      name = "Weighting") +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title    = "G_S5: Production-Weighted vs Area-Weighted Scenarios at +2 degC (M2)",
      subtitle = "PW = weighted by production tonnes | AW = weighted by harvested area\nOverlapping points = weighting choice does not drive results",
      caption  = "95%% CI from 10,000 MC bootstrap | M2 two-way FE",
      x = "Projected Yield Change (%)", y = NULL) +
    theme_scen +
    theme(legend.position = "bottom",
          axis.text.y     = element_text(size = 10, face = "bold"))
  save_plot(g_s5, "G_S5_AW_vs_PW_scenarios", width = 11, height = 7)
  cat("[G_S5] DONE\n")
} else {
  cat("[G_S5] SKIPPED -- AW models not available\n")
}


# ------------------------------------------------------------------------------
# G_S6 -- LITERATURE COMPARISON AT +1 degC
# Our estimates vs Lobell 2011 and Zhao 2017
# ------------------------------------------------------------------------------

cat("\n[G_S6] Literature comparison at +1 degC...\n")

our_1c <- results_df %>%
  dplyr::filter(mw_id == "M2_PW", delta_T == 1) %>%
  dplyr::mutate(source = "This study (M2 PW)",
                crop   = as.character(crop))

lit_plot <- lit_bench %>%
  dplyr::mutate(point_pct = pct_1C,
                lo95_pct  = NA_real_,
                hi95_pct  = NA_real_)

lit_comb <- dplyr::bind_rows(
  our_1c   %>% dplyr::select(crop, source, point_pct, lo95_pct, hi95_pct),
  lit_plot %>% dplyr::select(crop, source, point_pct, lo95_pct, hi95_pct)) %>%
  dplyr::filter(crop %in% c("wheat","rice","maize","soybean")) %>%
  dplyr::mutate(
    crop   = factor(crop,
                    levels = rev(c("wheat","rice","maize","soybean"))),
    source = factor(source,
                    levels = c("This study (M2 PW)",
                               "Lobell 2011", "Zhao 2017")))

if (nrow(lit_comb) > 0) {
  g_s6 <- ggplot(lit_comb,
                 aes(x = point_pct, y = crop,
                     color = source, shape = source)) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.5) +
    geom_errorbarh(
      data = lit_comb %>% dplyr::filter(!is.na(lo95_pct)),
      aes(xmin = lo95_pct, xmax = hi95_pct),
      height   = 0.15, linewidth = 0.8,
      position = position_dodge(width = 0.55)) +
    geom_point(size = 3.5, position = position_dodge(width = 0.55)) +
    scale_color_manual(
      values = c("This study (M2 PW)" = "#1A5276",
                 "Lobell 2011"        = "#E74C3C",
                 "Zhao 2017"          = "#27AE60"),
      name = "Source") +
    scale_shape_manual(
      values = c("This study (M2 PW)" = 16,
                 "Lobell 2011"        = 17,
                 "Zhao 2017"          = 15),
      name = "Source") +
    scale_x_continuous(breaks = seq(-15, 2, by = 2),
                       labels = function(x) paste0(x, "%")) +
    labs(
      title    = "G_S6: Literature Comparison -- Yield Change at +1 degC Warming",
      subtitle = paste0("Our estimates (M2 PW, 95%% CI) vs ",
                        "Lobell et al. (2011) and Zhao et al. (2017)\n",
                        "Selected crops with published benchmarks only"),
      caption  = "Lobell et al. (2011, Science) | Zhao et al. (2017, PNAS)",
      x = "Projected Yield Change per +1 degC (%)", y = NULL) +
    theme_scen +
    theme(legend.position = "bottom",
          axis.text.y     = element_text(size = 11, face = "bold"))
  save_plot(g_s6, "G_S6_literature_comparison", width = 11, height = 7)
  cat("[G_S6] DONE\n")
} else {
  cat("[G_S6] SKIPPED\n")
}


# ==============================================================================
# PART B -- TMX SQUARED NONLINEAR ANALYSIS
# Model: ln_yield ~ TMX + TMX_sq + PRE + IRR | Country_ISO + Yil
# Optimal TMX = -b_TMX / (2 * b_TMX_sq)  [valid only if concave: b_TMX_sq < 0]
# ==============================================================================

cat("\n", strrep("=", 65), "\n")
cat("PART B -- TMX SQUARED NONLINEAR ANALYSIS\n")
cat(strrep("=", 65), "\n\n")

results_tmx2_list <- list()
curve_list        <- list()

for (urun in urun_listesi) {
  cat(sprintf("[%s]\n", toupper(urun)))
  df <- tum_modeller[[urun]]$df
  if (is.null(df)) { cat("  df missing\n\n"); next }
  
  df$TMX_sq <- df$TMX^2   # squared TMX for nonlinear term
  
  m_tmx2 <- tryCatch(
    fixest::feols(
      ln_yield ~ TMX + TMX_sq + PRE + IRR | Country_ISO + Yil,
      data    = df,
      weights = ~production_tonnes,
      cluster = ~Country_ISO),
    error = function(e) {
      cat(sprintf("  feols error: %s\n", e$message)); NULL })
  if (is.null(m_tmx2)) next
  
  cf <- tryCatch(coef(m_tmx2), error = function(e) NULL)
  vc <- tryCatch(vcov(m_tmx2), error = function(e) NULL)
  if (is.null(cf) || is.null(vc)) next
  if (!all(c("TMX","TMX_sq") %in% names(cf))) next
  
  b1  <- as.numeric(cf["TMX"])
  b2  <- as.numeric(cf["TMX_sq"])
  se1 <- as.numeric(sqrt(vc["TMX",    "TMX"]))
  se2 <- as.numeric(sqrt(vc["TMX_sq", "TMX_sq"]))
  c12 <- as.numeric(vc["TMX", "TMX_sq"])
  pv1 <- 2 * pnorm(-abs(b1 / se1))
  pv2 <- 2 * pnorm(-abs(b2 / se2))
  
  # Optimal TMX = -b1 / (2*b2)  [delta method SE]
  tmx_opt <- if (!is.na(b2) && b2 != 0) -b1 / (2 * b2) else NA_real_
  g1      <- -1 / (2 * b2)
  g2      <-  b1 / (2 * b2^2)
  se_opt  <- sqrt(max(0, g1^2*se1^2 + g2^2*se2^2 + 2*g1*g2*c12))
  
  tmx_mean   <- mean(df$TMX, na.rm = TRUE)
  tmx_sd     <- sd(df$TMX,   na.rm = TRUE)
  is_concave <- !is.na(b2) && b2 < 0
  
  ar2 <- tryCatch(as.numeric(fixest::r2(m_tmx2, "ar2")),
                  error = function(e) NA_real_)
  n   <- tryCatch(as.integer(stats::nobs(m_tmx2)),
                  error = function(e) NA_integer_)
  
  cat(sprintf("  b_TMX    = %+.5f%s\n", b1, sig_s(pv1)))
  cat(sprintf("  b_TMX_sq = %+.8f%s\n", b2, sig_s(pv2)))
  cat(sprintf("  TMX_opt  = %.1f degC  (mean=%.1f degC)\n", tmx_opt, tmx_mean))
  cat(sprintf("  Concave  = %s | AdjR2=%.4f | N=%d\n\n",
              ifelse(is_concave, "YES (inverse-U)", "NO (U-shape or linear)"),
              ar2, n))
  
  results_tmx2_list[[urun]] <- data.frame(
    crop        = urun,
    b_TMX       = b1,  se_TMX      = se1,  pval_TMX    = pv1, sig_TMX    = sig_s(pv1),
    b_TMX_sq    = b2,  se_TMX_sq   = se2,  pval_TMX_sq = pv2, sig_TMX_sq = sig_s(pv2),
    is_concave  = is_concave,
    tmx_opt     = tmx_opt,
    se_opt      = se_opt,
    opt_lo95    = tmx_opt - 1.96 * se_opt,
    opt_hi95    = tmx_opt + 1.96 * se_opt,
    tmx_mean    = tmx_mean,
    tmx_sd      = tmx_sd,
    adjr2       = ar2,
    N           = n,
    stringsAsFactors = FALSE)
  
  # Response curve relative to mean TMX
  tmx_seq <- seq(tmx_mean - 3 * tmx_sd,
                 tmx_mean + 3 * tmx_sd,
                 length.out = 200)
  dly <- b1 * (tmx_seq - tmx_mean) +
    b2 * (tmx_seq^2 - tmx_mean^2)
  
  # MC confidence band (2,000 draws)
  draws   <- MASS::mvrnorm(
    2000,
    mu    = c(b1, b2),
    Sigma = vc[c("TMX","TMX_sq"), c("TMX","TMX_sq")])
  sim_mat <- outer(draws[, 1], tmx_seq - tmx_mean) +
    outer(draws[, 2], tmx_seq^2 - tmx_mean^2)
  
  curve_list[[urun]] <- data.frame(
    crop       = urun,
    TMX        = tmx_seq,
    pct_change = (exp(dly) - 1) * 100,
    lo95       = (exp(apply(sim_mat, 2, quantile, 0.025)) - 1) * 100,
    hi95       = (exp(apply(sim_mat, 2, quantile, 0.975)) - 1) * 100,
    tmx_mean   = tmx_mean,
    tmx_opt    = tmx_opt,
    is_concave = is_concave,
    stringsAsFactors = FALSE)
}

results_df_tmx2 <- dplyr::bind_rows(results_tmx2_list)
curve_df_tmx2   <- dplyr::bind_rows(curve_list)

# Console summary
cat("\n", strrep("=", 65), "\n")
cat("M_TMX2 SUMMARY\n")
cat(strrep("=", 65), "\n")
cat(sprintf("%-10s  %10s  %5s  %12s  %5s  %8s  %s\n",
            "Crop","b_TMX","sig","b_TMX_sq(1e6)","sig","TMX_opt","Shape"))
cat(strrep("-", 65), "\n")
for (i in seq_len(nrow(results_df_tmx2))) {
  r <- results_df_tmx2[i, ]
  cat(sprintf("%-10s  %+10.5f  %-5s  %+12.4f  %-5s  %8.1f  %s\n",
              r$crop, r$b_TMX, r$sig_TMX,
              r$b_TMX_sq * 1e6, r$sig_TMX_sq,
              r$tmx_opt,
              ifelse(r$is_concave, "CONCAVE (inverse-U)", "convex or linear")))
}
cat(sprintf("\nConcave crops: %d / %d\n\n",
            sum(results_df_tmx2$is_concave, na.rm = TRUE),
            nrow(results_df_tmx2)))


# ------------------------------------------------------------------------------
# G_TMX2 -- NONLINEAR TMX RESPONSE CURVES: ALL 8 CROPS
# Diamond = estimated optimal TMX (concave crops only)
# ------------------------------------------------------------------------------

cat("\n[G_TMX2] TMX squared response curves...\n")

if (nrow(curve_df_tmx2) > 0) {
  
  curve_plot <- curve_df_tmx2 %>%
    dplyr::mutate(
      crop      = factor(crop, levels = urun_listesi),
      shape_lbl = ifelse(is_concave,
                         "Concave (inverse-U)",
                         "Convex / linear"))
  
  # Optimal point markers for concave crops
  opt_pts <- results_df_tmx2 %>%
    dplyr::filter(is_concave & !is.na(tmx_opt)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      pct_at_opt = {
        cd <- curve_df_tmx2[curve_df_tmx2$crop == crop, ]
        if (nrow(cd) == 0) NA_real_ else
          cd$pct_change[which.min(abs(cd$TMX - tmx_opt))]
      }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(crop = factor(crop, levels = urun_listesi))
  
  # Shape label annotation data (top-left of each facet)
  shape_ann <- curve_plot %>%
    dplyr::group_by(crop) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  g_tmx2 <- ggplot(curve_plot,
                   aes(x = TMX, y = pct_change, color = crop)) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.5) +
    geom_ribbon(aes(ymin = lo95, ymax = hi95, fill = crop),
                alpha = 0.12, color = NA) +
    geom_line(linewidth = 1.0) +
    # Sample mean TMX vertical line
    geom_vline(
      data = results_df_tmx2 %>%
        dplyr::mutate(crop = factor(crop, levels = urun_listesi)),
      aes(xintercept = tmx_mean),
      linetype = "dotted", color = "grey55",
      linewidth = 0.6, inherit.aes = FALSE) +
    # Optimal TMX diamond (concave crops only)
    { if (nrow(opt_pts) > 0)
      geom_point(data = opt_pts,
                 aes(x = tmx_opt, y = pct_at_opt, color = crop),
                 shape = 18, size = 4.5, inherit.aes = FALSE) } +
    # Shape label inside facet
    geom_text(data        = shape_ann,
              aes(x = -Inf, y = Inf, label = shape_lbl),
              hjust        = -0.05, vjust = 1.4,
              size         = 2.8, color = "grey30",
              inherit.aes  = FALSE) +
    facet_wrap(~crop, scales = "free", nrow = 2) +
    scale_color_manual(values = renk_crop, guide = "none") +
    scale_fill_manual( values = renk_crop, guide = "none") +
    labs(
      title    = "G_TMX2: Nonlinear Temperature Response -- TMX + TMX_sq (Production-Weighted)",
      subtitle = paste0("Predicted yield change relative to crop mean TMX | ",
                        "Diamond = optimal TMX (concave crops only)\n",
                        "Dotted = sample mean TMX | Shaded = MC 95%% CI (n=2,000)"),
      caption  = paste0("Model: ln_yield ~ TMX + TMX_sq + PRE + IRR | ",
                        "Country FE + Year FE\n",
                        "Schlenker & Roberts (2009, PNAS)"),
      x = "Flowering Maximum Temperature (degC)",
      y = "Predicted Yield Change from Mean TMX (%%)") +
    theme_bw(base_size = 10) +
    theme(panel.grid.minor = element_blank(),
          strip.text       = element_text(face = "bold", size = 10),
          strip.background = element_rect(fill = "grey95"),
          plot.title       = element_text(face = "bold", size = 12),
          plot.subtitle    = element_text(size = 8, color = "grey40"),
          plot.caption     = element_text(size = 7.5, color = "grey50"))
  save_plot(g_tmx2, "G_TMX2_nonlinear_response", width = 14, height = 9)
  cat("[G_TMX2] DONE\n")
} else {
  cat("[G_TMX2] SKIPPED -- no curve data\n")
}


# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

cat("\n", strrep("=", 75), "\n")
cat("SCENARIO SUMMARY (M2 PW)\n")
cat(strrep("=", 75), "\n\n")

cat("--- Projected yield changes ---\n")
cat(sprintf("%-10s  %8s  %8s  %8s  %8s\n",
            "Crop","T+1C","T+2C","T+3C","T+4C"))
cat(strrep("-", 50), "\n")
for (urun in urun_listesi) {
  sub <- results_df[results_df$mw_id == "M2_PW" &
                      as.character(results_df$crop) == urun, ]
  sub <- sub[order(sub$delta_T), ]
  if (nrow(sub) < 4) next
  cat(sprintf("%-10s  %7.1f%%  %7.1f%%  %7.1f%%  %7.1f%%\n",
              urun, sub$point_pct[1], sub$point_pct[2],
              sub$point_pct[3], sub$point_pct[4]))
}

cat("\n--- Literature comparison at +1 degC ---\n")
cat(sprintf("%-10s  %10s  %10s  %10s\n",
            "Crop","This Study","Lobell2011","Zhao2017"))
cat(strrep("-", 46), "\n")
for (urun in c("wheat","rice","maize","soybean")) {
  our_v <- results_df[results_df$mw_id == "M2_PW" &
                        as.character(results_df$crop) == urun &
                        results_df$delta_T == 1, ]
  l11   <- lit_bench[lit_bench$crop == urun & lit_bench$source == "Lobell 2011", ]
  z17   <- lit_bench[lit_bench$crop == urun & lit_bench$source == "Zhao 2017", ]
  cat(sprintf("%-10s  %10s  %10s  %10s\n",
              urun,
              if (nrow(our_v) > 0) sprintf("%+.1f%%", our_v$point_pct[1]) else "--",
              if (nrow(l11)   > 0) sprintf("%+.1f%%", l11$pct_1C[1])      else "--",
              if (nrow(z17)   > 0) sprintf("%+.1f%%", z17$pct_1C[1])      else "--"))
}


# ==============================================================================
# SAVE RESULTS TO EXCEL
# ==============================================================================

cat("\nSaving adim9_senaryo.xlsx...\n")

writexl::write_xlsx(
  list(
    ScenarioResults_M2PW = m2pw_results,
    ScenarioResults_All  = results_df %>%
      dplyr::mutate(crop    = as.character(crop),
                    scen_id = as.character(scen_id),
                    model   = as.character(model),
                    weight  = as.character(weight)),
    CoefTable            = coef_df,
    WidePoint_M2PW       = wide_point %>%
      dplyr::mutate(crop = as.character(crop)),
    WideCI_Low_M2PW      = wide_lo %>%
      dplyr::mutate(crop = as.character(crop)),
    WideCI_High_M2PW     = wide_hi %>%
      dplyr::mutate(crop = as.character(crop)),
    Literature           = lit_bench,
    TMX2_Results         = results_df_tmx2,
    TMX2_Curves          = curve_df_tmx2),
  file.path(ana_yol, "adim9_senaryo.xlsx"))

cat("[SAVED] adim9_senaryo.xlsx\n")


# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n", strrep("=", 65), "\n")
cat("STEP 09 COMPLETE\n")
cat(strrep("=", 65), "\n\n")

cat("COMPLETED:\n")
cat("  Part A -- Temperature Scenarios:\n")
cat("    Step 1: b_TMX collected for M1-M4 x PW/AW x all crops\n")
cat("    Step 2: MC scenarios run (N=10,000)\n")
cat("  Part B -- TMX Squared Nonlinear Analysis:\n")
cat("    Step 3: TMX + TMX_sq model, optimal TMX, response curves\n\n")

cat("PLOTS (each printed labeled + unlabeled on screen):\n")
cat("  G_S1   : Forest plot 4 scenarios x 8 crops (M2 PW)\n")
cat("  G_S2   : +2 degC bar chart + literature benchmarks\n")
cat("  G_S3   : Heatmap scenario x crop\n")
cat("  G_S4   : Model robustness M1-M4 at +2 degC\n")
cat("  G_S5   : AW vs PW scenarios at +2 degC\n")
cat("  G_S6   : Literature comparison at +1 degC\n")
cat("  G_TMX2 : Nonlinear TMX response curves\n\n")

cat("FILES SAVED (6 per plot = 42 total):\n")
cat("  figures/labeled/   -- PDF + 300dpi PNG + 600dpi PNG\n")
cat("  figures/unlabeled/ -- PDF + 300dpi PNG + 600dpi PNG\n\n")

cat("OUTPUT: adim9_senaryo.xlsx\n")
cat("  Sheets: ScenarioResults_M2PW, ScenarioResults_All,\n")
cat("          CoefTable, WidePoint/CI_M2PW, Literature,\n")
cat("          TMX2_Results, TMX2_Curves\n\n")

cat("NEXT STEP: paper1_step10_vpdanalysis.R\n")
cat(strrep("=", 65), "\n")


















# ==============================================================================
# PAPER 1 -- STEP 10: VPD DERIVATION + TMX vs VPD SCENARIO COMPARISON
# Temperature, VPD, and Global Crop Yields: Evidence from a Multi-Crop Panel
#
# Reads:
#   modeller_v4_objects.rds        (from step03)
#   CRU_TS4.09_vap_1961_2024.nc   (vapour pressure raster)
#   GADM country shapefiles        (for spatial extraction)
#
# Contents:
#   Part 1A : Load vap raster, build time sequence
#   Part 1B : Extract country-year mean vap, compute VPD
#   Part 2  : Fit VPD model + run MC scenarios (+1/+2/+3/+4 degC)
#   Plots   : G_A1 to G_A4
#             -- R screen: labeled then unlabeled
#             -- Saved: PDF + PNG 300dpi + PNG 600dpi x labeled + unlabeled
#
# Plots:
#   G_A1 : TMX vs VPD scenario comparison (facet by warming level)
#   G_A2 : Per-degree effect -- TMX vs VPD at +1 degC
#   G_A3 : RH_eff vs T_base scatter (why VPD differs across crops)
#   G_A4 : dVPD per +1 degC bar chart (Clausius-Clapeyron transparency)
#
# VPD derivation:
#   esat(T) = 0.6108 * exp(17.27 * T / (T + 237.3))  [Allen et al. 1998 FAO-56]
#   VPD = max(esat(TMX) - vap_kPa, 0)
#   dVPD/dT = des/dT * (1 - RH_eff)  [Clausius-Clapeyron]
#
# UTF-8 NOTE: All special characters replaced with ASCII
#   degree sign  -> degC
#   em dash      -> --
#
# References:
#   Allen et al. (1998, FAO-56)   -- esat formula
#   Clausius-Clapeyron            -- dVPD/dT derivation
#   Zhao et al. (2017, PNAS)      -- VPD motivation
#   Lobell et al. (2013, NCC)     -- VPD crop damage mechanism
# ==============================================================================


# ==============================================================================
# 0. SETUP
# ==============================================================================

library(dplyr)
library(ggplot2)
library(writexl)
library(tidyr)
library(terra)
library(readxl)
library(fixest)

ana_yol      <- "C:/Users/Alper.Tosun/Desktop/cru130"
cru_yol      <- file.path(ana_yol, "cru-iklim-veri")
gadm_yol     <- file.path(ana_yol, "gadm")
urun_listesi <- c("wheat","barley","oats","rye","rice","maize","soybean","sorghum")

set.seed(2025)
N_MC         <- 10000L
delta_T_vals <- c(1, 2, 3, 4)

# Output directories
fig_labeled   <- file.path(ana_yol, "figures", "labeled")
fig_unlabeled <- file.path(ana_yol, "figures", "unlabeled")
dir.create(fig_labeled,   recursive = TRUE, showWarnings = FALSE)
dir.create(fig_unlabeled, recursive = TRUE, showWarnings = FALSE)

# Phenology windows (months) -- flowering period per crop
urun_pencere <- list(
  wheat   = c(3, 5), barley  = c(3, 5),
  oats    = c(3, 5), rye     = c(3, 5),
  rice    = c(7, 9), maize   = c(6, 8),
  soybean = c(7, 9), sorghum = c(6, 8))

renk_crop <- c(wheat   = "#1A5276", barley  = "#2E86C1",
               oats    = "#27AE60", rye     = "#1E8449",
               rice    = "#F39C12", maize   = "#E67E22",
               soybean = "#8E44AD", sorghum = "#C0392B")


# ==============================================================================
# SAVE_PLOT FUNCTION
# ==============================================================================

save_plot <- function(g_labeled, plot_name, width = 10, height = 7) {
  
  g_unlabeled <- g_labeled +
    labs(title    = NULL,
         subtitle = NULL,
         caption  = NULL,
         x        = NULL,
         y        = NULL) +
    theme(legend.title = element_blank(),
          plot.margin  = margin(4, 4, 4, 4))
  
  cat(sprintf("  [SCREEN] %s -- LABELED\n",   plot_name)); print(g_labeled)
  cat(sprintf("  [SCREEN] %s -- UNLABELED\n", plot_name)); print(g_unlabeled)
  
  save_one <- function(g, dir_path) {
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, ".pdf")),
                    plot = g, width = width, height = height, device = "pdf")
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, "_300dpi.png")),
                    plot = g, width = width, height = height,
                    dpi = 300, device = "png")
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, "_600dpi.png")),
                    plot = g, width = width, height = height,
                    dpi = 600, device = "png")
  }
  save_one(g_labeled,   fig_labeled)
  save_one(g_unlabeled, fig_unlabeled)
  
  cat(sprintf("  [SAVED]  %s -> labeled/ + unlabeled/ (PDF + 300 + 600 dpi)\n\n",
              plot_name))
  invisible(NULL)
}


# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

sig_s <- function(p)
  ifelse(is.na(p), "",
         ifelse(p < 0.001, "***",
                ifelse(p < 0.01,  "**",
                       ifelse(p < 0.05,  "*",
                              ifelse(p < 0.10,  ".", "")))))

# FAO-56 saturation vapour pressure (Allen et al. 1998)
esat_kPa <- function(T_C) 0.6108 * exp(17.27 * T_C / (T_C + 237.3))

# GADM file path
gadm_path <- function(iso)
  file.path(gadm_yol, sprintf("gadm41_%s_0_pk.rds", iso))

# Load GADM as SpatVector -- tries unwrap, vect, sf in sequence
gadm_to_sv <- function(gadm_f) {
  obj <- tryCatch(readRDS(gadm_f), error = function(e) NULL)
  if (is.null(obj)) return(NULL)
  sv <- tryCatch(terra::unwrap(obj), error = function(e) NULL)
  if (!is.null(sv) && inherits(sv, "SpatVector")) return(sv)
  if (inherits(obj, "SpatVector")) return(obj)
  sv <- tryCatch(terra::vect(obj), error = function(e) NULL)
  if (!is.null(sv) && inherits(sv, "SpatVector")) return(sv)
  if (requireNamespace("sf", quietly = TRUE)) {
    sf_obj <- tryCatch(sf::st_as_sf(obj), error = function(e) NULL)
    if (!is.null(sf_obj))
      return(tryCatch(terra::vect(sf_obj), error = function(e) NULL))
  }
  NULL
}

# Spatial extract with CRS alignment; NaN/Inf -> NA
extract_val <- function(rast_layer, sv) {
  if (!terra::same.crs(rast_layer, sv))
    sv <- tryCatch(terra::project(sv, terra::crs(rast_layer)),
                   error = function(e) NULL)
  if (is.null(sv)) return(NA_real_)
  ex <- tryCatch(
    terra::extract(rast_layer, sv, fun = "mean", na.rm = TRUE),
    error = function(e) NULL)
  if (is.null(ex) || ncol(ex) < 2) return(NA_real_)
  v <- as.numeric(ex[1, 2])
  if (is.nan(v) || is.infinite(v)) NA_real_ else v
}

# MC scenario: b and se -> point + 95% CI as percent change
mc_pct <- function(b, se, scale, n = N_MC) {
  if (is.na(b) || is.na(se) || is.na(scale))
    return(list(pt = NA, lo = NA, hi = NA))
  sim  <- rnorm(n, b, se)
  vals <- (exp(sim * scale) - 1) * 100
  list(pt = (exp(b * scale) - 1) * 100,
       lo = as.numeric(quantile(vals, 0.025)),
       hi = as.numeric(quantile(vals, 0.975)))
}

# Extract TMX coefficient from stored model objects
get_tmx_stored <- function(urun, mw_id) {
  m  <- tum_modeller[[urun]]$all_mods[[mw_id]]
  cf <- tryCatch(coef(m), error = function(e) NULL)
  vc <- tryCatch(vcov(m), error = function(e) NULL)
  if (is.null(cf) || is.null(vc) || !"TMX" %in% names(cf)) return(NULL)
  b  <- as.numeric(cf["TMX"])
  se <- sqrt(as.numeric(vc["TMX","TMX"]))
  list(b = b, se = se,
       sig = sig_s(2 * pt(abs(b / se), df = Inf, lower.tail = FALSE)))
}


# ==============================================================================
# LOAD RDS
# ==============================================================================

rds_v4 <- file.path(ana_yol, "modeller_v4_objects.rds")
rds_v3 <- file.path(ana_yol, "modeller_v3_objects.rds")

if (file.exists(rds_v4)) {
  tum_modeller <- readRDS(rds_v4)
  cat("[LOADED] modeller_v4_objects.rds\n\n")
} else if (file.exists(rds_v3)) {
  tum_modeller <- readRDS(rds_v3)
  cat("[LOADED] modeller_v3_objects.rds (fallback)\n\n")
} else {
  stop("[ERROR] No RDS file found. Run paper1_step03_models.R first.")
}


# ==============================================================================
# PART 1A -- VAP RASTER: LOAD AND BUILD TIME SEQUENCE
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("PART 1A -- Load vap raster\n")
cat(strrep("=", 65), "\n\n")

vap_nc_path <- file.path(cru_yol, "CRU_TS4.09_vap_1961_2024.nc")
if (!file.exists(vap_nc_path))
  stop("[ERROR] vap NC not found: ", vap_nc_path)

vap_rast   <- terra::rast(vap_nc_path)
n_layers   <- terra::nlyr(vap_rast)
vap_dates  <- seq(as.Date("1961-01-01"), by = "month",
                  length.out = n_layers)
vap_years  <- as.integer(format(vap_dates, "%Y"))
vap_months <- as.integer(format(vap_dates, "%m"))

cat(sprintf("Layers : %d\n", n_layers))
cat(sprintf("Dates  : %s -- %s\n", min(vap_dates), max(vap_dates)))
cat(sprintf("CRS    : %s\n\n",
            terra::crs(vap_rast, describe = TRUE)$name))


# ==============================================================================
# PART 1B -- VAP EXTRACT: COUNTRY-YEAR MEAN + VPD COMPUTATION
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("PART 1B -- vap extract + VPD computation\n")
cat(strrep("=", 65), "\n\n")

vpd_df_list <- list()

for (urun in urun_listesi) {
  cat(sprintf("[%s]\n", urun))
  
  df <- tum_modeller[[urun]]$df
  if (is.null(df)) { cat("  df missing\n\n"); next }
  
  # Ensure ln_yield exists
  if (!"ln_yield" %in% names(df)) {
    if ("yield_hg_per_ha" %in% names(df)) {
      df$ln_yield <- suppressWarnings(log(df$yield_hg_per_ha))
      df$ln_yield[!is.finite(df$ln_yield)] <- NA
    } else { cat("  ln_yield not available\n\n"); next }
  }
  
  # Detect TMX column
  tmx_col <- grep("TMX_flower|TMX_cicek", names(df),
                  value = TRUE, ignore.case = TRUE)[1]
  if (is.na(tmx_col))
    tmx_col <- grep("^TMX", names(df), value = TRUE)[1]
  cat(sprintf("  TMX col: %s\n", tmx_col))
  if (is.na(tmx_col)) { cat("  TMX not found\n\n"); next }
  
  p_start  <- urun_pencere[[urun]][1]
  p_end    <- urun_pencere[[urun]][2]
  ulkeler  <- sort(unique(df$Country_ISO))
  cidx     <- which(vap_months >= p_start & vap_months <= p_end &
                      vap_years >= 1961 & vap_years <= 2023)
  vap_sub  <- vap_rast[[cidx]]
  sub_years <- vap_years[cidx]
  yrs_uniq  <- sort(unique(sub_years))
  
  cat(sprintf("  Window: mo%d-mo%d | %d countries | %d layers\n",
              p_start, p_end, length(ulkeler), length(cidx)))
  
  # Load GADM files (cached per crop)
  sv_cache  <- list()
  n_ok_gadm <- 0L
  for (iso in ulkeler) {
    f  <- gadm_path(iso)
    if (!file.exists(f)) next
    sv <- gadm_to_sv(f)
    if (!is.null(sv)) {
      sv_cache[[iso]] <- sv
      n_ok_gadm <- n_ok_gadm + 1L
    }
  }
  cat(sprintf("  GADM loaded: %d/%d\n", n_ok_gadm, length(ulkeler)))
  if (n_ok_gadm == 0) { cat("  No GADM files\n\n"); next }
  
  # Extract country-year mean vap
  rows_list <- vector("list", length(yrs_uniq) * length(sv_cache))
  ri <- 0L
  
  for (yyy in yrs_uniq) {
    idx_y  <- which(sub_years == yyy)
    vap_y  <- if (length(idx_y) == 1) vap_sub[[idx_y]] else
      terra::mean(vap_sub[[idx_y]], na.rm = TRUE)
    for (iso in names(sv_cache)) {
      val <- extract_val(vap_y, sv_cache[[iso]])
      if (!is.na(val)) {
        ri <- ri + 1L
        rows_list[[ri]] <- data.frame(
          Country_ISO = iso, Yil = yyy, vap_hPa = val,
          stringsAsFactors = FALSE)
      }
    }
  }
  
  vap_ulke_yil <- dplyr::bind_rows(rows_list[seq_len(ri)])
  cat(sprintf("  Extract: %d rows\n", nrow(vap_ulke_yil)))
  if (nrow(vap_ulke_yil) == 0) { cat("  Extract empty\n\n"); next }
  
  # Compute VPD: esat(TMX) - vap_kPa, floored at 0
  df_vpd <- df %>%
    dplyr::left_join(vap_ulke_yil, by = c("Country_ISO","Yil")) %>%
    dplyr::mutate(
      vap_kPa        = vap_hPa / 10,
      esat_TMX       = esat_kPa(.data[[tmx_col]]),
      VPD_ciceklenme = pmax(esat_TMX - vap_kPa, 0))
  
  n_vpd <- sum(!is.na(df_vpd$VPD_ciceklenme))
  cat(sprintf("  VPD: n=%d | mean=%.3f kPa | sd=%.3f kPa\n",
              n_vpd,
              mean(df_vpd$VPD_ciceklenme, na.rm = TRUE),
              sd(df_vpd$VPD_ciceklenme,   na.rm = TRUE)))
  
  vpd_df_list[[urun]] <- df_vpd
  cat(sprintf("  [%s] OK\n\n", urun))
}

# Save VPD data for Paper 2 step
saveRDS(vpd_df_list, file.path(ana_yol, "adim10b_vpd_df.rds"))
cat(sprintf("[SAVED] adim10b_vpd_df.rds (%d crops)\n\n",
            length(vpd_df_list)))

vpd_available <- names(vpd_df_list)
if (length(vpd_available) == 0)
  stop("[CRITICAL] VPD extract empty -- check GADM files and NC path")

cat(sprintf("VPD available: %s\n\n",
            paste(vpd_available, collapse = ", ")))


# ==============================================================================
# PART 2 -- VPD MODEL + MC SCENARIOS
#
# FIX vs original:
#   [1] IRR added to VPD formula -- matches M2 structure
#       Original: ln_yield ~ VPD + PRE + PRE_sq | Country + Year
#       Fixed:    ln_yield ~ VPD + PRE + PRE_sq + IRR | Country + Year
#   [2] M1 PW also reported (no IRR) for comparison
#   [3] All CI from MC, not analytic delta method
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("PART 2 -- VPD model + MC scenarios\n")
cat(strrep("=", 65), "\n\n")

coef_comp_list <- list()
scen_list      <- list()

for (urun in vpd_available) {
  cat(sprintf("[%s]\n", urun))
  df_vpd <- vpd_df_list[[urun]]
  
  if (sum(!is.na(df_vpd$VPD_ciceklenme)) < 100) {
    cat("  VPD insufficient\n\n"); next
  }
  
  # TMX coefficients from stored model objects
  tmx_m2 <- get_tmx_stored(urun, "M2_PW")
  tmx_m1 <- get_tmx_stored(urun, "M1_PW")
  if (is.null(tmx_m2)) { cat("  TMX M2 missing\n\n"); next }
  
  # Detect IRR and PRE_sq columns
  irr_col <- intersect(c("IRR","irr_ratio"), names(df_vpd))[1]
  has_irr <- !is.na(irr_col) && sum(!is.na(df_vpd[[irr_col]])) > 100
  has_sq  <- "PRE_sq" %in% names(df_vpd)
  
  if (!"ln_yield" %in% names(df_vpd)) {
    cat("  ln_yield missing\n\n"); next
  }
  
  # VPD model formula: includes IRR to match M2
  irr_term <- if (has_irr) irr_col else NULL
  sq_term  <- if (has_sq)  "PRE_sq" else NULL
  rhs_vpd  <- paste(c("VPD_ciceklenme","PRE", sq_term, irr_term),
                    collapse = " + ")
  fml_vpd  <- as.formula(
    paste0("ln_yield ~ ", rhs_vpd, " | Country_ISO + Yil"))
  
  wt_col  <- if ("production_tonnes" %in% names(df_vpd))
    "production_tonnes" else NA_character_
  
  keep_vars <- c("ln_yield","VPD_ciceklenme","PRE",
                 sq_term, irr_term, "Country_ISO","Yil",
                 if (!is.na(wt_col)) wt_col)
  df_c <- df_vpd %>%
    dplyr::select(dplyr::all_of(keep_vars[!is.na(keep_vars)])) %>%
    tidyr::drop_na()
  
  cat(sprintf("  n=%d | formula: %s\n", nrow(df_c), deparse(fml_vpd)))
  
  m_vpd <- tryCatch({
    if (!is.na(wt_col))
      fixest::feols(fml_vpd, data = df_c,
                    weights = df_c[[wt_col]], cluster = ~Country_ISO)
    else
      fixest::feols(fml_vpd, data = df_c, cluster = ~Country_ISO)
  }, error = function(e) {
    cat(sprintf("  feols error: %s\n", e$message)); NULL })
  if (is.null(m_vpd)) next
  
  b_VPD  <- tryCatch(
    as.numeric(coef(m_vpd)["VPD_ciceklenme"]), error = function(e) NULL)
  se_VPD <- tryCatch(
    sqrt(as.numeric(vcov(m_vpd)["VPD_ciceklenme","VPD_ciceklenme"])),
    error = function(e) NULL)
  if (is.null(b_VPD) || is.null(se_VPD)) next
  
  pval_V <- 2 * pt(abs(b_VPD / se_VPD), df = Inf, lower.tail = FALSE)
  
  # Clausius-Clapeyron: dVPD/dT
  tmx_col2  <- grep("TMX_flower|TMX_cicek|^TMX",
                    names(df_vpd), value = TRUE,
                    ignore.case = TRUE)[1]
  T_base    <- mean(df_vpd[[tmx_col2]], na.rm = TRUE)
  es_base   <- esat_kPa(T_base)
  des_dT    <- es_base * 17.27 * 237.3 / (T_base + 237.3)^2
  e_act     <- mean(df_vpd$vap_kPa, na.rm = TRUE)
  RH_eff    <- min(e_act / es_base, 0.99)
  dVPD_1C   <- des_dT * (1 - RH_eff)
  
  cat(sprintf("  b_VPD=%+.5f%s (with IRR=%s)\n",
              b_VPD, sig_s(pval_V), ifelse(has_irr,"YES","no")))
  cat(sprintf("  b_TMX(M2)=%+.5f%s | b_TMX(M1)=%+.5f%s\n",
              tmx_m2$b, tmx_m2$sig,
              if (!is.null(tmx_m1)) tmx_m1$b  else NA,
              if (!is.null(tmx_m1)) tmx_m1$sig else ""))
  cat(sprintf("  T_base=%.1f degC | dVPD/1C=%.4f kPa | RH_eff=%.1f%%\n\n",
              T_base, dVPD_1C, RH_eff * 100))
  
  coef_comp_list[[urun]] <- data.frame(
    crop        = urun,
    b_TMX_M2    = tmx_m2$b,
    se_TMX_M2   = tmx_m2$se,
    sig_TMX_M2  = tmx_m2$sig,
    b_TMX_M1    = if (!is.null(tmx_m1)) tmx_m1$b  else NA_real_,
    se_TMX_M1   = if (!is.null(tmx_m1)) tmx_m1$se else NA_real_,
    b_VPD       = b_VPD,
    se_VPD      = se_VPD,
    sig_VPD     = sig_s(pval_V),
    irr_in_vpd  = has_irr,
    T_base_C    = T_base,
    dVPD_per_1C = dVPD_1C,
    RH_eff_pct  = RH_eff * 100,
    b_VPD_sc    = b_VPD * dVPD_1C,
    stringsAsFactors = FALSE)
  
  # MC scenarios -- all CI from MC (not analytic)
  sim_T <- rnorm(N_MC, tmx_m2$b, tmx_m2$se)
  sim_V <- rnorm(N_MC, b_VPD,    se_VPD)
  
  for (dT in delta_T_vals) {
    dVPD  <- dVPD_1C * dT
    mc_T  <- (exp(sim_T * dT)   - 1) * 100
    mc_V  <- (exp(sim_V * dVPD) - 1) * 100
    
    scen_list[[paste0(urun, "_T+", dT, "C")]] <- data.frame(
      crop    = urun,
      delta_T = dT,
      scen_id = paste0("T+", dT, "C"),
      pt_TMX  = (exp(tmx_m2$b * dT)  - 1) * 100,
      lo_TMX  = as.numeric(quantile(mc_T, 0.025)),
      hi_TMX  = as.numeric(quantile(mc_T, 0.975)),
      dVPD    = dVPD,
      pt_VPD  = (exp(b_VPD * dVPD)   - 1) * 100,
      lo_VPD  = as.numeric(quantile(mc_V, 0.025)),
      hi_VPD  = as.numeric(quantile(mc_V, 0.975)),
      diff_pp = (exp(b_VPD*dVPD)-1)*100 - (exp(tmx_m2$b*dT)-1)*100,
      stringsAsFactors = FALSE)
  }
  cat("  MC done\n\n")
}

coef_df  <- dplyr::bind_rows(coef_comp_list)
scen_raw <- dplyr::bind_rows(scen_list)

if (nrow(scen_raw) == 0)
  stop("[ERROR] Scenario table empty -- check VPD and model availability")

scen_df <- scen_raw %>%
  dplyr::mutate(
    crop    = factor(as.character(crop), levels = vpd_available),
    scen_id = factor(as.character(scen_id),
                     levels = paste0("T+", c(1,2,3,4), "C")))

# Console summary
cat("--- T+2C Summary ---\n")
cat(sprintf("%-10s  %9s  %9s  %7s\n",
            "Crop","TMX (%%)","VPD (%%)","Diff (pp)"))
cat(strrep("-", 42), "\n")
sub2 <- scen_df[as.character(scen_df$scen_id) == "T+2C", ]
for (i in seq_len(nrow(sub2)))
  cat(sprintf("%-10s  %9.1f  %9.1f  %7.1f\n",
              as.character(sub2$crop[i]),
              sub2$pt_TMX[i], sub2$pt_VPD[i], sub2$diff_pp[i]))
cat("\n")


# ==============================================================================
# PLOTS -- G_A1 to G_A4
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("GENERATING PLOTS (G_A1 to G_A4)\n")
cat("Each plot: screen print x2 (labeled + unlabeled), saved x6\n")
cat(strrep("=", 65), "\n")

theme_vpd <- theme_bw(base_size = 11) +
  theme(legend.position    = "bottom",
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title         = element_text(face = "bold", size = 12),
        plot.subtitle      = element_text(size = 8.5, color = "grey40"),
        plot.caption       = element_text(size = 7.5, color = "grey50"),
        plot.margin        = margin(8, 12, 8, 8))


# ------------------------------------------------------------------------------
# G_A1 -- TMX vs VPD SCENARIO COMPARISON (facet by warming level)
# ------------------------------------------------------------------------------

cat("\n[G_A1] Scenario comparison...\n")

scen_long <- dplyr::bind_rows(
  scen_df %>% dplyr::transmute(
    crop, scen_id, model = "TMX Model",
    point = pt_TMX, lo = lo_TMX, hi = hi_TMX),
  scen_df %>% dplyr::transmute(
    crop, scen_id, model = "VPD Model",
    point = pt_VPD, lo = lo_VPD, hi = hi_VPD)) %>%
  dplyr::mutate(
    crop  = factor(as.character(crop), levels = rev(vpd_available)),
    model = factor(model, levels = c("TMX Model","VPD Model")))

g_a1 <- ggplot(scen_long,
               aes(x = point, y = crop,
                   color = model, shape = model)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = lo, xmax = hi),
                 height   = 0.18, linewidth = 0.65,
                 position = position_dodge(width = 0.65)) +
  geom_point(size = 2.8, position = position_dodge(width = 0.65)) +
  facet_wrap(~scen_id, nrow = 1) +
  scale_color_manual(
    values = c("TMX Model" = "#C0392B", "VPD Model" = "#2471A3"),
    name   = NULL) +
  scale_shape_manual(
    values = c("TMX Model" = 16, "VPD Model" = 17),
    name   = NULL) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "G_A1: Projected Yield Change -- TMX vs VPD Model (Appendix)",
    subtitle = paste0("VPD via Clausius-Clapeyron and observed RH | ",
                      "95%% CI from 10,000 MC\n",
                      "VPD model includes IRR control (matches M2 structure)"),
    caption  = paste0("Allen et al. (1998) FAO-56 | ",
                      "Zhao et al. (2017, PNAS) | Lobell et al. (2013, NCC)"),
    x = "Projected Yield Change (%%)", y = NULL) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position    = "bottom",
        strip.text         = element_text(face = "bold"),
        strip.background   = element_rect(fill = "grey95"),
        axis.text.y        = element_text(size = 9, face = "bold"),
        plot.title         = element_text(face = "bold", size = 12),
        plot.subtitle      = element_text(size = 7, color = "grey40"),
        plot.caption       = element_text(size = 7.5, color = "grey50"))
save_plot(g_a1, "G_A1_TMX_vs_VPD_scenarios", width = 14, height = 7)
cat("[G_A1] DONE\n")


# ------------------------------------------------------------------------------
# G_A2 -- PER-DEGREE EFFECT: TMX vs VPD AT +1 degC
# All CI from MC (not analytic delta method)
# ------------------------------------------------------------------------------

cat("\n[G_A2] Per-degree effect forest...\n")

if (nrow(coef_df) > 0) {
  
  ga2_list <- list()
  for (i in seq_len(nrow(coef_df))) {
    r <- coef_df[i, ]
    mc_tmx <- mc_pct(r$b_TMX_M2, r$se_TMX_M2, scale = 1)
    mc_vpd <- mc_pct(r$b_VPD,    r$se_VPD,    scale = r$dVPD_per_1C)
    
    ga2_list[[paste0(r$crop,"_TMX")]] <- data.frame(
      crop  = r$crop,
      model = "TMX Model (M2 PW)",
      point = mc_tmx$pt, lo = mc_tmx$lo, hi = mc_tmx$hi,
      sig   = r$sig_TMX_M2, stringsAsFactors = FALSE)
    ga2_list[[paste0(r$crop,"_VPD")]] <- data.frame(
      crop  = r$crop,
      model = "VPD Model (M2 PW, +IRR)",
      point = mc_vpd$pt, lo = mc_vpd$lo, hi = mc_vpd$hi,
      sig   = r$sig_VPD, stringsAsFactors = FALSE)
  }
  
  ga2_df <- dplyr::bind_rows(ga2_list) %>%
    dplyr::mutate(
      crop  = factor(as.character(crop), levels = rev(vpd_available)),
      model = factor(model,
                     levels = c("TMX Model (M2 PW)",
                                "VPD Model (M2 PW, +IRR)")))
  
  g_a2 <- ggplot(ga2_df,
                 aes(x = point, y = crop,
                     color = model, shape = model)) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.6) +
    geom_errorbarh(aes(xmin = lo, xmax = hi),
                   height   = 0.22, linewidth = 0.75,
                   position = position_dodge(width = 0.6)) +
    geom_point(size = 3.2, position = position_dodge(width = 0.6)) +
    geom_text(aes(label = sig, x = hi + 0.3),
              size = 3.5, color = "grey30",
              position    = position_dodge(width = 0.6),
              show.legend = FALSE) +
    scale_color_manual(
      values = c("TMX Model (M2 PW)"       = "#C0392B",
                 "VPD Model (M2 PW, +IRR)" = "#2471A3"),
      name = NULL) +
    scale_shape_manual(
      values = c("TMX Model (M2 PW)"       = 16,
                 "VPD Model (M2 PW, +IRR)" = 17),
      name = NULL) +
    labs(
      title    = "G_A2: Per-Degree Effect -- TMX vs VPD at +1 degC",
      subtitle = paste0("b_VPD x dVPD/dT (Clausius-Clapeyron) | 95%% CI from MC\n",
                        "VPD model controls for IRR -- comparable to TMX M2"),
      caption  = "M2 Production-Weighted FE | Clustered SE | Allen et al. (1998)",
      x = "Yield Change per +1 degC (%%)", y = NULL) +
    theme_vpd +
    theme(axis.text.y = element_text(size = 11, face = "bold"))
  save_plot(g_a2, "G_A2_per_degree_effect", width = 11, height = 7)
  cat("[G_A2] DONE\n")
} else {
  cat("[G_A2] SKIPPED\n")
}


# ------------------------------------------------------------------------------
# G_A3 -- RH_eff vs T_base SCATTER
# High T + low RH = high dVPD/dT = larger VPD-driven damage
# Point size = VPD damage magnitude
# ------------------------------------------------------------------------------

cat("\n[G_A3] RH_eff vs T_base scatter...\n")

if (nrow(coef_df) > 0) {
  
  coef_plot <- coef_df %>%
    dplyr::mutate(crop_factor = factor(crop, levels = urun_listesi))
  
  g_a3 <- ggplot(coef_plot,
                 aes(x = T_base_C, y = RH_eff_pct,
                     color = crop_factor, size = abs(b_VPD_sc))) +
    geom_point(alpha = 0.85) +
    geom_text(aes(label = crop),
              vjust = -1.1, size = 3.0, show.legend = FALSE) +
    scale_color_manual(
      values = renk_crop[names(renk_crop) %in% coef_plot$crop],
      guide  = "none") +
    scale_size_continuous(
      range = c(3, 9),
      name  = "|b_VPD x dVPD/dT|\n(damage magnitude)") +
    labs(
      title    = "G_A3: Flowering Temperature vs Effective Relative Humidity",
      subtitle = paste0("Point size = VPD damage magnitude (|b_VPD x dVPD/dT|)\n",
                        "High T + Low RH = high dVPD/dT = larger VPD-driven damage"),
      caption  = paste0("Clausius-Clapeyron: dVPD/dT = des/dT x (1 - RH_eff) | ",
                        "Allen et al. (1998)"),
      x = "Mean Flowering Temperature (degC)",
      y = "Effective Relative Humidity (%%)") +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          legend.position  = "bottom",
          plot.title       = element_text(face = "bold", size = 12),
          plot.subtitle    = element_text(size = 8, color = "grey40"),
          plot.caption     = element_text(size = 7.5, color = "grey50"),
          plot.margin      = margin(8, 12, 8, 8))
  save_plot(g_a3, "G_A3_RH_vs_Tbase", width = 10, height = 7)
  cat("[G_A3] DONE\n")
} else {
  cat("[G_A3] SKIPPED\n")
}


# ------------------------------------------------------------------------------
# G_A4 -- dVPD per +1 degC BAR CHART (Clausius-Clapeyron transparency)
# Shows conversion factor used to translate b_VPD to per-degree effect
# ------------------------------------------------------------------------------

cat("\n[G_A4] dVPD per +1 degC bar chart...\n")

if (nrow(coef_df) > 0) {
  
  cc_plot <- coef_df %>%
    dplyr::mutate(crop = factor(as.character(crop),
                                levels = urun_listesi[urun_listesi %in% crop]))
  mean_dv <- mean(coef_df$dVPD_per_1C, na.rm = TRUE)
  
  g_a4 <- ggplot(cc_plot,
                 aes(x = reorder(crop, dVPD_per_1C),
                     y = dVPD_per_1C, fill = crop)) +
    geom_col(alpha = 0.80, width = 0.65) +
    geom_text(aes(label = sprintf("%.4f kPa", dVPD_per_1C)),
              hjust = -0.1, size = 3.2) +
    geom_hline(yintercept = mean_dv, linetype = "dashed",
               color = "grey40", linewidth = 0.7) +
    annotate("text", x = 0.6, y = mean_dv + 0.002,
             label = sprintf("mean=%.4f", mean_dv),
             color = "grey40", size = 2.8, hjust = 0) +
    coord_flip() +
    scale_fill_manual(
      values = renk_crop[names(renk_crop) %in%
                           as.character(cc_plot$crop)],
      guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
    labs(
      title    = "G_A4: Clausius-Clapeyron Conversion -- dVPD per +1 degC by Crop",
      subtitle = paste0("dVPD/dT = des/dT x (1 - RH_eff) | ",
                        "Computed from observed flowering T and vapour pressure\n",
                        "Larger value = same +1 degC warming causes larger VPD increase"),
      caption  = "Allen et al. (1998) FAO-56 | CRU TS4.09 vap data",
      x = NULL, y = "dVPD per +1 degC (kPa)") +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor   = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.y        = element_text(size = 11, face = "bold"),
          plot.title         = element_text(face = "bold", size = 12),
          plot.subtitle      = element_text(size = 8, color = "grey40"),
          plot.caption       = element_text(size = 7.5, color = "grey50"),
          plot.margin        = margin(8, 12, 8, 8))
  save_plot(g_a4, "G_A4_dVPD_per_degree", width = 10, height = 7)
  cat("[G_A4] DONE\n")
} else {
  cat("[G_A4] SKIPPED\n")
}


# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

cat("\n", strrep("=", 75), "\n")
cat("STEP 10 SUMMARY\n")
cat(strrep("=", 75), "\n\n")

if (nrow(coef_df) > 0) {
  cat("--- Coefficient Comparison ---\n")
  cat(sprintf("%-10s  %9s  %5s  %9s  %5s  %8s  %8s\n",
              "Crop","b_TMX_M2","sig","b_VPD_sc","sig",
              "dVPD/1C","RH_eff%%"))
  cat(strrep("-", 66), "\n")
  for (i in seq_len(nrow(coef_df))) {
    r <- coef_df[i, ]
    cat(sprintf("%-10s  %+9.5f  %-5s  %+9.5f  %-5s  %8.4f  %8.1f\n",
                r$crop, r$b_TMX_M2, r$sig_TMX_M2,
                r$b_VPD_sc, r$sig_VPD,
                r$dVPD_per_1C, r$RH_eff_pct))
  }
  cat("\n")
}

cat("--- T+2C Scenario (TMX vs VPD) ---\n")
cat(sprintf("%-10s  %9s  %9s  %7s\n",
            "Crop","TMX (%%)","VPD (%%)","Diff (pp)"))
cat(strrep("-", 42), "\n")
sub2 <- scen_df[as.character(scen_df$scen_id) == "T+2C", ]
for (i in seq_len(nrow(sub2)))
  cat(sprintf("%-10s  %9.1f  %9.1f  %7.1f\n",
              as.character(sub2$crop[i]),
              sub2$pt_TMX[i], sub2$pt_VPD[i], sub2$diff_pp[i]))


# ==============================================================================
# SAVE RESULTS TO EXCEL
# ==============================================================================

cat("\nSaving adim10_vpd_tmx.xlsx...\n")

writexl::write_xlsx(
  list(
    ScenarioComparison = scen_df %>%
      dplyr::mutate(crop    = as.character(crop),
                    scen_id = as.character(scen_id)) %>%
      dplyr::arrange(crop, delta_T),
    CoefComparison     = coef_df,
    CC_Conversion      = if (nrow(coef_df) > 0)
      coef_df %>%
      dplyr::select(crop, T_base_C, dVPD_per_1C,
                    RH_eff_pct, b_VPD, b_VPD_sc, irr_in_vpd)
    else data.frame()),
  file.path(ana_yol, "adim10_vpd_tmx.xlsx"))

cat("[SAVED] adim10_vpd_tmx.xlsx\n")


# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n", strrep("=", 65), "\n")
cat("STEP 10 COMPLETE\n")
cat(strrep("=", 65), "\n\n")

cat("KEY FIXES vs original:\n")
cat("  [1] IRR added to VPD model (matches M2 structure)\n")
cat("  [2] M1 PW also reported for comparison\n")
cat("  [3] All CI from MC -- not analytic delta method\n\n")

cat("COMPLETED:\n")
cat("  Part 1A: vap raster loaded, time sequence built\n")
cat("  Part 1B: country-year vap extracted, VPD computed\n")
cat("  Part 2 : VPD model fitted, MC scenarios run\n\n")

cat("PLOTS (each printed labeled + unlabeled on screen):\n")
cat("  G_A1 : TMX vs VPD scenario comparison (facet)\n")
cat("  G_A2 : Per-degree effect forest (TMX vs VPD)\n")
cat("  G_A3 : RH_eff vs T_base scatter\n")
cat("  G_A4 : dVPD per +1 degC bar chart\n\n")

cat("FILES SAVED (6 per plot = 24 total):\n")
cat("  figures/labeled/   -- PDF + 300dpi PNG + 600dpi PNG\n")
cat("  figures/unlabeled/ -- PDF + 300dpi PNG + 600dpi PNG\n\n")

cat("INTERMEDIATE OUTPUT:\n")
cat("  adim10b_vpd_df.rds -- VPD panel data (used by paper2_step_vpd_gap.R)\n\n")

cat("OUTPUT: adim10_vpd_tmx.xlsx\n")
cat("  Sheets: ScenarioComparison, CoefComparison, CC_Conversion\n\n")

cat("NEXT STEP: paper1_step11_appendix.R\n")
cat(strrep("=", 65), "\n")


















# ==============================================================================
# PAPER 1 -- STEP 11: PUBLICATION TABLES, FIGURES, AND CMIP6 PROJECTIONS
# Temperature, VPD, and Global Crop Yields: Evidence from a Multi-Crop Panel
#
# Reads:
#   modeller_v4_objects.rds       (from step03)
#   adim3_v5_sonuc.xlsx           (AllCoefficients sheet, for CMIP6)
#   pan7*.xlsx                    (for CMIP6 baseline TMX)
#   cmip6_raw/climate/wc2.1_10m/ (CMIP6 GCM rasters)
#   GADM country shapefiles
#
# Contents:
#   Part A -- Publication Tables
#     Table 1: Coefficient table (wheat/maize/soybean/sorghum, M2 PW)
#     Table 2: Scenario results (+1C/+2C/+4C, M2 PW)
#
#   Part B -- Publication Figures (5 figures, hardcoded from step03 results)
#     Figure 1: TMX coefficient forest plot (all 8 crops)
#     Figure 2: Crop ranking bar chart with +2C loss labels
#     Figure 3: Scenario projections line plot (+1C to +4C)
#     Figure 4: TMX vs VPD model comparison at +2C
#     Figure 5: Structural break -- rice vs sorghum pre/post-1990
#
#   Part C -- CMIP6 Projections
#     1. File check
#     2. Load coefficients from adim3_v5_sonuc.xlsx
#     3. Load panel countries + baseline TMX
#     4. Country boundary fallback: rnaturalearth -> geodata -> 0.5deg buffer
#     5. Extract CMIP6 TMX per country-crop-GCM-SSP-period
#     6. Monte Carlo projections (N = 10,000)
#     7. Global weighted aggregation
#     Plots: P1 global trajectory, P2 SSP585 bar, P3 model robustness
#
# UTF-8 NOTE: All special characters replaced with ASCII
#   degree sign  -> degC or C
#   superscripts -> sq
#   em dash      -> --
#
# References:
#   Lobell et al. (2011, Science) | Zhao et al. (2017, PNAS)
#   Allen et al. (1998, FAO-56)   | IPCC AR6 (2021)
# ==============================================================================


# ==============================================================================
# 0. SETUP
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)
library(readxl)
library(fixest)

ana_yol  <- "C:/Users/Alper.Tosun/Desktop/cru130"

urun_listesi <- c("wheat","barley","oats","rye","rice","maize","soybean","sorghum")

# Output directories
fig_labeled   <- file.path(ana_yol, "figures", "labeled")
fig_unlabeled <- file.path(ana_yol, "figures", "unlabeled")
dir.create(fig_labeled,   recursive = TRUE, showWarnings = FALSE)
dir.create(fig_unlabeled, recursive = TRUE, showWarnings = FALSE)


# ==============================================================================
# SAVE_PLOT FUNCTION
# ==============================================================================

save_plot <- function(g_labeled, plot_name, width = 10, height = 7) {
  
  g_unlabeled <- g_labeled +
    labs(title    = NULL,
         subtitle = NULL,
         caption  = NULL,
         x        = NULL,
         y        = NULL) +
    theme(legend.title = element_blank(),
          plot.margin  = margin(4, 4, 4, 4))
  
  cat(sprintf("  [SCREEN] %s -- LABELED\n",   plot_name)); print(g_labeled)
  cat(sprintf("  [SCREEN] %s -- UNLABELED\n", plot_name)); print(g_unlabeled)
  
  save_one <- function(g, dir_path) {
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, ".pdf")),
                    plot = g, width = width, height = height, device = "pdf")
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, "_300dpi.png")),
                    plot = g, width = width, height = height,
                    dpi = 300, device = "png")
    ggplot2::ggsave(file.path(dir_path, paste0(plot_name, "_600dpi.png")),
                    plot = g, width = width, height = height,
                    dpi = 600, device = "png")
  }
  save_one(g_labeled,   fig_labeled)
  save_one(g_unlabeled, fig_unlabeled)
  
  cat(sprintf("  [SAVED]  %s -> labeled/ + unlabeled/ (PDF + 300 + 600 dpi)\n\n",
              plot_name))
  invisible(NULL)
}


# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

sig_s <- function(p)
  ifelse(is.na(p), "",
         ifelse(p < 0.01, "***",
                ifelse(p < 0.05, "**",
                       ifelse(p < 0.10, "*", ""))))

format_coef <- function(var, b, se, p) {
  stars <- sig_s(p)
  if (var == "TMX") {
    list(coef = sprintf("%.3f%s", b, stars),
         se   = sprintf("(%.3f)", se))
  } else if (var == "PRE") {
    list(coef = sprintf("%.4f%s", b, stars),
         se   = sprintf("(%.4f)", se))
  } else if (var == "PRE_sq") {
    list(coef = paste0(formatC(b, format = "e", digits = 2), stars),
         se   = paste0("(", formatC(se, format = "e", digits = 2), ")"))
  } else if (var == "IRR") {
    list(coef = sprintf("%.3f%s", b, stars),
         se   = sprintf("(%.3f)", se))
  } else {
    list(coef = "", se = "")
  }
}

crop_colors <- c(
  wheat   = "#1A5276", barley  = "#2E86C1",
  oats    = "#27AE60", rye     = "#1E8449",
  rice    = "#F39C12", maize   = "#E67E22",
  soybean = "#8E44AD", sorghum = "#C0392B")

crop_colors_fig <- c(
  "Wheat" = "#E69F00", "Barley" = "#56B4E9", "Oats" = "#009E73",
  "Rye"   = "#F0E442", "Rice"   = "#0072B2", "Maize" = "#D55E00",
  "Soybean" = "#CC79A7", "Sorghum" = "#999999")

crop_order_en <- c("Wheat","Barley","Oats","Rye","Rice","Maize","Soybean","Sorghum")

theme_pub <- function(base_size = 12)
  theme_classic(base_size = base_size) +
  theme(
    text               = element_text(family = "sans"),
    plot.title         = element_text(face = "bold", size = base_size + 1),
    plot.subtitle      = element_text(color = "grey40", size = base_size - 1),
    axis.text          = element_text(size = base_size - 1),
    axis.title         = element_text(size = base_size),
    legend.text        = element_text(size = base_size - 1),
    legend.title       = element_text(size = base_size - 1, face = "bold"),
    legend.position    = "bottom",
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    plot.caption       = element_text(color = "grey50", size = base_size - 3),
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA))


# ==============================================================================
# LOAD RDS
# ==============================================================================

tum_modeller <- readRDS(file.path(ana_yol, "modeller_v4_objects.rds"))
cat("[LOADED] modeller_v4_objects.rds\n\n")


# ==============================================================================
# PART A -- PUBLICATION TABLES
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("PART A -- PUBLICATION TABLES\n")
cat(strrep("=", 65), "\n\n")


# ------------------------------------------------------------------------------
# TABLE 1 -- COEFFICIENT TABLE (wheat/maize/soybean/sorghum, M2 PW)
# Rows: TMX, (SE), PRE, (SE), PRE_sq, (SE), IRR, (SE),
#       Country FE, Year FE, N, Adj.R2
# ------------------------------------------------------------------------------

cat("--- Table 1: Coefficient table ---\n\n")

secili_urunler <- c("wheat","maize","soybean","sorghum")
coef_list_t1   <- list()
ozet_list_t1   <- list()

for (urun in secili_urunler) {
  mod <- tum_modeller[[urun]]$all_mods[["M2_PW"]]
  if (is.null(mod)) { cat(sprintf("[SKIP] %s M2_PW\n", urun)); next }
  
  cf <- coef(mod)
  vc <- vcov(mod)
  
  for (v in c("TMX","PRE","PRE_sq","IRR")) {
    if (v %in% names(cf)) {
      b  <- as.numeric(cf[v])
      se <- sqrt(as.numeric(vc[v, v]))
      z  <- b / se
      p  <- 2 * pnorm(-abs(z))
      fmt <- format_coef(v, b, se, p)
      coef_list_t1[[paste0(urun, "_", v)]] <- data.frame(
        crop = urun, var = v,
        coef = fmt$coef, se = fmt$se,
        stringsAsFactors = FALSE)
    } else {
      coef_list_t1[[paste0(urun, "_", v)]] <- data.frame(
        crop = urun, var = v, coef = "", se = "",
        stringsAsFactors = FALSE)
    }
  }
  
  ozet_list_t1[[urun]] <- data.frame(
    crop  = urun,
    N     = as.integer(stats::nobs(mod)),
    AdjR2 = round(as.numeric(fixest::r2(mod, "ar2")), 3),
    stringsAsFactors = FALSE)
}

coef_df_t1 <- dplyr::bind_rows(coef_list_t1)
ozet_df_t1 <- dplyr::bind_rows(ozet_list_t1)

# Build formatted table
rows_t1 <- c("TMX","", "PRE","", "PRE_sq","", "IRR","",
             "Country FE","Year FE","Observations","Adj. R2")
table1   <- data.frame(Variable = rows_t1, stringsAsFactors = FALSE)
col_names <- c("Variable","Wheat","Maize","Soybean","Sorghum")

for (urun in secili_urunler) {
  sub <- coef_df_t1[coef_df_t1$crop == urun, ]
  oz  <- ozet_df_t1[ozet_df_t1$crop == urun, ]
  vals <- c(
    sub$coef[sub$var == "TMX"],    sub$se[sub$var == "TMX"],
    sub$coef[sub$var == "PRE"],    sub$se[sub$var == "PRE"],
    sub$coef[sub$var == "PRE_sq"], sub$se[sub$var == "PRE_sq"],
    sub$coef[sub$var == "IRR"],    sub$se[sub$var == "IRR"],
    "Yes","Yes",
    as.character(oz$N),
    sprintf("%.3f", oz$AdjR2))
  table1[[tools::toTitleCase(urun)]] <- vals
}
colnames(table1) <- col_names

cat("\n=== TABLE 1 ===\n")
print(table1, row.names = FALSE)


# ------------------------------------------------------------------------------
# TABLE 2 -- SCENARIO RESULTS (+1C/+2C/+4C, M2 PW)
# ------------------------------------------------------------------------------

cat("\n--- Table 2: Scenario results ---\n\n")

delta_list    <- c(1, 2, 4)
senaryo_list  <- list()

for (urun in urun_listesi) {
  mod <- tum_modeller[[urun]]$all_mods[["M2_PW"]]
  if (is.null(mod)) next
  cf <- tryCatch(coef(mod), error = function(e) NULL)
  vc <- tryCatch(vcov(mod), error = function(e) NULL)
  if (is.null(cf) || is.null(vc) || !"TMX" %in% names(cf)) next
  
  beta <- as.numeric(cf["TMX"])
  se   <- sqrt(as.numeric(vc["TMX","TMX"]))
  
  for (dT in delta_list) {
    pt  <- (exp(beta * dT) - 1) * 100
    lo  <- (exp((beta - 1.96 * se) * dT) - 1) * 100
    hi  <- (exp((beta + 1.96 * se) * dT) - 1) * 100
    senaryo_list[[paste0(urun, "_", dT)]] <- data.frame(
      crop      = urun,
      deltaC    = dT,
      scenario  = paste0("+", dT, "C"),
      beta_tmx  = round(beta, 5),
      se_tmx    = round(se,   5),
      point_pct = round(pt,   1),
      lo95_pct  = round(lo,   1),
      hi95_pct  = round(hi,   1),
      stringsAsFactors = FALSE)
  }
}

senaryo_df <- dplyr::bind_rows(senaryo_list)

table2_main <- senaryo_df %>%
  dplyr::select(crop, scenario, point_pct) %>%
  tidyr::pivot_wider(names_from = scenario, values_from = point_pct) %>%
  dplyr::mutate(crop = factor(crop, levels = urun_listesi)) %>%
  dplyr::arrange(crop) %>%
  dplyr::mutate(crop = as.character(crop))

table2_ci <- senaryo_df %>%
  dplyr::mutate(result_ci = paste0(
    point_pct, "% [", lo95_pct, ", ", hi95_pct, "]")) %>%
  dplyr::select(crop, scenario, result_ci) %>%
  tidyr::pivot_wider(names_from = scenario, values_from = result_ci) %>%
  dplyr::mutate(crop = factor(crop, levels = urun_listesi)) %>%
  dplyr::arrange(crop) %>%
  dplyr::mutate(crop = as.character(crop))

cat("\n=== TABLE 2 MAIN ===\n");  print(table2_main, row.names = FALSE)
cat("\n=== TABLE 2 WITH 95%% CI ===\n"); print(table2_ci, row.names = FALSE)

# Save tables to Excel
writexl::write_xlsx(
  list(Table1           = table1,
       Scenario_Long    = senaryo_df,
       Table2_Main      = table2_main,
       Table2_With_95CI = table2_ci),
  file.path(ana_yol, "paper1_publication_tables.xlsx"))
cat("[SAVED] paper1_publication_tables.xlsx\n\n")


# ==============================================================================
# PART B -- PUBLICATION FIGURES (hardcoded from step03 results)
# These are the 5 main manuscript figures using fixed coefficients.
# To regenerate from live RDS, replace hardcoded values with RDS extraction.
# ==============================================================================

cat(strrep("=", 65), "\n")
cat("PART B -- PUBLICATION FIGURES\n")
cat(strrep("=", 65), "\n\n")

# Hardcoded M2 PW coefficients (from adim3 results)
coef_pub <- data.frame(
  crop = urun_listesi,
  b    = c(-0.0251,-0.0376,-0.0319,-0.0359,
           -0.0342,-0.0509,-0.0494,-0.0547),
  se   = c( 0.0075, 0.0078, 0.0049, 0.0099,
            0.0110, 0.0097, 0.0116, 0.0078),
  stringsAsFactors = FALSE) %>%
  dplyr::mutate(lo95 = b - 1.96 * se,
                hi95 = b + 1.96 * se,
                crop = factor(crop, levels = rev(urun_listesi)))

# Hardcoded scenario data
scen_pub <- data.frame(
  Crop = rep(crop_order_en, 4),
  Scenario = rep(c("+1C","+2C","+3C","+4C"), each = 8),
  YieldChange = c(
    -2.5,-3.7,-3.1,-3.5,-3.4,-5.0,-4.8,-5.3,
    -4.9,-7.2,-6.2,-6.9,-6.6,-9.7,-9.4,-10.4,
    -7.3,-10.7,-9.1,-10.2,-9.8,-14.2,-13.8,-15.1,
    -9.6,-14.0,-12.0,-13.4,-12.8,-18.4,-18.0,-19.7),
  lo95 = c(
    -4.9,-5.1,-4.0,-5.2,-5.3,-6.7,-6.7,-6.7,
    -7.6,-10.1,-7.9,-10.4,-10.5,-13.1,-13.4,-13.1,
    -10.4,-15.0,-11.6,-15.2,-15.6,-19.2,-19.7,-19.3,
    -14.3,-19.0,-15.1,-19.7,-20.0,-24.5,-25.1,-24.8),
  hi95 = c(
    -0.9,-2.2,-2.2,-1.7,-1.2,-3.1,-2.6,-3.8,
    -2.0,-4.3,-4.4,-3.3,-2.4,-6.2,-5.2,-7.6,
    -4.0,-6.2,-6.3,-4.8,-3.5,-9.0,-7.7,-10.8,
    -4.0,-8.3,-8.6,-6.3,-4.6,-11.8,-9.9,-14.6),
  stringsAsFactors = FALSE) %>%
  dplyr::mutate(
    Crop     = factor(Crop, levels = rev(crop_order_en)),
    Scenario = factor(Scenario, levels = c("+1C","+2C","+3C","+4C")))

# VPD comparison data (hardcoded)
vpd_pub <- data.frame(
  crop     = rep(urun_listesi, 2),
  model    = rep(c("TMX Model","VPD Model"), each = 8),
  point_2c = c(-4.90,-7.25,-6.19,-6.94,-6.61,-9.67,-9.41,-10.36,
               -2.8, -4.6, -3.7, -4.8, -3.3, -5.4, -5.8,  -5.3),
  stringsAsFactors = FALSE) %>%
  dplyr::mutate(crop = factor(crop, levels = rev(urun_listesi)))

# Structural break data (rice and sorghum)
break_pub <- data.frame(
  crop   = rep(c("Rice","Sorghum"), each = 2),
  period = rep(c("Pre-1990","Post-1990"), 2),
  b      = c(-0.061,-0.015,-0.036,-0.090),
  se     = c( 0.018, 0.022, 0.009, 0.012),
  stringsAsFactors = FALSE) %>%
  dplyr::mutate(
    lo95   = b - 1.96 * se,
    hi95   = b + 1.96 * se,
    loss2c = sprintf("%.1f%%", (exp(b * 2) - 1) * 100),
    period = factor(period, levels = c("Pre-1990","Post-1990")),
    crop   = factor(crop,   levels = c("Rice","Sorghum")))


# ------------------------------------------------------------------------------
# FIGURE 1 -- TMX COEFFICIENT FOREST PLOT (all 8 crops)
# ------------------------------------------------------------------------------

cat("[Figure 1] TMX coefficient forest...\n")

fig1 <- ggplot(coef_pub, aes(x = b, y = crop, color = crop)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey40", linewidth = 0.7) +
  geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                 height = 0.25, linewidth = 0.9) +
  geom_point(size = 3.5) +
  scale_color_manual(values = crop_colors, guide = "none") +
  scale_x_continuous(breaks = seq(-0.07, 0, by = 0.01),
                     labels = function(x) sprintf("%.2f", x)) +
  labs(
    title    = "Figure 1: Temperature Effects Across Crops",
    subtitle = "Baseline model (M2, production-weighted) | 95% CI",
    x        = "TMX coefficient [delta ln(yield) per 1 degC]",
    y        = NULL,
    caption  = "Two-way FE | Country-clustered SE") +
  theme_pub()
save_plot(fig1, "Figure1_TMX_forest", width = 9, height = 6)
cat("[Figure 1] DONE\n")


# ------------------------------------------------------------------------------
# FIGURE 2 -- CROP RANKING BAR CHART WITH +2C LOSS LABELS
# ------------------------------------------------------------------------------

cat("\n[Figure 2] Crop ranking bar...\n")

bar_pub <- coef_pub %>%
  dplyr::mutate(
    crop_ranked = reorder(crop, b),
    loss_2c     = (exp(b * 2) - 1) * 100,
    lbl         = sprintf("%.1f%%", loss_2c),
    lbl_x       = b / 2)

fig2 <- ggplot(bar_pub, aes(x = b, y = crop_ranked, fill = crop)) +
  geom_vline(xintercept = 0, color = "grey40",
             linewidth = 0.7, linetype = "dashed") +
  geom_col(width = 0.65, alpha = 0.90) +
  geom_text(aes(x = lbl_x, label = lbl),
            hjust = 0.5, size = 3.4, fontface = "bold", color = "white") +
  scale_fill_manual(values = crop_colors, guide = "none") +
  scale_x_continuous(breaks = seq(-0.07, 0, by = 0.01),
                     labels = function(x) sprintf("%.2f", x)) +
  labs(
    title    = "Figure 2: Crop Ranking by Temperature Sensitivity",
    subtitle = "Labels show implied yield losses under +2 degC warming",
    x        = "TMX coefficient [delta ln(yield) per 1 degC]",
    y        = NULL,
    caption  = "M2 production-weighted FE | Country-clustered SE") +
  theme_pub()
save_plot(fig2, "Figure2_crop_ranking", width = 9, height = 6)
cat("[Figure 2] DONE\n")


# ------------------------------------------------------------------------------
# FIGURE 3 -- SCENARIO PROJECTIONS LINE PLOT (+1C to +4C)
# ------------------------------------------------------------------------------

cat("\n[Figure 3] Scenario projections line...\n")

scen_cols <- c("+1C" = "#2471A3", "+2C" = "#E67E22",
               "+3C" = "#C0392B", "+4C" = "#6C3483")

# Line plot version
fig3_line_df <- data.frame(
  Crop = rep(crop_order_en, 4),
  Scenario = rep(c("+1C","+2C","+3C","+4C"), each = 8),
  YieldChange = c(
    -2.5,-3.7,-3.1,-3.5,-3.4,-5.0,-4.8,-5.3,
    -4.9,-7.2,-6.2,-6.9,-6.6,-9.7,-9.4,-10.4,
    -7.3,-10.7,-9.1,-10.2,-9.8,-14.2,-13.8,-15.1,
    -9.6,-14.0,-12.0,-13.4,-12.8,-18.4,-18.0,-19.7),
  stringsAsFactors = FALSE) %>%
  dplyr::mutate(
    Scenario = factor(Scenario, levels = c("+1C","+2C","+3C","+4C")),
    Crop     = factor(Crop,     levels = crop_order_en))

fig3 <- ggplot(fig3_line_df,
               aes(x = Scenario, y = YieldChange,
                   color = Crop, group = Crop)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray50") +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = crop_colors_fig) +
  scale_y_continuous(name   = "Projected Yield Change (%%)",
                     limits = c(-22, 2),
                     breaks = seq(-20, 0, 5)) +
  scale_x_discrete(name = "Warming Scenario") +
  labs(
    title    = "Figure 3: Yield Projections Under Warming Scenarios",
    subtitle = "M2 Production-Weighted Estimates",
    caption  = "M2 PW two-way FE | 95%% CI from 10,000 MC bootstrap") +
  theme_pub() +
  theme(legend.position = "right")
save_plot(fig3, "Figure3_scenarios_lines", width = 9, height = 6)
cat("[Figure 3] DONE\n")


# ------------------------------------------------------------------------------
# FIGURE 4 -- TMX vs VPD MODEL COMPARISON AT +2C
# ------------------------------------------------------------------------------

cat("\n[Figure 4] TMX vs VPD comparison...\n")

vpd_wide <- vpd_pub %>%
  tidyr::pivot_wider(names_from = model, values_from = point_2c)

fig4 <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey40", linewidth = 0.6) +
  geom_segment(
    data = vpd_wide,
    aes(x = `VPD Model`, xend = `TMX Model`, y = crop, yend = crop),
    color = "grey65", linewidth = 0.7) +
  geom_point(
    data = vpd_pub,
    aes(x = point_2c, y = crop, color = model, shape = model),
    size = 3.5) +
  scale_color_manual(
    values = c("TMX Model" = "darkorange", "VPD Model" = "#2471A3"),
    name   = NULL) +
  scale_shape_manual(
    values = c("TMX Model" = 16, "VPD Model" = 17),
    name   = NULL) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     breaks = seq(-12, 0, by = 2)) +
  labs(
    title    = "Figure 4: Temperature versus Atmospheric Dryness",
    subtitle = "Projected yield losses under +2 degC warming: TMX vs VPD models",
    x        = "Projected yield change (%)",
    y        = NULL,
    caption  = "TMX: M2 PW | VPD: Clausius-Clapeyron, IRR controlled") +
  theme_pub()
save_plot(fig4, "Figure4_TMX_vs_VPD", width = 9, height = 6)
cat("[Figure 4] DONE\n")


# ------------------------------------------------------------------------------
# FIGURE 5 -- STRUCTURAL BREAK: RICE vs SORGHUM PRE/POST-1990
# ------------------------------------------------------------------------------

cat("\n[Figure 5] Structural break rice vs sorghum...\n")

per_colors <- c("Pre-1990" = "#2471A3", "Post-1990" = "#C0392B")

fig5 <- ggplot(break_pub, aes(x = b, y = period, color = period)) +
  annotate("rect", xmin = -0.15, xmax = -0.05,
           ymin = -Inf, ymax = Inf,
           fill = "#FADBD8", alpha = 0.45) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.7) +
  geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                 height = 0.22, linewidth = 1.5) +
  geom_point(size = 5) +
  geom_text(aes(label = paste0(loss2c, " at +2 degC")),
            vjust = -1.3, hjust = 0.5, size = 3.3,
            fontface = "bold", show.legend = FALSE) +
  facet_wrap(~crop, ncol = 1) +
  scale_color_manual(values = per_colors, guide = "none") +
  scale_x_continuous(
    limits = c(-0.155, 0.06),
    breaks = seq(-0.14, 0.04, by = 0.02),
    labels = function(x) sprintf("%.2f", x)) +
  scale_y_discrete(expand = expansion(add = c(0.8, 0.8))) +
  labs(
    title    = "Figure 5: Temporal Change in Temperature Sensitivity",
    subtitle = "Pink band = high-damage zone | Numbers = implied +2 degC yield loss",
    x        = "TMX coefficient [delta ln(yield) per 1 degC]",
    y        = NULL,
    caption  = "M2 PW two-way FE | TMX x post1990 interaction | 95%% CI") +
  theme_pub() +
  theme(
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text       = element_text(face = "bold", size = 13),
    axis.text.y      = element_text(size = 11))
save_plot(fig5, "Figure5_structural_break", width = 9, height = 7)
cat("[Figure 5] DONE\n")


# ==============================================================================
# PART C -- CMIP6 PROJECTIONS
# ==============================================================================

cat("\n", strrep("=", 65), "\n")
cat("PART C -- CMIP6 PROJECTIONS\n")
cat(strrep("=", 65), "\n\n")

library(terra)
library(geodata)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

set.seed(42)

raw_dir  <- file.path(ana_yol, "cmip6_raw", "climate", "wc2.1_10m")
gadm_dir <- file.path(ana_yol, "gadm_cache")
dir.create(gadm_dir, showWarnings = FALSE)

CMIP6_MODELLER <- c("MPI-ESM1-2-HR","IPSL-CM6A-LR","GFDL-ESM4",
                    "MRI-ESM2-0","UKESM1-0-LL")
SENARYOLAR     <- c("245","585")
DONEMLER       <- c("2041-2060","2061-2080","2081-2100")
N_MC_CMIP      <- 10000L

URUN_ADI_EN <- c(wheat="Wheat",barley="Barley",oats="Oats",rye="Rye",
                 rice="Rice",maize="Maize",soybean="Soybean",sorghum="Sorghum")

FENO_GLOBAL <- list(wheat=c(4,5),barley=c(4,5),oats=c(5,6),rye=c(6,7),
                    rice=c(7,8),maize=c(7,8),soybean=c(7,8),sorghum=c(8,9))

PROD_ADLAR <- c("production_tonnes","production","prod_tonnes","prod","tonnes")
ALAN_ADLAR <- c("area_harvested_ha","harvested_area_ha","harvested_area",
                "area_ha","harv_area","area","hectares")

bul_dosya <- function(gcm, ssp, donem) {
  f <- file.path(raw_dir,
                 paste0("wc2.1_10m_tmax_",gcm,"_ssp",ssp,"_",donem,".tif"))
  if (file.exists(f)) f else NULL
}

bul_sutun <- function(df, candidates) {
  isimler <- tolower(names(df))
  for (a in tolower(candidates)) {
    idx <- which(isimler == a)
    if (length(idx) > 0) return(names(df)[idx[1]])
    idx <- which(grepl(a, isimler, fixed = TRUE))
    if (length(idx) > 0) return(names(df)[idx[1]])
  }
  NULL
}

# 3-stage boundary lookup: rnaturalearth -> geodata -> buffer
sinir_cache <- new.env(hash = TRUE)

ne_full <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::mutate(iso_fix = dplyr::case_when(
    iso_a3  != "-99" ~ iso_a3,
    adm0_a3 != "-99" ~ adm0_a3,
    TRUE             ~ name))

sinir_al <- function(iso) {
  if (exists(iso, envir = sinir_cache))
    return(get(iso, envir = sinir_cache))
  sv <- NULL
  ne_row <- ne_full[ne_full$iso_fix == iso, ]
  if (nrow(ne_row) > 0)
    sv <- tryCatch(terra::vect(ne_row), error = function(e) NULL)
  if (is.null(sv))
    sv <- tryCatch({
      g <- geodata::gadm(country = iso, level = 0, path = gadm_dir)
      if (!is.null(g)) g else NULL
    }, error = function(e) NULL)
  if (is.null(sv) && nrow(ne_row) > 0)
    sv <- tryCatch(terra::buffer(terra::vect(ne_row), width = 55000),
                   error = function(e) NULL)
  assign(iso, sv, envir = sinir_cache)
  sv
}

extract_val_cmip <- function(rast_layer, sv) {
  if (!terra::same.crs(rast_layer, sv))
    sv <- tryCatch(terra::project(sv, terra::crs(rast_layer)),
                   error = function(e) NULL)
  if (is.null(sv)) return(NA_real_)
  ex <- tryCatch(terra::extract(rast_layer, sv, fun = "mean", na.rm = TRUE),
                 error = function(e) NULL)
  if (is.null(ex) || ncol(ex) < 2) return(NA_real_)
  v <- as.numeric(ex[1, 2])
  if (is.nan(v) || is.infinite(v)) NA_real_ else v
}

# ── Step C1: File check ──────────────────────────────────────────────────────

cat("STEP C1 -- File check\n\n")

kontrol_df <- expand.grid(gcm = CMIP6_MODELLER, ssp = SENARYOLAR,
                          donem = DONEMLER, stringsAsFactors = FALSE)
kontrol_df$dosya_var <- FALSE
kontrol_df$dosya_yol <- NA_character_

for (i in seq_len(nrow(kontrol_df))) {
  f <- bul_dosya(kontrol_df$gcm[i], kontrol_df$ssp[i], kontrol_df$donem[i])
  if (!is.null(f)) {
    kontrol_df$dosya_var[i] <- TRUE
    kontrol_df$dosya_yol[i] <- f
  }
}

n_indi <- sum(kontrol_df$dosya_var)
cat(sprintf("%d/%d CMIP6 files found\n", n_indi, nrow(kontrol_df)))
if (n_indi == 0) {
  cat("[WARNING] No CMIP6 files found -- Part C skipped\n\n")
} else {
  
  mevcut_gcm <- unique(kontrol_df$gcm[kontrol_df$dosya_var])
  cat("GCMs available:", paste(mevcut_gcm, collapse = ", "), "\n\n")
  
  # ── Step C2: Load coefficients ───────────────────────────────────────────────
  
  cat("STEP C2 -- Load coefficients\n\n")
  
  coef_raw <- tryCatch(
    readxl::read_excel(file.path(ana_yol,"adim3_v5_sonuc.xlsx"),
                       sheet = "AllCoefficients"),
    error = function(e) NULL)
  
  if (is.null(coef_raw)) {
    cat("[WARNING] adim3_v5_sonuc.xlsx not found -- using hardcoded coefs\n")
    katsayilar <- data.frame(
      crop = urun_listesi,
      model_id = "M2_PW",
      beta = c(-0.0251,-0.0376,-0.0319,-0.0359,
               -0.0342,-0.0509,-0.0494,-0.0547),
      se   = c( 0.0075, 0.0078, 0.0049, 0.0099,
                0.0110, 0.0097, 0.0116, 0.0078),
      stringsAsFactors = FALSE)
  } else {
    katsayilar <- coef_raw %>%
      dplyr::filter(variable == "TMX",
                    (model_short == "M1" & weight == "Production-Weighted") |
                      (model_short == "M2" & weight == "Production-Weighted") |
                      (model_short == "M2" & weight == "Area-Weighted")) %>%
      dplyr::mutate(model_id = dplyr::case_when(
        model_short == "M1" & weight == "Production-Weighted" ~ "M1_PW",
        model_short == "M2" & weight == "Production-Weighted" ~ "M2_PW",
        model_short == "M2" & weight == "Area-Weighted"       ~ "M2_AW")) %>%
      dplyr::select(crop, model_id, beta = coef, se)
  }
  cat(sprintf("Coefficients: %d rows\n\n", nrow(katsayilar)))
  
  # ── Step C3: Panel countries + baseline TMX ──────────────────────────────────
  
  cat("STEP C3 -- Panel countries + baseline TMX\n\n")
  
  panel_ulkeler <- list()
  baseline_list <- list()
  
  for (urun in urun_listesi) {
    f <- file.path(ana_yol, paste0("pan7", urun, ".xlsx"))
    if (!file.exists(f)) next
    df <- as.data.frame(readxl::read_excel(f))
    panel_ulkeler[[urun]] <- sort(unique(df$Country_ISO))
    
    df2    <- df %>% dplyr::filter(Yil >= 1990, Yil <= 2020,
                                   !is.na(TMX_floweringmean))
    p_col  <- bul_sutun(df2, PROD_ADLAR)
    a_col  <- bul_sutun(df2, ALAN_ADLAR)
    
    baseline_list[[urun]] <- df2 %>%
      dplyr::group_by(Country_ISO) %>%
      dplyr::summarise(
        tmx_baseline = mean(TMX_floweringmean, na.rm = TRUE),
        prod_mean    = if (!is.null(p_col) && p_col %in% names(df2))
          mean(.data[[p_col]], na.rm = TRUE) else 1,
        area_mean    = if (!is.null(a_col) && a_col %in% names(df2))
          mean(.data[[a_col]], na.rm = TRUE) else 1,
        .groups = "drop") %>%
      dplyr::mutate(crop = urun)
    
    cat(sprintf("[%-8s] %d countries\n", urun, length(panel_ulkeler[[urun]])))
  }
  
  baseline_ulke <- dplyr::bind_rows(baseline_list)
  ulkeler_tum   <- sort(unique(unlist(panel_ulkeler)))
  cat(sprintf("\nTotal unique countries: %d\n\n", length(ulkeler_tum)))
  
  # ── Step C4: Boundary coverage report ───────────────────────────────────────
  
  cat("STEP C4 -- Loading country boundaries\n\n")
  
  ne_buldu     <- ulkeler_tum[ulkeler_tum %in% ne_full$iso_fix]
  gadm_gerekli <- ulkeler_tum[!ulkeler_tum %in% ne_full$iso_fix]
  cat(sprintf("NE match: %d | Need GADM: %d\n",
              length(ne_buldu), length(gadm_gerekli)))
  
  # Pre-cache boundaries
  for (iso in ulkeler_tum) sinir_al(iso)
  cat("[OK] Boundaries cached\n\n")
  
  # ── Step C5: CMIP6 extraction ────────────────────────────────────────────────
  
  cat("STEP C5 -- CMIP6 extraction\n\n")
  
  extraction_cache <- file.path(ana_yol, "cmip6_final_extraction.rds")
  cache_ok <- FALSE
  
  if (file.exists(extraction_cache)) {
    test <- tryCatch(readRDS(extraction_cache), error = function(e) NULL)
    if (!is.null(test) &&
        all(c("gcm","ssp","donem","crop","Country_ISO","tmx_flower") %in%
            names(test)) &&
        nrow(test) > 0) {
      cache_ok <- TRUE
      extraction_df <- test
      cat(sprintf("[Cache OK] %d rows\n\n", nrow(extraction_df)))
    } else {
      file.remove(extraction_cache)
    }
  }
  
  if (!cache_ok) {
    cat("[*] Running extraction...\n\n")
    tum_kayitlar <- list()
    
    for (i in seq_len(nrow(kontrol_df))) {
      if (!kontrol_df$dosya_var[i]) next
      gcm   <- kontrol_df$gcm[i]
      ssp   <- kontrol_df$ssp[i]
      donem <- kontrol_df$donem[i]
      cat(sprintf("  [%-20s | SSP%s | %s]\n", gcm, ssp, donem))
      
      r_cmip <- tryCatch(terra::rast(kontrol_df$dosya_yol[i]),
                         error = function(e) { cat("  READ ERROR\n"); NULL })
      if (is.null(r_cmip)) next
      
      for (urun in urun_listesi) {
        urun_ulkeler <- panel_ulkeler[[urun]]
        feno_u       <- FENO_GLOBAL[[urun]]
        n_ok <- 0; n_fail <- 0
        
        for (iso in urun_ulkeler) {
          sv <- sinir_al(iso)
          if (is.null(sv)) { n_fail <- n_fail + 1; next }
          
          tryCatch({
            kat    <- feno_u[feno_u >= 1 & feno_u <= 12]
            r_ulke <- terra::mask(terra::crop(r_cmip, terra::ext(sv)), sv)
            r_flow <- if (length(kat) == 1) r_ulke[[kat]] else
              terra::app(r_ulke[[kat]], "mean")
            tmx_val <- as.numeric(terra::global(r_flow, "mean", na.rm = TRUE))
            
            # Buffer fallback if all NA
            if (is.na(tmx_val) || length(tmx_val) == 0) {
              sv_buf   <- terra::buffer(sv, width = 55000)
              r_ulke2  <- terra::mask(terra::crop(r_cmip, terra::ext(sv_buf)),
                                      sv_buf)
              r_flow2  <- if (length(kat) == 1) r_ulke2[[kat]] else
                terra::app(r_ulke2[[kat]], "mean")
              tmx_val  <- as.numeric(terra::global(r_flow2, "mean",
                                                   na.rm = TRUE))
            }
            
            if (!is.na(tmx_val) && length(tmx_val) > 0) {
              tum_kayitlar[[paste(gcm,ssp,donem,urun,iso,sep="|")]] <-
                data.frame(gcm = gcm, ssp = ssp, donem = donem,
                           crop = urun, Country_ISO = iso,
                           tmx_flower = tmx_val,
                           stringsAsFactors = FALSE)
              n_ok <- n_ok + 1
            } else {
              n_fail <- n_fail + 1
            }
          }, error = function(e) { n_fail <<- n_fail + 1 })
        }
        cat(sprintf("    [%-8s] OK:%d  Fail:%d\n", urun, n_ok, n_fail))
      }
    }
    
    extraction_df <- dplyr::bind_rows(tum_kayitlar)
    saveRDS(extraction_df, extraction_cache)
    cat(sprintf("[SAVED] extraction cache: %d rows\n\n", nrow(extraction_df)))
  }
  
  # ── Step C6: Delta TMX + Monte Carlo ─────────────────────────────────────────
  
  cat("STEP C6 -- Delta TMX + Monte Carlo (N =", N_MC_CMIP, ")\n\n")
  
  delta_df <- extraction_df %>%
    dplyr::left_join(
      baseline_ulke %>%
        dplyr::select(Country_ISO, crop, tmx_baseline, prod_mean, area_mean),
      by = c("Country_ISO","crop")) %>%
    dplyr::filter(!is.na(tmx_baseline), !is.na(tmx_flower)) %>%
    dplyr::mutate(delta = tmx_flower - tmx_baseline)
  
  mc_cache <- file.path(ana_yol, "cmip6_final_mc.rds")
  mc_ok    <- FALSE
  
  if (file.exists(mc_cache)) {
    test_mc <- tryCatch(readRDS(mc_cache), error = function(e) NULL)
    if (!is.null(test_mc) && "yc_median" %in% names(test_mc) &&
        nrow(test_mc) > 0) {
      mc_ok <- TRUE; mc_df <- test_mc
      cat(sprintf("[Cache OK] MC: %d rows\n\n", nrow(mc_df)))
    } else {
      file.remove(mc_cache)
    }
  }
  
  if (!mc_ok) {
    cat("[*] Running MC...\n")
    mc_listesi <- list()
    
    for (mid in c("M1_PW","M2_PW","M2_AW")) {
      for (urun in urun_listesi) {
        k <- katsayilar %>% dplyr::filter(model_id == mid, crop == urun)
        if (nrow(k) == 0) next
        b_mean    <- k$beta; b_se <- k$se
        delta_urun <- delta_df %>% dplyr::filter(crop == urun)
        
        for (iso in panel_ulkeler[[urun]]) {
          for (ssp in SENARYOLAR) {
            for (donem in DONEMLER) {
              d_vals <- delta_urun %>%
                dplyr::filter(Country_ISO == iso,
                              ssp == !!ssp, donem == !!donem) %>%
                dplyr::pull(delta)
              d_vals <- d_vals[!is.na(d_vals)]
              if (length(d_vals) == 0) next
              
              all_yc <- unlist(lapply(d_vals, function(d)
                (exp(rnorm(N_MC_CMIP, b_mean, b_se) * d) - 1) * 100))
              
              mc_listesi[[paste(mid,urun,iso,ssp,donem,sep="|")]] <-
                data.frame(model_id = mid, crop = urun,
                           Country_ISO = iso, ssp = ssp, donem = donem,
                           n_gcm = length(d_vals), delta_mean = mean(d_vals),
                           yc_median = median(all_yc),
                           yc_mean   = mean(all_yc),
                           yc_lo95   = quantile(all_yc, 0.025),
                           yc_hi95   = quantile(all_yc, 0.975),
                           stringsAsFactors = FALSE)
            }
          }
        }
      }
      cat(sprintf("  [%s] done\n", mid))
    }
    
    mc_df <- dplyr::bind_rows(mc_listesi)
    saveRDS(mc_df, mc_cache)
    cat(sprintf("[SAVED] MC cache: %d rows\n\n", nrow(mc_df)))
  }
  
  # ── Step C7: Global weighted aggregation ─────────────────────────────────────
  
  cat("STEP C7 -- Global weighted aggregation\n\n")
  
  hesapla_global <- function(mc_data, bl, model, wcol) {
    mc_data %>%
      dplyr::filter(model_id == model) %>%
      dplyr::left_join(
        bl %>% dplyr::select(Country_ISO, crop, prod_mean, area_mean),
        by = c("Country_ISO","crop")) %>%
      dplyr::mutate(wt = .data[[wcol]]) %>%
      dplyr::group_by(crop, ssp, donem) %>%
      dplyr::summarise(
        yc      = weighted.mean(yc_median, wt, na.rm = TRUE),
        yc_lo95 = weighted.mean(yc_lo95,   wt, na.rm = TRUE),
        yc_hi95 = weighted.mean(yc_hi95,   wt, na.rm = TRUE),
        n_cntry = dplyr::n(),
        .groups = "drop") %>%
      dplyr::mutate(model_id = model, weight_type = wcol)
  }
  
  g_m2_pw <- hesapla_global(mc_df, baseline_ulke, "M2_PW", "prod_mean")
  g_m2_aw <- hesapla_global(mc_df, baseline_ulke, "M2_AW", "area_mean")
  g_m1_pw <- hesapla_global(mc_df, baseline_ulke, "M1_PW", "prod_mean")
  g_all   <- dplyr::bind_rows(g_m2_pw, g_m2_aw, g_m1_pw)
  
  cat("=== M2 PW (SSP585) ===\n")
  g_m2_pw %>%
    dplyr::filter(ssp == "585") %>%
    dplyr::mutate(v = sprintf("%.1f [%.1f,%.1f]", yc, yc_lo95, yc_hi95)) %>%
    dplyr::select(crop, donem, v) %>%
    tidyr::pivot_wider(names_from = donem, values_from = v) %>%
    print()
  
  # Coverage report
  coverage_df <- dplyr::bind_rows(lapply(urun_listesi, function(urun) {
    panel_set <- panel_ulkeler[[urun]]
    cmip6_set <- delta_df %>%
      dplyr::filter(crop == urun) %>%
      dplyr::distinct(Country_ISO) %>%
      dplyr::pull()
    eksik <- panel_set[!panel_set %in% cmip6_set]
    data.frame(crop = urun,
               n_panel   = length(panel_set),
               n_cmip6   = length(cmip6_set),
               n_missing = length(eksik),
               coverage_pct = round(100*length(cmip6_set)/length(panel_set),1),
               missing_iso  = paste(eksik, collapse = ", "),
               stringsAsFactors = FALSE)
  }))
  
  # CMIP6 plots
  theme_cmip <- function(base = 11)
    theme_bw(base_size = base) +
    theme(plot.title    = element_text(face = "bold", hjust = 0.5, size = base + 2),
          plot.subtitle = element_text(hjust = 0.5, color = "grey40", size = base - 1),
          legend.position = "bottom",
          strip.text      = element_text(face = "bold"))
  
  # CMIP6 Plot 1: Global trajectory
  p_cmip1 <- dplyr::bind_rows(
    g_m2_pw %>% dplyr::mutate(wt_lbl = "Prod.-Weighted (M2)"),
    g_m2_aw %>% dplyr::mutate(wt_lbl = "Area-Weighted (M2)")) %>%
    dplyr::mutate(
      crop_en = URUN_ADI_EN[crop],
      ssp_lbl = ifelse(ssp == "245","SSP2-4.5","SSP5-8.5"),
      period  = factor(donem,
                       levels = c("2041-2060","2061-2080","2081-2100"))) %>%
    ggplot(aes(x = period, y = yc,
               color = ssp_lbl, linetype = wt_lbl,
               group = interaction(ssp_lbl, wt_lbl))) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.35) +
    geom_ribbon(
      data = . %>% dplyr::filter(wt_lbl == "Prod.-Weighted (M2)"),
      aes(ymin = yc_lo95, ymax = yc_hi95, fill = ssp_lbl),
      alpha = 0.12, color = NA) +
    geom_line(linewidth = 0.85) +
    geom_point(size = 2.2) +
    facet_wrap(~crop_en, nrow = 2) +
    scale_color_manual(values = c("SSP2-4.5"="#2166AC","SSP5-8.5"="#D6604D")) +
    scale_fill_manual( values = c("SSP2-4.5"="#2166AC","SSP5-8.5"="#D6604D")) +
    scale_linetype_manual(values = c("Prod.-Weighted (M2)" = "solid",
                                     "Area-Weighted (M2)"  = "longdash")) +
    labs(
      title   = "CMIP6-Based Projected Global Yield Change -- 8 Crops",
      subtitle = sprintf("%d GCM Ensemble | Band: MC 95%% CI | Solid: Production-Weighted",
                         length(mevcut_gcm)),
      x = NULL, y = "Yield Change (%%)",
      color = "Scenario", linetype = "Weighting") +
    theme_cmip() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  save_plot(p_cmip1, "CMIP6_P1_global_trajectory", width = 14, height = 9)
  cat("[CMIP6 P1] DONE\n")
  
  # CMIP6 Plot 2: SSP585 bar chart
  p_cmip2 <- g_m2_pw %>%
    dplyr::filter(ssp == "585") %>%
    dplyr::mutate(
      crop_en = URUN_ADI_EN[crop],
      period  = factor(donem,
                       levels = c("2041-2060","2061-2080","2081-2100"),
                       labels = c("2041-2060","2061-2080","2081-2100"))) %>%
    ggplot(aes(x = reorder(crop_en, -yc), y = yc, fill = yc)) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.35) +
    geom_col(width = 0.65, alpha = 0.9) +
    geom_errorbar(aes(ymin = yc_lo95, ymax = yc_hi95),
                  width = 0.3, color = "grey20", linewidth = 0.7) +
    facet_wrap(~period, nrow = 1) +
    scale_fill_gradient2(low     = "#B2182B",
                         mid     = "#F7F7F7",
                         high    = "#1A7837",
                         midpoint = 0, guide = "none") +
    coord_flip() +
    labs(
      title    = "Projected Global Yield Change under SSP5-8.5",
      subtitle = sprintf("M2 Production-Weighted | %d GCMs | Bars: MC 95%% CI",
                         length(mevcut_gcm)),
      x = NULL, y = "Yield Change (%%)") +
    theme_cmip()
  save_plot(p_cmip2, "CMIP6_P2_SSP585_bar", width = 14, height = 7)
  cat("[CMIP6 P2] DONE\n")
  
  # CMIP6 Plot 3: Model robustness
  p_cmip3 <- g_all %>%
    dplyr::filter(ssp == "585", donem == "2081-2100") %>%
    dplyr::mutate(
      crop_en   = URUN_ADI_EN[crop],
      model_lbl = dplyr::recode(
        paste0(model_id, "_",
               ifelse(weight_type == "prod_mean","PW","AW")),
        "M1_PW_PW" = "M1 Prod.-W.",
        "M2_PW_PW" = "M2 Prod.-W. (Main)",
        "M2_AW_AW" = "M2 Area-W.")) %>%
    ggplot(aes(x = reorder(crop_en, yc), y = yc,
               color = model_lbl, shape = model_lbl)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(size = 3, position = position_dodge(width = 0.6)) +
    geom_errorbar(aes(ymin = yc_lo95, ymax = yc_hi95),
                  width = 0.2,
                  position = position_dodge(width = 0.6)) +
    coord_flip() +
    scale_color_manual(values = c("M1 Prod.-W."        = "#74ADD1",
                                  "M2 Prod.-W. (Main)" = "#D6604D",
                                  "M2 Area-W."         = "#4DAC26")) +
    labs(
      title    = "Model Robustness -- Global Projected Yield Change",
      subtitle = sprintf("SSP5-8.5 | 2081-2100 | %d GCMs | Bars: MC 95%% CI",
                         length(mevcut_gcm)),
      x = NULL, y = "Yield Change (%%)",
      color = NULL, shape = NULL) +
    theme_cmip()
  save_plot(p_cmip3, "CMIP6_P3_robustness", width = 10, height = 7)
  cat("[CMIP6 P3] DONE\n")
  
  # Save CMIP6 Excel
  fmt_cmip <- function(df)
    df %>%
    dplyr::mutate(v = sprintf("%.1f [%.1f,%.1f]", yc, yc_lo95, yc_hi95)) %>%
    dplyr::select(crop, ssp, donem, v, n_cntry) %>%
    tidyr::pivot_wider(names_from  = c(ssp, donem),
                       values_from = v, names_sep = "_")
  
  rob_cmip <- g_all %>%
    dplyr::filter(ssp == "585", donem == "2081-2100") %>%
    dplyr::mutate(
      label = paste0(model_id,"_",
                     ifelse(weight_type == "prod_mean","PW","AW")),
      yc    = round(yc, 1)) %>%
    dplyr::select(crop, label, yc) %>%
    tidyr::pivot_wider(names_from = label, values_from = yc)
  
  writexl::write_xlsx(
    list(M2_ProdWeighted = fmt_cmip(g_m2_pw),
         M2_AreaWeighted = fmt_cmip(g_m2_aw),
         Robustness      = rob_cmip,
         Coverage        = coverage_df,
         Global_Full     = g_all %>%
           dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 3)))),
    file.path(ana_yol, "paper1_cmip6_projections.xlsx"))
  cat("[SAVED] paper1_cmip6_projections.xlsx\n\n")
  
} # end if(n_indi > 0)


# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n", strrep("=", 65), "\n")
cat("STEP 11 COMPLETE\n")
cat(strrep("=", 65), "\n\n")

cat("COMPLETED:\n")
cat("  Part A: Publication tables (Table 1 + Table 2)\n")
cat("  Part B: 5 publication figures\n")
cat("  Part C: CMIP6 projections (if files available)\n\n")

cat("PLOTS (each printed labeled + unlabeled, saved x6):\n")
cat("  Figure1  : TMX coefficient forest (all 8 crops)\n")
cat("  Figure2  : Crop ranking bar with +2 degC labels\n")
cat("  Figure3  : Scenario projections +1C to +4C\n")
cat("  Figure4  : TMX vs VPD comparison at +2 degC\n")
cat("  Figure5  : Structural break rice vs sorghum\n")
cat("  CMIP6_P1 : Global trajectory (if CMIP6 files found)\n")
cat("  CMIP6_P2 : SSP5-8.5 bar chart (if CMIP6 files found)\n")
cat("  CMIP6_P3 : Model robustness (if CMIP6 files found)\n\n")

cat("FILES SAVED (6 per plot, up to 48 total):\n")
cat("  figures/labeled/   -- PDF + 300dpi PNG + 600dpi PNG\n")
cat("  figures/unlabeled/ -- PDF + 300dpi PNG + 600dpi PNG\n\n")

cat("OUTPUT:\n")
cat("  paper1_publication_tables.xlsx -- Table1, Table2\n")
cat("  paper1_cmip6_projections.xlsx  -- CMIP6 results (if available)\n\n")

cat("=== PAPER 1 PIPELINE COMPLETE (Steps 01-11) ===\n")
cat(strrep("=", 65), "\n")





















# ============================================================
# GRAPHICAL ABSTRACT — Paper 1
# G_GA_B  : Forest plot — TMX coefficients, 8 crops
# G_GA_C  : Paired dot  — TMX vs VPD gap at +2C
# G_GA_D  : Bar chart   — CMIP6 SSP5-8.5, 3 periods
# G_GA_D2 : Bar chart   — CMIP6 SSP5-8.5, only 2081-2100
#
# Her figur RStudio plots panelinde labeled + unlabeled gosterilir
# Her figur 6 dosya olarak kaydedilir (PDF + 300dpi + 600dpi)
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

ana_yol <- "C:/Users/Alper.Tosun/Desktop/cru130"
setwd(ana_yol)

COL_WARM  <- "#D35400"
COL_COOL  <- "#1B7BA0"
COL_DARK  <- "#1C2B36"
COL_MID   <- "#4A5E6A"
COL_LIGHT <- "#8FA3AD"

warm_crops <- c("Rice", "Maize", "Soybean", "Sorghum")
crop_order <- c("Sorghum","Maize","Soybean","Rice","Barley","Rye","Oats","Wheat")

# ── save_plot: labeled + unlabeled, 6 dosya ──────────────────
save_plot <- function(p_lab, p_unlab, name, w = 7, h = 5.5) {
  dir_l <- file.path(ana_yol, "figures", "labeled")
  dir_u <- file.path(ana_yol, "figures", "unlabeled")
  dir.create(dir_l, recursive = TRUE, showWarnings = FALSE)
  dir.create(dir_u, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(dir_l, paste0(name, ".pdf")),              p_lab,   width=w, height=h, device=cairo_pdf)
  ggsave(file.path(dir_l, paste0(name, "_300.png")),           p_lab,   width=w, height=h, dpi=300)
  ggsave(file.path(dir_l, paste0(name, "_600.png")),           p_lab,   width=w, height=h, dpi=600)
  ggsave(file.path(dir_u, paste0(name, "_unlabeled.pdf")),     p_unlab, width=w, height=h, device=cairo_pdf)
  ggsave(file.path(dir_u, paste0(name, "_unlabeled_300.png")), p_unlab, width=w, height=h, dpi=300)
  ggsave(file.path(dir_u, paste0(name, "_unlabeled_600.png")), p_unlab, width=w, height=h, dpi=600)
  message("Kaydedildi: ", name, " (6 dosya)")
}

# ── Ortak tema ────────────────────────────────────────────────
theme_ga <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "white", color = NA),
      panel.grid.major.x = element_line(color = "#EEEEEE", linewidth = 0.4),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text          = element_text(color = COL_MID, size = 9),
      axis.title         = element_text(color = COL_MID, size = 9),
      legend.position    = "bottom",
      legend.text        = element_text(size = 9, color = COL_MID),
      legend.title       = element_blank(),
      legend.key.size    = unit(0.5, "cm"),
      strip.text         = element_text(face="bold", color="white",
                                        size=9.5, margin=margin(4,4,4,4)),
      strip.background   = element_rect(fill = COL_DARK, color = NA),
      plot.title         = element_text(face="bold", size=11,
                                        color=COL_DARK, margin=margin(b=4)),
      plot.subtitle      = element_text(size=8.5, color=COL_MID,
                                        margin=margin(b=8)),
      plot.caption       = element_text(size=7.5, color=COL_LIGHT,
                                        hjust=0, margin=margin(t=6)),
      plot.margin        = margin(10, 20, 8, 8)
    )
}

# ================================================================
# G_GA_B — Forest Plot: TMX katsayilari, 8 urun
# ================================================================
df_b <- read_excel(file.path(ana_yol, "adim3_v5_sonuc.xlsx"),
                   sheet = "KeyCoefficients") %>%
  filter(variable    == "TMX",
         model_short == "M2",
         weight      == "Production-Weighted") %>%
  mutate(
    crop   = tools::toTitleCase(as.character(crop)),
    crop   = factor(crop, levels = crop_order),
    season = if_else(crop %in% warm_crops, "Warm-season", "Cool-season"),
    pct    = (exp(coef) - 1) * 100,
    pct_lo = (exp(lo95) - 1) * 100,
    pct_hi = (exp(hi95) - 1) * 100
  )

build_b <- function(labeled = TRUE) {
  ggplot(df_b, aes(x = pct, y = crop, color = season)) +
    geom_vline(xintercept = 0, color="#CCCCCC", linewidth=0.4) +
    geom_errorbarh(aes(xmin=pct_lo, xmax=pct_hi), height=0.25, linewidth=0.7) +
    geom_point(size=3.2) +
    geom_text(aes(label=sprintf("%.1f%%", pct), x=pct_hi+0.15),
              hjust=0, size=2.9, fontface="bold") +
    scale_color_manual(values=c("Warm-season"=COL_WARM, "Cool-season"=COL_COOL)) +
    scale_x_continuous(labels=function(x) paste0(x,"%"),
                       expand=expansion(mult=c(0.05, 0.22))) +
    labs(
      x        = "Yield change per 1\u00b0C (flowering-period TMX)",
      y        = NULL,
      title    = if (labeled) "B. Heat Sensitivity Across Eight Crops" else NULL,
      subtitle = if (labeled) "M2, production-weighted TWFE  |  95% CI  |  all p < 0.01" else NULL,
      caption  = if (labeled) "Source: adim3_v5_sonuc.xlsx" else NULL
    ) +
    theme_ga()
}

G_GA_B_labeled   <- build_b(TRUE)
G_GA_B_unlabeled <- build_b(FALSE)

# RStudio plots panelinde goster — her ikisi de
print(G_GA_B_labeled)
print(G_GA_B_unlabeled)

save_plot(G_GA_B_labeled, G_GA_B_unlabeled, "G_GA_B", w=6.5, h=5)

# ================================================================
# G_GA_C — TMX vs VPD Gap (+2C)
# ================================================================
df_c <- read_excel(file.path(ana_yol, "adim10_vpd_tmx.xlsx"),
                   sheet = "ScenarioComparison") %>%
  filter(delta_T == 2) %>%
  mutate(
    crop   = tools::toTitleCase(as.character(crop)),
    crop   = factor(crop, levels = crop_order),
    season = if_else(crop %in% warm_crops, "Warm-season", "Cool-season")
  )

df_c_long <- df_c %>%
  pivot_longer(cols=c(pt_TMX, pt_VPD), names_to="model", values_to="pct") %>%
  mutate(model = recode(model, pt_TMX="TMX model", pt_VPD="VPD model"))

# Gap etiketi — connector ortasinda
df_c_gap <- df_c %>%
  mutate(mid_x = (pt_TMX + pt_VPD) / 2,
         label = paste0(sprintf("%.1f", diff_pp), " pp"))

build_c <- function(labeled = TRUE) {
  ggplot() +
    geom_vline(xintercept=0, color="#CCCCCC", linewidth=0.4) +
    # Connector
    geom_segment(data=df_c,
                 aes(x=pt_VPD, xend=pt_TMX, y=crop, yend=crop),
                 color="#DDDDDD", linewidth=4) +
    # Noktalar: daire=TMX, ucgen=VPD
    geom_point(data=df_c_long,
               aes(x=pct, y=crop, color=model, shape=model), size=3.5) +
    # Gap etiketleri — tum urunler ayni: gri, italic, size 3.8
    geom_text(data=df_c_gap,
              aes(x=mid_x, y=crop, label=label),
              nudge_y=0.40, size=3.8, color="#999999", fontface="italic") +
    scale_color_manual(values=c("TMX model"=COL_WARM, "VPD model"=COL_COOL)) +
    scale_shape_manual(values=c("TMX model"=16, "VPD model"=17)) +
    scale_x_continuous(labels=function(x) paste0(x,"%"),
                       expand=expansion(mult=c(0.05, 0.10))) +
    labs(
      x        = "Projected yield change at +2\u00b0C (%)",
      y        = NULL,
      title    = if (labeled) "C. Temperature Effects Extend Beyond VPD" else NULL,
      subtitle = if (labeled) "At +2\u00b0C, VPD explains only part  |  gap = non-VPD heat damage" else NULL,
      caption  = if (labeled) "Source: adim10_vpd_tmx.xlsx" else NULL
    ) +
    guides(color=guide_legend(override.aes=list(size=3)),
           shape=guide_legend(override.aes=list(size=3))) +
    theme_ga()
}

G_GA_C_labeled   <- build_c(TRUE)
G_GA_C_unlabeled <- build_c(FALSE)

print(G_GA_C_labeled)
print(G_GA_C_unlabeled)

save_plot(G_GA_C_labeled, G_GA_C_unlabeled, "G_GA_C", w=6.5, h=5)

# ================================================================
# CMIP6 veri hazirligi — D ve D2 icin ortak
# ================================================================
parse_val <- function(x) {
  mean_v <- as.numeric(trimws(sub("\\[.*", "", x)))
  ci     <- sub(".*\\[(.*)\\]", "\\1", x)
  parts  <- strsplit(ci, ",")[[1]]
  lo_v   <- as.numeric(trimws(parts[1]))
  hi_v   <- as.numeric(trimws(parts[2]))
  list(mean_c=mean_v, lo_c=lo_v, hi_c=hi_v)
}

raw_cmip <- read_excel(file.path(ana_yol, "paper1_cmip6_projections.xlsx"),
                       sheet = "M2_ProdWeighted")

# dplyr::select — MASS paketi cakismasini onler
df_cmip_all <- raw_cmip %>%
  dplyr::select(crop,
                p1 = `585_2041-2060`,
                p2 = `585_2061-2080`,
                p3 = `585_2081-2100`) %>%
  pivot_longer(cols=c(p1,p2,p3), names_to="period_code", values_to="val_str") %>%
  mutate(
    period = recode(period_code,
                    p1 = "2041-2060",
                    p2 = "2061-2080",
                    p3 = "2081-2100"),
    period = factor(period, levels=c("2041-2060","2061-2080","2081-2100")),
    crop   = tools::toTitleCase(as.character(crop)),
    crop   = factor(crop, levels=crop_order)
  ) %>%
  rowwise() %>%
  mutate(
    mean_c = parse_val(val_str)$mean_c,
    lo_c   = parse_val(val_str)$lo_c,
    hi_c   = parse_val(val_str)$hi_c
  ) %>%
  ungroup() %>%
  mutate(
    sig      = (lo_c > 0 | hi_c < 0),
    fill_col = case_when(
      mean_c < 0 & sig  ~ COL_WARM,   # anlamli kayip
      mean_c < 0 & !sig ~ "#F0B897",  # anlamsiz kayip
      mean_c > 0 & sig  ~ COL_COOL,   # anlamli kazanim
      mean_c > 0 & !sig ~ "#A8D4E8",  # anlamsiz kazanim
      TRUE              ~ "#CCCCCC"
    )
  )

# ================================================================
# G_GA_D — CMIP6, 3 donem yan yana (facet)
# ================================================================
build_d <- function(data, labeled = TRUE) {
  ggplot(data, aes(x=mean_c, y=crop)) +
    geom_vline(xintercept=0, color="#CCCCCC", linewidth=0.5, linetype="dashed") +
    geom_col(aes(fill=fill_col), width=0.65) +
    geom_errorbarh(aes(xmin=lo_c, xmax=hi_c),
                   height=0.25, linewidth=0.5, color=COL_DARK) +
    geom_text(data=filter(data, sig),
              aes(label=paste0(sprintf("%.1f", mean_c), "%"),
                  x    =if_else(mean_c < 0, lo_c - 0.8, hi_c + 0.8),
                  hjust=if_else(mean_c < 0, 1, 0)),
              size=2.4, fontface="bold", color=COL_DARK) +
    facet_wrap(~period, nrow=1) +
    scale_fill_identity() +
    scale_x_continuous(labels=function(x) paste0(x,"%"),
                       expand=expansion(mult=c(0.12, 0.12))) +
    labs(
      x        = "Yield change (%)",
      y        = NULL,
      title    = if (labeled) "D. CMIP6 End-of-Century Projections (SSP5-8.5, 3 Periods)" else NULL,
      subtitle = if (labeled) "4-GCM ensemble  |  M2 production-weighted  |  bars: MC 95% CI" else NULL,
      caption  = if (labeled) "Source: paper1_cmip6_projections.xlsx" else NULL
    ) +
    theme_ga()
}

G_GA_D_labeled   <- build_d(df_cmip_all, TRUE)
G_GA_D_unlabeled <- build_d(df_cmip_all, FALSE)

print(G_GA_D_labeled)
print(G_GA_D_unlabeled)

save_plot(G_GA_D_labeled, G_GA_D_unlabeled, "G_GA_D", w=8.5, h=5)

# ================================================================
# G_GA_D2 — CMIP6, sadece 2081-2100
# ================================================================
df_cmip_2100 <- df_cmip_all %>%
  filter(period == "2081-2100")

build_d2 <- function(data, labeled = TRUE) {
  ggplot(data, aes(x=mean_c, y=crop)) +
    geom_vline(xintercept=0, color="#CCCCCC", linewidth=0.5, linetype="dashed") +
    geom_col(aes(fill=fill_col), width=0.65) +
    geom_errorbarh(aes(xmin=lo_c, xmax=hi_c),
                   height=0.25, linewidth=0.5, color=COL_DARK) +
    # Deger etiketleri: anlamli olanlara
    geom_text(data=filter(data, sig),
              aes(label=paste0(sprintf("%.1f", mean_c), "%"),
                  x    =if_else(mean_c < 0, lo_c - 0.8, hi_c + 0.8),
                  hjust=if_else(mean_c < 0, 1, 0)),
              size=3.0, fontface="bold", color=COL_DARK) +
    scale_fill_identity() +
    scale_x_continuous(labels=function(x) paste0(x,"%"),
                       expand=expansion(mult=c(0.15, 0.15))) +
    labs(
      x        = "Yield change (%), SSP5-8.5",
      y        = NULL,
      title    = if (labeled) "D. CMIP6 End-of-Century Projections (SSP5-8.5, 2081-2100)" else NULL,
      subtitle = if (labeled) "4-GCM ensemble  |  M2 production-weighted  |  bars: MC 95% CI" else NULL,
      caption  = if (labeled) "Source: paper1_cmip6_projections.xlsx" else NULL
    ) +
    theme_ga()
}

G_GA_D2_labeled   <- build_d2(df_cmip_2100, TRUE)
G_GA_D2_unlabeled <- build_d2(df_cmip_2100, FALSE)

print(G_GA_D2_labeled)
print(G_GA_D2_unlabeled)

save_plot(G_GA_D2_labeled, G_GA_D2_unlabeled, "G_GA_D2", w=6.0, h=5)

# ================================================================
message("\n=== TAMAMLANDI ===")
message("G_GA_B  : Forest plot — 8 urun TMX katsayilari")
message("G_GA_C  : Paired dot  — TMX vs VPD gap (+2C)")
message("G_GA_D  : Bar chart   — CMIP6 SSP5-8.5, 3 donem")
message("G_GA_D2 : Bar chart   — CMIP6 SSP5-8.5, sadece 2081-2100")
message("Her figur: figures/labeled/ ve figures/unlabeled/ altinda")
message("Her figur: PDF + 300dpi PNG + 600dpi PNG = 6 dosya")












