# ==============================================================================
# PAPER 1 — STEP 00: DATA PREPARATION PIPELINE
# Temperature, VPD, and Global Crop Yields: Evidence from a Multi-Crop Panel
#
# Pipeline overview:
#   Step 1 : CRU TS4.09 + SPAM2020 → monthly climate panels (pan1*)
#   Step 2 : FAO yield data + phenology → annual panels (panfin_* → pan2*)
#   Step 3 : Extra 4 countries (IRN, EGY, NGA, SDN) → pan3*
#   Step 3b: Egypt patch via rnaturalearth → pan3* update
#   Step 4 : FAOSTAT irrigation data → pan4*
#   Step 5 : Cropland area + irr_ratio → pan5*
#   Step 6 : Aridity indicator (PRE_ann < 400 mm) → pan6*
#   Step 7 : GDP per capita (World Bank WDI) → pan7*
#   Step 7b: GDP backfill fix + duplicate removal → pan7* (overwrite)
#
# Input files (all in ana_yol):
#   CRU_TS4.09_*.nc              — CRU TS4.09 monthly climate grids
#   spam2020_V2r0_global_H_*_A.tif — SPAM2020 harvested area rasters
#   Global_AgriClimate_v5.xlsx   — phenology windows (patch + Excel, unified)
#   Production_Crops_Livestock_E_All_Data_(Normalized).csv — FAOSTAT
#   GLOBAL_ATMOSPHERIC_CO2_NOAA.csv
#   gdp_per_person.csv           — fallback GDP (overridden by WDI in Step 7)
#   FAOSTAT_data_en_3-5-2026.xls — FAOSTAT irrigation data
#   sudanvs.xls                  — FAO data for IRN/EGY/NGA/SDN
#
# Output files: pan7wheat.xlsx ... pan7sorghum.xlsx
#
# Required packages:
#   install.packages(c("terra","geodata","dplyr","readxl","writexl",
#                      "data.table","tidyr","zoo","rnaturalearth",
#                      "rnaturalearthdata","sf","WDI"))
# ==============================================================================


# ==============================================================================
# 0. SETUP — packages and path configuration
# ==============================================================================

# Load terra before dplyr to avoid select() conflicts
library(terra)
library(geodata)
library(dplyr)
library(readxl)
library(writexl)
library(data.table)
library(tidyr)
library(zoo)

# Base directory — adjust if running on a different machine
ana_yol     <- "C:/Users/Alper.Tosun/Desktop/cru130"
iklim_yol   <- file.path(ana_yol, "cru-iklim-veri")       # CRU .nc files
# fenoloji_xl: used ONLY for GDP_Ranking sheet (phenology is now fully inline)
fenoloji_xl <- file.path(ana_yol, "Global_AgriClimate_v5.xlsx")
fao_csv     <- file.path(ana_yol,
                         "Production_Crops_Livestock_E_All_Data_(Normalized).csv")
co2_csv     <- file.path(ana_yol, "GLOBAL_ATMOSPHERIC_CO2_NOAA.csv")
gdp_csv     <- file.path(ana_yol, "gdp_per_person.csv")
sulama_xl   <- file.path(ana_yol, "FAOSTAT_data_en_3-5-2026.xls")
sudanvs_xls <- file.path(ana_yol, "sudanvs.xls")

setwd(ana_yol)


# ==============================================================================
# 1. COUNTRY LIST — 130 countries from GDP ranking sheet
# ==============================================================================

gdp_sheet   <- read_excel(fenoloji_xl, sheet = "GDP_Ranking")
names(gdp_sheet) <- gsub("[\n\r]", " ", names(gdp_sheet))  # clean column names
iso_col     <- grep("ISO", names(gdp_sheet), value = TRUE)[1]
ulkeler_130 <- gdp_sheet[[iso_col]]
ulkeler_130 <- ulkeler_130[!is.na(ulkeler_130)]
cat("[OK]", length(ulkeler_130), "countries loaded\n\n")


# ==============================================================================
# 2. CROP CONFIGURATION — SPAM code, phenology sheet, FAO item names, Tbase
# ==============================================================================

urun_config <- list(
  wheat   = list(spam = "WHEA", xl = "Wheat",
                 p1   = "pan1wheat",
                 fao  = c("Wheat"),                       tbase = 0),
  barley  = list(spam = "WHEA", xl = "Barley",
                 p1   = "pan1barley",
                 fao  = c("Barley"),                      tbase = 0),
  oats    = list(spam = "WHEA", xl = "Oats",
                 p1   = "pan1oats",
                 fao  = c("Oats"),                        tbase = 0),
  rye     = list(spam = "WHEA", xl = "Rye",
                 p1   = "pan1rye",
                 fao  = c("Rye"),                         tbase = 0),
  rice    = list(spam = "RICE", xl = "Rice",
                 p1   = "pan1rice",
                 fao  = c("Rice, paddy", "Rice"),         tbase = 10),
  maize   = list(spam = "MAIZ", xl = "Maize",
                 p1   = "pan1maize",
                 fao  = c("Maize (corn)", "Maize"),       tbase = 10),
  soybean = list(spam = "SOYB", xl = "Soybean",
                 p1   = "pan1soybean",
                 fao  = c("Soya beans", "Soybeans"),      tbase = 10),
  sorghum = list(spam = "SORG", xl = "Sorghum",
                 p1   = "pan1sorghum",
                 fao  = c("Sorghum"),                     tbase = 10)
)


# ==============================================================================
# 3. CRU VARIABLE LIST — file names and scale flags
#    CRU stores tmp/tmn/tmx/dtr/vap as INT16 × 0.1 → check mean > 100
# ==============================================================================

degisken_bilgi <- list(
  tmp = list(nc = "CRU_TS4.09_tmp_1961_2024.nc", olcek = TRUE),
  pre = list(nc = "CRU_TS4.09_pre_1961_2024.nc", olcek = FALSE),
  tmn = list(nc = "CRU_TS4.09_tmn_1961_2024.nc", olcek = TRUE),
  tmx = list(nc = "CRU_TS4.09_tmx_1961_2024.nc", olcek = TRUE),
  dtr = list(nc = "CRU_TS4.09_dtr_1961_2024.nc", olcek = TRUE),
  wet = list(nc = "CRU_TS4.09_wet_1961_2024.nc", olcek = FALSE),
  frs = list(nc = "CRU_TS4.09_frs_1961_2024.nc", olcek = FALSE),
  cld = list(nc = "CRU_TS4.09_cld_1961_2024.nc", olcek = FALSE),
  vap = list(nc = "CRU_TS4.09_vap_1961_2024.nc", olcek = TRUE),
  pet = list(nc = "CRU_TS4.09_pet_1961_2024.nc", olcek = FALSE)
)

# Build year × month skeleton: 1961–2024 × 12 months = 768 rows
yillar    <- 1961:2024
yil_ay_df <- expand.grid(Ay = 1:12, Yil = yillar)
yil_ay_df <- yil_ay_df[order(yil_ay_df$Yil, yil_ay_df$Ay), ]
rownames(yil_ay_df) <- NULL

# Scale check: if abs(mean) > 100 the INT16 × 0.1 factor was not applied
scale_kontrolu <- function(v, vadi) {
  if (abs(mean(v, na.rm = TRUE)) > 100) {
    cat("  [SCALE]", vadi, "0.1 applied\n")
    return(v * 0.1)
  }
  return(v)
}


# ==============================================================================
# STEP 1 — READ PHENOLOGY WINDOWS FROM EXCEL
# ==============================================================================

cat("[*] Reading phenology windows...\n")
fenoloji_listesi <- list()

for (urun_adi in names(urun_config)) {
  sheet  <- urun_config[[urun_adi]]$xl
  df_raw <- read_excel(fenoloji_xl, sheet = sheet)
  names(df_raw) <- gsub("[\n\r]", " ", names(df_raw))
  
  # Identify columns by keyword matching
  iso_c <- grep("ISO",     names(df_raw), value = TRUE)[1]
  ep_c  <- grep("Early",   names(df_raw), value = TRUE)[1]
  fl_c  <- grep("Flower",  names(df_raw), value = TRUE)[1]
  gs_c  <- grep("Growing", names(df_raw), value = TRUE)[1]
  dm_c  <- grep("Drought", names(df_raw), value = TRUE)[1]
  
  feno_df <- dplyr::select(df_raw,
                           Country_ISO    = dplyr::all_of(iso_c),
                           Early_Period   = dplyr::all_of(ep_c),
                           Flowering      = dplyr::all_of(fl_c),
                           Growing_Season = dplyr::all_of(gs_c),
                           Drought_Memory = dplyr::all_of(dm_c))
  
  feno_df <- dplyr::filter(feno_df, !is.na(Country_ISO))
  fenoloji_listesi[[urun_adi]] <- feno_df
  cat("  [OK]", sheet, "-", nrow(feno_df), "rows\n")
}


# ==============================================================================
# STEP 1 — CRU + SPAM → monthly panel (pan1*)
#
# For each crop:
#   1. Load SPAM2020 harvested area raster (global)
#   2. For each country: clip to GADM boundary, resample SPAM to CRU resolution
#   3. Compute SPAM-weighted mean of each CRU variable across the country
#   4. Bind all months → country × month data frame
#   5. Write to pan1<crop>.xlsx
# ==============================================================================

islem_urun_adim1 <- function(urun_adi) {
  
  cfg       <- urun_config[[urun_adi]]
  spam_kodu <- cfg$spam
  cikti_adi <- cfg$p1
  
  cat("\n", strrep("=", 60), "\n")
  cat("STEP 1:", toupper(urun_adi), "| SPAM:", spam_kodu,
      "| Output:", cikti_adi, "\n")
  cat(strrep("=", 60), "\n")
  
  # Load SPAM harvested area raster
  spam_dosya <- file.path(ana_yol,
                          paste0("spam2020_V2r0_global_H_", spam_kodu, "_A.tif"))
  if (!file.exists(spam_dosya)) stop(paste("SPAM file missing:", spam_dosya))
  spam_global <- terra::rast(spam_dosya)
  cat("[OK] SPAM:", basename(spam_dosya), "\n")
  
  # Load all CRU .nc files into memory (768 layers each)
  cat("[*] Loading CRU files...\n")
  cru_list <- list()
  for (v in names(degisken_bilgi)) {
    nc_tam <- file.path(iklim_yol, degisken_bilgi[[v]]$nc)
    if (!file.exists(nc_tam)) { cat("  [!] Missing:", nc_tam, "\n"); next }
    cru_list[[v]] <- terra::rast(nc_tam)
    cat("  [OK]", v, "-", terra::nlyr(cru_list[[v]]), "layers\n")
  }
  
  panel_listesi <- list()
  
  for (iso in ulkeler_130) {
    cat("\n  >>", iso)
    t0 <- Sys.time()
    
    # Load GADM country boundary (use cached .rds if available)
    gadm_cache <- file.path(ana_yol, "gadm",
                            paste0("gadm_41_", iso, "_0_pk.rds"))
    if (file.exists(gadm_cache)) {
      sinir <- readRDS(gadm_cache)
      cat(" [cache]")
    } else {
      sinir <- tryCatch(
        geodata::gadm(country = iso, level = 0, path = ana_yol),
        error = function(e) { cat(" [GADM error]"); NULL })
    }
    if (is.null(sinir)) next
    
    ulke_tbl <- data.frame(Country_ISO = iso,
                           Yil = yil_ay_df$Yil, Ay = yil_ay_df$Ay)
    
    # Resample SPAM to CRU resolution and mask to country boundary
    spam_crop <- terra::crop(spam_global, terra::ext(sinir))
    ref_r     <- terra::mask(
      terra::crop(cru_list[[1]][[1]], terra::ext(sinir)), sinir)
    spam_res  <- terra::resample(spam_crop, ref_r, method = "sum")
    spam_m    <- terra::mask(spam_res, sinir)
    w_base    <- as.vector(terra::values(spam_m))
    alan_var  <- !is.na(sum(w_base, na.rm = TRUE)) &&
      sum(w_base, na.rm = TRUE) > 0
    if (!alan_var) cat(" [area=0->simple mean]")
    
    # For each CRU variable: compute SPAM-weighted spatial mean across 768 months
    for (v in names(cru_list)) {
      r_ulke <- terra::mask(
        terra::crop(cru_list[[v]], terra::ext(sinir)), sinir)
      v_mat  <- terra::values(r_ulke)   # matrix: pixels × 768 months
      
      if (alan_var) {
        # Re-resample SPAM to this variable's resolution
        spam_res2 <- terra::resample(spam_crop, r_ulke[[1]], method = "sum")
        w  <- as.vector(terra::values(terra::mask(spam_res2, sinir)))
        ws <- sum(w, na.rm = TRUE)
        aylik <- if (!is.na(ws) && ws > 0)
          colSums(v_mat * w, na.rm = TRUE) / ws   # weighted mean
        else
          colMeans(v_mat, na.rm = TRUE)            # fallback: simple mean
      } else {
        aylik <- colMeans(v_mat, na.rm = TRUE)
      }
      
      if (degisken_bilgi[[v]]$olcek) aylik <- scale_kontrolu(aylik, v)
      ulke_tbl[[toupper(v)]] <- aylik
    }
    
    panel_listesi[[iso]] <- ulke_tbl
    cat("  [", round(difftime(Sys.time(), t0, units = "secs"), 1), "s]")
  }
  
  # Combine all countries, join phenology windows
  panel_final <- do.call(rbind, panel_listesi)
  panel_final <- panel_final[order(panel_final$Country_ISO,
                                   panel_final$Yil, panel_final$Ay), ]
  rownames(panel_final) <- NULL
  panel_final <- dplyr::left_join(panel_final,
                                  fenoloji_listesi[[urun_adi]],
                                  by = "Country_ISO")
  
  cat("\n[OK] Rows:", nrow(panel_final), "| Cols:", ncol(panel_final), "\n")
  
  cikti_yol <- file.path(ana_yol, paste0(cikti_adi, ".xlsx"))
  writexl::write_xlsx(panel_final, cikti_yol)
  cat("[SAVED]", cikti_yol, "\n")
  gc()
  invisible(panel_final)
}

# Run Step 1 for all crops (add completed crops to zaten_yapildi to skip)
zaten_yapildi <- character(0)

cat("\n", strrep("#", 60), "\nSTEP 1: PROCESSING 8 CROPS\n",
    strrep("#", 60), "\n\n")

for (urun_adi in names(urun_config)) {
  if (urun_adi %in% zaten_yapildi) {
    cat("[SKIP]", urun_adi, "\n"); next
  }
  tryCatch(islem_urun_adim1(urun_adi),
           error = function(e) cat("[ERROR]", urun_adi, "-",
                                   conditionMessage(e), "\n"))
}


# ==============================================================================
# STEP 2 — FAO YIELD DATA PREPARATION
# ==============================================================================

library(readxl); library(writexl); library(dplyr)
library(tidyr);  library(zoo);     library(data.table)

# Filter thresholds
VERIM_ESIGI <- 1    # minimum yield (hg/ha)
MIN_YIL     <- 1    # minimum years per country

# ── ISO → FAO country name mapping ──────────────────────────────────────────
iso_fao <- list(
  AFG="Afghanistan",    ALB="Albania",        DZA="Algeria",
  AGO="Angola",         ARG="Argentina",      ARM="Armenia",
  AUS="Australia",      AUT="Austria",        AZE="Azerbaijan",
  BHS="Bahamas",        BHR="Bahrain",        BGD="Bangladesh",
  BLR="Belarus",        BEL="Belgium",        BLZ="Belize",
  BEN="Benin",          BOL="Bolivia (Plurinational State of)",
  BIH="Bosnia and Herzegovina",               BWA="Botswana",
  BRA="Brazil",         BRN="Brunei Darussalam",
  BGR="Bulgaria",       BFA="Burkina Faso",   BDI="Burundi",
  KHM="Cambodia",       CMR="Cameroon",       CAN="Canada",
  TCD="Chad",           CHL="Chile",
  CHN="China, mainland",
  COL="Colombia",       COG="Congo",
  CRI="Costa Rica",
  CIV="CIV_PLACEHOLDER",
  HRV="Croatia",        CUB="Cuba",           CYP="Cyprus",
  CZE="Czechia",        COD="Democratic Republic of the Congo",
  PRK="Democratic People's Republic of Korea",
  DNK="Denmark",        DOM="Dominican Republic",
  ECU="Ecuador",        EGY="Egypt",          SLV="El Salvador",
  GNQ="Equatorial Guinea",                    ERI="Eritrea",
  EST="Estonia",        ETH="Ethiopia",
  FIN="Finland",        FRA="France",
  GAB="Gabon",          GMB="Gambia",         GEO="Georgia",
  DEU="Germany",        GHA="Ghana",          GRC="Greece",
  GTM="Guatemala",      GIN="Guinea",         GNB="Guinea-Bissau",
  GUY="Guyana",         HTI="Haiti",          HND="Honduras",
  HUN="Hungary",        ISL="Iceland",        IND="India",
  IDN="Indonesia",
  IRN="Iran (Islamic Republic of)",
  IRQ="Iraq",           IRL="Ireland",        ISR="Israel",
  ITA="Italy",          JAM="Jamaica",        JPN="Japan",
  JOR="Jordan",         KAZ="Kazakhstan",     KEN="Kenya",
  KWT="Kuwait",         KGZ="Kyrgyzstan",
  LAO="Lao People's Democratic Republic",
  LVA="Latvia",         LBN="Lebanon",        LSO="Lesotho",
  LBR="Liberia",        LBY="Libya",          LTU="Lithuania",
  MDG="Madagascar",     MWI="Malawi",         MYS="Malaysia",
  MLI="Mali",           MLT="Malta",          MRT="Mauritania",
  MUS="Mauritius",      MEX="Mexico",         MDA="Republic of Moldova",
  MNG="Mongolia",       MNE="Montenegro",     MAR="Morocco",
  MOZ="Mozambique",     MMR="Myanmar",        NAM="Namibia",
  NPL="Nepal",
  NLD="Netherlands (Kingdom of the)",
  NZL="New Zealand",    NIC="Nicaragua",      NER="Niger",
  NGA="Nigeria",        MKD="North Macedonia",
  NOR="Norway",         OMN="Oman",           PAK="Pakistan",
  PAN="Panama",         PNG="Papua New Guinea",
  PRY="Paraguay",       PER="Peru",           PHL="Philippines",
  POL="Poland",         PRT="Portugal",       QAT="Qatar",
  KOR="Republic of Korea",
  ROU="Romania",        RUS="Russian Federation",
  RWA="Rwanda",         SAU="Saudi Arabia",   SEN="Senegal",
  SRB="Serbia",         SLE="Sierra Leone",   SGP="Singapore",
  SVK="Slovakia",       SVN="Slovenia",       SOM="Somalia",
  ZAF="South Africa",   SSD="South Sudan",    ESP="Spain",
  LKA="Sri Lanka",      SDN="Sudan",          SWE="Sweden",
  CHE="Switzerland",    SYR="Syrian Arab Republic",
  TJK="Tajikistan",     THA="Thailand",       TGO="Togo",
  TTO="Trinidad and Tobago",
  TUN="Tunisia",
  TUR="TUR_PLACEHOLDER",
  TKM="Turkmenistan",   UGA="Uganda",         UKR="Ukraine",
  ARE="United Arab Emirates",
  GBR="United Kingdom of Great Britain and Northern Ireland",
  TZA="United Republic of Tanzania",
  USA="United States of America",
  URY="Uruguay",        UZB="Uzbekistan",
  VEN="Venezuela (Bolivarian Republic of)",
  VNM="Viet Nam",       YEM="Yemen",
  ZMB="Zambia",         ZWE="Zimbabwe"
)

iso_map <- do.call(rbind, lapply(names(iso_fao), function(i)
  data.frame(Country_ISO = i, fao_area = iso_fao[[i]],
             stringsAsFactors = FALSE)))

yinelenen <- iso_map$Country_ISO[duplicated(iso_map$Country_ISO)]
if (length(yinelenen) > 0) {
  cat("[WARNING] Duplicate ISO codes:\n"); print(yinelenen)
} else cat("[OK] iso_map:", nrow(iso_map), "countries\n")


# ── Load FAO production data ─────────────────────────────────────────────────

cat("\n[*] Loading FAO data...\n")

if (exists("genisexcel") && is.data.frame(genisexcel) && nrow(genisexcel) > 0) {
  cat("[OK] genisexcel found in memory:",
      format(nrow(genisexcel), big.mark = ","), "rows\n")
  fao_raw_df <- genisexcel
  names(fao_raw_df) <- trimws(names(fao_raw_df))
} else {
  cat("[*] Reading from CSV...\n")
  fao_raw_df <- as.data.frame(data.table::fread(
    fao_csv, encoding = "UTF-8",
    select = c("Area", "Item", "Element", "Year", "Value"),
    showProgress = FALSE))
  cat("[OK]", format(nrow(fao_raw_df), big.mark = ","), "rows read\n")
}

fao_raw_df$Year  <- as.integer(gsub("'", "", as.character(fao_raw_df$Year)))
fao_raw_df$Value <- as.numeric(as.character(fao_raw_df$Value))

# Fix Turkey and Côte d'Ivoire country names (encoding differences)
tur_gercek <- unique(fao_raw_df$Area[
  grepl("rkiye|Turkey|TURKEY", fao_raw_df$Area)])
cat("[TUR] actual name:", paste(tur_gercek, collapse = " | "), "\n")
if (length(tur_gercek) >= 1)
  iso_map$fao_area[iso_map$Country_ISO == "TUR"] <- tur_gercek[1]

civ_gercek <- unique(fao_raw_df$Area[
  grepl("voire|Ivory", fao_raw_df$Area, ignore.case = TRUE)])
cat("[CIV] actual name:", paste(civ_gercek, collapse = " | "), "\n")
if (length(civ_gercek) >= 1)
  iso_map$fao_area[iso_map$Country_ISO == "CIV"] <- civ_gercek[1]

# Remove regional aggregates
bolgeler <- c(
  "World","Africa","Eastern Africa","Middle Africa","Northern Africa",
  "Southern Africa","Western Africa","Americas","Northern America",
  "Central America","Caribbean","South America","Asia","Central Asia",
  "Eastern Asia","Southern Asia","South-eastern Asia","Western Asia",
  "Europe","Eastern Europe","Northern Europe","Southern Europe",
  "Western Europe","Oceania","Australia and New Zealand","Melanesia",
  "Micronesia","Polynesia","European Union (27)",
  "Least Developed Countries (LDCs)","Land Locked Developing Countries (LLDCs)",
  "Small Island Developing States (SIDS)","Low Income Food Deficit Countries (LIFDCs)",
  "Net Food Importing Developing Countries (NFIDCs)",
  "USSR","Yugoslav SFR","Czechoslovakia","Ethiopia PDR","Sudan (former)",
  "Serbia and Montenegro","Belgium-Luxembourg","China","China, Hong Kong SAR",
  "China, Macao SAR","China, Taiwan Province of","Palestine"
)

tum_items <- unique(unlist(lapply(urun_config, `[[`, "fao")))

fao_fil <- fao_raw_df[
  fao_raw_df$Item    %in% tum_items &
    fao_raw_df$Element %in% c("Area harvested", "Production", "Yield") &
    fao_raw_df$Year    >= 1961 & fao_raw_df$Year <= 2024 &
    !fao_raw_df$Area   %in% bolgeler, ]

cat("[OK] Filtered FAO:", format(nrow(fao_fil), big.mark = ","), "rows\n")

# Pivot to wide format (one row per country × year × item)
fao_wide <- fao_fil %>%
  dplyr::group_by(Area, Year, Item, Element) %>%
  dplyr::summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Element, values_from = Value) %>%
  as.data.frame()

names(fao_wide)[names(fao_wide) == "Area harvested"] <- "area_harvested_ha"
names(fao_wide)[names(fao_wide) == "Production"]     <- "production_tonnes"
names(fao_wide)[names(fao_wide) == "Yield"]          <- "yield_hg_per_ha"
names(fao_wide)[names(fao_wide) == "Area"]           <- "fao_area"

fao_wide <- merge(fao_wide, iso_map, by = "fao_area", all.x = TRUE)
fao_wide <- fao_wide[!is.na(fao_wide$Country_ISO), ]
cat("[OK] fao_wide:", nrow(fao_wide), "rows |",
    length(unique(fao_wide$Country_ISO)), "countries\n")


# ── Load CO2 ─────────────────────────────────────────────────────────────────
cat("[*] CO2...\n")
co2_df <- read.csv(co2_csv, stringsAsFactors = FALSE)
names(co2_df) <- trimws(names(co2_df))
co2_df <- data.frame(
  Yil     = as.integer(co2_df[[grep("^[Yy]ear$",   names(co2_df), value=TRUE)[1]]]),
  CO2_ppm = as.numeric(co2_df[[grep("CO2|co2|ppm", names(co2_df), value=TRUE)[1]]]))
co2_df <- co2_df[!is.na(co2_df$Yil), ]
cat("[OK] CO2:", min(co2_df$Yil), "-", max(co2_df$Yil), "\n")


# ── Load GDP (fallback CSV — replaced by WDI in Step 7) ─────────────────────
cat("[*] GDP (fallback)...\n")
gdp_df <- read.csv(gdp_csv, stringsAsFactors = FALSE)
names(gdp_df) <- trimws(names(gdp_df))
gdp_df <- data.frame(
  Country_ISO    = gdp_df[[grep("ISO|country_iso",
                                names(gdp_df), ignore.case=TRUE, value=TRUE)[1]]],
  Yil            = as.integer(gdp_df[[grep("^[Yy]ear$",
                                           names(gdp_df), value=TRUE)[1]]]),
  GDP_per_Capita = as.numeric(gdp_df[[grep("GDP|Capita",
                                           names(gdp_df), value=TRUE)[1]]]))
gdp_df <- gdp_df[!is.na(gdp_df$Yil), ]
cat("[OK] GDP:", length(unique(gdp_df$Country_ISO)), "countries\n")


# ── Load irrigation data ──────────────────────────────────────────────────────
cat("[*] Irrigation...\n")
sulama_df <- NULL
if (file.exists(sulama_xl)) {
  sul_raw <- as.data.frame(readxl::read_excel(sulama_xl, sheet = 1))
  names(sul_raw) <- trimws(names(sul_raw))
  sul_raw$Area[grepl("rkiye|Turkey", sul_raw$Area)] <-
    iso_map$fao_area[iso_map$Country_ISO == "TUR"]
  sul_raw$Area[grepl("voire|Ivory", sul_raw$Area, ignore.case = TRUE)] <-
    iso_map$fao_area[iso_map$Country_ISO == "CIV"]
  sul_raw   <- merge(sul_raw, iso_map, by.x = "Area", by.y = "fao_area",
                     all.x = TRUE)
  sul_raw$Year  <- as.integer(gsub("'", "", as.character(sul_raw$Year)))
  sul_raw$Value <- as.numeric(sul_raw$Value)
  sul_fil <- sul_raw[
    !is.na(sul_raw$Country_ISO) &
      sul_raw$Year >= 1961 & sul_raw$Year <= 2024, ]
  sul_wide <- sul_fil %>%
    dplyr::select(Country_ISO, Year, Item, Value) %>%
    dplyr::group_by(Country_ISO, Year, Item) %>%
    dplyr::summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = Item, values_from = Value)
  # Standardize column names
  names(sul_wide) <- gsub("[^A-Za-z0-9]", "_", names(sul_wide))
  names(sul_wide) <- gsub("__+", "_", names(sul_wide))
  names(sul_wide) <- gsub("_$", "", names(sul_wide))
  # Rename irrigation columns to standard names
  ri <- function(df, pat, nm) {
    m <- grep(pat, names(df), value = TRUE, ignore.case = TRUE)[1]
    if (!is.na(m)) names(df)[names(df) == m] <- nm
    df
  }
  sul_wide <- ri(sul_wide, "^Land_area$|^Land_area_[0-9]", "land_area_1000ha")
  sul_wide <- ri(sul_wide, "equipped.*irrig|irrig.*equip",  "equipped_irrig_1000ha")
  sul_wide <- ri(sul_wide, "Land.*actually|actually_irrig", "actual_irrig_1000ha")
  sul_wide <- ri(sul_wide, "Cropland.*irrig|Crop.*actually","cropland_irrig_1000ha")
  sul_wide <- ri(sul_wide, "Agri.*irrig|Agriculture.*irrig","agri_irrig_1000ha")
  if (all(c("actual_irrig_1000ha","land_area_1000ha") %in% names(sul_wide)))
    sul_wide <- sul_wide %>% dplyr::mutate(
      irrig_intensity = dplyr::case_when(
        land_area_1000ha > 0 ~ actual_irrig_1000ha / land_area_1000ha,
        TRUE ~ NA_real_),
      irrig_intensity = ifelse(irrig_intensity > 1, NA_real_, irrig_intensity))
  if (all(c("cropland_irrig_1000ha","equipped_irrig_1000ha") %in% names(sul_wide)))
    sul_wide <- sul_wide %>% dplyr::mutate(
      cropland_irrig_share = dplyr::case_when(
        equipped_irrig_1000ha > 0 ~
          cropland_irrig_1000ha / equipped_irrig_1000ha,
        TRUE ~ NA_real_),
      cropland_irrig_share = ifelse(
        cropland_irrig_share > 1, NA_real_, cropland_irrig_share))
  sulama_df <- sul_wide %>% dplyr::rename(Yil = Year)
  cat("[OK] Irrigation:", length(unique(sulama_df$Country_ISO)), "countries\n")
} else {
  cat("[WARNING] Irrigation file not found\n")
}


# ==============================================================================
# STEP 2 — PHENOLOGY WINDOWS (fully inline, no Excel dependency)
#
# All country-crop phenology windows defined here in R code.
# Sources: Sacks et al. (2010); FAO GAEZ v4 (2021); MIRCA2000;
#          national statistics offices; custom assignment (patch).
#
# Columns per entry: Early_Period, Flowering, Growing_Season, Drought_Memory
# Month encoding: 1=Jan ... 12=Dec  (comma-separated = two months)
# Southern Hemisphere producers: flowering ~6 months shifted vs N. Hemisphere
# patch entries = countries added beyond Sacks et al. (2010) top-70 list
# ==============================================================================

feno_patch <- list(
  
  # ────────────────────────────────────────────────────────────
  # WHEAT — 118 countries
  # Tbase = 0°C  |  SPAM = WHEA
  # ────────────────────────────────────────────────────────────
  wheat = data.frame(stringsAsFactors = FALSE,
                     Country_ISO    = c(
                       "AFG", "AGO", "ALB", "ARG", "ARM", "AUS", "AUT", "AZE", "BDI", "BEL",
                       "BFA", "BGD", "BGR", "BIH", "BLR", "BOL", "BRA", "BWA", "CAN", "CHE",
                       "CHL", "CHN", "CMR", "COL", "CUB", "CYP", "CZE", "DEU", "DNK", "DZA",
                       "ECU", "EGY", "ERI", "ESP", "EST", "ETH", "FIN", "FRA", "GBR", "GEO",
                       "GIN", "GRC", "GTM", "HND", "HRV", "HUN", "IND", "IRL", "IRN", "IRQ",
                       "ISR", "ITA", "JOR", "JPN", "KAZ", "KEN", "KGZ", "KOR", "LBN", "LBY",
                       "LKA", "LTU", "LVA", "MAR", "MDA", "MDG", "MEX", "MKD", "MLI", "MLT",
                       "MMR", "MNE", "MNG", "MOZ", "MWI", "NAM", "NER", "NGA", "NLD", "NOR",
                       "NPL", "NZL", "PAK", "PER", "POL", "PRK", "PRT", "PRY", "ROU", "RUS",
                       "RWA", "SAU", "SDN", "SEN", "SOM", "SRB", "SSD", "SVK", "SVN", "SWE",
                       "SYR", "TCD", "THA", "TJK", "TKM", "TUN", "TUR", "TZA", "UGA", "UKR",
                       "URY", "USA", "UZB", "VEN", "YEM", "ZAF", "ZMB", "ZWE"
                     ),
                     Early_Period = c(
                       "11,12", "4,5", "10,11", "5,6", "10,11", "5,6", "10,11", "10,11", "10,11", "10,11",
                       "10,11", "10,11", "10,11", "10,11", "9,10", "5,6", "4,5", "4,5", "4,5", "10,11",
                       "5,6", "10,11", "10,11", "9,10", "10,11", "11,12", "10,11", "10,11", "10,11", "11,12",
                       "10,11", "11,12", "10,11", "11,12", "9,10", "10,11", "4,5", "10,11", "10,11", "10,11",
                       "10,11", "11,12", "10,11", "10,11", "10,11", "10,11", "11,12", "10,11", "11,12", "11,12",
                       "11,12", "11,12", "11,12", "10,11", "3,4", "10,11", "10,11", "10,11", "11,12", "11,12",
                       "10,11", "9,10", "9,10", "11,12", "10,11", "4,5", "11,12", "10,11", "10,11", "11,12",
                       "10,11", "10,11", "9,10", "4,5", "10,11", "4,5", "10,11", "10,11", "10,11", "10,11",
                       "10,11", "5,6", "11,12", "9,10", "10,11", "10,11", "10,11", "10,11", "10,11", "4,5",
                       "10,11", "11,12", "11,12", "10,11", "11,12", "10,11", "10,11", "10,11", "10,11", "9,10",
                       "11,12", "11,12", "10,11", "10,11", "10,11", "11,12", "10,11", "10,11", "10,11", "9,10",
                       "5,6", "9,10", "10,11", "9,10", "11,12", "4,5", "4,5", "4,5"
                     ),
                     Flowering = c(
                       "3,4", "8,9", "4,5", "9,10", "5,6", "9,10", "5,6", "4,5", "2,3", "5,6",
                       "2,3", "4,5", "5,6", "4,5", "6,7", "9,10", "8,9", "9,10", "7,8", "5,6",
                       "10,11", "4,5", "2,3", "2,3", "3,4", "3,4", "5,6", "5,6", "5,6", "3,4",
                       "2,3", "2,3", "2,3", "4,5", "6,7", "2,3", "7,8", "5,6", "5,6", "4,5",
                       "2,3", "4,5", "2,3", "2,3", "4,5", "5,6", "2,3", "5,6", "3,4", "3,4",
                       "3,4", "4,5", "3,4", "4,5", "6,7", "2,3", "4,5", "4,5", "3,4", "2,3",
                       "4,5", "6,7", "6,7", "3,4", "5,6", "9,10", "2,3", "4,5", "2,3", "3,4",
                       "4,5", "5,6", "7,8", "8,9", "2,3", "9,10", "2,3", "2,3", "5,6", "5,6",
                       "4,5", "10,11", "2,3", "2,3", "5,6", "4,5", "4,5", "9,10", "5,6", "7,8",
                       "2,3", "3,4", "2,3", "2,3", "2,3", "5,6", "2,3", "5,6", "4,5", "6,7",
                       "3,4", "3,4", "2,3", "4,5", "4,5", "2,3", "4,5", "2,3", "2,3", "5,6",
                       "9,10", "5,6", "4,5", "2,3", "2,3", "9,10", "9,10", "9,10"
                     ),
                     Growing_Season = c(
                       "5,6", "10,11", "6,7", "11,12", "7,8", "11,12", "7,8", "6,7", "4,5", "7,8",
                       "4,5", "6,7", "7,8", "6,7", "8,9", "11,12", "10,11", "11,12", "9,10", "7,8",
                       "12,1", "6,7", "4,5", "4,5", "5,6", "5,6", "7,8", "7,8", "7,8", "5,6",
                       "4,5", "4,5", "4,5", "6,7", "8,9", "4,5", "9,10", "7,8", "7,8", "6,7",
                       "4,5", "6,7", "4,5", "4,5", "6,7", "7,8", "4,5", "7,8", "5,6", "5,6",
                       "5,6", "6,7", "5,6", "6,7", "8,9", "4,5", "6,7", "6,7", "5,6", "4,5",
                       "6,7", "8,9", "8,9", "5,6", "7,8", "11,12", "4,5", "6,7", "4,5", "5,6",
                       "6,7", "7,8", "9,10", "10,11", "4,5", "11,12", "4,5", "4,5", "7,8", "7,8",
                       "6,7", "12,1", "4,5", "4,5", "7,8", "6,7", "6,7", "12,1", "7,8", "8,9",
                       "4,5", "5,6", "4,5", "4,5", "4,5", "7,8", "4,5", "7,8", "6,7", "8,9",
                       "5,6", "5,6", "4,5", "6,7", "6,7", "4,5", "6,7", "4,5", "4,5", "7,8",
                       "11,12", "7,8", "6,7", "4,5", "4,5", "11,12", "11,12", "11,12"
                     ),
                     Drought_Memory = c(
                       "2,3", "1", "1,2", "2", "1,2", "2", "1", "1,2", "1", "1",
                       "1", "1,2", "2", "1,2", "1,2", "1,2", "1,2", "1", "1", "1",
                       "2", "1,2", "1", "1", "1", "1", "1,2", "1,2", "1", "1",
                       "1", "1", "1", "2", "1,2", "1", "1", "1,2", "1", "1,2",
                       "1", "1,2", "1", "1", "1,2", "2", "1", "1,2", "1", "1",
                       "1", "1,2", "1", "1,2", "1,2", "1", "1,2", "1,2", "1", "1",
                       "1,2", "1,2", "1,2", "1", "2", "1", "1", "1,2", "1", "1",
                       "1,2", "1,2", "1,2", "1", "1", "1", "1", "1", "1", "1,2",
                       "1,2", "1,2", "1", "1", "1,2", "1,2", "1,2", "1,2", "2", "1,2",
                       "1", "1", "1", "1", "1", "1,2", "1", "1,2", "1,2", "1",
                       "1", "1", "1", "1,2", "1,2", "1", "1,2", "1", "1", "2",
                       "2", "2", "1,2", "1", "1", "1", "1", "1"
                     )
  ),
  
  # ────────────────────────────────────────────────────────────
  # BARLEY — 110 countries
  # Tbase = 0°C  |  SPAM = WHEA
  # ────────────────────────────────────────────────────────────
  barley = data.frame(stringsAsFactors = FALSE,
                      Country_ISO    = c(
                        "AFG", "AGO", "ALB", "ARE", "ARG", "ARM", "AUS", "AUT", "AZE", "BEL",
                        "BFA", "BGD", "BGR", "BIH", "BLR", "BOL", "BWA", "CAN", "CHE", "CHL",
                        "CHN", "CMR", "CYP", "CZE", "DEU", "DNK", "DZA", "ECU", "EGY", "ESP",
                        "EST", "ETH", "FIN", "FRA", "GBR", "GEO", "GRC", "GTM", "HND", "HRV",
                        "HUN", "IND", "IRL", "IRN", "IRQ", "ISL", "ISR", "ITA", "JOR", "JPN",
                        "KAZ", "KEN", "KGZ", "KOR", "KWT", "LBN", "LBY", "LKA", "LTU", "LVA",
                        "MAR", "MDA", "MDG", "MEX", "MKD", "MLI", "MLT", "MMR", "MNE", "MNG",
                        "MOZ", "NAM", "NER", "NGA", "NLD", "NOR", "NPL", "NZL", "OMN", "PAK",
                        "PER", "POL", "PRK", "PRT", "PRY", "QAT", "ROU", "RUS", "RWA", "SAU",
                        "SDN", "SEN", "SRB", "SVK", "SVN", "SWE", "SYR", "TJK", "TKM", "TUN",
                        "TUR", "TZA", "UGA", "UKR", "URY", "USA", "YEM", "ZAF", "ZMB", "ZWE"
                      ),
                      Early_Period = c(
                        "11,12", "9,10", "10,11", "11,12", "5,6", "10,11", "4,5", "10,11", "10,11", "10,11",
                        "10,11", "10,11", "10,11", "10,11", "9,10", "5,6", "10,11", "4,5", "10,11", "5,6",
                        "10,11", "10,11", "11,12", "10,11", "10,11", "10,11", "11,12", "3,4", "11,12", "11,12",
                        "9,10", "10,11", "4,5", "10,11", "10,11", "10,11", "11,12", "4,5", "4,5", "10,11",
                        "10,11", "11,12", "10,11", "11,12", "11,12", "4,5", "11,12", "11,12", "11,12", "9,10",
                        "4,5", "10,11", "10,11", "10,11", "11,12", "11,12", "11,12", "10,11", "9,10", "9,10",
                        "11,12", "10,11", "4,5", "11,12", "10,11", "10,11", "11,12", "10,11", "10,11", "9,10",
                        "10,11", "4,5", "10,11", "10,11", "10,11", "4,5", "11,12", "5,6", "11,12", "11,12",
                        "9,10", "10,11", "10,11", "11,12", "10,11", "11,12", "10,11", "4,5", "3,4", "11,12",
                        "11,12", "10,11", "10,11", "10,11", "10,11", "4,5", "11,12", "10,11", "10,11", "11,12",
                        "10,11", "10,11", "3,4", "9,10", "5,6", "9,10", "11,12", "4,5", "4,5", "4,5"
                      ),
                      Flowering = c(
                        "3,4", "12,1", "4,5", "3,4", "9,10", "5,6", "8,9", "5,6", "4,5", "5,6",
                        "2,3", "4,5", "5,6", "4,5", "6,7", "9,10", "9,10", "7,8", "5,6", "10,11",
                        "4,5", "2,3", "3,4", "5,6", "5,6", "5,6", "3,4", "6,7", "2,3", "4,5",
                        "6,7", "2,3", "7,8", "4,5", "5,6", "4,5", "4,5", "4,5", "4,5", "4,5",
                        "5,6", "2,3", "5,6", "3,4", "3,4", "7,8", "3,4", "4,5", "3,4", "5,6",
                        "7,8", "2,3", "4,5", "4,5", "2,3", "3,4", "2,3", "4,5", "6,7", "6,7",
                        "3,4", "5,6", "9,10", "2,3", "4,5", "2,3", "3,4", "4,5", "5,6", "7,8",
                        "8,9", "9,10", "2,3", "2,3", "5,6", "7,8", "2,3", "10,11", "2,3", "2,3",
                        "1,2", "5,6", "4,5", "4,5", "9,10", "2,3", "5,6", "6,7", "6,7", "3,4",
                        "2,3", "2,3", "5,6", "5,6", "5,6", "7,8", "3,4", "4,5", "4,5", "2,3",
                        "4,5", "2,3", "6,7", "5,6", "9,10", "5,6", "2,3", "9,10", "9,10", "9,10"
                      ),
                      Growing_Season = c(
                        "5,6", "2,3", "6,7", "5,6", "11,12", "7,8", "10,11", "7,8", "6,7", "7,8",
                        "4,5", "6,7", "7,8", "6,7", "8,9", "11,12", "11,12", "9,10", "7,8", "12,1",
                        "6,7", "4,5", "5,6", "7,8", "7,8", "7,8", "5,6", "8,9", "4,5", "6,7",
                        "8,9", "4,5", "9,10", "6,7", "7,8", "6,7", "6,7", "6,7", "6,7", "6,7",
                        "7,8", "4,5", "7,8", "5,6", "5,6", "9,10", "5,6", "6,7", "5,6", "7,8",
                        "9,10", "4,5", "6,7", "6,7", "4,5", "5,6", "4,5", "6,7", "8,9", "8,9",
                        "5,6", "7,8", "11,12", "4,5", "6,7", "4,5", "5,6", "6,7", "7,8", "9,10",
                        "10,11", "11,12", "4,5", "4,5", "7,8", "9,10", "4,5", "12,1", "4,5", "4,5",
                        "3,4", "7,8", "6,7", "6,7", "11,12", "4,5", "7,8", "8,9", "8,9", "5,6",
                        "4,5", "4,5", "7,8", "7,8", "7,8", "9,10", "5,6", "6,7", "6,7", "4,5",
                        "6,7", "4,5", "8,9", "7,8", "11,12", "7,8", "4,5", "11,12", "11,12", "11,12"
                      ),
                      Drought_Memory = c(
                        "2,3", "1", "1,2", "1", "2", "1,2", "2", "1", "1,2", "1",
                        "1", "1,2", "2", "1,2", "1", "1,2", "1", "1", "1", "2",
                        "1,2", "1", "1", "1,2", "1,2", "1", "1", "1", "1", "2",
                        "1", "1", "1", "1,2", "1", "1,2", "2", "1", "1", "1,2",
                        "2", "1", "1,2", "1", "1", "1", "1", "2", "1", "1",
                        "1", "1", "1,2", "1", "1", "1", "1", "1,2", "1", "1",
                        "1", "2", "1", "1", "1,2", "1", "1", "1,2", "1,2", "1,2",
                        "1", "1", "1", "1", "1", "1", "1", "1,2", "1", "1",
                        "1,2", "1,2", "1,2", "2", "1", "1", "2", "1", "1", "1",
                        "1", "1", "1,2", "1,2", "1,2", "1", "1", "1,2", "1,2", "1",
                        "1,2", "1", "1", "2", "2", "2", "1", "1", "1", "1"
                      )
  ),
  
  # ────────────────────────────────────────────────────────────
  # OATS — 86 countries
  # Tbase = 0°C  |  SPAM = WHEA
  # ────────────────────────────────────────────────────────────
  oats = data.frame(stringsAsFactors = FALSE,
                    Country_ISO    = c(
                      "AFG", "AGO", "ALB", "ARG", "ARM", "AUS", "AUT", "AZE", "BEL", "BGR",
                      "BIH", "BLR", "BOL", "BRA", "CAN", "CHE", "CHL", "CHN", "CMR", "COL",
                      "CZE", "DEU", "DNK", "DZA", "EGY", "ESP", "EST", "ETH", "FIN", "FRA",
                      "GBR", "GEO", "GHA", "GRC", "HRV", "HUN", "IND", "IRL", "IRN", "IRQ",
                      "ISL", "ISR", "ITA", "JOR", "JPN", "KAZ", "KEN", "KGZ", "KOR", "LBN",
                      "LTU", "LVA", "MAR", "MDA", "MEX", "MKD", "MLT", "MNE", "MNG", "MOZ",
                      "NGA", "NLD", "NOR", "NZL", "PER", "POL", "PRT", "ROU", "RUS", "SDN",
                      "SRB", "SVK", "SWE", "SYR", "TJK", "TUN", "TUR", "TZA", "UGA", "UKR",
                      "URY", "USA", "UZB", "ZAF", "ZMB", "ZWE"
                    ),
                    Early_Period = c(
                      "3,4", "9,10", "10,11", "4,5", "10,11", "4,5", "3,4", "10,11", "3,4", "3,4",
                      "10,11", "9,10", "5,6", "4,5", "4,5", "4,5", "4,5", "3,4", "9,10", "9,10",
                      "3,4", "3,4", "3,4", "10,11", "11,12", "11,12", "9,10", "10,11", "4,5", "10,11",
                      "3,4", "10,11", "4,5", "11,12", "10,11", "3,4", "11,12", "3,4", "11,12", "11,12",
                      "9,10", "11,12", "11,12", "11,12", "9,10", "4,5", "10,11", "4,5", "10,11", "11,12",
                      "9,10", "9,10", "10,11", "4,5", "11,12", "10,11", "11,12", "10,11", "4,5", "10,11",
                      "4,5", "3,4", "4,5", "3,4", "9,10", "3,4", "11,12", "3,4", "4,5", "11,12",
                      "10,11", "3,4", "4,5", "11,12", "4,5", "11,12", "10,11", "10,11", "3,4", "3,4",
                      "4,5", "4,5", "10,11", "4,5", "4,5", "4,5"
                    ),
                    Flowering = c(
                      "6,7", "12,1", "4,5", "9,10", "5,6", "8,9", "6,7", "5,6", "6,7", "6,7",
                      "4,5", "6,7", "9,10", "8,9", "7,8", "7,8", "9,10", "6,7", "1,2", "1,2",
                      "6,7", "6,7", "6,7", "3,4", "2,3", "4,5", "6,7", "2,3", "7,8", "5,6",
                      "6,7", "5,6", "7,8", "4,5", "4,5", "7,8", "2,3", "6,7", "3,4", "3,4",
                      "7,8", "3,4", "4,5", "3,4", "4,5", "7,8", "1,2", "7,8", "4,5", "3,4",
                      "6,7", "6,7", "3,4", "7,8", "2,3", "5,6", "3,4", "5,6", "7,8", "1,2",
                      "7,8", "6,7", "7,8", "9,10", "1,2", "7,8", "4,5", "7,8", "7,8", "2,3",
                      "5,6", "7,8", "7,8", "3,4", "7,8", "3,4", "4,5", "1,2", "6,7", "6,7",
                      "9,10", "7,8", "5,6", "9,10", "8,9", "8,9"
                    ),
                    Growing_Season = c(
                      "8,9", "2,3", "6,7", "11,12", "7,8", "10,11", "8,9", "7,8", "8,9", "8,9",
                      "6,7", "8,9", "11,12", "10,11", "9,10", "9,10", "11,12", "8,9", "3,4", "3,4",
                      "8,9", "8,9", "8,9", "5,6", "4,5", "6,7", "8,9", "4,5", "9,10", "7,8",
                      "8,9", "7,8", "9,10", "6,7", "6,7", "9,10", "4,5", "8,9", "5,6", "5,6",
                      "9,10", "5,6", "6,7", "5,6", "6,7", "9,10", "3,4", "9,10", "6,7", "5,6",
                      "8,9", "8,9", "5,6", "9,10", "4,5", "7,8", "5,6", "7,8", "9,10", "3,4",
                      "9,10", "8,9", "9,10", "11,12", "3,4", "9,10", "6,7", "9,10", "9,10", "4,5",
                      "7,8", "9,10", "9,10", "5,6", "9,10", "5,6", "6,7", "3,4", "8,9", "8,9",
                      "11,12", "9,10", "7,8", "11,12", "10,11", "10,11"
                    ),
                    Drought_Memory = c(
                      "2,3", "1", "1,2", "2", "1,2", "2", "1", "1,2", "1", "2",
                      "1,2", "1,2", "1,2", "1,2", "1", "1", "2", "1,2", "1,2", "1,2",
                      "1", "1", "1", "1", "1", "2", "1,2", "1", "1", "1",
                      "1", "1,2", "1", "2", "1,2", "2", "1", "1", "1", "1",
                      "1,2", "1", "2", "2,3", "1", "1", "1", "1,2", "1", "1",
                      "1,2", "1,2", "1", "1,2", "1", "1,2", "1", "1,2", "1", "1",
                      "1", "1", "1", "2", "2", "1,2", "2", "1,2", "1", "1",
                      "1,2", "1,2", "1", "2,3", "1,2", "2,3", "1,2", "1", "1", "1",
                      "2", "1", "1,2", "2", "2", "2"
                    )
  ),
  
  # ────────────────────────────────────────────────────────────
  # RYE — 81 countries
  # Tbase = 0°C  |  SPAM = WHEA
  # ────────────────────────────────────────────────────────────
  rye = data.frame(stringsAsFactors = FALSE,
                   Country_ISO    = c(
                     "AFG", "ALB", "ARG", "ARM", "AUS", "AUT", "AZE", "BEL", "BGD", "BGR",
                     "BIH", "BLR", "BOL", "BRA", "CAN", "CHE", "CHL", "CHN", "COL", "CZE",
                     "DEU", "DNK", "DZA", "ECU", "EGY", "ESP", "EST", "ETH", "FIN", "FRA",
                     "GBR", "GEO", "GRC", "HRV", "HUN", "IND", "IRL", "IRN", "IRQ", "ITA",
                     "JPN", "KAZ", "KEN", "KGZ", "KOR", "LTU", "LVA", "MAR", "MDA", "MEX",
                     "MKD", "MLT", "MNE", "MNG", "NGA", "NLD", "NOR", "NPL", "NZL", "PAK",
                     "PER", "POL", "PRT", "PRY", "ROU", "RUS", "SDN", "SRB", "SVK", "SVN",
                     "SWE", "TJK", "TKM", "TUN", "TUR", "TZA", "UKR", "URY", "USA", "UZB",
                     "ZAF"
                   ),
                   Early_Period = c(
                     "10,11", "11,12", "4,5", "10,11", "4,5", "9,10", "10,11", "9,10", "11,12", "9,10",
                     "9,10", "9,10", "5,6", "4,5", "4,5", "9,10", "5,6", "3,4", "9,10", "9,10",
                     "9,10", "9,10", "11,12", "3,4", "11,12", "11,12", "9,10", "10,11", "8,9", "9,10",
                     "10,11", "10,11", "11,12", "9,10", "9,10", "11,12", "10,11", "11,12", "11,12", "10,11",
                     "9,10", "4,5", "3,4", "10,11", "10,11", "9,10", "9,10", "11,12", "9,10", "11,12",
                     "10,11", "11,12", "10,11", "9,10", "10,11", "9,10", "8,9", "11,12", "5,6", "11,12",
                     "9,10", "9,10", "11,12", "4,5", "9,10", "9,10", "11,12", "10,11", "9,10", "9,10",
                     "9,10", "10,11", "10,11", "11,12", "10,11", "3,4", "9,10", "4,5", "9,10", "10,11",
                     "4,5"
                   ),
                   Flowering = c(
                     "3,4", "4,5", "9,10", "5,6", "8,9", "5,6", "5,6", "5,6", "2,3", "5,6",
                     "5,6", "6,7", "9,10", "8,9", "7,8", "5,6", "10,11", "7,8", "1,2", "5,6",
                     "5,6", "5,6", "3,4", "6,7", "2,3", "4,5", "6,7", "2,3", "6,7", "5,6",
                     "5,6", "5,6", "4,5", "5,6", "5,6", "2,3", "5,6", "3,4", "3,4", "4,5",
                     "4,5", "7,8", "6,7", "5,6", "4,5", "6,7", "6,7", "3,4", "5,6", "2,3",
                     "4,5", "3,4", "5,6", "7,8", "2,3", "5,6", "6,7", "2,3", "10,11", "2,3",
                     "1,2", "5,6", "4,5", "9,10", "5,6", "6,7", "2,3", "5,6", "5,6", "5,6",
                     "6,7", "5,6", "5,6", "3,4", "4,5", "7,8", "5,6", "9,10", "5,6", "4,5",
                     "9,10"
                   ),
                   Growing_Season = c(
                     "5,6", "6,7", "11,12", "7,8", "10,11", "7,8", "7,8", "7,8", "4,5", "7,8",
                     "7,8", "8,9", "11,12", "10,11", "9,10", "7,8", "12,1", "9,10", "3,4", "7,8",
                     "7,8", "7,8", "5,6", "8,9", "4,5", "6,7", "8,9", "4,5", "8,9", "7,8",
                     "7,8", "7,8", "6,7", "7,8", "7,8", "4,5", "7,8", "5,6", "5,6", "6,7",
                     "6,7", "9,10", "8,9", "7,8", "6,7", "8,9", "8,9", "5,6", "7,8", "4,5",
                     "6,7", "5,6", "7,8", "9,10", "4,5", "7,8", "8,9", "4,5", "12,1", "4,5",
                     "3,4", "7,8", "6,7", "11,12", "7,8", "8,9", "4,5", "7,8", "7,8", "7,8",
                     "8,9", "7,8", "7,8", "5,6", "6,7", "9,10", "7,8", "11,12", "7,8", "6,7",
                     "11,12"
                   ),
                   Drought_Memory = c(
                     "2,3", "2", "2", "1,2", "1,2", "1", "1,2", "1", "1", "2",
                     "1,2", "1,2", "1,2", "1,2", "1", "1", "1,2", "1,2", "1,2", "1,2",
                     "1,2", "1", "2,3", "1", "1", "2", "1,2", "1", "1", "1",
                     "1,2", "1,2", "1,2", "2", "2", "1", "1,2", "1", "1", "1,2",
                     "1", "1", "1,2", "1,2", "1", "1,2", "1,2", "2,3", "2", "1",
                     "2", "1", "1,2", "1,2", "1", "1", "1", "1", "1,2", "1",
                     "2", "1,2", "2", "2", "2", "2", "1", "1,2", "1,2", "1",
                     "1", "1,2", "1,2", "2,3", "1,2", "1,2", "2", "2", "2", "1,2",
                     "2"
                   )
  ),
  
  # ────────────────────────────────────────────────────────────
  # RICE — 113 countries
  # Tbase = 10°C  |  SPAM = RICE
  # ────────────────────────────────────────────────────────────
  rice = data.frame(stringsAsFactors = FALSE,
                    Country_ISO    = c(
                      "AFG", "AGO", "ALB", "ARG", "ARM", "AUS", "AZE", "BDI", "BEN", "BFA",
                      "BGD", "BGR", "BOL", "BRA", "CHL", "CHN", "CIV", "CMR", "COD", "COL",
                      "CRI", "CUB", "CZE", "DEU", "DNK", "DOM", "ECU", "EGY", "ESP", "EST",
                      "ETH", "FIN", "FRA", "GEO", "GHA", "GIN", "GMB", "GRC", "GUY", "HRV",
                      "HTI", "HUN", "IDN", "IND", "IRL", "IRN", "IRQ", "ISR", "ITA", "JOR",
                      "JPN", "KAZ", "KEN", "KHM", "KOR", "LAO", "LBR", "LKA", "LTU", "LVA",
                      "MDG", "MEX", "MKD", "MLI", "MMR", "MNG", "MOZ", "MWI", "MYS", "NER",
                      "NGA", "NIC", "NLD", "NPL", "PAK", "PAN", "PER", "PHL", "PNG", "POL",
                      "PRK", "PRT", "PRY", "ROU", "RUS", "RWA", "SDN", "SEN", "SLE", "SOM",
                      "SRB", "SSD", "SUR", "SVK", "SVN", "SWE", "SYR", "TCD", "TGO", "THA",
                      "TKM", "TLS", "TUR", "TZA", "UGA", "UKR", "URY", "USA", "UZB", "VEN",
                      "VNM", "ZMB", "ZWE"
                    ),
                    Early_Period = c(
                      "5,6", "10,11", "5,6", "10,11", "5,6", "10,11", "5,6", "3,4", "4,5", "6,7",
                      "6,7", "5,6", "10,11", "10,11", "10,11", "4,5", "4,5", "3,4", "3,4", "3,4",
                      "4,5", "3,4", "5,6", "5,6", "5,6", "3,4", "1,2", "4,5", "4,5", "5,6",
                      "3,4", "5,6", "5,6", "5,6", "4,5", "6,7", "7,8", "5,6", "3,4", "5,6",
                      "4,5", "5,6", "11,12", "6,7", "5,6", "5,6", "4,5", "4,5", "4,5", "4,5",
                      "4,5", "5,6", "3,4", "6,7", "4,5", "6,7", "4,5", "4,5", "5,6", "5,6",
                      "10,11", "4,5", "5,6", "6,7", "6,7", "5,6", "10,11", "10,11", "11,12", "7,8",
                      "4,5", "4,5", "5,6", "6,7", "6,7", "3,4", "11,12", "6,7", "11,12", "5,6",
                      "5,6", "5,6", "10,11", "5,6", "5,6", "3,4", "4,5", "7,8", "5,6", "3,4",
                      "5,6", "3,4", "4,5", "5,6", "5,6", "5,6", "5,6", "6,7", "4,5", "6,7",
                      "5,6", "11,12", "4,5", "3,4", "3,4", "5,6", "10,11", "4,5", "5,6", "4,5",
                      "2,3", "10,11", "10,11"
                    ),
                    Flowering = c(
                      "7,8", "2,3", "7,8", "1,2", "8,9", "1,2", "8,9", "6,7", "8,9", "10,11",
                      "9,10", "7,8", "1,2", "1,2", "1,2", "7,8", "8,9", "6,7", "6,7", "6,7",
                      "7,8", "6,7", "7,8", "7,8", "7,8", "6,7", "4,5", "7,8", "7,8", "7,8",
                      "6,7", "7,8", "7,8", "8,9", "7,8", "10,11", "11,12", "7,8", "7,8", "7,8",
                      "7,8", "7,8", "3,4", "9,10", "7,8", "8,9", "7,8", "8,9", "7,8", "7,8",
                      "8,9", "7,8", "6,7", "10,11", "8,9", "10,11", "8,9", "8,9", "7,8", "7,8",
                      "1,2", "8,9", "7,8", "10,11", "9,10", "8,9", "1,2", "1,2", "3,4", "10,11",
                      "7,8", "7,8", "7,8", "9,10", "9,10", "6,7", "2,3", "9,10", "2,3", "7,8",
                      "8,9", "7,8", "1,2", "7,8", "8,9", "6,7", "7,8", "10,11", "9,10", "6,7",
                      "8,9", "6,7", "7,8", "7,8", "7,8", "7,8", "8,9", "10,11", "8,9", "10,11",
                      "8,9", "2,3", "7,8", "6,7", "6,7", "7,8", "1,2", "7,8", "7,8", "8,9",
                      "5,6", "1,2", "1,2"
                    ),
                    Growing_Season = c(
                      "9,10", "4", "9,10", "3", "10,11", "3", "10,11", "8", "10", "12",
                      "11", "9,10", "3", "3", "3,4", "9", "10", "8,9", "8,9", "8,9",
                      "9,10", "8,9", "9,10", "9,10", "9,10", "8", "6", "9,10", "9", "9,10",
                      "8,9", "9,10", "9,10", "10,11", "9,10", "12", "1", "9,10", "9", "9,10",
                      "9,10", "9,10", "5", "11", "9,10", "10,11", "9,10", "10,11", "9", "9,10",
                      "10", "9,10", "8", "12", "10", "12", "10", "10", "9,10", "9,10",
                      "3,4", "10", "9,10", "12", "11", "10,11", "3,4", "3,4", "5", "12",
                      "9,10", "9,10", "9,10", "11", "11", "8,9", "4", "11", "4", "9,10",
                      "10,11", "9,10", "3", "9,10", "9,10", "8,9", "9,10", "12", "11", "8,9",
                      "10,11", "8,9", "9", "9,10", "9,10", "9,10", "10,11", "12", "10", "12",
                      "10,11", "4", "9,10", "8,9", "8,9", "9,10", "3", "9", "9,10", "10",
                      "7", "3,4", "3,4"
                    ),
                    Drought_Memory = c(
                      "1,2", "1,2", "1,2", "1", "1,2", "2", "1,2", "1", "1", "1,2",
                      "1,2", "1,2", "1", "1", "1,2", "1", "1", "1", "1", "1",
                      "1", "1", "1,2", "1,2", "1,2", "1", "1", "1,2", "1", "1,2",
                      "1", "1,2", "1,2", "1,2", "1", "1,2", "2", "1,2", "1", "1,2",
                      "1", "1,2", "1", "1", "1,2", "1,2", "1", "1,2", "1", "1",
                      "1", "1,2", "1,2", "1,2", "1", "1", "1,2", "1", "1,2", "1,2",
                      "1", "1", "1,2", "1,2", "1", "1,2", "1", "1", "1", "2",
                      "1", "1", "1,2", "1", "1", "1", "1", "1", "1,2", "1,2",
                      "1,2", "1,2", "1", "1,2", "1,2", "1", "1", "2", "1,2", "1",
                      "1,2", "1", "1", "1,2", "1,2", "1,2", "1,2", "2", "1", "1",
                      "1,2", "2", "1,2", "1", "1", "1,2", "1", "1", "1,2", "1",
                      "1", "1", "1"
                    )
  ),
  
  # ────────────────────────────────────────────────────────────
  # MAIZE — 116 countries
  # Tbase = 10°C  |  SPAM = MAIZ
  # ────────────────────────────────────────────────────────────
  maize = data.frame(stringsAsFactors = FALSE,
                     Country_ISO    = c(
                       "AFG", "AGO", "ALB", "ARG", "ARM", "AUT", "AZE", "BEN", "BFA", "BGD",
                       "BGR", "BIH", "BLR", "BOL", "BRA", "BWA", "CAN", "CHL", "CHN", "CIV",
                       "CMR", "COD", "COL", "CRI", "CUB", "CZE", "DEU", "DOM", "DZA", "ECU",
                       "EGY", "ERI", "ESP", "ETH", "FRA", "GEO", "GHA", "GIN", "GRC", "GTM",
                       "HND", "HRV", "HTI", "HUN", "IDN", "IND", "IRL", "IRN", "IRQ", "ISR",
                       "ITA", "JOR", "JPN", "KAZ", "KEN", "KHM", "KOR", "LAO", "LBN", "LBY",
                       "LKA", "MAR", "MDA", "MDG", "MEX", "MKD", "MLI", "MLT", "MMR", "MNE",
                       "MNG", "MOZ", "MWI", "NAM", "NER", "NGA", "NIC", "NOR", "NPL", "PAK",
                       "PAN", "PER", "PHL", "POL", "PRK", "PRT", "PRY", "ROU", "RUS", "RWA",
                       "SAU", "SDN", "SEN", "SLE", "SOM", "SRB", "SSD", "SVK", "SVN", "SYR",
                       "TCD", "THA", "TKM", "TUN", "TUR", "TZA", "UGA", "UKR", "USA", "UZB",
                       "VEN", "VNM", "YEM", "ZAF", "ZMB", "ZWE"
                     ),
                     Early_Period = c(
                       "4,5", "9,10", "4,5", "10,11", "4,5", "4,5", "4,5", "4,5", "4,5", "4,5",
                       "4,5", "4,5", "4,5", "10,11", "10,11", "10,11", "4,5", "10,11", "4,5", "3,4",
                       "3,4", "9,10", "3,4", "4,5", "3,4", "4,5", "4,5", "4,5", "3,4", "1,2",
                       "3,4", "5,6", "4,5", "3,4", "4,5", "4,5", "4,5", "4,5", "4,5", "5,6",
                       "4,5", "4,5", "4,5", "4,5", "11,12", "6,7", "4,5", "3,4", "4,5", "3,4",
                       "4,5", "3,4", "4,5", "5,6", "3,4", "4,5", "4,5", "4,5", "11,12", "3,4",
                       "3,4", "3,4", "4,5", "10,11", "5,6", "4,5", "5,6", "11,12", "4,5", "4,5",
                       "5,6", "10,11", "10,11", "10,11", "5,6", "4,5", "4,5", "4,5", "3,4", "5,6",
                       "4,5", "9,10", "11,12", "4,5", "4,5", "4,5", "10,11", "4,5", "5,6", "3,4",
                       "4,5", "5,6", "5,6", "4,5", "3,4", "4,5", "3,4", "4,5", "4,5", "4,5",
                       "5,6", "4,5", "4,5", "3,4", "4,5", "3,4", "3,4", "4,5", "4,5", "5,6",
                       "3,4", "2,3", "3,4", "11,12", "10,11", "10,11"
                     ),
                     Flowering = c(
                       "7,8", "12,1", "7,8", "1,2", "7,8", "7,8", "7,8", "7,8", "7,8", "7,8",
                       "7,8", "7,8", "7,8", "1,2", "1,2", "1,2", "7,8", "1,2", "7,8", "7,8",
                       "6,7", "12,1", "6,7", "7,8", "6,7", "7,8", "7,8", "7,8", "6,7", "4,5",
                       "6,7", "8,9", "7,8", "6,7", "7,8", "7,8", "7,8", "7,8", "7,8", "8,9",
                       "8,9", "7,8", "7,8", "7,8", "2,3", "9,10", "7,8", "6,7", "7,8", "6,7",
                       "7,8", "6,7", "7,8", "8,9", "6,7", "7,8", "7,8", "7,8", "3,4", "6,7",
                       "6,7", "6,7", "7,8", "1,2", "8,9", "7,8", "8,9", "3,4", "7,8", "7,8",
                       "8,9", "1,2", "1,2", "1,2", "8,9", "7,8", "7,8", "7,8", "6,7", "8,9",
                       "7,8", "12,1", "2,3", "7,8", "8,9", "7,8", "1,2", "7,8", "8,9", "6,7",
                       "7,8", "8,9", "8,9", "7,8", "6,7", "7,8", "6,7", "7,8", "7,8", "7,8",
                       "8,9", "7,8", "7,8", "6,7", "7,8", "6,7", "6,7", "7,8", "7,8", "8,9",
                       "6,7", "5,6", "6,7", "2,3", "1,2", "1,2"
                     ),
                     Growing_Season = c(
                       "9,10", "2,3", "9,10", "3,4", "9,10", "9,10", "9,10", "9,10", "9,10", "9,10",
                       "9,10", "9,10", "9,10", "3,4", "3,4", "3,4", "9,10", "3,4", "9,10", "9,10",
                       "8,9", "2,3", "8,9", "9,10", "8,9", "9,10", "9,10", "9,10", "8,9", "6,7",
                       "8,9", "10,11", "9,10", "8,9", "9,10", "9,10", "9,10", "9,10", "9,10", "10,11",
                       "10,11", "9,10", "9,10", "9,10", "4,5", "11,12", "9,10", "8,9", "9,10", "8,9",
                       "9,10", "8,9", "9,10", "9,10", "8,9", "9,10", "9,10", "9,10", "5,6", "8,9",
                       "8,9", "8,9", "9,10", "3,4", "10,11", "9,10", "10,11", "5,6", "9,10", "9,10",
                       "9,10", "3,4", "3,4", "3,4", "10,11", "9,10", "9,10", "9,10", "8,9", "10,11",
                       "9,10", "2,3", "4,5", "9,10", "10,11", "9,10", "3,4", "9,10", "9,10", "8,9",
                       "9,10", "10,11", "10,11", "9,10", "8,9", "9,10", "8,9", "9,10", "9,10", "9,10",
                       "10,11", "9,10", "9,10", "8,9", "9,10", "8,9", "8,9", "9,10", "9,10", "9,10",
                       "8,9", "7,8", "8,9", "4,5", "3,4", "3,4"
                     ),
                     Drought_Memory = c(
                       "1,2", "1", "1,2", "2", "1,2", "1", "1,2", "1", "1", "1,2",
                       "1", "1,2", "1", "2", "1", "1", "1", "1", "1", "1",
                       "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
                       "1,2", "1", "1", "1", "1", "1,2", "1", "1", "1", "1",
                       "1", "1,2", "1", "1", "1", "1", "1,2", "1,2", "1", "1",
                       "1", "1", "1", "1,2", "1", "1", "1", "1", "1", "1",
                       "1,2", "1", "1,2", "1", "1", "1,2", "1", "1", "1", "1,2",
                       "1,2", "1", "1", "1", "1", "1", "1", "1,2", "1,2", "1",
                       "1", "1", "1", "1", "1,2", "1", "2", "1", "1,2", "1",
                       "1,2", "1", "1", "1", "1", "1,2", "1", "1", "1,2", "1,2",
                       "1", "1", "1,2", "1", "1,2", "1", "1", "1", "1", "1,2",
                       "1", "1", "1", "1,2", "1", "1"
                     )
  ),
  
  # ────────────────────────────────────────────────────────────
  # SOYBEAN — 105 countries
  # Tbase = 10°C  |  SPAM = SOYB
  # ────────────────────────────────────────────────────────────
  soybean = data.frame(stringsAsFactors = FALSE,
                       Country_ISO    = c(
                         "AFG", "AGO", "ALB", "ARG", "ARM", "AUS", "AUT", "AZE", "BDI", "BEN",
                         "BFA", "BGD", "BGR", "BIH", "BOL", "BRA", "CAN", "CHL", "CHN", "CIV",
                         "CMR", "COD", "COL", "CZE", "DEU", "DOM", "DZA", "ECU", "EGY", "ESP",
                         "EST", "ETH", "FRA", "GEO", "GHA", "GRC", "GTM", "HND", "HRV", "HTI",
                         "HUN", "IDN", "IND", "IRL", "IRN", "IRQ", "ITA", "JOR", "JPN", "KAZ",
                         "KEN", "KHM", "KOR", "LAO", "LTU", "LVA", "MAR", "MDA", "MDG", "MEX",
                         "MKD", "MLI", "MMR", "MNE", "MNG", "MOZ", "MWI", "MYS", "NAM", "NER",
                         "NGA", "NIC", "NLD", "NZL", "PAK", "PER", "POL", "PRK", "PRT", "PRY",
                         "ROU", "RUS", "RWA", "SDN", "SEN", "SLE", "SLV", "SRB", "SVK", "TGO",
                         "THA", "TUN", "TUR", "TZA", "UGA", "UKR", "URY", "USA", "UZB", "VEN",
                         "VNM", "YEM", "ZAF", "ZMB", "ZWE"
                       ),
                       Early_Period = c(
                         "4,5", "9,10", "5,6", "11,12", "4,5", "10,11", "4,5", "4,5", "3,4", "4,5",
                         "4,5", "4,5", "4,5", "5,6", "9,10", "10,11", "5,6", "9,10", "5,6", "3,4",
                         "3,4", "3,4", "3,4", "4,5", "5,6", "4,5", "3,4", "1,2", "4,5", "4,5",
                         "5,6", "3,4", "4,5", "4,5", "4,5", "4,5", "5,6", "5,6", "5,6", "4,5",
                         "5,6", "4,5", "6,7", "5,6", "4,5", "4,5", "4,5", "4,5", "5,6", "5,6",
                         "3,4", "4,5", "6,7", "4,5", "5,6", "5,6", "3,4", "4,5", "10,11", "5,6",
                         "5,6", "5,6", "5,6", "5,6", "5,6", "10,11", "10,11", "4,5", "10,11", "5,6",
                         "4,5", "4,5", "5,6", "10,11", "4,5", "9,10", "5,6", "5,6", "5,6", "9,10",
                         "5,6", "5,6", "3,4", "5,6", "5,6", "4,5", "5,6", "5,6", "5,6", "4,5",
                         "10,11", "3,4", "5,6", "3,4", "3,4", "5,6", "11,12", "5,6", "4,5", "3,4",
                         "3,4", "3,4", "11,12", "10,11", "10,11"
                       ),
                       Flowering = c(
                         "7,8", "12,1", "7,8", "2,3", "7,8", "1,2", "7,8", "7,8", "6,7", "7,8",
                         "7,8", "7,8", "7,8", "7,8", "12,1", "1,2", "8,9", "1,2", "8,9", "7,8",
                         "6,7", "6,7", "6,7", "7,8", "7,8", "7,8", "6,7", "4,5", "7,8", "7,8",
                         "7,8", "6,7", "7,8", "7,8", "7,8", "7,8", "8,9", "8,9", "7,8", "7,8",
                         "7,8", "7,8", "9,10", "7,8", "7,8", "7,8", "7,8", "7,8", "8,9", "7,8",
                         "6,7", "7,8", "9,10", "7,8", "7,8", "7,8", "6,7", "8,9", "1,2", "8,9",
                         "7,8", "8,9", "8,9", "7,8", "8,9", "1,2", "1,2", "7,8", "1,2", "8,9",
                         "7,8", "7,8", "7,8", "1,2", "7,8", "1,2", "7,8", "8,9", "7,8", "12,1",
                         "7,8", "7,8", "6,7", "8,9", "8,9", "7,8", "8,9", "7,8", "7,8", "7,8",
                         "1,2", "6,7", "7,8", "6,7", "6,7", "7,8", "2,3", "8,9", "7,8", "6,7",
                         "6,7", "6,7", "2,3", "1,2", "1,2"
                       ),
                       Growing_Season = c(
                         "9,10", "2,3", "9,10", "4,5", "9,10", "3,4", "9,10", "9,10", "8,9", "9,10",
                         "9,10", "9,10", "9,10", "9,10", "2,3", "3,4", "10,11", "3,4", "10,11", "9,10",
                         "8,9", "8,9", "8,9", "9,10", "9,10", "9,10", "8,9", "6,7", "9,10", "9,10",
                         "9,10", "8,9", "9,10", "9,10", "9,10", "9,10", "10,11", "10,11", "9,10", "9,10",
                         "9,10", "9,10", "11,12", "9,10", "9,10", "9,10", "9,10", "9,10", "10,11", "9,10",
                         "8,9", "9,10", "11,12", "9,10", "9,10", "9,10", "8,9", "10,11", "3,4", "10,11",
                         "9,10", "10,11", "10,11", "9,10", "10,11", "3,4", "3,4", "9,10", "3,4", "10,11",
                         "9,10", "9,10", "9,10", "3,4", "9,10", "3,4", "9,10", "10,11", "9,10", "2,3",
                         "9,10", "9,10", "8,9", "10,11", "10,11", "9,10", "10,11", "9,10", "9,10", "9,10",
                         "3,4", "8,9", "9,10", "8,9", "8,9", "9,10", "4,5", "10,11", "9,10", "8,9",
                         "8,9", "8,9", "4,5", "3,4", "3,4"
                       ),
                       Drought_Memory = c(
                         "1,2", "1", "1,2", "2", "1,2", "1,2", "1", "1,2", "1", "1",
                         "1", "1", "1", "1,2", "1", "1,2", "1", "1,2", "1", "1",
                         "1", "1", "1", "1", "1,2", "1", "1", "1", "1,2", "1",
                         "1,2", "1", "1", "1,2", "1,2", "1", "1", "1", "1,2", "1",
                         "1,2", "1", "1", "1,2", "1,2", "1", "1", "1", "1", "1,2",
                         "1", "1,2", "1", "1", "1,2", "1,2", "1", "1", "1", "1",
                         "1,2", "1", "1", "1,2", "1", "1", "1", "1", "1", "1",
                         "1", "1", "1,2", "1", "1", "1,2", "1,2", "1", "1,2", "1",
                         "1,2", "1,2", "1", "1", "1", "1", "1", "1,2", "1,2", "1",
                         "1", "1", "1,2", "1", "1", "1,2", "2", "1", "1,2", "1",
                         "1", "1", "2", "1", "1"
                       )
  ),
  
  # ────────────────────────────────────────────────────────────
  # SORGHUM — 108 countries
  # Tbase = 10°C  |  SPAM = SORG
  # ────────────────────────────────────────────────────────────
  sorghum = data.frame(stringsAsFactors = FALSE,
                       Country_ISO    = c(
                         "AFG", "AGO", "ALB", "ARG", "ARM", "AUS", "AUT", "AZE", "BDI", "BEN",
                         "BFA", "BGD", "BGR", "BOL", "BRA", "BWA", "CHN", "CMR", "COD", "COG",
                         "COL", "CRI", "CUB", "DOM", "DZA", "ECU", "EGY", "ERI", "ESP", "ETH",
                         "FRA", "GEO", "GHA", "GIN", "GMB", "GRC", "GTM", "HND", "HRV", "HTI",
                         "HUN", "IDN", "IND", "IRN", "IRQ", "ISR", "ITA", "JOR", "JPN", "KAZ",
                         "KEN", "KGZ", "KOR", "LBR", "LBY", "LTU", "LVA", "MAR", "MDA", "MDG",
                         "MEX", "MLI", "MMR", "MOZ", "MWI", "NAM", "NER", "NGA", "NIC", "OMN",
                         "PAK", "PAN", "PER", "PHL", "PNG", "POL", "PRT", "PRY", "ROU", "RUS",
                         "RWA", "SAU", "SDN", "SEN", "SLE", "SOM", "SRB", "SSD", "SVK", "SVN",
                         "SYR", "TCD", "TGO", "THA", "TKM", "TUN", "TUR", "TZA", "UGA", "UKR",
                         "URY", "USA", "UZB", "VEN", "YEM", "ZAF", "ZMB", "ZWE"
                       ),
                       Early_Period = c(
                         "5,6", "9,10", "4,5", "11,12", "4,5", "10,11", "5,6", "4,5", "3,4", "5,6",
                         "5,6", "4,5", "4,5", "10,11", "2,3", "10,11", "5,6", "4,5", "3,4", "3,4",
                         "3,4", "4,5", "3,4", "4,5", "3,4", "3,4", "5,6", "5,6", "4,5", "5,6",
                         "4,5", "4,5", "4,5", "4,5", "7,8", "4,5", "4,5", "4,5", "4,5", "3,4",
                         "4,5", "6,7", "6,7", "4,5", "4,5", "3,4", "4,5", "4,5", "4,5", "5,6",
                         "4,5", "5,6", "4,5", "4,5", "5,6", "5,6", "5,6", "3,4", "4,5", "10,11",
                         "6,7", "5,6", "5,6", "10,11", "10,11", "10,11", "6,7", "5,6", "4,5", "9,10",
                         "6,7", "4,5", "9,10", "10,11", "4,5", "9,10", "10,11", "10,11", "4,5", "5,6",
                         "3,4", "4,5", "7,8", "6,7", "5,6", "3,4", "4,5", "3,4", "9,10", "10,11",
                         "4,5", "6,7", "5,6", "10,11", "5,6", "3,4", "4,5", "3,4", "3,4", "4,5",
                         "11,12", "5,6", "5,6", "3,4", "3,4", "10,11", "10,11", "10,11"
                       ),
                       Flowering = c(
                         "8,9", "12,1", "7,8", "2,3", "7,8", "1,2", "7,8", "7,8", "6,7", "8,9",
                         "8,9", "7,8", "7,8", "1,2", "5,6", "1,2", "8,9", "7,8", "6,7", "6,7",
                         "6,7", "7,8", "6,7", "7,8", "6,7", "6,7", "8,9", "8,9", "7,8", "8,9",
                         "7,8", "7,8", "7,8", "7,8", "10,11", "7,8", "7,8", "7,8", "7,8", "6,7",
                         "7,8", "9,10", "9,10", "7,8", "7,8", "6,7", "7,8", "7,8", "8,9", "8,9",
                         "7,8", "8,9", "7,8", "7,8", "8,9", "8,9", "8,9", "6,7", "7,8", "1,2",
                         "9,10", "8,9", "8,9", "1,2", "1,2", "1,2", "9,10", "8,9", "7,8", "12,1",
                         "9,10", "7,8", "12,1", "1,2", "7,8", "12,1", "1,2", "1,2", "7,8", "8,9",
                         "6,7", "7,8", "9,10", "9,10", "8,9", "6,7", "7,8", "6,7", "12,1", "1,2",
                         "7,8", "9,10", "8,9", "1,2", "8,9", "6,7", "7,8", "6,7", "6,7", "7,8",
                         "2,3", "7,8", "8,9", "6,7", "6,7", "1,2", "1,2", "1,2"
                       ),
                       Growing_Season = c(
                         "10,11", "2,3", "9,10", "4,5", "9,10", "3,4", "9,10", "9,10", "8,9", "10,11",
                         "10,11", "9,10", "9,10", "3,4", "7,8", "3,4", "10,11", "9,10", "8,9", "8,9",
                         "8,9", "9,10", "8,9", "9,10", "8,9", "8,9", "9,10", "10,11", "9,10", "10,11",
                         "9,10", "9,10", "9,10", "9,10", "12,1", "9,10", "9,10", "9,10", "9,10", "8,9",
                         "9,10", "11,12", "10,11", "9,10", "9,10", "8,9", "9,10", "9,10", "10,11", "9,10",
                         "9,10", "9,10", "9,10", "9,10", "10,11", "9,10", "9,10", "8,9", "9,10", "3,4",
                         "11,12", "10,11", "10,11", "3,4", "3,4", "3,4", "10,11", "10,11", "9,10", "2,3",
                         "10,11", "9,10", "2,3", "3,4", "9,10", "2,3", "3,4", "3,4", "9,10", "9,10",
                         "8,9", "9,10", "10,11", "10,11", "10,11", "8,9", "9,10", "8,9", "2,3", "3,4",
                         "9,10", "10,11", "10,11", "3,4", "9,10", "8,9", "9,10", "8,9", "8,9", "9,10",
                         "4,5", "9,10", "9,10", "8,9", "8,9", "3,4", "3,4", "3,4"
                       ),
                       Drought_Memory = c(
                         "1,2", "1", "1,2", "1", "1,2", "2", "1,2", "1,2", "1", "1",
                         "1", "1,2", "1", "1", "1", "1", "1,2", "1", "1", "1",
                         "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
                         "1", "1,2", "1", "1", "2", "1,2", "1", "1", "1,2", "1",
                         "1", "1", "1", "1,2", "1", "1", "1", "1", "1", "1,2",
                         "1", "1,2", "1", "1", "2", "1", "1", "1", "1,2", "1",
                         "1", "1", "1", "1", "1", "1", "1", "1", "1", "2",
                         "1", "1", "1", "1", "1", "1", "1", "1,2", "1", "1,2",
                         "1", "1,2", "1", "1", "1", "1", "1", "1", "1", "1",
                         "1", "1", "1", "1", "1,2", "1", "1,2", "1", "1", "1",
                         "1", "1", "1,2", "1", "1", "1", "1", "1"
                       )
  )
  
)

cat("[OK] Phenology windows defined inline for all crops:\n")
for (u in names(feno_patch))
  cat(sprintf("  %-10s: %d countries\n", u, nrow(feno_patch[[u]])))


# ==============================================================================
# STEP 2 — FUNCTION: panfin_* generation (monthly → annual + FAO join)
#
# For each crop:
#   1. Merge phenology windows (patch + Excel) into monthly panel
#   2. Assign phenological flags: is_early, is_flower, is_grain, is_memory
#   3. Compute SPEI3 (3-month water balance index) and GDD
#   4. Aggregate to annual: TMX flowering mean, PRE sums, GDD, heat days, etc.
#   5. Join FAO yield, CO2, GDP
# ==============================================================================

isle_adim2 <- function(urun_adi, pan_df, tbase, fao_urun_df) {
  
  # v5: phenology loaded from Global_AgriClimate_v5.xlsx at startup
  feno_ulke <- feno_patch[[urun_adi]]
  cat(sprintf("  [Pheno v5] %d countries\n", nrow(feno_ulke)))
  
  # Join phenology to monthly panel
  pan_df <- pan_df %>%
    dplyr::select(-dplyr::any_of(
      c("Early_Period","Flowering","Growing_Season","Drought_Memory"))) %>%
    dplyr::left_join(dplyr::distinct(feno_ulke, Country_ISO, .keep_all = TRUE),
                     by = "Country_ISO", relationship = "many-to-one") %>%
    dplyr::filter(!is.na(Flowering))
  
  # Parse month strings ("4,5" → c(4, 5))
  feno_parse <- pan_df %>%
    dplyr::distinct(Country_ISO, .keep_all = TRUE) %>%
    dplyr::select(Country_ISO, Early_Period, Flowering,
                  Growing_Season, Drought_Memory) %>%
    dplyr::mutate(
      early_m  = lapply(strsplit(as.character(Early_Period),   ","),
                        function(x) as.integer(trimws(x))),
      flower_m = lapply(strsplit(as.character(Flowering),      ","),
                        function(x) as.integer(trimws(x))),
      grain_m  = lapply(strsplit(as.character(Growing_Season), ","),
                        function(x) as.integer(trimws(x))),
      memory_m = lapply(strsplit(as.character(Drought_Memory), ","),
                        function(x) as.integer(trimws(x))))
  
  # Helper: unnest month lists to long format with a boolean flag
  unnest_m <- function(df, col, flag) {
    df %>%
      dplyr::select(Country_ISO, months = dplyr::all_of(col)) %>%
      tidyr::unnest(months) %>%
      dplyr::rename(Ay = months) %>%
      dplyr::mutate(!!flag := TRUE)
  }
  
  # Assign phenological flags to each month row
  pan_df <- pan_df %>%
    dplyr::left_join(unnest_m(feno_parse, "early_m",  "is_early"),
                     by = c("Country_ISO","Ay"), relationship = "many-to-one") %>%
    dplyr::left_join(unnest_m(feno_parse, "flower_m", "is_flower"),
                     by = c("Country_ISO","Ay"), relationship = "many-to-one") %>%
    dplyr::left_join(unnest_m(feno_parse, "grain_m",  "is_grain"),
                     by = c("Country_ISO","Ay"), relationship = "many-to-one") %>%
    dplyr::left_join(unnest_m(feno_parse, "memory_m", "is_memory"),
                     by = c("Country_ISO","Ay"), relationship = "many-to-one") %>%
    dplyr::mutate(dplyr::across(
      c(is_early, is_flower, is_grain, is_memory),
      ~tidyr::replace_na(., FALSE)))
  
  # Compute SPEI3 (3-month rolling water balance) and monthly GDD
  pan_df <- pan_df %>%
    dplyr::arrange(Country_ISO, Yil, Ay) %>%
    dplyr::group_by(Country_ISO) %>%
    dplyr::mutate(
      WB_monthly  = PRE - PET,                         # monthly water balance
      SPEI3_raw   = zoo::rollsum(WB_monthly, k = 3,
                                 fill = NA, align = "right"),
      SPEI3_std   = (SPEI3_raw - mean(SPEI3_raw, na.rm = TRUE)) /
        sd(SPEI3_raw, na.rm = TRUE),       # standardised SPEI3
      GDD_monthly = pmax(TMP - tbase, 0) * 30) %>%     # GDD × 30 days
    dplyr::ungroup()
  
  # Aggregate to annual: one row per country × year
  pan_yil <- pan_df %>%
    dplyr::group_by(Country_ISO, Yil) %>%
    dplyr::summarise(
      # Key variables used in TWFE models
      TMP_earlymean     = mean(TMP[is_early],        na.rm = TRUE),
      TMX_floweringmean = mean(TMX[is_flower],       na.rm = TRUE),
      TMN_floweringmean = mean(TMN[is_flower],       na.rm = TRUE),
      PRE_floweringsum  = sum(PRE[is_flower],        na.rm = TRUE),
      PRE_grainfillsum  = sum(PRE[is_grain],         na.rm = TRUE),
      GDD_growth        = sum(GDD_monthly[is_grain], na.rm = TRUE),
      Frost_flowering   = sum(TMN[is_flower] < 0,    na.rm = TRUE),
      Heat_flowering    = sum(TMX[is_flower] > 30,   na.rm = TRUE),
      SPEI3_memory      = mean(SPEI3_std[is_memory], na.rm = TRUE),
      WaterBalance      = sum(PRE, na.rm = TRUE) - sum(PET, na.rm = TRUE),
      AridityIndex      = ifelse(sum(PET, na.rm = TRUE) > 0,
                                 sum(PRE, na.rm = TRUE) /
                                   sum(PET, na.rm = TRUE), NA_real_),
      TempVariance      = var(TMP, na.rm = TRUE),
      # Annual summaries
      TMP_ann = mean(TMP, na.rm = TRUE), TMX_ann = mean(TMX, na.rm = TRUE),
      TMN_ann = mean(TMN, na.rm = TRUE), DTR_ann = mean(DTR, na.rm = TRUE),
      CLD_ann = mean(CLD, na.rm = TRUE), VAP_ann = mean(VAP, na.rm = TRUE),
      PRE_ann = sum(PRE,  na.rm = TRUE), PET_ann = sum(PET, na.rm = TRUE),
      WET_ann = sum(WET,  na.rm = TRUE), FRS_ann = sum(FRS, na.rm = TRUE),
      .groups = "drop") %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                ~ifelse(is.nan(.), NA_real_, .)))
  
  # Join phenology, FAO yield, CO2, GDP
  pan_yil <- pan_yil %>%
    dplyr::left_join(
      dplyr::select(feno_ulke, Country_ISO,
                    Early_Period, Flowering, Growing_Season, Drought_Memory),
      by = "Country_ISO", relationship = "many-to-one") %>%
    dplyr::left_join(
      fao_urun_df[, c("Country_ISO","Year","area_harvested_ha",
                      "production_tonnes","yield_hg_per_ha")],
      by = c("Country_ISO", "Yil" = "Year")) %>%
    dplyr::left_join(co2_df, by = "Yil") %>%
    dplyr::left_join(gdp_df, by = c("Country_ISO","Yil"))
  
  # Reorder columns: key identifiers first
  onc <- c("Country_ISO","Yil","production_tonnes","area_harvested_ha",
           "yield_hg_per_ha","CO2_ppm","GDP_per_Capita",
           "TMP_earlymean","TMX_floweringmean","TMN_floweringmean",
           "PRE_floweringsum","PRE_grainfillsum","GDD_growth",
           "Frost_flowering","Heat_flowering","SPEI3_memory",
           "WaterBalance","AridityIndex","TempVariance",
           "TMP_ann","TMX_ann","TMN_ann","DTR_ann","CLD_ann","VAP_ann",
           "PRE_ann","PET_ann","WET_ann","FRS_ann",
           "Early_Period","Flowering","Growing_Season","Drought_Memory")
  onc_mev <- onc[onc %in% names(pan_yil)]
  pan_yil[, c(onc_mev, setdiff(names(pan_yil), onc_mev))]
}


# ── Pre-Step 2 diagnostics: pan1 × FAO × Phenology coverage check ────────────

cat("\n", strrep("=", 70), "\n")
cat("PRE-STEP 2 DIAGNOSTICS — pan1 × FAO × Phenology coverage\n")
cat(strrep("=", 70), "\n\n")

pan1_iso_liste <- list()
for (urun_adi in names(urun_config)) {
  cfg   <- urun_config[[urun_adi]]
  pan_f <- file.path(ana_yol, paste0(cfg$p1, ".xlsx"))
  if (!file.exists(pan_f)) {
    pan1_iso_liste[[urun_adi]] <- character(0); next
  }
  pan_tmp <- as.data.frame(readxl::read_excel(pan_f))
  pan_iso <- sort(unique(pan_tmp$Country_ISO))
  pan1_iso_liste[[urun_adi]] <- pan_iso
  
  fao_iso  <- unique(fao_wide$Country_ISO[fao_wide$Item %in% cfg$fao])
  # v5: all phenology in feno_patch
  feno_tum <- unique(feno_patch[[urun_adi]]$Country_ISO)
  
  pan1_no_fao  <- pan_iso[!pan_iso %in% fao_iso]
  pan1_no_feno <- pan_iso[!pan_iso %in% feno_tum]
  
  cat(sprintf("[%-8s] pan1:%3d  FAO:%3d  Pheno:%3d  Both:%3d\n",
              toupper(urun_adi), length(pan_iso),
              sum(pan_iso %in% fao_iso),
              sum(pan_iso %in% feno_tum),
              sum(pan_iso %in% fao_iso & pan_iso %in% feno_tum)))
  if (length(pan1_no_fao) > 0)
    cat(sprintf("  [NO FAO   (%2d)]: %s\n",
                length(pan1_no_fao), paste(pan1_no_fao, collapse = ", ")))
  if (length(pan1_no_feno) > 0)
    cat(sprintf("  [NO PHENO (%2d)]: %s\n",
                length(pan1_no_feno), paste(pan1_no_feno, collapse = ", ")))
  cat("\n")
}


# ── Run Step 2: pan1* → panfin_* ─────────────────────────────────────────────

cat(strrep("=", 60), "\nSTEP 2: pan1* → panfin_*\n",
    strrep("=", 60), "\n\n")

for (urun_adi in names(urun_config)) {
  cfg    <- urun_config[[urun_adi]]
  pan_in <- file.path(ana_yol, paste0(cfg$p1, ".xlsx"))
  panout <- file.path(ana_yol, paste0("panfin_", urun_adi, ".xlsx"))
  cat("\n---", toupper(urun_adi), "---\n")
  if (!file.exists(pan_in)) { cat("[ERROR] pan1 missing\n"); next }
  pan_raw <- as.data.frame(readxl::read_excel(pan_in))
  cat("[OK]", nrow(pan_raw), "rows |",
      length(unique(pan_raw$Country_ISO)), "countries\n")
  fao_sub <- fao_wide[fao_wide$Item %in% cfg$fao, ]
  pan_son <- tryCatch(
    isle_adim2(urun_adi, pan_raw, cfg$tbase, fao_sub),
    error = function(e) { cat("[ERROR]", conditionMessage(e), "\n"); NULL })
  if (is.null(pan_son)) next
  cat("[OK] Rows:", nrow(pan_son), "| Countries:",
      length(unique(pan_son$Country_ISO)), "\n")
  writexl::write_xlsx(pan_son, panout)
  cat("[SAVED]", basename(panout), "\n")
  rm(pan_raw, pan_son, fao_sub); gc()
}


# ── Step 3: panfin_* → pan2* (apply yield/year filters) ─────────────────────

cat("\n", strrep("=", 60), "\nSTEP 3: panfin_* → pan2*\n",
    sprintf("(yield >= %d hg/ha, min >= %d years)\n", VERIM_ESIGI, MIN_YIL),
    strrep("=", 60), "\n\n")

rapor_son <- data.frame()
for (urun_adi in names(urun_config)) {
  pan_in  <- file.path(ana_yol, paste0("panfin_", urun_adi, ".xlsx"))
  pan_out <- file.path(ana_yol, paste0("pan2", urun_adi, ".xlsx"))
  cat("\n---", toupper(urun_adi), "---\n")
  if (!file.exists(pan_in)) { cat("[ERROR] missing\n"); next }
  df  <- as.data.frame(readxl::read_excel(pan_in)); n0 <- nrow(df)
  
  # Remove rows with no FAO yield record
  df  <- df[!is.na(df$production_tonnes), ];  n1 <- nrow(df)
  cat("  Remove FAO NA:", n0 - n1, "| remaining:", n1, "\n")
  
  # Remove implausible yield values
  df  <- df[!is.na(df$yield_hg_per_ha) &
              df$yield_hg_per_ha >= VERIM_ESIGI, ]; n2 <- nrow(df)
  cat("  Remove yield <", VERIM_ESIGI, ":", n1 - n2, "| remaining:", n2, "\n")
  
  # Require minimum years per country
  goz <- df %>%
    dplyr::group_by(Country_ISO) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  df  <- df[df$Country_ISO %in% goz$Country_ISO[goz$n >= MIN_YIL], ]
  n3  <- nrow(df); u3 <- length(unique(df$Country_ISO))
  cat("  Remove < ", MIN_YIL, " years:", n2 - n3,
      "| remaining:", n3, "rows |", u3, "countries\n")
  
  # Join irrigation if available
  if (!is.null(sulama_df)) {
    df <- dplyr::left_join(df, sulama_df, by = c("Country_ISO","Yil"))
  }
  
  # Report NA rates for key climate variables
  ik <- c("TMP_earlymean","TMX_floweringmean","PRE_floweringsum",
          "PRE_grainfillsum","GDD_growth","SPEI3_memory","TMP_ann","PRE_ann")
  ik     <- ik[ik %in% names(df)]
  na_say <- colSums(is.na(df[, ik]))
  if (any(na_say > 0)) {
    cat("  [CLIMATE NA]:\n"); print(na_say[na_say > 0])
  } else cat("  [OK] No climate NAs\n")
  
  writexl::write_xlsx(df, pan_out)
  cat("  [SAVED]", basename(pan_out), "\n")
  
  son_iso   <- unique(df$Country_ISO)
  eksik_son <- setdiff(pan1_iso_liste[[urun_adi]], son_iso)
  
  rapor_son <- rbind(rapor_son, data.frame(
    urun      = urun_adi, n_son = n3, n_ulke = u3,
    pan1_ulke = length(pan1_iso_liste[[urun_adi]]),
    eksik     = paste(eksik_son, collapse = ", "),
    stringsAsFactors = FALSE))
}

# Print final summary
cat("\n", strrep("=", 90), "\nFINAL SUMMARY\n", strrep("=", 90), "\n")
cat(sprintf("%-10s %4s %5s %5s  %s\n",
            "Crop","p1N","FinalN","Countries","Missing from pan1"))
cat(strrep("-", 70), "\n")
for (i in seq_len(nrow(rapor_son))) {
  r <- rapor_son[i, ]
  cat(sprintf("%-10s %4d %5d %5d  %s\n",
              r$urun, r$pan1_ulke, r$n_son, r$n_ulke,
              if (nchar(r$eksik) == 0) "none" else r$eksik))
}
cat(sprintf("\n[Params] yield_threshold=%d hg/ha | min_years=%d\n",
            VERIM_ESIGI, MIN_YIL))
cat("[Next] Steps 4-7: irrigation, cropland, aridity, GDP → pan7*\n\n")


# ==============================================================================
# STEP 3b — EXTRA 4 COUNTRIES (IRN, EGY, NGA, SDN) → pan3*
#
# These countries were missing from the main pan2* due to GADM boundary issues.
# Fix: extract climate using GADM cache, join FAO data from sudanvs.xls.
# Combines ek (extra) rows with pan2* to produce pan3*.
# ==============================================================================

library(terra); library(geodata)
library(dplyr); library(readxl); library(writexl)
library(data.table); library(tidyr); library(zoo)

EK_ULKELER  <- c("IRN", "EGY", "NGA", "SDN")
spam_kodlari <- unique(sapply(urun_config, `[[`, "spam"))

cat("\n", strrep("=", 60), "\n")
cat("STEP 3b: Climate extraction for extra 4 countries\n")
cat(strrep("=", 60), "\n\n")

# Load CRU files once (all 768 layers into memory)
cat("[*] Loading CRU files...\n")
cru_list <- list()
for (v in names(degisken_bilgi)) {
  nc_tam <- file.path(iklim_yol, degisken_bilgi[[v]]$nc)
  if (!file.exists(nc_tam)) { cat("  [!] Missing:", v, "\n"); next }
  cru_list[[v]] <- terra::rast(nc_tam)
  cat("  [OK]", v, "\n")
}

# Extract SPAM-weighted climate for each SPAM code × extra country
iklim_ek_spam <- list()

for (spam_kodu in spam_kodlari) {
  cat("\n--- SPAM:", spam_kodu, "---\n")
  spam_dosya <- file.path(ana_yol,
                          paste0("spam2020_V2r0_global_H_", spam_kodu, "_A.tif"))
  if (!file.exists(spam_dosya)) { cat("[ERROR] SPAM missing\n"); next }
  spam_global <- terra::rast(spam_dosya)
  
  iklim_ek_spam[[spam_kodu]] <- list()
  
  for (iso in EK_ULKELER) {
    cat("  >>", iso)
    t0 <- Sys.time()
    
    gadm_cache <- file.path(ana_yol, "gadm",
                            paste0("gadm_41_", iso, "_0_pk.rds"))
    if (file.exists(gadm_cache)) {
      sinir <- readRDS(gadm_cache); cat(" [cache]")
    } else {
      sinir <- tryCatch(
        geodata::gadm(country = iso, level = 0, path = ana_yol),
        error = function(e) { cat(" [GADM error]"); NULL })
    }
    if (is.null(sinir)) next
    
    ulke_tbl <- data.frame(Country_ISO = iso,
                           Yil = yil_ay_df$Yil, Ay = yil_ay_df$Ay)
    
    spam_crop <- terra::crop(spam_global, terra::ext(sinir))
    ref_r     <- terra::mask(
      terra::crop(cru_list[[1]][[1]], terra::ext(sinir)), sinir)
    spam_res  <- terra::resample(spam_crop, ref_r, method = "sum")
    spam_m    <- terra::mask(spam_res, sinir)
    w_base    <- as.vector(terra::values(spam_m))
    alan_var  <- !is.na(sum(w_base, na.rm = TRUE)) &&
      sum(w_base, na.rm = TRUE) > 0
    
    for (v in names(cru_list)) {
      r_ulke <- terra::mask(
        terra::crop(cru_list[[v]], terra::ext(sinir)), sinir)
      v_mat  <- terra::values(r_ulke)
      
      if (alan_var) {
        spam_res2 <- terra::resample(spam_crop, r_ulke[[1]], method = "sum")
        w  <- as.vector(terra::values(terra::mask(spam_res2, sinir)))
        ws <- sum(w, na.rm = TRUE)
        aylik <- if (!is.na(ws) && ws > 0)
          colSums(v_mat * w, na.rm = TRUE) / ws
        else
          colMeans(v_mat, na.rm = TRUE)
      } else {
        aylik <- colMeans(v_mat, na.rm = TRUE)
      }
      
      if (degisken_bilgi[[v]]$olcek) aylik <- scale_kontrolu(aylik, v)
      ulke_tbl[[toupper(v)]] <- aylik
    }
    
    iklim_ek_spam[[spam_kodu]][[iso]] <- ulke_tbl
    cat("  [", round(difftime(Sys.time(), t0, units = "secs"), 1), "s]\n")
  }
}

cat("\n[OK] Step 3b climate extraction complete.\n")


# ── Load FAO data from sudanvs.xls ───────────────────────────────────────────

cat("\n[*] Reading sudanvs.xls...\n")
sv_raw        <- as.data.frame(readxl::read_xls(sudanvs_xls))
names(sv_raw) <- trimws(names(sv_raw))
sv_raw$Value  <- as.numeric(gsub(",", ".", as.character(sv_raw$Value)))
sv_raw$Year   <- as.integer(gsub("'", "", as.character(sv_raw$Year)))

# Map FAO country names to ISO codes
iso_sv <- c(
  "Iran (Islamic Republic of)" = "IRN",
  "Egypt"                       = "EGY",
  "Nigeria"                     = "NGA",
  "Sudan"                       = "SDN")
sv_raw$Country_ISO <- iso_sv[sv_raw$Area]

tum_items <- unique(unlist(lapply(urun_config, `[[`, "fao")))
sv_fil <- sv_raw[
  !is.na(sv_raw$Country_ISO) &
    sv_raw$Item    %in% tum_items &
    sv_raw$Element %in% c("Area harvested","Production","Yield") &
    sv_raw$Year    >= 1961 & sv_raw$Year <= 2024, ]

# Wide format
fao_ek_wide <- sv_fil %>%
  dplyr::group_by(Country_ISO, Year, Item, Element) %>%
  dplyr::summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Element, values_from = Value) %>%
  as.data.frame()

names(fao_ek_wide)[names(fao_ek_wide) == "Area harvested"] <- "area_harvested_ha"
names(fao_ek_wide)[names(fao_ek_wide) == "Production"]     <- "production_tonnes"
names(fao_ek_wide)[names(fao_ek_wide) == "Yield"]          <- "yield_hg_per_ha"
cat("[OK] fao_ek_wide:", nrow(fao_ek_wide), "rows\n")


# ── Append extra countries to pan2* → pan3* ──────────────────────────────────

cat("\n", strrep("=", 60), "\nStep 3b: Append extra countries → pan3*\n",
    strrep("=", 60), "\n\n")

parse_m <- function(s) as.integer(trimws(strsplit(as.character(s), ",")[[1]]))

for (urun_adi in names(urun_config)) {
  cfg   <- urun_config[[urun_adi]]
  pan2f <- file.path(ana_yol, paste0("pan2", urun_adi, ".xlsx"))
  pan3f <- file.path(ana_yol, paste0("pan3", urun_adi, ".xlsx"))
  cat("\n---", toupper(urun_adi), "---\n")
  if (!file.exists(pan2f)) { cat("[ERROR] pan2 missing\n"); next }
  
  pan2     <- as.data.frame(readxl::read_excel(pan2f))
  fao_sub  <- fao_ek_wide[fao_ek_wide$Item %in% cfg$fao, ]
  
  if (nrow(fao_sub) == 0) {
    cat("  [SKIP] No FAO data in sudanvs for this crop\n")
    writexl::write_xlsx(pan2, pan3f); next
  }
  
  # v5: phenology already loaded at startup
  feno_ulke <- feno_patch[[urun_adi]]
  
  ek_satirlar <- list()
  
  for (iso in EK_ULKELER) {
    if (!iso %in% unique(fao_sub$Country_ISO)) next
    spam_kodu <- cfg$spam
    if (is.null(iklim_ek_spam[[spam_kodu]][[iso]])) next
    
    pan_iso  <- iklim_ek_spam[[spam_kodu]][[iso]]
    feno_iso <- dplyr::distinct(
      feno_ulke[feno_ulke$Country_ISO == iso, ], Country_ISO, .keep_all = TRUE)
    if (nrow(feno_iso) == 0) next
    
    # Assign phenological month flags
    pan_iso$is_early  <- pan_iso$Ay %in% parse_m(feno_iso$Early_Period)
    pan_iso$is_flower <- pan_iso$Ay %in% parse_m(feno_iso$Flowering)
    pan_iso$is_grain  <- pan_iso$Ay %in% parse_m(feno_iso$Growing_Season)
    pan_iso$is_memory <- pan_iso$Ay %in% parse_m(feno_iso$Drought_Memory)
    
    # Compute SPEI3 and GDD
    pan_iso <- pan_iso %>%
      dplyr::arrange(Yil, Ay) %>%
      dplyr::mutate(
        WB_monthly  = PRE - PET,
        SPEI3_raw   = zoo::rollsum(WB_monthly, k = 3, fill = NA, align = "right"),
        SPEI3_std   = (SPEI3_raw - mean(SPEI3_raw, na.rm = TRUE)) /
          sd(SPEI3_raw, na.rm = TRUE),
        GDD_monthly = pmax(TMP - cfg$tbase, 0) * 30)
    
    # Annual aggregation
    yil_ek <- pan_iso %>%
      dplyr::group_by(Country_ISO, Yil) %>%
      dplyr::summarise(
        TMP_earlymean     = mean(TMP[is_early],        na.rm = TRUE),
        TMX_floweringmean = mean(TMX[is_flower],       na.rm = TRUE),
        TMN_floweringmean = mean(TMN[is_flower],       na.rm = TRUE),
        PRE_floweringsum  = sum(PRE[is_flower],        na.rm = TRUE),
        PRE_grainfillsum  = sum(PRE[is_grain],         na.rm = TRUE),
        GDD_growth        = sum(GDD_monthly[is_grain], na.rm = TRUE),
        Frost_flowering   = sum(TMN[is_flower] < 0,    na.rm = TRUE),
        Heat_flowering    = sum(TMX[is_flower] > 30,   na.rm = TRUE),
        SPEI3_memory      = mean(SPEI3_std[is_memory], na.rm = TRUE),
        WaterBalance   = sum(PRE, na.rm = TRUE) - sum(PET, na.rm = TRUE),
        AridityIndex   = ifelse(sum(PET, na.rm = TRUE) > 0,
                                sum(PRE, na.rm = TRUE) / sum(PET, na.rm = TRUE),
                                NA_real_),
        TempVariance   = var(TMP, na.rm = TRUE),
        TMP_ann  = mean(TMP, na.rm = TRUE), TMX_ann = mean(TMX, na.rm = TRUE),
        TMN_ann  = mean(TMN, na.rm = TRUE), DTR_ann = mean(DTR, na.rm = TRUE),
        CLD_ann  = mean(CLD, na.rm = TRUE), VAP_ann = mean(VAP, na.rm = TRUE),
        PRE_ann  = sum(PRE,  na.rm = TRUE), PET_ann = sum(PET, na.rm = TRUE),
        WET_ann  = sum(WET,  na.rm = TRUE), FRS_ann = sum(FRS, na.rm = TRUE),
        .groups  = "drop") %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                  ~ifelse(is.nan(.), NA_real_, .)))
    
    # Join FAO, CO2, GDP
    fao_join <- fao_sub[fao_sub$Country_ISO == iso,
                        c("Country_ISO","Year","area_harvested_ha",
                          "production_tonnes","yield_hg_per_ha")]
    yil_ek <- yil_ek %>%
      dplyr::left_join(fao_join, by = c("Country_ISO","Yil" = "Year")) %>%
      dplyr::left_join(co2_df,   by = "Yil") %>%
      dplyr::left_join(gdp_df,   by = c("Country_ISO","Yil"))
    
    yil_ek$Early_Period   <- feno_iso$Early_Period
    yil_ek$Flowering      <- feno_iso$Flowering
    yil_ek$Growing_Season <- feno_iso$Growing_Season
    yil_ek$Drought_Memory <- feno_iso$Drought_Memory
    
    ek_satirlar[[iso]] <- yil_ek
    cat("  [Annual]", iso, nrow(yil_ek), "rows\n")
  }
  
  if (length(ek_satirlar) == 0) {
    writexl::write_xlsx(pan2, pan3f); next
  }
  
  ek_df <- dplyr::bind_rows(ek_satirlar)
  # Apply same filters as Step 3
  ek_df <- ek_df[!is.na(ek_df$production_tonnes), ]
  ek_df <- ek_df[!is.na(ek_df$yield_hg_per_ha) &
                   ek_df$yield_hg_per_ha >= VERIM_ESIGI, ]
  goz   <- ek_df %>%
    dplyr::group_by(Country_ISO) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  ek_df <- ek_df[ek_df$Country_ISO %in%
                   goz$Country_ISO[goz$n >= MIN_YIL], ]
  
  # Column-align and bind with pan2
  for (k in setdiff(names(pan2),  names(ek_df))) ek_df[[k]]  <- NA
  for (k in setdiff(names(ek_df), names(pan2)))  pan2[[k]]   <- NA
  ortak <- union(names(pan2), names(ek_df))
  pan3  <- rbind(pan2[, ortak], ek_df[, ortak])
  pan3  <- pan3[order(pan3$Country_ISO, pan3$Yil), ]
  rownames(pan3) <- NULL
  
  cat("  [pan3]", nrow(pan3), "rows |",
      length(unique(pan3$Country_ISO)), "countries\n")
  writexl::write_xlsx(pan3, pan3f)
  cat("  [SAVED]", basename(pan3f), "\n")
  rm(pan2, ek_df, pan3, ek_satirlar); gc()
}


# ==============================================================================
# STEP 3c — EGYPT PATCH via rnaturalearth
# GADM failed for EGY; use rnaturalearth boundary instead.
# Only applies to crops where EGY count is still 0 after Step 3b.
# ==============================================================================

library(terra); library(sf)
library(rnaturalearth); library(rnaturalearthdata)

PATCH_ISO     <- "EGY"
PATCH_URUNLER <- c("wheat","barley","oats","rye","sorghum")  # crops still missing EGY

cat("\n", strrep("=", 60), "\n")
cat("STEP 3c: Egypt patch via rnaturalearth\n")
cat(strrep("=", 60), "\n\n")

# Get Egypt boundary from rnaturalearth
egy_sf    <- rnaturalearth::ne_countries(country = "Egypt", returnclass = "sf")
egy_sinir <- terra::vect(egy_sf)
cat("[OK] EGY boundary loaded\n")

# v5: EGY phenology is already in feno_patch (loaded from Global_AgriClimate_v5.xlsx)
# No separate feno_patch_egy needed

# Extract climate for EGY per SPAM code
spam_kodlari_egy <- unique(sapply(urun_config[PATCH_URUNLER], `[[`, "spam"))
iklim_egy <- list()

for (spam_kodu in spam_kodlari_egy) {
  spam_dosya <- file.path(ana_yol,
                          paste0("spam2020_V2r0_global_H_", spam_kodu, "_A.tif"))
  if (!file.exists(spam_dosya)) next
  spam_global <- terra::rast(spam_dosya)
  ulke_tbl    <- data.frame(Country_ISO = "EGY",
                            Yil = yil_ay_df$Yil, Ay = yil_ay_df$Ay)
  
  spam_crop <- terra::crop(spam_global, terra::ext(egy_sinir))
  ref_r     <- terra::mask(
    terra::crop(cru_list[[1]][[1]], terra::ext(egy_sinir)), egy_sinir)
  spam_res  <- terra::resample(spam_crop, ref_r, method = "sum")
  spam_m    <- terra::mask(spam_res, egy_sinir)
  w_base    <- as.vector(terra::values(spam_m))
  alan_var  <- !is.na(sum(w_base, na.rm = TRUE)) && sum(w_base, na.rm = TRUE) > 0
  
  for (v in names(cru_list)) {
    r_ulke <- terra::mask(
      terra::crop(cru_list[[v]], terra::ext(egy_sinir)), egy_sinir)
    v_mat  <- terra::values(r_ulke)
    if (alan_var) {
      spam_r2 <- terra::resample(spam_crop, r_ulke[[1]], method = "sum")
      w  <- as.vector(terra::values(terra::mask(spam_r2, egy_sinir)))
      ws <- sum(w, na.rm = TRUE)
      aylik <- if (!is.na(ws) && ws > 0)
        colSums(v_mat * w, na.rm = TRUE) / ws
      else colMeans(v_mat, na.rm = TRUE)
    } else {
      aylik <- colMeans(v_mat, na.rm = TRUE)
    }
    if (degisken_bilgi[[v]]$olcek) aylik <- scale_kontrolu(aylik, v)
    ulke_tbl[[toupper(v)]] <- aylik
  }
  iklim_egy[[spam_kodu]] <- ulke_tbl
  cat("  [OK] EGY SPAM:", spam_kodu, nrow(ulke_tbl), "rows\n")
}

# EGY FAO data from sudanvs.xls
sv_raw2        <- as.data.frame(readxl::read_xls(sudanvs_xls))
names(sv_raw2) <- trimws(names(sv_raw2))
sv_raw2$Value  <- as.numeric(gsub(",", ".", as.character(sv_raw2$Value)))
sv_raw2$Year   <- as.integer(gsub("'", "", as.character(sv_raw2$Year)))

fao_egy <- sv_raw2[
  sv_raw2$Area == "Egypt" &
    sv_raw2$Item %in% tum_items &
    sv_raw2$Element %in% c("Area harvested","Production","Yield") &
    sv_raw2$Year >= 1961 & sv_raw2$Year <= 2024, ]
fao_egy$Country_ISO <- "EGY"

fao_egy_wide <- fao_egy %>%
  dplyr::group_by(Country_ISO, Year, Item, Element) %>%
  dplyr::summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Element, values_from = Value) %>%
  as.data.frame()
names(fao_egy_wide)[names(fao_egy_wide) == "Area harvested"] <- "area_harvested_ha"
names(fao_egy_wide)[names(fao_egy_wide) == "Production"]     <- "production_tonnes"
names(fao_egy_wide)[names(fao_egy_wide) == "Yield"]          <- "yield_hg_per_ha"

# Append EGY to pan3* for each crop
for (urun_adi in PATCH_URUNLER) {
  cfg   <- urun_config[[urun_adi]]
  pan3f <- file.path(ana_yol, paste0("pan3", urun_adi, ".xlsx"))
  if (!file.exists(pan3f)) next
  pan3 <- as.data.frame(readxl::read_excel(pan3f))
  # Skip if EGY already present
  if (sum(pan3$Country_ISO == "EGY") > 0) {
    cat("  [SKIP]", urun_adi, "EGY already in pan3\n"); next
  }
  
  spam_kodu <- cfg$spam
  if (is.null(iklim_egy[[spam_kodu]])) next
  
  pan_egy  <- iklim_egy[[spam_kodu]]
  fao_sub  <- fao_egy_wide[fao_egy_wide$Item %in% cfg$fao, ]
  feno_iso <- feno_patch[[urun_adi]][feno_patch[[urun_adi]]$Country_ISO == "EGY", ]
  feno_iso <- dplyr::distinct(feno_iso, Country_ISO, .keep_all = TRUE)
  
  pan_egy$is_early  <- pan_egy$Ay %in% parse_m(feno_iso$Early_Period)
  pan_egy$is_flower <- pan_egy$Ay %in% parse_m(feno_iso$Flowering)
  pan_egy$is_grain  <- pan_egy$Ay %in% parse_m(feno_iso$Growing_Season)
  pan_egy$is_memory <- pan_egy$Ay %in% parse_m(feno_iso$Drought_Memory)
  
  pan_egy <- pan_egy %>%
    dplyr::arrange(Yil, Ay) %>%
    dplyr::mutate(
      WB_monthly  = PRE - PET,
      SPEI3_raw   = zoo::rollsum(WB_monthly, k = 3, fill = NA, align = "right"),
      SPEI3_std   = (SPEI3_raw - mean(SPEI3_raw, na.rm = TRUE)) /
        sd(SPEI3_raw, na.rm = TRUE),
      GDD_monthly = pmax(TMP - cfg$tbase, 0) * 30)
  
  yil_egy <- pan_egy %>%
    dplyr::group_by(Country_ISO, Yil) %>%
    dplyr::summarise(
      TMP_earlymean     = mean(TMP[is_early],        na.rm = TRUE),
      TMX_floweringmean = mean(TMX[is_flower],       na.rm = TRUE),
      TMN_floweringmean = mean(TMN[is_flower],       na.rm = TRUE),
      PRE_floweringsum  = sum(PRE[is_flower],        na.rm = TRUE),
      PRE_grainfillsum  = sum(PRE[is_grain],         na.rm = TRUE),
      GDD_growth        = sum(GDD_monthly[is_grain], na.rm = TRUE),
      Frost_flowering   = sum(TMN[is_flower] < 0,    na.rm = TRUE),
      Heat_flowering    = sum(TMX[is_flower] > 30,   na.rm = TRUE),
      SPEI3_memory      = mean(SPEI3_std[is_memory], na.rm = TRUE),
      WaterBalance   = sum(PRE, na.rm = TRUE) - sum(PET, na.rm = TRUE),
      AridityIndex   = ifelse(sum(PET, na.rm = TRUE) > 0,
                              sum(PRE, na.rm = TRUE) / sum(PET, na.rm = TRUE),
                              NA_real_),
      TempVariance   = var(TMP, na.rm = TRUE),
      TMP_ann = mean(TMP, na.rm = TRUE), TMX_ann = mean(TMX, na.rm = TRUE),
      TMN_ann = mean(TMN, na.rm = TRUE), DTR_ann = mean(DTR, na.rm = TRUE),
      CLD_ann = mean(CLD, na.rm = TRUE), VAP_ann = mean(VAP, na.rm = TRUE),
      PRE_ann = sum(PRE,  na.rm = TRUE), PET_ann = sum(PET, na.rm = TRUE),
      WET_ann = sum(WET,  na.rm = TRUE), FRS_ann = sum(FRS, na.rm = TRUE),
      .groups = "drop") %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                ~ifelse(is.nan(.), NA_real_, .)))
  
  yil_egy <- yil_egy %>%
    dplyr::left_join(
      fao_sub[, c("Country_ISO","Year","area_harvested_ha",
                  "production_tonnes","yield_hg_per_ha")],
      by = c("Country_ISO","Yil" = "Year")) %>%
    dplyr::left_join(co2_df, by = "Yil") %>%
    dplyr::left_join(gdp_df, by = c("Country_ISO","Yil"))
  
  yil_egy$Early_Period   <- feno_iso$Early_Period
  yil_egy$Flowering      <- feno_iso$Flowering
  yil_egy$Growing_Season <- feno_iso$Growing_Season
  yil_egy$Drought_Memory <- feno_iso$Drought_Memory
  
  # Filter and bind
  yil_egy <- yil_egy[!is.na(yil_egy$production_tonnes), ]
  yil_egy <- yil_egy[!is.na(yil_egy$yield_hg_per_ha) &
                       yil_egy$yield_hg_per_ha >= VERIM_ESIGI, ]
  
  for (k in setdiff(names(pan3),    names(yil_egy))) yil_egy[[k]] <- NA
  for (k in setdiff(names(yil_egy), names(pan3)))    pan3[[k]]    <- NA
  ortak     <- union(names(pan3), names(yil_egy))
  pan3_yeni <- rbind(pan3[, ortak], yil_egy[, ortak])
  pan3_yeni <- pan3_yeni[order(pan3_yeni$Country_ISO, pan3_yeni$Yil), ]
  rownames(pan3_yeni) <- NULL
  
  writexl::write_xlsx(pan3_yeni, pan3f)
  cat("  [SAVED]", urun_adi, "EGY added →",
      nrow(pan3_yeni), "rows\n")
  rm(pan3, yil_egy, pan3_yeni); gc()
}


# ==============================================================================
# STEP 4 — IRRIGATION DATA (FAOSTAT) → pan4*
#
# Adds: land_area_1000ha, equipped_irrig_1000ha, actual_irrig_1000ha,
#       cropland_irrig_1000ha, irrig_share, cropland_irrig_share,
#       irrig_intensity
# Reference: Lobell et al. (2011); Burke & Emerick (2016)
# ==============================================================================

library(dplyr); library(readxl); library(writexl); library(tidyr)

cat("\n", strrep("=", 60), "\nSTEP 4: Irrigation data → pan4*\n",
    strrep("=", 60), "\n\n")

# Load FAOSTAT irrigation file
irrig_raw        <- as.data.frame(readxl::read_excel(sulama_xl))
names(irrig_raw) <- trimws(names(irrig_raw))
irrig_raw$Value  <- as.numeric(gsub(",", ".", as.character(irrig_raw$Value)))
irrig_raw$Year   <- as.integer(as.character(irrig_raw$Year))

# Target items
hedef_itemler <- c(
  "Land area",
  "Land area equipped for irrigation",
  "Land area actually irrigated",
  "Cropland area actually irrigated")

irrig_fil <- irrig_raw[
  irrig_raw$Item    %in% hedef_itemler &
    irrig_raw$Element == "Area" &
    irrig_raw$Year    >= 1961 &
    irrig_raw$Year    <= 2024, ]

# ISO mapping for irrigation data
iso_map_irrig <- c(
  "Afghanistan"="AFG","Albania"="ALB","Algeria"="DZA","Angola"="AGO",
  "Argentina"="ARG","Armenia"="ARM","Australia"="AUS","Austria"="AUT",
  "Azerbaijan"="AZE","Bangladesh"="BGD","Belarus"="BLR","Belgium"="BEL",
  "Benin"="BEN","Bolivia (Plurinational State of)"="BOL",
  "Bosnia and Herzegovina"="BIH","Botswana"="BWA","Brazil"="BRA",
  "Bulgaria"="BGR","Burkina Faso"="BFA","Burundi"="BDI",
  "Cambodia"="KHM","Cameroon"="CMR","Canada"="CAN","Chad"="TCD",
  "Chile"="CHL","China, mainland"="CHN","Colombia"="COL","Congo"="COG",
  "Costa Rica"="CRI","Croatia"="HRV","Cuba"="CUB","Cyprus"="CYP",
  "Czechia"="CZE","Denmark"="DNK","Dominican Republic"="DOM",
  "Ecuador"="ECU","Egypt"="EGY","El Salvador"="SLV","Eritrea"="ERI",
  "Estonia"="EST","Ethiopia"="ETH","Finland"="FIN","France"="FRA",
  "Georgia"="GEO","Germany"="DEU","Ghana"="GHA","Greece"="GRC",
  "Guatemala"="GTM","Guinea"="GIN","Haiti"="HTI","Honduras"="HND",
  "Hungary"="HUN","India"="IND","Indonesia"="IDN",
  "Iran (Islamic Republic of)"="IRN","Iraq"="IRQ","Ireland"="IRL",
  "Israel"="ISR","Italy"="ITA","Japan"="JPN","Jordan"="JOR",
  "Kazakhstan"="KAZ","Kenya"="KEN","Republic of Korea"="KOR",
  "Kuwait"="KWT","Kyrgyzstan"="KGZ","Latvia"="LVA","Lebanon"="LBN",
  "Libya"="LBY","Lithuania"="LTU","Madagascar"="MDG","Malawi"="MWI",
  "Malaysia"="MYS","Mali"="MLI","Malta"="MLT","Mexico"="MEX",
  "Republic of Moldova"="MDA","Mongolia"="MNG","Montenegro"="MNE",
  "Morocco"="MAR","Mozambique"="MOZ","Myanmar"="MMR","Namibia"="NAM",
  "Nepal"="NPL","Netherlands (Kingdom of the)"="NLD","New Zealand"="NZL",
  "Nicaragua"="NIC","Niger"="NER","Nigeria"="NGA",
  "North Macedonia"="MKD","Norway"="NOR","Pakistan"="PAK",
  "Panama"="PAN","Paraguay"="PRY","Peru"="PER","Philippines"="PHL",
  "Poland"="POL","Portugal"="PRT",
  "Democratic People's Republic of Korea"="PRK",
  "Romania"="ROU","Russian Federation"="RUS","Rwanda"="RWA",
  "Saudi Arabia"="SAU","Senegal"="SEN","Serbia"="SRB",
  "Sierra Leone"="SLE","Slovakia"="SVK","Slovenia"="SVN",
  "Somalia"="SOM","South Africa"="ZAF","South Sudan"="SSD",
  "Spain"="ESP","Sri Lanka"="LKA","Sudan"="SDN","Sweden"="SWE",
  "Switzerland"="CHE","Syrian Arab Republic"="SYR","Tajikistan"="TJK",
  "United Republic of Tanzania"="TZA","Thailand"="THA","Togo"="TGO",
  "Tunisia"="TUN","Turkmenistan"="TKM","Uganda"="UGA","Ukraine"="UKR",
  "United Kingdom of Great Britain and Northern Ireland"="GBR",
  "United States of America"="USA","Uruguay"="URY","Uzbekistan"="UZB",
  "Venezuela (Bolivarian Republic of)"="VEN","Viet Nam"="VNM",
  "Yemen"="YEM","Zambia"="ZMB","Zimbabwe"="ZWE",
  "Democratic Republic of the Congo"="COD","Côte d'Ivoire"="CIV",
  "Papua New Guinea"="PNG","Oman"="OMN","Qatar"="QAT",
  "United Arab Emirates"="ARE","Liberia"="LBR")

irrig_fil$Country_ISO <- iso_map_irrig[irrig_fil$Area]
irrig_fil$Country_ISO[grepl("rkiye|Turkey", irrig_fil$Area)] <- "TUR"
irrig_fil$Country_ISO[grepl("voire|Ivory",  irrig_fil$Area)] <- "CIV"

irrig_iso <- irrig_fil[!is.na(irrig_fil$Country_ISO), ]

# Item name → standardised column name mapping
item_map <- c(
  "Land area"                         = "land_area_1000ha",
  "Land area equipped for irrigation" = "equipped_irrig_1000ha",
  "Land area actually irrigated"      = "actual_irrig_1000ha",
  "Cropland area actually irrigated"  = "cropland_irrig_1000ha")

irrig_iso$sutun <- item_map[irrig_iso$Item]

# Pivot to wide format
irrig_wide <- irrig_iso %>%
  dplyr::group_by(Country_ISO, Year, sutun) %>%
  dplyr::summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = sutun, values_from = Value) %>%
  as.data.frame()

for (k in item_map) {
  if (!k %in% names(irrig_wide)) irrig_wide[[k]] <- NA_real_
}

# Compute irrigation ratio variables
irrig_wide <- irrig_wide %>%
  dplyr::mutate(
    # Fraction of total land area actually irrigated
    irrig_share = ifelse(
      !is.na(land_area_1000ha) & land_area_1000ha > 0,
      actual_irrig_1000ha / land_area_1000ha, NA_real_),
    # Cropland irrigation fraction
    cropland_irrig_share = ifelse(
      !is.na(land_area_1000ha) & land_area_1000ha > 0,
      cropland_irrig_1000ha / land_area_1000ha, NA_real_),
    # Capacity utilisation: actual / equipped (capped at 1)
    irrig_intensity = ifelse(
      !is.na(equipped_irrig_1000ha) & equipped_irrig_1000ha > 0,
      pmin(actual_irrig_1000ha / equipped_irrig_1000ha, 1), NA_real_))

# Join irrigation to pan3* → pan4*
for (urun_adi in names(urun_config)) {
  pan3f <- file.path(ana_yol, paste0("pan3", urun_adi, ".xlsx"))
  pan4f <- file.path(ana_yol, paste0("pan4", urun_adi, ".xlsx"))
  cat("---", toupper(urun_adi), "---\n")
  if (!file.exists(pan3f)) { cat("  [ERROR] pan3 missing\n\n"); next }
  
  pan3      <- as.data.frame(readxl::read_excel(pan3f))
  yil_sutun <- if ("Yil" %in% names(pan3)) "Yil" else "Year"
  
  pan4 <- pan3 %>%
    dplyr::left_join(
      irrig_wide[, c("Country_ISO","Year",
                     "land_area_1000ha","equipped_irrig_1000ha",
                     "actual_irrig_1000ha","cropland_irrig_1000ha",
                     "irrig_share","cropland_irrig_share","irrig_intensity")],
      by = setNames(c("Country_ISO","Year"),
                    c("Country_ISO", yil_sutun)),
      relationship = "many-to-one")
  
  writexl::write_xlsx(pan4, pan4f)
  cat("  [SAVED]", basename(pan4f), "\n\n")
  rm(pan3, pan4); gc()
}


# ==============================================================================
# STEP 5 — CROPLAND AREA + irr_ratio → pan5*
#
# irr_ratio = equipped_irrig_1000ha / cropland_area_1000ha
# This is the main irrigation variable in the TWFE models (M2, M4, M5).
# If cropland NA: fallback to land_area as denominator.
# ==============================================================================

cat("\n", strrep("=", 60), "\nSTEP 5: Cropland + irr_ratio → pan5*\n",
    strrep("=", 60), "\n\n")

cropland_df <- NULL

# Try FAOSTAT package first
if (requireNamespace("FAOSTAT", quietly = TRUE)) {
  cat("[*] Fetching cropland via FAOSTAT package...\n")
  tryCatch({
    library(FAOSTAT)
    raw <- FAOSTAT::get_faostat_item(
      item_code    = "6620",
      element_code = "5110",
      domain_code  = "RL")
    names(raw)     <- tolower(gsub("[^a-zA-Z0-9]", "_", names(raw)))
    iso_col        <- grep("iso|area_code", names(raw), value = TRUE)[1]
    yil_col        <- grep("^year$|^yil$",  names(raw), value = TRUE)[1]
    val_col        <- grep("^value$",       names(raw), value = TRUE)[1]
    cropland_df    <- data.frame(
      Country_ISO          = raw[[iso_col]],
      Year                 = as.integer(raw[[yil_col]]),
      cropland_area_1000ha = as.numeric(
        gsub(",", ".", as.character(raw[[val_col]]))))
    cropland_df <- cropland_df[
      !is.na(cropland_df$Country_ISO) &
        !is.na(cropland_df$Year) &
        cropland_df$Year >= 1961 &
        cropland_df$Year <= 2024, ]
    cat("[OK] Cropland:", nrow(cropland_df), "rows\n")
  }, error = function(e) {
    cat("[FALLBACK] FAOSTAT package failed:", conditionMessage(e), "\n")
  })
}

# Fallback: manual CSV/Excel
if (is.null(cropland_df) || nrow(cropland_df) == 0) {
  manuel_yollar <- c(
    file.path(ana_yol, "cropland.csv"),
    file.path(ana_yol, "cropland.xlsx"),
    file.path(ana_yol, "FAOSTAT_cropland.csv"))
  bulunan <- manuel_yollar[file.exists(manuel_yollar)]
  if (length(bulunan) > 0) {
    dosya   <- bulunan[1]
    cat("[FALLBACK] Using:", basename(dosya), "\n")
    m_raw   <- if (grepl("\\.csv$", dosya, ignore.case = TRUE))
      read.csv(dosya, stringsAsFactors = FALSE)
    else
      as.data.frame(readxl::read_excel(dosya))
    names(m_raw) <- trimws(names(m_raw))
    # (ISO mapping and filtering omitted for brevity — same logic as Step 4)
  } else {
    stop("Cropland data not found. Download from FAOSTAT (item 6620) and save as cropland.csv")
  }
}

cropland_df <- cropland_df[
  !is.na(cropland_df$cropland_area_1000ha) &
    cropland_df$cropland_area_1000ha > 0, ]

# Join cropland + compute irr_ratio → pan5*
for (urun_adi in names(urun_config)) {
  pan4f <- file.path(ana_yol, paste0("pan4", urun_adi, ".xlsx"))
  pan5f <- file.path(ana_yol, paste0("pan5", urun_adi, ".xlsx"))
  cat("---", toupper(urun_adi), "---\n")
  if (!file.exists(pan4f)) { cat("  [ERROR] pan4 missing\n\n"); next }
  
  pan4      <- as.data.frame(readxl::read_excel(pan4f))
  yil_sutun <- if ("Yil" %in% names(pan4)) "Yil" else "Year"
  
  pan5 <- pan4 %>%
    dplyr::left_join(
      cropland_df,
      by = setNames(c("Country_ISO","Year"),
                    c("Country_ISO", yil_sutun)),
      relationship = "many-to-one") %>%
    dplyr::mutate(
      # irr_ratio: equipped irrigation / cropland (preferred denominator)
      # Falls back to land_area if cropland is missing
      irr_ratio = dplyr::case_when(
        !is.na(cropland_area_1000ha) & cropland_area_1000ha > 0
        ~ pmin(equipped_irrig_1000ha / cropland_area_1000ha, 1),
        !is.na(land_area_1000ha) & land_area_1000ha > 0
        ~ pmin(equipped_irrig_1000ha / land_area_1000ha, 1),
        TRUE ~ NA_real_),
      # Record which denominator was used
      irr_ratio_denom = dplyr::case_when(
        !is.na(cropland_area_1000ha) & cropland_area_1000ha > 0 ~ "cropland",
        !is.na(land_area_1000ha)     & land_area_1000ha > 0     ~ "land_area",
        TRUE ~ NA_character_))
  
  # Winsorise: if equipped > cropland → cap at 0.95
  pan5 <- pan5 %>%
    dplyr::mutate(
      irr_ratio = ifelse(
        !is.na(equipped_irrig_1000ha) &
          !is.na(cropland_area_1000ha) &
          equipped_irrig_1000ha > cropland_area_1000ha,
        0.95, irr_ratio))
  
  writexl::write_xlsx(pan5, pan5f)
  cat("  [SAVED]", basename(pan5f), "\n\n")
  rm(pan4, pan5); gc()
}


# ==============================================================================
# STEP 6 — ARIDITY INDICATOR → pan6*
#
# is_arid          = 1 if PRE_ann < 400 mm (IPCC/WMO semi-arid threshold)
# is_arid_irrigator = 1 if is_arid AND irr_ratio > 0.8
#
# NOTE: AridityIndex (PRE/PET) is NOT used — PET scale issues in CRU data.
# PRE_ann is used directly as it is measured on a consistent scale.
# ==============================================================================

cat("\n", strrep("=", 60), "\nSTEP 6: Aridity indicator → pan6*\n",
    strrep("=", 60), "\n\n")

PRE_ESIK <- 400   # PRE_ann threshold (mm) — IPCC WMO semi-arid boundary
IRR_ESIK <- 0.8   # irr_ratio threshold for "genuinely irrigation-dependent"

for (urun_adi in names(urun_config)) {
  pan5f <- file.path(ana_yol, paste0("pan5", urun_adi, ".xlsx"))
  pan6f <- file.path(ana_yol, paste0("pan6", urun_adi, ".xlsx"))
  cat("---", toupper(urun_adi), "---\n")
  if (!file.exists(pan5f)) { cat("  [ERROR] pan5 missing\n\n"); next }
  
  pan5 <- as.data.frame(readxl::read_excel(pan5f))
  if (!"PRE_ann" %in% names(pan5)) {
    cat("  [ERROR] PRE_ann column missing\n\n"); next
  }
  
  pan5$irr_ratio <- pmin(pan5$irr_ratio, 1.0)   # ensure cap at 1
  
  pan6 <- pan5 %>%
    dplyr::mutate(
      # Binary aridity flag: 1 if annual precipitation < 400 mm
      is_arid = dplyr::case_when(
        is.na(PRE_ann)     ~ NA_real_,
        PRE_ann < PRE_ESIK ~ 1,
        TRUE               ~ 0),
      # Genuinely irrigation-dependent: arid AND high irrigation infrastructure
      is_arid_irrigator = dplyr::case_when(
        is.na(PRE_ann) | is.na(irr_ratio)         ~ NA_real_,
        PRE_ann < PRE_ESIK & irr_ratio > IRR_ESIK ~ 1,
        TRUE                                       ~ 0))
  
  writexl::write_xlsx(pan6, pan6f)
  cat("  [SAVED]", basename(pan6f),
      "| is_arid=1:", sum(pan6$is_arid == 1, na.rm = TRUE),
      "rows\n\n")
  rm(pan5, pan6); gc()
}


# ==============================================================================
# STEP 7 — GDP PER CAPITA (World Bank WDI) → pan7*
#
# Variable: NY.GDP.PCAP.KD (constant 2015 USD per capita)
# Fallback: PPP-adjusted GDP if constant USD missing
# Interpolation: linear within-country interpolation for remaining gaps
# ==============================================================================

cat("\n", strrep("=", 60), "\nSTEP 7: GDP per capita → pan7*\n",
    strrep("=", 60), "\n\n")

if (!requireNamespace("WDI", quietly = TRUE)) install.packages("WDI")
library(WDI)

cat("[*] Downloading GDP from World Bank WDI...\n")
gdp_raw <- WDI::WDI(
  indicator = "NY.GDP.PCAP.KD",   # constant 2015 USD per capita
  country   = "all",
  start     = 1961, end = 2024,
  extra     = TRUE)                # includes iso3c, region

# Keep only country rows (remove regional aggregates)
gdp_raw <- gdp_raw[
  !is.na(gdp_raw$iso3c) & gdp_raw$iso3c != "" &
    (is.na(gdp_raw$region) | gdp_raw$region != "Aggregates"), ]

cat("[OK] Downloaded. Range: min =",
    round(min(gdp_raw$NY.GDP.PCAP.KD, na.rm = TRUE), 0),
    "max =", round(max(gdp_raw$NY.GDP.PCAP.KD, na.rm = TRUE), 0),
    "USD\n")

# Fix known ISO3 mismatches
iso_duzelt <- c("ROM" = "ROU", "ZAR" = "COD", "TMP" = "TLS",
                "YUG" = "SRB", "CSK" = "CZE")

gdp_clean <- data.frame(
  Country_ISO    = dplyr::recode(gdp_raw$iso3c, !!!iso_duzelt),
  Yil            = as.integer(gdp_raw$year),
  GDP_per_Capita = as.numeric(gdp_raw$NY.GDP.PCAP.KD),
  stringsAsFactors = FALSE) %>%
  dplyr::filter(!is.na(GDP_per_Capita), Yil >= 1961, Yil <= 2024) %>%
  dplyr::group_by(Country_ISO, Yil) %>%
  dplyr::summarise(GDP_per_Capita = mean(GDP_per_Capita, na.rm = TRUE),
                   .groups = "drop")

# Supplement with PPP-adjusted GDP where constant USD missing
gdp_ppp_raw <- tryCatch(
  WDI::WDI(indicator = "NY.GDP.PCAP.PP.KD",
           country = "all", start = 1961, end = 2024, extra = TRUE),
  error = function(e) NULL)

if (!is.null(gdp_ppp_raw)) {
  gdp_ppp_raw <- gdp_ppp_raw[
    !is.na(gdp_ppp_raw$iso3c) & gdp_ppp_raw$iso3c != "" &
      (is.na(gdp_ppp_raw$region) | gdp_ppp_raw$region != "Aggregates"), ]
  gdp_ppp <- data.frame(
    Country_ISO = gdp_ppp_raw$iso3c,
    Yil         = as.integer(gdp_ppp_raw$year),
    GDP_ppp     = as.numeric(gdp_ppp_raw$NY.GDP.PCAP.PP.KD),
    stringsAsFactors = FALSE) %>%
    dplyr::filter(!is.na(GDP_ppp), Yil >= 1961, Yil <= 2024) %>%
    dplyr::group_by(Country_ISO, Yil) %>%
    dplyr::summarise(GDP_ppp = mean(GDP_ppp, na.rm = TRUE), .groups = "drop")
  
  gdp_clean <- gdp_clean %>%
    dplyr::full_join(gdp_ppp, by = c("Country_ISO","Yil")) %>%
    dplyr::mutate(
      GDP_per_Capita = dplyr::case_when(
        !is.na(GDP_per_Capita) ~ GDP_per_Capita,
        !is.na(GDP_ppp)        ~ GDP_ppp,
        TRUE                   ~ NA_real_)) %>%
    dplyr::select(-GDP_ppp) %>%
    dplyr::filter(!is.na(GDP_per_Capita))
}

# Within-country linear interpolation to fill remaining gaps
tum_grid <- expand.grid(
  Country_ISO = unique(gdp_clean$Country_ISO),
  Yil         = 1961:2024,
  stringsAsFactors = FALSE)

gdp_full <- tum_grid %>%
  dplyr::left_join(gdp_clean, by = c("Country_ISO","Yil")) %>%
  dplyr::group_by(Country_ISO) %>%
  dplyr::mutate(
    n_gercek = sum(!is.na(GDP_per_Capita)),
    # Interpolate only if at least 2 observations available
    GDP_per_Capita = ifelse(
      n_gercek >= 2,
      approx(x    = Yil[!is.na(GDP_per_Capita)],
             y    = GDP_per_Capita[!is.na(GDP_per_Capita)],
             xout = Yil,
             rule = 2)$y,   # rule=2: extrapolate at ends with nearest value
      GDP_per_Capita)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-n_gercek) %>%
  dplyr::filter(!is.na(GDP_per_Capita))

cat("[OK] GDP full grid:", nrow(gdp_full), "rows |",
    length(unique(gdp_full$Country_ISO)), "countries\n")

# Write GDP to pan6* → pan7*
for (urun_adi in names(urun_config)) {
  pan6f <- file.path(ana_yol, paste0("pan6", urun_adi, ".xlsx"))
  pan7f <- file.path(ana_yol, paste0("pan7", urun_adi, ".xlsx"))
  cat("---", toupper(urun_adi), "---\n")
  if (!file.exists(pan6f)) { cat("  [ERROR] pan6 missing\n\n"); next }
  
  pan6      <- as.data.frame(readxl::read_excel(pan6f))
  yil_sutun <- if ("Yil" %in% names(pan6)) "Yil" else "Year"
  
  # Drop old GDP (may be incorrect), replace with WDI
  pan6$GDP_per_Capita <- NULL
  
  pan7 <- pan6 %>%
    dplyr::left_join(
      gdp_full[, c("Country_ISO","Yil","GDP_per_Capita")],
      by = setNames(c("Country_ISO","Yil"),
                    c("Country_ISO", yil_sutun)))
  
  writexl::write_xlsx(pan7, pan7f)
  cat("  [SAVED]", basename(pan7f),
      "| GDP filled:", sum(!is.na(pan7$GDP_per_Capita)),
      "| GDP NA:", sum(is.na(pan7$GDP_per_Capita)), "\n\n")
  rm(pan6, pan7); gc()
}


# ==============================================================================
# STEP 7b — GDP BACKFILL FIX + DUPLICATE REMOVAL → pan7* (overwrite)
#
# WDI often backfills early years with the first available observation.
# This creates artificial constant sequences at the start of series.
# Fix: find first actual change point; set all prior values to NA.
# Also removes duplicate Country_ISO × Yil rows if any.
# ==============================================================================

cat("\n", strrep("=", 60), "\nSTEP 7b: GDP backfill fix + duplicate removal\n",
    strrep("=", 60), "\n\n")

# Function: remove backfilled GDP values (constant run at start of series)
remove_gdp_backfill <- function(df) {
  df %>%
    dplyr::group_by(Country_ISO) %>%
    dplyr::arrange(Yil, .by_group = TRUE) %>%
    dplyr::mutate(
      GDP_per_Capita = (function(x) {
        result   <- x
        non_na   <- which(!is.na(x))
        if (length(non_na) < 2) return(result)
        # Find first year where GDP actually changes
        first_change <- NA_integer_
        for (k in 2:length(non_na)) {
          if (x[non_na[k]] != x[non_na[k - 1]]) {
            first_change <- non_na[k - 1]; break
          }
        }
        # If series is entirely flat → all NA (no usable data)
        if (is.na(first_change)) { result[] <- NA; return(result) }
        # Set everything before the first change to NA
        if (first_change > 1) result[1:(first_change - 1)] <- NA
        result
      })(GDP_per_Capita)) %>%
    dplyr::ungroup()
}

for (urun_adi in names(urun_config)) {
  pan7f <- file.path(ana_yol, paste0("pan7", urun_adi, ".xlsx"))
  if (!file.exists(pan7f)) next
  
  df      <- as.data.frame(readxl::read_excel(pan7f))
  df$Yil  <- as.integer(as.character(df$Yil))
  
  # Remove backfilled GDP values
  df_clean <- remove_gdp_backfill(df)
  
  # Remove duplicate Country_ISO × Yil rows (keep row with most data)
  dup_check <- df_clean %>%
    dplyr::group_by(Country_ISO, Yil) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n > 1)
  
  if (nrow(dup_check) > 0) {
    cat("  [", urun_adi, "]", nrow(dup_check), "duplicate pairs — removing\n")
    df_clean <- df_clean %>%
      dplyr::group_by(Country_ISO, Yil) %>%
      dplyr::arrange(
        dplyr::desc(!is.na(GDP_per_Capita)),
        dplyr::desc(!is.na(production_tonnes))) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
  }
  
  writexl::write_xlsx(df_clean, pan7f)   # overwrite pan7* in place
  cat("  [SAVED]", urun_adi, "→", nrow(df_clean), "rows |",
      length(unique(df_clean$Country_ISO)), "countries\n")
  rm(df, df_clean); gc()
}


# ==============================================================================
# FINAL SUMMARY — pan7* overview
# ==============================================================================

cat("\n", strrep("=", 70), "\nFINAL SUMMARY — pan7* (ready for modelling)\n",
    strrep("=", 70), "\n")
cat(sprintf("%-10s %5s %5s  %5s  %5s  %6s\n",
            "Crop","N","Ctry","IRN","EGY","GDP_NA%"))
cat(strrep("-", 50), "\n")

for (urun_adi in names(urun_config)) {
  pf <- file.path(ana_yol, paste0("pan7", urun_adi, ".xlsx"))
  if (!file.exists(pf)) { cat(sprintf("%-10s  [MISSING]\n", urun_adi)); next }
  df <- as.data.frame(readxl::read_excel(pf))
  cat(sprintf("%-10s %5d %5d  %5d  %5d  %5.0f%%\n",
              urun_adi,
              nrow(df),
              length(unique(df$Country_ISO)),
              sum(df$Country_ISO == "IRN"),
              sum(df$Country_ISO == "EGY"),
              mean(is.na(df$GDP_per_Capita)) * 100))
}

cat("\n[Pipeline complete]\n")
cat("[Output files] pan7wheat.xlsx ... pan7sorghum.xlsx\n")
cat("[Next step]    paper1_step01_models.R — TWFE estimation\n\n")


