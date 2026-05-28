#DESCRIPTIVE STATISTICS TABLE FOR REGRESSION VARIABLES
# Produces a descriptive statistics table for all variables entering the
# regression analysis in "04_Statistical Analysis.R".
# Rows are filtered identically to script 04 so N matches the regression sample.
# Continuous variables are reported on their ORIGINAL (untransformed) scale,
# because standardized variables always have mean 0 and sd 1 which is not
# informative. The transformation actually used in the regression is noted
# in a separate column.


#LIBRARIES----
library(sf)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(flextable)
library(officer)

options(scipen = 999)


#READ DATA (same path logic as script 04) ----
# Run from the project root (open Liverpool_Paper.Rproj in RStudio).
dens_grid <- st_read("R Output/grid_full.gpkg") %>%
  filter(builtup2011 == 1) %>%
  mutate(across(c(nature, agriculture, industry, water, sports, parks, port, airport, dump, rail),
                ~ replace_na(.x, 0))) %>%
  filter(nature != 1 & agriculture != 1 & water != 1 & airport != 1 & dump != 1 & rail != 1) %>%
  st_drop_geometry()


#BUILD REGRESSION FRAME (mirrors script 04, but keeps raw companions) ----
desc_grid <- dens_grid %>%
  mutate(
    across(c(subdivision, hmo, office_rental,
             large_mfh, small_mfh, large_sfh, small_sfh,
             addresses_2013, sfh_share), ~ replace_na(.x, 0)),

    densification = if_any(c(hmo, office_rental, large_mfh, small_mfh,
                             large_sfh, small_sfh, subdivision), ~ .x == 1) %>%
      as.integer(),

    # raw inputs to transformed predictors
    m_to_train_capped = pmin(m_to_train, 800),
    log_amenities     = log(amenity_count + 1),
    log_density       = log(addresses_2013 + 1),

    # dichotomised / dummy forms that enter the model directly
    dich_sfh         = as.numeric(ifelse(sfh_share > 0.5, 1, 0)),
    oac_constrained  = as.numeric(ifelse(SPRGRP == "7", 1, 0)),
    oac_cosmopolitan = as.numeric(ifelse(SPRGRP == "2", 1, 0)),
    oac_ethnicentral = as.numeric(ifelse(SPRGRP == "3", 1, 0)),
    oac_hardpressed  = as.numeric(ifelse(SPRGRP == "8", 1, 0)),
    oac_suburban     = as.numeric(ifelse(SPRGRP == "6", 1, 0)),

    share_pre1919bld = p_pre1919
  ) %>%
  dplyr::select(
    # dependent variables
    densification,
    subdivision, hmo, office_rental,
    large_mfh, small_mfh, large_sfh, small_sfh,
    # continuous predictors (raw scale)
    m_to_train, m_to_train_capped, min_to_livmain,
    addresses_2013, log_density,
    amenity_count, log_amenities,
    nb11_HDcount, nb11_LDcount, nb11_NBcount,
    share_pre1919bld, deprivation,
    # dichotomous / dummy predictors
    dich_sfh,
    oac_constrained, oac_cosmopolitan, oac_ethnicentral,
    oac_hardpressed, oac_suburban
  )

# Apply the same na.omit restriction as script 04 so N matches the regression
desc_grid <- na.omit(desc_grid)


#HELPERS ----
fmt <- function(x, digits = 2) formatC(x, format = "f", digits = digits, big.mark = ",")

summarise_continuous <- function(x, digits = 2) {
  tibble(
    N      = sum(!is.na(x)),
    Mean   = fmt(mean(x, na.rm = TRUE), digits),
    SD     = fmt(sd(x, na.rm = TRUE), digits),
    Min    = fmt(min(x, na.rm = TRUE), digits),
    Median = fmt(median(x, na.rm = TRUE), digits),
    Max    = fmt(max(x, na.rm = TRUE), digits),
    `n (=1)` = NA_character_,
    `% (=1)` = NA_character_
  )
}

summarise_binary <- function(x) {
  n1 <- sum(x == 1, na.rm = TRUE)
  N  <- sum(!is.na(x))
  tibble(
    N        = N,
    Mean     = NA_character_,
    SD       = NA_character_,
    Min      = NA_character_,
    Median   = NA_character_,
    Max      = NA_character_,
    `n (=1)` = formatC(n1, format = "d", big.mark = ","),
    `% (=1)` = formatC(100 * n1 / N, format = "f", digits = 1)
  )
}


#BUILD TABLE SPECIFICATION ----
# Each row = one variable. "type" drives which summary function is used.
# "transformation" documents what actually enters the regression in script 04.
var_spec <- tribble(
  ~group,                 ~variable,          ~label,                                              ~type,        ~transformation,
  # dependent variables
  "Dependent variables",  "densification",    "Any densification (1/0)",                           "binary",     "used as outcome in plain model",
  "Dependent variables",  "subdivision",      "Subdivision (1/0)",                                 "binary",     "outcome in type-specific model",
  "Dependent variables",  "hmo",              "HMO conversion (1/0)",                              "binary",     "outcome in type-specific model",
  "Dependent variables",  "office_rental",    "Office / rental conversion (1/0)",                  "binary",     "outcome in type-specific model",
  "Dependent variables",  "large_mfh",        "Large multi-family housing (1/0)",                  "binary",     "outcome in type-specific model",
  "Dependent variables",  "small_mfh",        "Small multi-family housing (1/0)",                  "binary",     "outcome in type-specific model",
  "Dependent variables",  "large_sfh",        "Large single-family housing (1/0)",                 "binary",     "outcome in type-specific model",
  "Dependent variables",  "small_sfh",        "Small single-family housing (1/0)",                 "binary",     "outcome in type-specific model",
  # continuous predictors
  "Continuous predictors","m_to_train_capped","Distance to train station (m, capped at 800)",     "continuous", "capped at 800 m, then standardized (scaled_disttrain)",
  "Continuous predictors","min_to_livmain",   "Driving time to Liverpool centre (min)",            "continuous", "standardized (scaled_distcenter)",
  "Continuous predictors","addresses_2013",   "Addresses in cell, 2013",                           "continuous", "log(x+1) then standardized (scaled_logdensity)",
  "Continuous predictors","amenity_count",    "Amenity count in cell",                             "continuous", "log(x+1) then standardized (scaled_logamenities)",
  "Continuous predictors","nb11_HDcount",     "High-density neighbours (2011)",                    "continuous", "standardized (scaled_hd_nb)",
  "Continuous predictors","nb11_LDcount",     "Low-density neighbours (2011)",                     "continuous", "standardized (scaled_ld_nb)",
  "Continuous predictors","nb11_NBcount",     "Non-built neighbours (2011)",                       "continuous", "standardized (scaled_ub_nb)",
  "Continuous predictors","share_pre1919bld", "Share of pre-1919 buildings in LSOA",               "continuous", "entered as share (share_pre1919bld)",
  "Continuous predictors","deprivation",      "Deprivation score",                                 "continuous", "standardized (scaled_deprivation)",
  # binary / dummy predictors
  "Binary predictors",    "dich_sfh",         "Neighbourhood SFH share > 0.5 (1/0)",               "binary",     "dichotomised from sfh_share (dich_sfh)",
  "Binary predictors",    "oac_constrained",  "OAC: Constrained city dwellers (1/0)",              "binary",     "dummy from SPRGRP == 7",
  "Binary predictors",    "oac_cosmopolitan", "OAC: Cosmopolitans (1/0)",                          "binary",     "dummy from SPRGRP == 2",
  "Binary predictors",    "oac_ethnicentral", "OAC: Ethnicity central (1/0)",                      "binary",     "dummy from SPRGRP == 3",
  "Binary predictors",    "oac_hardpressed",  "OAC: Hard-pressed living (1/0)",                    "binary",     "dummy from SPRGRP == 8",
  "Binary predictors",    "oac_suburban",     "OAC: Suburbanites (1/0)",                           "binary",     "dummy from SPRGRP == 6"
)


#COMPUTE STATISTICS ----
desc_tbl <- var_spec %>%
  mutate(stats = map2(variable, type, function(v, tp) {
    x <- desc_grid[[v]]
    if (tp == "continuous") summarise_continuous(x) else summarise_binary(x)
  })) %>%
  unnest(stats) %>%
  dplyr::select(group, Variable = label, Transformation = transformation,
                N, Mean, SD, Min, Median, Max, `n (=1)`, `% (=1)`)


#FLEXTABLE OUTPUT ----
# Column widths sum to ~9.5 in so the table fits on a landscape A4/Letter page
# (usable width ~ 9.5 in after default 1 in margins on 11 in width).
col_widths <- c(
  Variable       = 2.30,
  Transformation = 2.40,
  N              = 0.55,
  Mean           = 0.60,
  SD             = 0.55,
  Min            = 0.55,
  Median         = 0.60,
  Max            = 0.55,
  `n (=1)`       = 0.70,
  `% (=1)`       = 0.70
)

ft <- flextable(desc_tbl %>% dplyr::select(-group)) %>%
  set_header_labels(
    Variable       = "Variable",
    Transformation = "Transformation in regression",
    N              = "N",
    Mean           = "Mean",
    SD             = "SD",
    Min            = "Min",
    Median         = "Median",
    Max            = "Max",
    `n (=1)`       = "n (=1)",
    `% (=1)`       = "% (=1)"
  ) %>%
  bold(part = "header") %>%
  fontsize(size = 8, part = "all") %>%
  padding(padding = 2, part = "all") %>%
  width(j = names(col_widths), width = unname(col_widths))

# add group sub-headers via body row separators
group_rows <- desc_tbl %>%
  mutate(row = row_number()) %>%
  group_by(group) %>%
  summarise(first_row = min(row), .groups = "drop")

for (i in seq_len(nrow(group_rows))) {
  ft <- ft %>%
    bg(i = group_rows$first_row[i], bg = "#F0F0F0") %>%
    bold(i = group_rows$first_row[i], bold = TRUE)
}

ft


#EXPORT TO WORD (landscape section so the wide table fits) ----
landscape_section <- prop_section(
  page_size = page_size(orient = "landscape"),
  type      = "continuous"
)

doc <- read_docx() %>%
  body_add_par("Descriptive statistics for regression variables (script 04)",
               style = "heading 1") %>%
  body_add_par(
    paste0("N = ", nrow(desc_grid),
           " grid cells in built-up area 2011 after exclusions and na.omit, ",
           "matching the regression sample in 04_Statistical Analysis.R."),
    style = "Normal") %>%
  body_add_flextable(ft) %>%
  body_end_block_section(block_section(landscape_section))

print(doc, target = "R Export/table_descriptive_stats.docx")

# also write a plain CSV for quick inspection
write.csv(desc_tbl,
          file = "R Export/table_descriptive_stats.csv",
          row.names = FALSE)
