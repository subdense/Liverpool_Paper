#REGRESSION


#LIBRARIES----
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr) #for pivoting

library(forcats) #factor relevel
library(pscl) #for model evaluation mcfadden
library(marginaleffects)
library(pROC)
library(car) #for vif test

library(broom)
library(purrr)
library(flextable)
library(officer)

options(scipen = 999)

#READ DATA----
setwd("C:/Users/Vera/Documents/SUBDENSE")
#setwd("G:/ai_daten/P1047_SUBDENSE/liverpool_paper")
#dens_grid <- st_read("G:/ai_daten/P1047_SUBDENSE/liverpool_paper/Projects/Liverpool_Dembski/R Outputs/grid_full.gpkg") %>% #Pfad Denise
dens_grid <- st_read("Projects/Liverpool_Dembski/R Output/grid_full.gpkg") %>% 
  filter(builtup2011 == 1) %>% #from now on only interested in cells in builtup area
  st_drop_geometry() 

#Transform variables----
trans_grid <- dens_grid %>% 
  mutate(
    #make densification factor
    densification = if_any(c(hmo, office_rental, large_mfh, small_mfh, large_sfh, small_sfh, subdivision), ~ .x == 1) %>% as.integer(),
    densification = as.factor(densification),
    
    #dist to train, standardized with cutoff at 5 km
    scaled_disttrain = as.numeric(scale(pmin(m_to_train, 800))),

    #dist to park 
    scaled_distpark = as.numeric(scale(pmin(m_to_park, 2000))),
    
    #dist center
    scaled_distcenter = as.numeric(scale(min_to_livmain)),

    #amenities (log transform before standardizing)
    scaled_logamenities = as.numeric(scale(log(amenity_count + 1))),
    
    #density2013
    scaled_logdensity = as.numeric(scale(log(addresses_2013 + 1))),

    #density nb
    scaled_hd_nb = as.numeric(scale(nb11_HDcount)),
    scaled_ld_nb = as.numeric(scale(nb11_LDcount)),
    scaled_ub_nb = as.numeric(scale(nb11_NBcount)),
    
    #sfh share in neighborhood (decided to go dichotomous)
    dich_sfh = as.numeric(ifelse(sfh_share > 0.5, 1, 0)),
    
    #deprivation and income
    scaled_incomerank = as.numeric(scale(income_rank)),
    #deprivation = deprivation/max(deprivation),

    #output area classification
    oac_constrained = as.numeric(ifelse(SPRGRP == "7",1,0)),
    oac_cosmopolitan = as.numeric(ifelse(SPRGRP == "2",1,0)),
    oac_ethnicentral = as.numeric(ifelse(SPRGRP == "3",1,0)),
    oac_hardpressed = as.numeric(ifelse(SPRGRP == "8",1,0)),
    oac_suburban = as.numeric(ifelse(SPRGRP == "6",1,0)),
    
    #building age
    dominant_year = fct_relevel(dominant_year, "1945_1999"), #back to unordered factor and set post war as reference 

    #all land use variables turn into dichotomous. i do not scale binary variables
    dich_unlikely = as.numeric(ifelse(water > 0 |
                                                     rail > 0 |
                                                     airport > 0 |
                                                     dump > 0, 1, 0)),
    dich_park = as.numeric(ifelse(parks > 0, 1, 0)),
    dich_sports = as.numeric(ifelse(sports > 0, 1, 0)),
    dich_industry = as.numeric(ifelse(industry > 0, 1, 0)),
    dich_port = as.numeric(ifelse(port > 0, 1, 0)),
    dich_nature = as.numeric(ifelse(nature > 0, 1, 0)),
    dich_agriculture = as.numeric(ifelse(agriculture > 0, 1, 0))
  ) %>%
  
  dplyr::select(grid_id, densification, 
         subdivision, hmo, office_rental, large_mfh, small_mfh, large_sfh, small_sfh,
         scaled_disttrain, scaled_distpark, scaled_distcenter,
         scaled_logdensity,
         scaled_logamenities, 
         scaled_hd_nb, scaled_ld_nb, scaled_ub_nb,
         dominant_year,
         oac_constrained, oac_cosmopolitan, oac_ethnicentral, oac_hardpressed, oac_suburban,
         scaled_incomerank,
         dich_sfh, 
         dich_unlikely, dich_park, dich_sports, dich_industry, dich_port, dich_nature, dich_agriculture
  ) %>%
  
  #finally, all cells that don't overlap with special non-residential land uses have NA but should be 0
  mutate(across(c(dich_unlikely, dich_park, dich_sports, dich_industry, dich_port, dich_nature, dich_agriculture), ~ replace_na(.x, 0)))
  #all other NA values are because the cell did not have any address data at any point

sapply(trans_grid,function(x) sum(is.na(x)))
trans_grid <- na.omit(trans_grid)


#Helper function to calculate optimal f1----
calculate_optimal_f1 <- function(pred_probs, actual_densification) {
  thresholds_seq <- seq(0, 1, by = 0.01)
  f1_scores <- sapply(thresholds_seq, function(cutoff) {
    preds <- ifelse(pred_probs > cutoff, 1, 0)
    conf_mat <- table(factor(preds, levels = c(0, 1)), 
                      factor(actual_densification, levels = c(0, 1)))
    
    TP <- conf_mat[2, 2]
    FP <- conf_mat[2, 1]
    FN <- conf_mat[1, 2]
    
    precision <- ifelse((TP + FP) == 0, 0, TP / (TP + FP))
    recall <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
    
    f1 <- ifelse((precision + recall) == 0, 0, 2 * precision * recall / (precision + recall))
    return(f1) 
  })
  
  optimal_idx <- which.max(f1_scores)
  optimal_f1 <- f1_scores[optimal_idx]
  optimal_cutoff <- thresholds_seq[optimal_idx]
  
  return(list(f1 = optimal_f1, optimal_cutoff = optimal_cutoff))
}

#Run plain model----
plain_grid <- trans_grid %>% dplyr::select(-c(subdivision, hmo, office_rental, large_mfh, small_mfh, large_sfh, small_sfh))
model_plain <- glm(densification ~ .-grid_id, data = plain_grid, family = "binomial")
summary(model_plain)
vif(model_plain) 

pR2(model_plain) #mcfadden

avg_effects <- avg_slopes(model_plain)
avg_effects
#Run models for densification types ----
dependent_vars <- c("subdivision", "hmo", "office_rental", "large_mfh", "small_mfh", "large_sfh", "small_sfh")

all_results <- map_dfr(dependent_vars, function(dep) {
  
  df <- trans_grid %>%
    filter(densification == 1) %>%
    dplyr::select(-c(densification, hmo, subdivision, office_rental,
                     large_mfh, large_sfh, small_mfh, small_sfh))
  df <- bind_cols(df, trans_grid %>% filter(densification == 1) %>% select(all_of(dep))) #reattach dependent variable
  
  names(df)[ncol(df)] <- dep #renames last column to current dependent variable name
  
  model <- glm(reformulate(" . - grid_id", dep), data = df, family = "binomial")
  
  pred <- predict(model, type = "response")
  roc_obj <- roc(df[[dep]], pred)
  auc_val <- as.numeric(auc(roc_obj))
  r2_val <- pR2(model)["McFadden"]
  f1_val <- calculate_optimal_f1(pred, df[[dep]])$f1
  
  coef_df <- tidy(model) %>%
    filter(p.value < 0.05) %>%
    select(term, estimate)
  
  metrics <- tibble(
    term = c("AUC", "McFadden_R2", "F1"),
    estimate = c(auc_val, r2_val, f1_val)
  )
  
  bind_rows(coef_df, metrics) %>%
    mutate(model = dep)
})

# Order terms: coefficients first, indicators last ----

indicator_order <- c("AUC", "McFadden_R2", "F1")

wide_results <- all_results %>%
  mutate(
    estimate = round(estimate, 3),
    is_indicator = term %in% indicator_order
  ) %>%
  arrange(is_indicator, term) %>%
  pivot_wider(names_from = model, values_from = estimate)

# Build formatted flextable----

ft <- flextable(wide_results) %>%
  set_header_labels(term = "Variable / Indicator")

# Color coefficients only (exclude indicators)
coef_rows <- which(!wide_results$is_indicator)

for (col in dependent_vars) {
  ft <- color(ft, i = coef_rows[wide_results[[col]][coef_rows] > 0],
              j = col, color = "green")
  ft <- color(ft, i = coef_rows[wide_results[[col]][coef_rows] < 0],
              j = col, color = "red")
}

# Style indicators (bold, gray background)
ft <- ft %>%
  bold(i = ~ term %in% indicator_order, bold = TRUE) %>%
  bg(i = ~ term %in% indicator_order, bg = "#F0F0F0") %>%
  autofit()

ft

#export final table to word----
doc <- read_docx() %>%
  body_add_par("Regression Results (grouped by dependent variable)", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = "model_results_grouped.docx")
