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
# Run from the project root (open Liverpool_Paper.Rproj in RStudio).
dens_grid <- st_read("R Output/grid_full.gpkg") %>%
  filter(builtup2011 == 1) %>% #from now on only interested in cells in builtup area
  #all cells that don't overlap with special non-residential land uses have NA but should be 0
  mutate(across(c(nature,agriculture, industry, water,sports,parks,port,airport,dump,rail), ~ replace_na(.x, 0))) %>%
  filter(nature != 1 & agriculture != 1 & water != 1 & airport != 1 & dump != 1 & rail != 1) %>%
  st_drop_geometry() 

#Transform variables----
trans_grid <- dens_grid %>% 
  mutate(
    across(c(
      subdivision, hmo, office_rental, 
      large_mfh, small_mfh, large_sfh, small_sfh, addresses_2013, sfh_share), ~ replace_na(.x, 0)),
      
    #make densification factor
    densification = if_any(c(hmo, office_rental, large_mfh, small_mfh, large_sfh, small_sfh, subdivision), ~ .x == 1) %>% as.integer(),
    densification = as.factor(densification),
    
    #dist to train, standardized with cutoff at 5 km
    scaled_disttrain = as.numeric(scale(pmin(m_to_train, 800))),
    
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
    
    #deprivation
    scaled_deprivation = -scale(deprivation),

    #output area classification
    oac_constrained = as.numeric(ifelse(SPRGRP == "7",1,0)),
    oac_cosmopolitan = as.numeric(ifelse(SPRGRP == "2",1,0)),
    oac_ethnicentral = as.numeric(ifelse(SPRGRP == "3",1,0)),
    oac_hardpressed = as.numeric(ifelse(SPRGRP == "8",1,0)),
    oac_suburban = as.numeric(ifelse(SPRGRP == "6",1,0)),
    
    #building age
    #can you scale a share? 
    share_pre1919bld = p_pre1919
  ) %>%
  
  dplyr::select(grid_id, densification, 
         subdivision, hmo, office_rental, large_mfh, small_mfh, large_sfh, small_sfh,
         scaled_disttrain, scaled_distcenter,
         scaled_logdensity,
         scaled_logamenities, 
         scaled_hd_nb, scaled_ld_nb, scaled_ub_nb,
         share_pre1919bld,
         oac_constrained, oac_cosmopolitan, oac_ethnicentral, oac_hardpressed, oac_suburban,
         scaled_deprivation,
         dich_sfh 
  )  

sapply(trans_grid,function(x) sum(is.na(x))) #all NA values are because the cell did not have any address data at any point
trans_grid <- na.omit(trans_grid)


#Variable label mapping and display order----
term_labels <- c(
  "(Intercept)"         = "(Intercept)",
  "scaled_logamenities" = "(ln) Amenity count",
  "scaled_disttrain"    = "Distance to train station",
  "scaled_distcenter"   = "Distance to Liverpool centre",
  "scaled_hd_nb"        = "High-density cells nearby",
  "scaled_ld_nb"        = "Low-density cells nearby",
  "scaled_ub_nb"        = "Unbuilt cells nearby",
  "scaled_deprivation"  = "Deprivation decile",
  "oac_constrained"     = "OAC Supergroup Constrained City Dwellers",
  "oac_cosmopolitan"    = "OAC Supergroup Cosmopolitans",
  "oac_ethnicentral"    = "OAC Supergroup Ethnicity Central",
  "oac_hardpressed"     = "OAC Supergroup Hard-Pressed Living",
  "oac_suburban"        = "OAC Supergroup Suburbanites",
  "scaled_logdensity"   = "(ln) Address density 2013",
  "share_pre1919bld"    = "Share of buildings constructed pre-1919",
  "dich_sfh"            = "Single-family housing dominant",
  "AUC"                 = "AUC",
  "F1"                  = "F1",
  "McFadden_R2"         = "McFadden_R2"
)
term_order <- unname(term_labels)

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

pred <- predict(model_plain, type = "response")
roc_obj <- roc(plain_grid$densification, pred)
auc(roc_obj)
calculate_optimal_f1(pred, plain_grid$densification)

#Plain model results for table----
plain_auc <- as.numeric(auc(roc_obj))
plain_r2  <- pR2(model_plain)["McFadden"]
plain_f1  <- calculate_optimal_f1(pred, plain_grid$densification)$f1

plain_results <- bind_rows(
  tidy(model_plain) %>%
    filter(p.value < 0.05) %>%
    select(term, estimate) %>%
    mutate(term = unname(term_labels[term])),
  tibble(term = c("AUC", "F1", "McFadden_R2"),
         estimate = c(plain_auc, plain_f1, plain_r2))
) %>%
  mutate(model = "densification")


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
    select(term, estimate) %>%
    mutate(term = unname(term_labels[term]))

  metrics <- tibble(
    term = c("AUC", "F1", "McFadden_R2"),
    estimate = c(auc_val, f1_val, r2_val)
  )

  bind_rows(coef_df, metrics) %>%
    mutate(model = dep)
})

all_results <- bind_rows(plain_results, all_results)

# Order terms according to term_order; indicators sit at the end ----

indicator_order <- c("AUC", "F1", "McFadden_R2")
display_vars <- c("densification", dependent_vars)

wide_results <- all_results %>%
  mutate(
    estimate = round(estimate, 2),
    is_indicator = term %in% indicator_order,
    term = factor(term, levels = term_order)
  ) %>%
  arrange(term) %>%
  pivot_wider(names_from = model, values_from = estimate) %>%
  select(term, is_indicator, all_of(display_vars))

# Build formatted flextable----

ft <- flextable(wide_results,
                col_keys = setdiff(names(wide_results), "is_indicator")) %>%
  set_header_labels(term = "Variable / Indicator")

# Color coefficients only (exclude indicators)
coef_rows <- which(!wide_results$is_indicator)

for (col in display_vars) {
  vals <- wide_results[[col]][coef_rows]
  ft <- color(ft, i = coef_rows[which(vals > 0)],
              j = col, color = "green")
  ft <- color(ft, i = coef_rows[which(vals < 0)],
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

print(doc, target = "R Export/model_results_grouped.docx")



#Run models for densification types - compared to all cells ----
dependent_vars <- c("subdivision", "hmo", "office_rental", "large_mfh", "small_mfh", "large_sfh", "small_sfh")

all_results <- map_dfr(dependent_vars, function(dep) {
  
  df <- trans_grid %>%
    # filter(densification == 1) %>%
    dplyr::select(-c(densification, hmo, subdivision, office_rental,
                     large_mfh, large_sfh, small_mfh, small_sfh))
  df <- bind_cols(df, trans_grid %>% 
                    # filter(densification == 1) %>% 
                    select(all_of(dep))) #reattach dependent variable
  
  names(df)[ncol(df)] <- dep #renames last column to current dependent variable name
  
  model <- glm(reformulate(" . - grid_id", dep), data = df, family = "binomial")
  
  pred <- predict(model, type = "response")
  roc_obj <- roc(df[[dep]], pred)
  auc_val <- as.numeric(auc(roc_obj))
  r2_val <- pR2(model)["McFadden"]
  f1_val <- calculate_optimal_f1(pred, df[[dep]])$f1
  
  coef_df <- tidy(model) %>%
    filter(p.value < 0.05) %>%
    select(term, estimate) %>%
    mutate(term = unname(term_labels[term]))

  metrics <- tibble(
    term = c("AUC", "F1", "McFadden_R2"),
    estimate = c(auc_val, f1_val, r2_val)
  )

  bind_rows(coef_df, metrics) %>%
    mutate(model = dep)
})

all_results <- bind_rows(plain_results, all_results)

# Order terms according to term_order; indicators sit at the end ----

indicator_order <- c("AUC", "F1", "McFadden_R2")
display_vars <- c("densification", dependent_vars)

wide_results <- all_results %>%
  mutate(
    estimate = round(estimate, 2),
    is_indicator = term %in% indicator_order,
    term = factor(term, levels = term_order)
  ) %>%
  arrange(term) %>%
  pivot_wider(names_from = model, values_from = estimate) %>%
  select(term, is_indicator, all_of(display_vars))

# Build formatted flextable----

ft <- flextable(wide_results,
                col_keys = setdiff(names(wide_results), "is_indicator")) %>%
  set_header_labels(term = "Variable / Indicator")

# Color coefficients only (exclude indicators)
coef_rows <- which(!wide_results$is_indicator)

for (col in display_vars) {
  vals <- wide_results[[col]][coef_rows]
  ft <- color(ft, i = coef_rows[which(vals > 0)],
              j = col, color = "green")
  ft <- color(ft, i = coef_rows[which(vals < 0)],
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

print(doc, target = "R Export/table_allgrid.docx")


