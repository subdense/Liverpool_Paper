#REGRESSION


#LIBRARIES----
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr) #for pivoting

library(pscl) #for model evaluation mcfadden
library(forcats)
library(corrplot)
library(gridExtra)
library(marginaleffects)
library(pROC)


options(scipen = 999)

#READ DATA----
#setwd("C:/Users/Vera/Documents/SUBDENSE")
setwd("G:/ai_daten/P1047_SUBDENSE/liverpool_paper")
dens_grid <- st_read("G:/ai_daten/P1047_SUBDENSE/liverpool_paper/Projects/Liverpool_Dembski/R Outputs/grid_full.gpkg") %>% #Pfad Denise
# dens_grid <- st_read("Projects/Liverpool_Dembski/R Output/grid_full.gpkg") %>% 
  filter(builtup2011 == 1) %>% #from now on only interested in cells in builtup area
  st_drop_geometry() #%>% 
  #mutate(dominant_year = factor(dominant_year, levels = c("pre1900", "1900_1918", "1919_1939", "1945_1999", "post2000"), ordered = TRUE))

trans_grid <- dens_grid %>% 
  mutate(
    #make densification factor
    densification = if_any(c(hmo, office_rental, large_mfh, small_mfh, large_sfh, small_sfh, subdivision), ~ .x == 1) %>% as.integer(),
    densification = as.factor(densification),
    
    #dist to train, standardized with cutoff at 5 km
    scaled_disttrain = as.numeric(scale(pmin(m_to_train, 5000))),

    #dist to park 
    scaled_distpark = as.numeric(scale(pmin(m_to_park, 2000))),
    
    #dist center
    scaled_distcenter = as.numeric(scale(min_to_livmain)),

    #amenities (log transform before standardizing)
    scaled_logamenities = as.numeric(scale(log(amenity_count + 1))),
    
    #density2013
    scaled_density = as.numeric(scale(addresses_2013)),

    #density nb
    scaled_hd_nb = as.numeric(scale(nb11_HDcount)),
    scaled_ld_nb = as.numeric(scale(nb11_LDcount)),
    scaled_ub_nb = as.numeric(scale(nb11_NBcount)),
    
    #sfh share in neighborhood (decided to go dichotomous)
    scaled_dich_sfh = as.numeric(scale(ifelse(sfh_share > 0.5, 1, 0))),
    
    #deprivation and income
    scaled_incomerank = as.numeric(scale(income_rank)),
    #deprivation = deprivation/max(deprivation),

    #output area classification
    oac_challenged = as.numeric(scale(ifelse(GRP %in% c("7a", "7b", "7c", "7d", "8a", "8b", "8c", "8d"),1,0))),
    oac_students = as.numeric(scale(ifelse(GRP %in% c("2a", "2b"),1,0))),
    oac_success = as.numeric(scale(ifelse(GRP %in% c("2c", "2d", "3d", "5a"),1,0))),
    
    #building age
    dominant_year = fct_relevel(dominant_year, "1945_1999"), #back to unordered factor and set post war as reference 
    #projects_reg$dominant_year <- factor(projects_reg$dominant_year, ordered = FALSE )

    #all land use variables turn into dichotomous
    scaled_dich_unlikely = as.numeric(scale(ifelse(water > 0 |
                                                     rail > 0 |
                                                     airport > 0 |
                                                     dump > 0, 1, 0))),
    scaled_dich_park = as.numeric(scale(ifelse(parks > 0, 1, 0))),
    scaled_dich_sports = as.numeric(scale(ifelse(sports > 0, 1, 0))),
    scaled_dich_industry = as.numeric(scale(ifelse(industry > 0, 1, 0))),
    scaled_dich_port = as.numeric(scale(ifelse(port > 0, 1, 0))),
    scaled_dich_nature = as.numeric(scale(ifelse(nature > 0, 1, 0))),
    scaled_dich_agriculture = as.numeric(scale(ifelse(agriculture > 0, 1, 0)))
  ) %>%
  
  select(grid_id, densification, 
         subdivision, hmo, office_rental, large_mfh, small_mfh, large_sfh, small_sfh,
         scaled_disttrain, scaled_distpark, scaled_distcenter,
         scaled_density,
         scaled_logamenities, 
         scaled_hd_nb, scaled_ld_nb, scaled_ub_nb,
         dominant_year,
         oac_challenged, oac_students, oac_success, 
         scaled_incomerank,
         scaled_dich_sfh, 
         scaled_dich_unlikely, scaled_dich_park, scaled_dich_sports, scaled_dich_industry, scaled_dich_port, scaled_dich_nature, scaled_dich_agriculture
  )

#sapply(dens_grid,function(x) sum(is.na(x)))
trans_grid <- na.omit(trans_grid)

plain_grid <- trans_grid %>% select(-c(subdivision, hmo, office_rental, large_mfh, small_mfh, large_sfh, small_sfh))
model_plain <- glm(densification ~ .-grid_id, data = plain_grid, family = "binomial")
summary(model_plain)
pR2(model_plain) #mcfadden

avg_effects <- avg_slopes(model_plain)
avg_effects

#03.4 AUC and F1 of plain model ----
pred <- predict(model_plain, type = "response")
roc <- roc(plain_grid$densification, pred)
auc(roc)

calculate_optimal_f1 <- function(pred_probs, actual_densification) {
  thresholds_seq <- seq(0, 1, by = 0.01)
  f1_scores <- sapply(thresholds_seq, function(cutoff) {
    preds <- ifelse(pred > cutoff, 1, 0)
    conf_mat <- table(factor(preds, levels = c(0, 1)), factor(actual_densification, levels = c(0, 1)))
    
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

result <- calculate_optimal_f1(pred, plain_grid$densification)
result$f1
result$optimal_cutoff 


#what if we take out large projects and only predict all the other ones? ----
small_grid <- trans_grid %>%
  filter(large_mfh == 0 & large_sfh == 0) %>%
  select(-c(subdivision, hmo, office_rental, large_mfh, small_mfh, large_sfh, small_sfh))

model_small <- glm(densification ~ .-grid_id, data = small_grid, family = "binomial")
summary(model_small)
pR2(model_small) #mcfadden

avg_effects <- avg_slopes(model_small)
avg_effects

#AMONG DENSIFICATION PROJECTS - DID SUBDIVISIONS HAPPEN? ----
subdi_grid <- trans_grid %>% 
  filter(densification == 1) %>%
  select(-c(densification, hmo, office_rental, large_mfh, small_mfh, large_sfh, small_sfh))
model_subdi <- glm(subdivision ~ .-grid_id, data = subdi_grid, family = "binomial")
summary(model_subdi)
pR2(model_subdi) #mcfadden

pred <- predict(model_subdi, type = "response")
roc <- roc(subdi_grid$subdivision, pred)
auc(roc)

result <- calculate_optimal_f1(pred, subdi_grid$subdivision)
result$f1
result$optimal_cutoff 
    
#AMONG DENSIFICATION PROJECTS - DID FLATS PREVAIL?----

#AMONG DENSIFICATION PROJECTS - WERE HMO CONSTRUCTED?----
hmo_grid <- trans_grid %>% 
  filter(densification == 1) %>%
  select(-c(densification, subdivision, office_rental, large_mfh, small_mfh, large_sfh, small_sfh))
model_hmo <- glm(hmo ~ .-grid_id, data = hmo_grid, family = "binomial")
summary(model_hmo)
pR2(model_hmo) #mcfadden

pred <- predict(model_hmo, type = "response")
roc <- roc(hmo_grid$hmo, pred)
auc(roc)

result <- calculate_optimal_f1(pred, hmo_grid$hmo)
result$f1
result$optimal_cutoff 

#AMONG DENSIFICATION PROJECTS - LARGE PROJECTS?----

#AMONG DENSIFICATION PROJECTS - OFFICE-RENTAL?----
