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
library(car) #for vif test


options(scipen = 999)

#READ DATA----
setwd("C:/Users/Vera/Documents/SUBDENSE")
#setwd("G:/ai_daten/P1047_SUBDENSE/liverpool_paper")
#dens_grid <- st_read("G:/ai_daten/P1047_SUBDENSE/liverpool_paper/Projects/Liverpool_Dembski/R Outputs/grid_full.gpkg") %>% #Pfad Denise
dens_grid <- st_read("Projects/Liverpool_Dembski/R Output/grid_full.gpkg") %>% 
  filter(builtup2011 == 1) %>% #from now on only interested in cells in builtup area
  st_drop_geometry() #%>% 
  #mutate(dominant_year = factor(dominant_year, levels = c("pre1900", "1900_1918", "1919_1939", "1945_1999", "post2000"), ordered = TRUE))

#Transform variables----
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
    dich_sfh = as.numeric(ifelse(sfh_share > 0.5, 1, 0)),
    
    #deprivation and income
    scaled_incomerank = as.numeric(scale(income_rank)),
    #deprivation = deprivation/max(deprivation),

    #output area classification
    oac_challenged = as.numeric(ifelse(GRP %in% c("7a", "7b", "7c", "7d", "8a", "8b", "8c", "8d"),1,0)),
    oac_students = as.numeric(ifelse(GRP %in% c("2a", "2b"),1,0)),
    oac_success = as.numeric(ifelse(GRP %in% c("2c", "2d", "3d", "5a"),1,0)),
    
    #building age
    dominant_year = fct_relevel(dominant_year, "1945_1999"), #back to unordered factor and set post war as reference 
    #projects_reg$dominant_year <- factor(projects_reg$dominant_year, ordered = FALSE )

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
         scaled_density,
         scaled_logamenities, 
         scaled_hd_nb, scaled_ld_nb, scaled_ub_nb,
         dominant_year,
         oac_challenged, oac_students, oac_success, 
         scaled_incomerank,
         dich_sfh, 
         dich_unlikely, dich_park, dich_sports, dich_industry, dich_port, dich_nature, dich_agriculture
  ) %>%
  
  #finally, all cells that don't overlap with special non-residential land uses have NA but should be 0
  mutate(across(c(dich_unlikely, dich_park, dich_sports, dich_industry, dich_port, dich_nature, dich_agriculture), ~ replace_na(.x, 0)))
  #all other NA values are because the cell did not have any address data at any point

sapply(trans_grid,function(x) sum(is.na(x)))
trans_grid <- na.omit(trans_grid)

#Plain regression----
plain_grid <- trans_grid %>% dplyr::select(-c(subdivision, hmo, office_rental, large_mfh, small_mfh, large_sfh, small_sfh))
model_plain <- glm(densification ~ .-grid_id, data = plain_grid, family = "binomial")
summary(model_plain)
vif(model_plain) 

pR2(model_plain) #mcfadden

avg_effects <- avg_slopes(model_plain)
avg_effects

#AUC and F1 of plain model
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

#Stepwise regression----
library(MASS)
step_model <- stepAIC(model_plain, direction = "both", trace = FALSE)
summary(step_model)
pred <- predict(step_model, type = "response")
result <- calculate_optimal_f1(pred, plain_grid$densification)
result$f1 #0.27 just like previous model

#Model without cells with incompatible land use (better but this also only leaves us with 26000 cells compared to 32500 above----
plain_red_grid <- plain_grid %>% 
  filter(dich_unlikely != 1 & dich_agriculture != 1 & dich_nature != 1 & dich_park != 1 & dich_sports != 1) %>% 
  dplyr::select(-c(dich_unlikely, dich_agriculture, dich_nature, dich_park, dich_sports))
model_red <- glm(densification ~ .-grid_id, data = plain_red_grid, family = "binomial")
summary(model_red)
vif(model_red) 
pred <- predict(model_red, type = "response")
result <- calculate_optimal_f1(pred, plain_red_grid$densification)
result$f1 #0.34 = a bit better

#What if we take out large projects and only predict all the other ones? ----
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
  dplyr::select(-c(densification, hmo, office_rental, large_mfh, small_mfh, large_sfh, small_sfh))

model_subdi <- glm(subdivision ~ .-grid_id, data = subdi_grid, family = "binomial")
summary(model_subdi)
pR2(model_subdi) #mcfadden

pred <- predict(model_subdi, type = "response")
roc <- roc(subdi_grid$subdivision, pred)
auc(roc)

result <- calculate_optimal_f1(pred, subdi_grid$subdivision)
result$f1
result$optimal_cutoff 
    
#AMONG DENSIFICATION PROJECTS - WERE HMO CONSTRUCTED?----
hmo_grid <- trans_grid %>% 
  filter(densification == 1) %>%
  dplyr::select(-c(densification, subdivision, office_rental, large_mfh, small_mfh, large_sfh, small_sfh))
model_hmo <- glm(hmo ~ .-grid_id, data = hmo_grid, family = "binomial")
summary(model_hmo)
pR2(model_hmo) #mcfadden

pred <- predict(model_hmo, type = "response")
roc <- roc(hmo_grid$hmo, pred)
auc(roc)

result <- calculate_optimal_f1(pred, hmo_grid$hmo)
result$f1
result$optimal_cutoff 

#AMONG DENSIFICATION PROJECTS - LARGE PROJECTS MFH?----
mfhl_grid <- trans_grid %>% 
  filter(densification == 1) %>%
  dplyr::select(-c(densification, subdivision, office_rental, hmo, small_mfh, large_sfh, small_sfh))
model_mfhl <- glm(large_mfh ~ .-grid_id, data = mfhl_grid, family = "binomial")
summary(model_mfhl)
pR2(model_mfhl) #mcfadden

pred <- predict(model_mfhl, type = "response")
roc <- roc(mfhl_grid$large_mfh, pred)
auc(roc)

result <- calculate_optimal_f1(pred, mfhl_grid$large_mfh)
result$f1
result$optimal_cutoff 

#AMONG DENSIFICATION PROJECTS - LARGE PROJECTS SFH?----
hmo_grid <- trans_grid %>% 
  filter(densification == 1) %>%
  dplyr::select(-c(densification, subdivision, office_rental, large_mfh, small_mfh, large_sfh, small_sfh))
model_hmo <- glm(hmo ~ .-grid_id, data = hmo_grid, family = "binomial")
summary(model_hmo)
pR2(model_hmo) #mcfadden

pred <- predict(model_hmo, type = "response")
roc <- roc(hmo_grid$hmo, pred)
auc(roc)

result <- calculate_optimal_f1(pred, hmo_grid$hmo)
result$f1
result$optimal_cutoff 
#AMONG DENSIFICATION PROJECTS - SMALL PROJECTS MFH?----
hmo_grid <- trans_grid %>% 
  filter(densification == 1) %>%
  dplyr::select(-c(densification, subdivision, office_rental, large_mfh, small_mfh, large_sfh, small_sfh))
model_hmo <- glm(hmo ~ .-grid_id, data = hmo_grid, family = "binomial")
summary(model_hmo)
pR2(model_hmo) #mcfadden

pred <- predict(model_hmo, type = "response")
roc <- roc(hmo_grid$hmo, pred)
auc(roc)

result <- calculate_optimal_f1(pred, hmo_grid$hmo)
result$f1
result$optimal_cutoff 
#AMONG DENSIFICATION PROJECTS - SMALL PROJECTS SFH?----
hmo_grid <- trans_grid %>% 
  filter(densification == 1) %>%
  dplyr::select(-c(densification, subdivision, office_rental, large_mfh, small_mfh, large_sfh, small_sfh))
model_hmo <- glm(hmo ~ .-grid_id, data = hmo_grid, family = "binomial")
summary(model_hmo)
pR2(model_hmo) #mcfadden

pred <- predict(model_hmo, type = "response")
roc <- roc(hmo_grid$hmo, pred)
auc(roc)

result <- calculate_optimal_f1(pred, hmo_grid$hmo)
result$f1
result$optimal_cutoff 
#AMONG DENSIFICATION PROJECTS - OFFICE-RENTAL?----
hmo_grid <- trans_grid %>% 
  filter(densification == 1) %>%
  dplyr::select(-c(densification, subdivision, office_rental, large_mfh, small_mfh, large_sfh, small_sfh))
model_hmo <- glm(hmo ~ .-grid_id, data = hmo_grid, family = "binomial")
summary(model_hmo)
pR2(model_hmo) #mcfadden

pred <- predict(model_hmo, type = "response")
roc <- roc(hmo_grid$hmo, pred)
auc(roc)

result <- calculate_optimal_f1(pred, hmo_grid$hmo)
result$f1
result$optimal_cutoff 