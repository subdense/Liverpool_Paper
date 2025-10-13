#DESCRIPTIVE ANALYSIS
library(sf)
library(ggplot2)
library(dplyr)
library(PerformanceAnalytics)
library(tidyr)
library(forcats)
library(corrplot)
library(svglite)

# dens_grid <- st_read("C:/Users/Vera/Documents/SUBDENSE/Projects/Liverpool_Dembski/R Output/grid_full.gpkg") %>% st_drop_geometry() #Pfad Vera
dens_grid <- st_read("G:/ai_daten/P1047_SUBDENSE/liverpool_paper/01_data_input/in_vera/251013/grid_full.gpkg") %>% st_drop_geometry() #Pfad Denise

#reduce to grid cells in built-up area 2011
dens_grid <- dens_grid %>% filter(builtup2011 == 1) %>% 
  mutate(oac_challenged = as.numeric(ifelse(GRP %in% c("7a", "7b", "7c", "7d", "8a", "8b", "8c", "8d"),1,0)),     
         oac_students = as.numeric(ifelse(GRP %in% c("2a", "2b"),1,0)),     
         oac_success = as.numeric(ifelse(GRP %in% c("2c", "2d", "3d", "5a"),1,0)))

#Prevalence----
  color_mapping <- c(
    "Total Densification" = "#636363",
    "large_sfh" = "#156082",
    "small_sfh" = "#00b0f0",
    "small_mfh" = "#ff0000",
    "large_mfh" = "#c00000",
    "office_rental" = "#ffc000",
    "hmo" = "#92d050", 
    "subdivision" = "#c51b8a"
  )

#data_preparation----
##identify number of cells per type
cells <- dens_grid %>% 
  select(hmo:subdivision) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "type",
    values_to = "count_cells")
  
##identify number of housing units per type
units <- dens_grid %>% 
  select(hmo_ct:subdivision_ct) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "type",
    values_to = "count_units")

dat_plot <- cells %>% 
  bind_cols(units %>% select(-type)) %>% 
  mutate(count_all_cells = sum(count_cells),
         share_cells = count_cells/count_all_cells,
         count_all_units = sum(count_units),
         share_units = count_units/count_all_units)

dat_plot$type <- ordered(dat_plot$type,
                         levels = c("large_mfh", "small_mfh", "large_sfh",  "small_sfh", "hmo","subdivision", "office_rental"))
  
##table
table <- dat_plot %>% 
  select(type, count_cells, share_cells, count_units, share_units) %>% 
  mutate(share_cells = round(share_cells*100, 1),
         share_units = round(share_units*100, 1))

#plot quantities per type-----
dat_plot %>% 
  ggplot(aes(y = type, x = count_cells))+
  geom_col()+
  theme_light()

dat_plot %>% 
  ggplot(aes(y = type, x = count_units))+
  geom_col()+
  theme_light()

dat_plot %>% 
  ggplot(aes(y = type, x = share_cells))+
  geom_col()+
  theme_light()

dat_plot %>% 
  ggplot(aes(y = type, x = share_units))+
  geom_col()+
  theme_light()

#prepare data explanatory variables
dat_explanatory <- dens_grid %>% 
  mutate(no_dens = if_else(hmo == 0 & #here we create the negative group for comparison
                             office_rental == 0 &
                             subdivision == 0 &
                             large_mfh == 0 &
                             small_mfh == 0 &
                             large_sfh == 0 &
                             small_sfh == 0, 1, 0), 
         no_dens = if_else(is.na(no_dens), 1, 0)) %>%  # ask Vera if this is correct
  pivot_longer(cols = c(hmo, office_rental, subdivision, large_mfh, small_mfh, large_sfh, small_sfh, no_dens),   
               names_to = "type", 
               values_to = "type_0_1") %>% 
  filter(type_0_1 == 1) %>% 
  select(addresses_2013:income_rank, oac_challenged:type_0_1) %>% 
  mutate(min_to_livmain = if_else(min_to_livmain >50, 50, min_to_livmain)) #there is one outlier in the nodense group which we exclude
  

dat_explanatory$type <- ordered(dat_explanatory$type,
                         levels = c("no_dens", "large_mfh", "small_mfh", "large_sfh",  "small_sfh", "hmo","subdivision", "office_rental"))

# plot for all numeric variables

# where to save
out_dir <- "G:/ai_daten/P1047_SUBDENSE/exchange_tarox/liverpool/boxplots"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# pick the numeric columns to plot on the x-axis
num_vars <- dat_explanatory %>%
  dplyr::select(where(is.numeric)) %>%
  names()

# helper to make the same plot for any numeric variable vs y = type
make_box <- function(df, x_var, y_var = "type") {
  ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_boxplot() +
    theme_light() +
    xlab(x_var) +
    ylab("")
}

# save a plot for each numeric variable
purrr::walk(num_vars, function(v) {
  p <- make_box(dat_explanatory, v)
  
  # filename
  fname <- paste0(gsub("[^[:alnum:]_]+", "_", v), ".svg")
  
  ggsave(
    filename = fname,
    plot = p,
    device = "svg",
    path = out_dir,
    width = 4,
    height = 2,
    dpi = 300,
    bg = "transparent"
  )
})

----------------

dat_explanatory %>% 
  ggplot(aes(x = m_to_train))+
  # geom_density()+
  geom_histogram()+
  # geom_boxplot()+
  facet_wrap(~type, ncol = 1, strip.position="left")+
  theme_light()


m_to_train <- 
  dat_explanatory %>% 
  ggplot(aes(x = m_to_train, y = type))+
  geom_boxplot()+
  theme_light()+
  ylab("")
m_to_train
income_rank <- 
  dat_explanatory %>% 
  ggplot(aes(x = income_rank, y = type))+
  geom_boxplot()+
  theme_light()+
  ylab("")


library(svglite)
ggsave(file="m_to_train.svg", plot= m_to_train, 
       device = "svg", path = "G:/ai_daten/P1047_SUBDENSE/liverpool_paper/figures", width=3.5, height=1.5)

ggsave(file="income_rank.svg", plot= income_rank, 
       device = "svg", path = "G:/ai_daten/P1047_SUBDENSE/liverpool_paper/figures", width=3.5, height=1.5)



++++++++++++ old vera +++++++++++++++

  totals <- dens_grid %>%
    summarise(
      # 'Total Densification' = sum(hunits_dens, na.rm = TRUE),
      'large_sfh'= sum(large_sfh, na.rm = TRUE),
      'small_sfh'= sum(small_sfh, na.rm = TRUE),
      'small_mfh'= sum(small_mfh, na.rm = TRUE),
      'large_mfh'= sum(large_mfh, na.rm = TRUE),
      'office_rental' = sum(office_rental, na.rm = TRUE),
      'hmo' = sum(hmo, na.rm = TRUE),
      'subdivision' = sum(subdivision, na.rm = TRUE)
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Type",
      values_to = "count"
    )
  
  totals_all <- totals %>% 
    summarise(count = sum(count)) %>% 
    transmute(Type = "Total Densification", count)
  
  totals <- totals %>% 
    bind_rows(totals_all)
  
  totals$Type <- fct_relevel(totals$Type, 
                             "Total Densification", "large_sfh", "small_sfh", "small_mfh", "large_mfh", "office_rental", "hmo", "subdivision")
  

  
  
  ggplot(totals, aes(x = count, y = Type, fill = Type)) +
    geom_bar(stat = "identity", color = "black") +
    labs(
      x = "New units on built-up area 2011-2021",
      y = ""
    ) +
    scale_y_discrete(limits = rev(levels(totals$Type))) +
    scale_fill_manual(values = color_mapping) + # Apply the custom colors
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.title.x = element_text(),
      axis.title.x.top = element_text(), # Ensure the title is at the top axis
      axis.ticks.x.top = element_line(), # Add ticks to the top axis
      axis.line.x.top = element_line(), # Add axis line to the top axis
      axis.text.x.top = element_text(), # Ensure the axis text is on the top axis
      axis.title.x.bottom = element_blank(), # Remove bottom axis title
      axis.text.x.bottom = element_blank(), # Remove bottom axis text
      axis.ticks.x.bottom = element_blank(), # Remove bottom axis ticks
      axis.line.x.bottom = element_blank(), # remove bottom axis line
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.position = "none" # Remove the legend as the colors are already mapped
    ) +
    scale_x_continuous(position = "top") # Move the x-axis to the top
  
  # To save the plot with a transparent background:
  setwd("~/Documents/SUBDENSE/Projects/Liverpool_Dembski")
  ggsave("bar_plot_transparent.png", bg = "transparent")
  

#Correlation----
  dens_corr <- dens_grid %>% select(c(amenity_count, m_to_park, m_to_train, min_to_livmain, 
                                      #share_transport, 
                                      water, parks, sports, industry, port, 
                                      sfh_share, nb11_LDcount, nb11_MDcount, nb11_HDcount, nb11_NBcount, deprivation)) %>% st_drop_geometry()
  dens_corr <- na.omit(dens_corr)
  dens_corr <- cor(dens_corr)
  corrplot(dens_corr, type = "upper",  tl.col = "black", tl.srt = 45)
  #NB and MD correlate strongly, take MD out for regression
  
#Variable inflation----

#Variable distribution (graph) (using old types and old data sources) ----
  #dominant year plot across types----
  ggplot(
    subset(projects_type, dominant_year != "post2000"),
    aes(x = dominant_year, fill = dominant_year) # Fill by the factor itself for distinct colors
  ) +
    geom_bar(position = "dodge", color = "black") + # Use geom_bar
    facet_wrap(~ group_name, scales = "free_y", ncol = 2) + # Facet by your groups
    labs(x = "Dominant construction year in LSOA", y = "Count", fill = "Dominant construction year in LSOA") + # Adjust labels
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  #plots of other independent variables across types----
  indep_var <- c("amenity_count", "m_to_park", "m_to_train", "min_to_livmain",
                 "sfh_share", "Rank", "deprivation", "income_rank")
  binwidths <- c(1, 50, 50, 1, 
                 0.1, 10, 0.5, 1000)
  x_labels <- c("Amenity count", "Distance to park (m)", "Distance to Train Station (m)", "Driving time to Liverpool centre",
                "Share of detached housing in LSOA", "Deprivation Rank", "Deprivation", "Income Rank")
  plot_list <- list()
  
  for (i in 1:length(indep_var)) {
    var_name <- indep_var[i]
    # binwidth_i <- binwidths[i] # This is not used for geom_density, and not for geom_bar either
    x_label_i <- x_labels[i]
    
    # Check if the variable is a factor
    p <- ggplot(
      projects_type,
      aes(x = .data[[var_name]], fill = group_name)
    ) +
      geom_density(alpha = 0.5) +
      facet_wrap(~ group_name, scales = "free_y", ncol = 2) +
      labs(x = x_label_i, y = "Density", fill = "Group Type") +
      theme_minimal() +
      theme(legend.position = "none") # Legend might be redundant with faceting
    
    # Store the plot in the list
    plot_list[[i]] <- p
  }
  
  # Arrange plots
  grid.arrange(grobs = plot_list, ncol = 2)
#Variable distribution (table) ----
  #with min mean max sd for no-densification, densification and each of the types for all independent variables
  