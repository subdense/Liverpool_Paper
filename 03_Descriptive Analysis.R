#DESCRIPTIVE ANALYSIS
library(sf)
library(ggplot2)
library(dplyr)
library(PerformanceAnalytics)
library(tidyr)
library(forcats)
library(corrplot)

dens_grid <- st_read("C:/Users/Vera/Documents/SUBDENSE/Projects/Liverpool_Dembski/R Output/grid_full.gpkg") %>% st_drop_geometry() #Pfad Vera
#dens_grid <- st_read("G:/ai_daten/P1047_SUBDENSE/liverpool_paper/Projects/Liverpool_Dembski/R Outputs/grid_full.gpkg") %>% st_drop_geometry() #Pfad Denise

#reduce to grid cells in built-up area 2011
dens_grid <- dens_grid %>% filter(builtup2011 == 1)

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
  