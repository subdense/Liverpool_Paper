#ATTACHING INDEPENDENT VARIABLES TO HECTARE GRID


#LIBRARIES----
library(sf)
library(dplyr)
library(osmdata)
library(tidyr)
library(nngeo) #to identify nearest point
#library(classInt) #for jenks breaks

#00 Read data ----
  setwd("C:/Users/Vera/Documents/SUBDENSE/Data")
  dens_grid <- read_sf("C:/Users/Vera/Documents/SUBDENSE/Projects/Liverpool_Dembski/R Output/grid_depvar.gpkg")
  metro <- read_sf("boundaries/liverpool_metropolitan_dissolved.gpkg")
  builtup <- read_sf("England/Boundaries/Built_up_Areas_Dec_2011_Boundaries_V2_2022_6094869787211526009.gpkg") %>% select(BUA11CD)
  lsoa <- st_read("England/Boundaries/glcr-lsoa-2011.gpkg") %>% select(LSOA11CD)
  
  #00.1 OSM data: amenities and parks----
  #this downloads the latest version, if you want earlier snapshots, go to Geofabrik
  
    #first we define the bounding box based on the case area 
    bbox_sfc <- st_as_sfc(st_bbox(metro, crs = 27700)) #bbox Liverpool region, crs = 27700 #creating the bbox object
    
    bbox_wgs84 <- st_transform(bbox_sfc, 4326) #transform bbox to WGS 84 for osm
    bbox_coords <- st_bbox(bbox_wgs84) #get coordinates
    
    # Create initial query for restaurants and bars (points)
    query1 <- opq(bbox = bbox_coords) %>%
      add_osm_feature(key = "amenity", value = c("restaurant", "bar"))
    
    # Add supermarkets (points)
    query2 <- opq(bbox = bbox_coords) %>%
      add_osm_feature(key = "shop", value = "supermarket")
    
    # Add parks (polygons)
    query3 <- opq(bbox = bbox_coords) %>%
      add_osm_feature(key = "leisure", value = "park")
    
    # Download data
    osm_data1 <- osmdata_sf(query1)
    osm_data2 <- osmdata_sf(query2)
    osm_data3 <- osmdata_sf(query3)
    
    # Extract relevant layers
    points_data <- bind_rows(
      osm_data1$osm_points,
      osm_data2$osm_points
    )
    
    parks_polygons <- osm_data3$osm_polygons
    
    # Transform all to CRS 27700
    amenities <- st_transform(points_data, 27700)
    parks <- st_transform(parks_polygons, 27700)
    
    rm(query1, query2, query3, osm_data1, osm_data2, osm_data3, points_data, parks_polygons)
  
  #00.2 Train stations from open map tiles ----
    stations <- 
      c("opmplc_essh_sj/OS OpenMap Local (ESRI Shape File) SJ/data/SJ_RailwayStation.shp",
        "opmplc_essh_sd/OS OpenMap Local (ESRI Shape File) SD/data/SD_RailwayStation.shp") %>%
      lapply(st_read, quiet = TRUE) %>% # Read each shapefile
      lapply(select, CLASSIFICA) %>%
      do.call(rbind, .)
    stations <- st_zm(stations, drop = TRUE, what = "ZM")
  #00.3 ORS Driving distance to liverpool central station----
    regacc_ors <- st_read("trains/distance_matrices/ors_carduration_livmanstation.gpkg") %>% st_transform(regacc_ors, crs = 27700) %>% select(min_to_livmain)
    
  #00.4 Dominant age in LSOA in 2011----
    age <- read.csv("construction_year/ctsop4-1-1993-2021__5_/CTSOP4_1_2011_03_31.csv") %>% 
      filter(band == "All") %>%
      rename(LSOA11CD = ecode) %>%
      #regrouping the construction periods
      mutate(
        across(6:31, as.numeric), 
        across(6:31, replace_na, 0), 
        p_pre1919 = (bp_pre_1900 + bp_1900_1918) / all_properties
        ) %>%
      filter(p_pre1919 <= 1) %>%
      select(LSOA11CD, p_pre1919)
    
  #00.5 Deprivation and income----
    depri <- read.csv("England/Drivers Liverpool/English_Welsh_Deprivation/File_1_ID_2015_Index_of_Multiple_Deprivation.csv") %>% 
      select(c(1,5,6)) %>% 
      rename(LSOA11CD = 1, Rank = 2, deprivation = 3) 
    
    depri$Rank <- as.numeric(gsub(",", ".", depri$Rank)) 
    
    income <- read.csv("England/Drivers Liverpool/English_Welsh_Deprivation/English Indices of Deprivation_Income_2010.csv") %>% 
      select(c(1,7)) %>% 
      rename(LSOA11CD = 1, income_rank = 2) 
    
  #00.6 Landuse ----
    clc <- st_read("landuse/corine/U2018_CLC2012_V2020_20u1.gpkg") #automatically chooses the first layer (the others are French islands)
    metro_84 <- st_transform(metro, st_crs(clc)) #set case area crs to clc crs
    clc_liverpool <- st_filter(clc, metro_84, .predicate = st_intersects) #reduce clc to liverpool region
    clc_liverpool <- st_transform(clc_liverpool, crs = 27700) #set to crs = 27700
    
    landuse <- clc_liverpool %>%
      select(Code_12) %>%
      mutate(Code_12 = as.numeric(Code_12)) %>%
      mutate(
        landuse_class = case_when(
          Code_12 == 121 ~ "industry",
          Code_12 == 123 ~ "port",
          Code_12 == 124 ~ "airport",
          Code_12 %in% c(131, 132) ~ "dump",
          Code_12 == 122 ~ "rail",
          Code_12 > 200 & Code_12 < 300 ~ "agriculture",
          Code_12 > 300 & Code_12 < 500 ~ "nature",
          Code_12 == 141 ~ "parks",
          Code_12 == 142 ~ "sports",
          Code_12 > 500 | Code_12 == 512 ~ "water"
        )
      ) %>%
      filter(!is.na(landuse_class)) %>%
      mutate(landuse_class = factor(landuse_class, levels = c(
        "industry", "port", "airport", "dump", "rail",
        "agriculture", "nature", "parks", "sports", "water"
      ))) %>%
      select(-Code_12) 
    
    rm(clc, metro_84, clc_liverpool)
    
  #00.7 Socio-economic neighborhood type----
    oa_c <- read_sf("England/OA Classification/2011_OAC.shp") %>%
      select(c(SPRGRP, GRP, SUBGRP)) 
      
    
#01 Amenities in 500m neighborhood ----
  
  #create 500m buffers around grid cells  
  dens_grid_buffer <- st_centroid(dens_grid) %>% select(grid_id) %>% st_buffer(dist = 500)
  
  #intersect buffers with amenities    
  intersections <- st_join(dens_grid_buffer, amenities) %>% filter(!is.na(osm_id))
  
  #count amenities
  count <- intersections %>% st_drop_geometry() %>% group_by(grid_id) %>% summarize(amenity_count = n(), .groups = "drop")
  
  #join amenity information with original grid
  dens_grid <- left_join(dens_grid, count, by = "grid_id")
  dens_grid$amenity_count[is.na(dens_grid$amenity_count)] <- 0  
  rm(dens_grid_buffer, intersections, count, amenities, bbox_coords, bbox_sfc, bbox_wgs84, metro)
  
#02 Euclidean distance to nearest park edge ----
  
  grid_centroids <- dens_grid %>% st_centroid() %>% select(grid_id)
  nearest_park_index <- st_nearest_feature(grid_centroids, parks) #identify nearest park
  nearest_parks <- parks[nearest_park_index, ]
  dist_to_park_edge <- st_distance(grid_centroids, nearest_parks, by_element = TRUE) #calculate distance
  dens_grid$m_to_park <- as.numeric(dist_to_park_edge)
  
  rm(nearest_park_index, nearest_parks, dist_to_park_edge)

#03 Distance to nearest train station ----
  
  nearest_idx <- st_nn(grid_centroids, stations, k = 1, returnDist = TRUE) #identify nearest station to each grid centroid
  nearest_distances <- sapply(nearest_idx$dist, function(d) d) # nearest_idx is a list of indices and distances
  dens_grid$m_to_train <- nearest_distances # Add results to the grid
  rm(nearest_idx, nearest_distances)
  
#04 Driving distance to Liverpool Central Station ----
  dens_grid <- st_join(dens_grid, regacc_ors, join = st_nearest_feature)


#05 Landuse ----
  #Intersect landuse with grid and calculate share of each type----
  intersection <- st_intersection(dens_grid, landuse)
  intersection$area <- st_area(intersection)
  landuse_summary <- intersection %>%
    st_drop_geometry() %>%
    group_by(grid_id, landuse_class) %>%  
    summarise(share = as.numeric(sum(area))/10000, .groups = "drop")
  
  #Transform ----
  landuse_wide <- landuse_summary %>%
    pivot_wider(
      names_from = landuse_class,
      values_from = share,
      values_fill = 0
    )
  
  dens_grid <- left_join(dens_grid, landuse_wide, by = "grid_id")
  rm(intersection, landuse_summary, landuse_wide)
  
#06 Neighborhood density ----
  #partition into natural breaks LD-MD-HD and NB based on address count 2011----
  filtered_data <- dens_grid %>% filter(addresses_2013 > 0) 
  #breaks <- classInt::classIntervals(filtered_data$addresses_2013, n = 3, style = "jenks")[["brks"]][-1]
  #takes too long, 3 groups in qgis returned high category with only 10 observations, so I do 4 groups and take the highest two together
  breaks <- c(19,57)
  
  dens_grid <- dens_grid %>%
    mutate(dens_group11 = case_when(
      addresses_2013 == 0 | is.na(addresses_2013) ~ "NB",
      addresses_2013 <= breaks[1] ~ "LD",
      addresses_2013 <= breaks[2] ~ "MD",
      TRUE ~ "HD"
    ))
  
  #240m buffer around each cell to search neighborhood 5x5 cell like mustafa 2018----
  buffer_df <- st_buffer(grid_centroids, dist = 240) 
  dens_groups <- dens_grid %>% select(c(dens_group11, grid_id)) %>% rename(nb_id = grid_id)
  nb_counts <- st_join(buffer_df, dens_groups, join = st_intersects)    
  nb_counts <- nb_counts %>% filter(grid_id != nb_id) %>% select(-nb_id) #take out the one in the middle
  
  #count cells of each class in neighborhood----
  nb_counts <- nb_counts %>%
    st_drop_geometry() %>%
    group_by(grid_id) %>%
    summarize(
      nb11_LDcount = sum(dens_group11 == "LD"),
      nb11_MDcount = sum(dens_group11 == "MD"),
      nb11_HDcount = sum(dens_group11 == "HD"), 
      nb11_NBcount = 24 - nb11_LDcount - nb11_MDcount - nb11_HDcount #takes into account that the grid only contains built up area 
    ) 
  
  dens_grid <- left_join(dens_grid, nb_counts, by = "grid_id")
  rm(buffer_df, dens_groups, filtered_data, nb_counts)
  
#07 Share SFH in 3x3 cell neighborhood 2013 ----
  hunits <- read_sf("C:/Users/Vera/Documents/SUBDENSE/Projects/Liverpool_Dembski/R Output/classified_addresses.gpkg") %>%
    filter(hunits_2013 > 0) %>%
    select(c(sfh_2013, hunits_2013))
  
  buffer_df <- st_buffer(dens_grid, dist = 145) %>% select(grid_id) %>% rename(buffer_id = grid_id)
  sfh_intersect <- st_join(buffer_df, hunits, join = st_intersects) %>% st_drop_geometry()
  sfh_counts <- sfh_intersect %>%
    group_by(buffer_id) %>%
    summarize(sfh_count = sum(sfh_2013, na.rm = TRUE),
              hunit_count = sum(hunits_2013, na.rm = TRUE),
              sfh_share = sfh_count/hunit_count) %>%
    mutate(sfh_share = if_else(is.nan(sfh_share), NA_real_, sfh_share)) %>%
    select(c(buffer_id, sfh_share)) %>%
    rename(grid_id = buffer_id)
  dens_grid <- left_join(dens_grid, sfh_counts, by = "grid_id")
  rm(hunits_2013, buffer_df, sfh_counts, sfh_intersect)
  
#08 Building age ----
  #ADD LSOA CODE TO DENS GRID
  grid_lsoa <- st_join(grid_centroids, lsoa) %>% st_drop_geometry()
  dens_grid <- left_join(dens_grid, grid_lsoa, by = "grid_id")
  
  #JOIN BUILDING AGE USING LSOA CODES
  dens_grid <- left_join(dens_grid, age, by = "LSOA11CD")
  
#09 Deprivation and Income level ----
  dens_grid <- left_join(dens_grid, depri, by = "LSOA11CD")
  dens_grid <- left_join(dens_grid, income, by = "LSOA11CD")  
  
#10 OA Classification ----
  centroids_joined <- st_join(dens_grid %>% st_centroid() %>% select(grid_id), oa_c, join = st_within)
  dens_grid <- dens_grid %>%
    left_join(st_drop_geometry(centroids_joined),
              by = "grid_id")
  
#11 Mark grid cells in built-up area ----
  dens_grid <- dens_grid %>% mutate(builtup2011 = as.integer(lengths(st_intersects(., builtup)) > 0))
  
#12 Export ----
  st_write(dens_grid, "C:/Users/Vera/Documents/SUBDENSE/Projects/Liverpool_Dembski/R Output/grid_full.gpkg")


#00 code to overwrite grid_full with new dependent variables in case i changed things in 01_Dependent Variable.R----
  setwd("C:/Users/Vera/Documents/SUBDENSE")
  grid_full <- st_read("Projects/Liverpool_Dembski/R Output/grid_full.gpkg")
  dens_grid <- read_sf("Projects/Liverpool_Dembski/R Output/grid_depvar.gpkg")
  
  grid_full <- grid_full %>% select(-c()) %>% st_drop_geometry
  dens_grid <- left_join(dens_grid, grid_full, by = "grid_id")
  st_write(dens_grid, "C:/Users/Vera/Documents/SUBDENSE/Projects/Liverpool_Dembski/R Output/grid_full.gpkg", append = FALSE)
