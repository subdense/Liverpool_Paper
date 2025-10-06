#01 SORTING NEW ADDRESSES INTO DENSIFICATION TYPES, AGGREGATE TO HECTARE LEVEL

library(sf)
library(dplyr)
library(dbscan)


#00 Read Data----
  
  setwd("C:/Users/Vera/Documents/SUBDENSE/Data")  
  
  #boundaries: built-up area 2011 and liverpool case boundary
  builtup <- read_sf("England/Boundaries/Built_up_Areas_Dec_2011_Boundaries_V2_2022_6094869787211526009.gpkg") %>% select(BUA11CD) #built-up area 2011
  metro <- read_sf("boundaries/liverpool_metropolitan_dissolved.gpkg")
  
  #addresses: joining addresses with classification file
  addb_blpu <- st_read("England/AddressBase/0040176195-6425786-1/6425786.gpkg/6425786.gpkg", layer = "blpu")
  addb_clas <- st_read("England/AddressBase/0040176195-6425786-1/6425786.gpkg/6425786.gpkg", layer = "classification")
  addresses <- addb_blpu %>% 
    select(uprn, start_date, end_date) %>% 
    left_join(addb_clas %>% 
                transmute(uprn, class_key, classification_code, 
                          start_date, 
                          end_date))
  rm(addb_blpu, addb_clas)
  
  #building footprints from 2013 that exist in 2023 for subdivisions
  stable_bld <- st_read("C:/Users/Vera/Documents/SUBDENSE/Projects/Liverpool_Dembski/Input data for analysis/liv_bldg_densetype.gpkg") %>%
    filter(denstype == 'stable')
  
  #create or read building footprint polygons for 2023
  #library(tidyverse)
  #library(vroom)
  #footprints_2023 <-
  #  list.files("England/OS MasterMap/Liverpool 2023", full.names = TRUE, recursive = TRUE) %>% 
  #  str_subset("gpkg$") %>% # selects all strings ending with gpkg
  #  map_dfr(read_sf, layer = "Topographicarea") %>% 
  #  filter(theme == "Buildings") %>%
  #  mutate(fid_os = fid) %>% select(-fid) %>%
  #  st_as_sf() %>%
  #  distinct(fid_os, .keep_all = TRUE) #at the tile intersections, buildings are double
  footprints_2023 <- st_read("England/OS MasterMap/Liverpool 2023/footprints_2023.gpkg")
  
#01 Filter to addresses in case area ----
  
  addresses <- addresses[st_within(addresses, metro, sparse = FALSE), ]

#02 Divide into new and existing addresses, distinguish hmo from sfh and mfh (05.2013 - 05.2023) ----
  addresses <- addresses %>%
    mutate(#new flats
           flat_new = ifelse(start_date  >= '2013-05-01' & start_date < '2023-05-01' & (end_date >= '2023-05-01' | is.na(end_date))
                             & classification_code == 'RD06', 1, 0), #new flats
           #new single family
           sfh_new = ifelse(start_date  >= '2013-05-01' & start_date < '2023-05-01' & (end_date >= '2023-05-01' | is.na(end_date))
                            & (classification_code == 'RD02' | classification_code == 'RD03' | classification_code == 'RD04'), 1, 0), 
           #new hmo 
           hmo_new = ifelse(start_date >= '2013-05-01' & start_date < '2023-05-01' & (end_date >= '2023-05-01' | is.na(end_date))
                            & (classification_code == 'RH02'),1 ,0), 
           #all new housing units since 2013 - including hmo's
           hunits_new = ifelse(start_date >= '2013-05-01' & start_date < '2023-05-01'  & (end_date >= '2023-05-01' | is.na(end_date))
                               & (classification_code == 'RD01' | classification_code == 'RD02'| classification_code == 'RD03' | classification_code == 'RD04' | classification_code == 'RD06' | 
                                    classification_code == 'RH01' | classification_code == 'RH02'| classification_code == 'RH03' | classification_code == 'X'), 1, 0), 
           
           #housing units that existed in 2013
           hunits_2013 = ifelse(start_date < '2013-05-01' & (end_date >= '2013-05-01' | is.na(end_date))
                                & (classification_code == 'RD01' | classification_code == 'RD02' | classification_code == 'RD03' | classification_code == 'RD04' | classification_code == 'RD06' | classification_code == 'RH01' | classification_code == 'RH03' | classification_code == 'X'), 1, 0),
           #housing units that exist in 2023
           hunits_2023 = ifelse(start_date < '2023-05-01' & (end_date >= '2023-05-01' | is.na(end_date))
                                & (classification_code == 'RD01' | classification_code == 'RD02' | classification_code == 'RD03' | classification_code == 'RD04' | classification_code == 'RD06' | classification_code == 'RH01' | classification_code == 'RH03' | classification_code == 'X'), 1, 0), 
           #sfh that existed in 2013
           sfh_2013 = ifelse(start_date < '2013-05-01' & (end_date >= '2013-05-01' | is.na(end_date))
                             & (classification_code == 'RD02' | classification_code == 'RD03' | classification_code == 'RD04'), 1, 0),
           #all addresses that existed in 2013 (including non-residential)
           all_2013 = ifelse(start_date < '2013-05-01' & (end_date >= '2013-05-01' | is.na(end_date)), 1, 0)
           )
  
#02 Group new addresses in projects ----
  #we only group new addresses
  addresses_new <- addresses %>% filter(hunits_new == 1) %>% select(uprn)
  
  #we use dbscan to cluster: min size 5, max distance 50m
  coords <- st_coordinates(addresses_new)
  db <- dbscan(coords, eps = 50, minPts = 5) #cluster points
  
  #we attach cluster id and cluster size to the original df
  addresses_new <- addresses_new %>% mutate(cluster_id = db$cluster) #attach cluster id back to original df
  cluster_sizes <- addresses_new %>% group_by(cluster_id) %>% summarise(group_size = n(), .groups = "drop") %>% st_drop_geometry()
  addresses_new <- addresses_new %>% left_join(cluster_sizes, by = "cluster_id") #attach cluster size
  addresses <- addresses %>% left_join(addresses_new %>% st_drop_geometry(), by = "uprn") #join using individual address id
  rm(coords, db, cluster_sizes) #clean up
  #individual new addresses have cluster group 0 and size 3422, existing addresses have cluster group and size = NA
  
#03 Take out projects that are (partially) outside built-up area ----
  
  #intersect built up area and new addresses
  intersects <- st_join(addresses_new, builtup)
  intersects <- intersects %>% mutate(builtup_flag = ifelse(!is.na(BUA11CD), 1, 0)) #we mark points in builtup area
  intersects_clusters <- intersects %>% filter(cluster_id > 0) #we look only at addresses in clusters for now
  
  #summarize share of cluster in builtup area and mark as densification cluster if >80% of points are in builtup area
  cluster_builtup_stats <- intersects_clusters %>%
    st_drop_geometry() %>%
    group_by(cluster_id) %>%
    summarise(
      n_total = n(),
      n_builtup = sum(builtup_flag, na.rm = TRUE),
      builtup_ratio = n_builtup / n_total,
      builtup_2011 = as.integer(builtup_ratio >= 0.8),
      .groups = "drop"
    )
  
  #join information on densification clusters with original dataset
  addresses <- addresses %>% 
    left_join(cluster_builtup_stats %>% select(cluster_id, builtup_2011), by = "cluster_id")
  
  intersects_individual <- intersects %>% 
    filter(cluster_id == 0) %>% 
    select(uprn, builtup_flag) %>% 
    rename(builtup_2011 = builtup_flag) %>% 
    st_drop_geometry()
  
  addresses <-addresses %>% 
    left_join(intersects_individual, by = "uprn") %>%
    mutate(builtup_2011 = coalesce(builtup_2011.x, builtup_2011.y)) %>%
    select(-builtup_2011.x, -builtup_2011.y) 
  
  rm(intersects_clusters, intersects_individual, addresses_new)
  
#04 Mark new flats, sfh, hmos etc if they are densification ----
  addresses <- addresses %>%
    mutate(flat_dens = ifelse(flat_new == 1 & builtup_2011 == 1, 1, 0), 
           sfh_dens = ifelse(sfh_new == 1 & builtup_2011 == 1, 1, 0), 
           hunits_dens = ifelse(hunits_new == 1 & builtup_2011 == 1, 1, 0), 
           hmo_dens = ifelse(hmo_new == 1 & builtup_2011 == 1, 1, 0), 
           hunits_2013_bua = ifelse(hunits_2013 == 1 & builtup_2011 == 1, 1, 0)) #yes, not all housing units of 2013 are in the builtup area of 2011
  
#05 Large and small projects ----
  addresses <- mutate(addresses, large_project = ifelse(group_size > 9 & cluster_id != 0, 1, 0))
  
#06 Subdivisions ----
  #addresses <- read_sf("C:/Users/Vera/Documents/SUBDENSE/Projects/Liverpool_Dembski/R Output/classified_addresses.gpkg")

  #mark new addresses that overlap with 2013 buildings that exist still in 2023 as subdivision == 1
  addresses <- addresses %>%
    mutate(subdivision = ifelse(hunits_dens == 1 & lengths(st_intersects(., stable_bld)) > 0, 1, 0))
  
  
#07 Office to rental conversions ----
  #import LUCS data where J = Offices and K = Retail
  lucs <- st_read("England/LUCS/lucs_liv.gpkg") %>% 
    st_drop_geometry() %>%
    select(c(fid_os, V_FROM_CODE)) %>%
    filter(V_FROM_CODE == "J" | V_FROM_CODE == "K") %>% #filter to conversions
    mutate(
      office_retail = if_else(V_FROM_CODE == 'J' | V_FROM_CODE == 'K', 1, 0)
    ) %>%
    select(-V_FROM_CODE)
  
  #join addresses with building footprint id
  addresses <- st_join(addresses, footprints_2023[, "fid_os"], join = st_intersects, left = TRUE)
  addresses <- distinct(addresses, uprn, .keep_all = TRUE) #some addresses are at the intersection of 2 buildings
  
  #join addresses with conversion info through building footprint
  addresses <- left_join(addresses, lucs, by = "fid_os", relationship = "many-to-many")
  addresses <- distinct(addresses, uprn, .keep_all = TRUE) #some addresses are at the intersection of 2 buildings
  
  rm(lucs, footprints_2023)
  
#08 Is the project dominated by mfh or sfh ----
  projects <- addresses %>% 
    st_drop_geometry() %>%
    filter(group_size > 0 & group_size < 3422) %>% #the 3422 individual projects are all in one group
    group_by(cluster_id) %>%
    summarize(flats = sum(flat_dens == 1),
              sfh = sum(sfh_dens == 1)) %>%
    mutate(flat_dominant = ifelse(flats > sfh, 1, 0),
           sfh_dominant = ifelse(sfh > flats, 1, 0)) %>%
    select(-flats, -sfh)
  
  addresses <- left_join(addresses, projects, by = "cluster_id")
  rm(projects)
  
  #handle individual new buildings 
  addresses <- addresses %>%
    mutate(
      #individuals are cluster id 0 with group size 3422
      sfh_dominant = ifelse(cluster_id == 0 & sfh_dens == 1, 1, sfh_dominant),
      flat_dominant = ifelse(cluster_id == 0 & flat_dens == 1, 1, flat_dominant)
    )
  
  #Export addresses----
  addresses[is.na(addresses)] <- 0
  st_write(addresses, "C:/Users/Vera/Documents/SUBDENSE/Projects/Liverpool_Dembski/R Output/classified_addresses.gpkg")
  
#09 make grid and join with grid----
  #addresses <- read_sf("C:/Users/Vera/Documents/SUBDENSE/Projects/Liverpool_Dembski/R Output/classified_addresses.gpkg")
  #Create 100m grid using case area polygon
  grid <- st_make_grid(metro, cellsize = 100) #creating the grid with bounding box of case area
  grid <- st_sf(geometry = grid) #convert to sf object
  grid <- grid[st_within(grid, metro, sparse = FALSE), ] #keep only cells that intersect with case area
  grid <- grid %>% mutate(grid_id = row_number())
  
  rm(metro)
  
  #Join address data with grid
  intersections <- st_join(grid, addresses) %>% st_drop_geometry()
  count <- intersections %>% group_by(grid_id) %>%
    summarize(
      #dichotomous variables
      hmo = as.integer(any(hmo_dens == 1)), #returns 1 if there are any hmo_dens observations in the grid cell
      office_rental = as.integer(any(office_retail == 1)), #same for office/retail to rental conversions
      large_mfh = as.integer(any(hmo_dens != 1 & office_retail != 1 & flat_dominant == 1 & large_project == 1)), 
      small_mfh = as.integer(any(hmo_dens != 1 & office_retail != 1 & flat_dominant == 1 & large_project == 0)), 
      large_sfh = as.integer(any(hmo_dens != 1 & office_retail != 1 & sfh_dominant == 1 & large_project == 1)), 
      small_sfh = as.integer(any(hmo_dens != 1 & office_retail != 1 & sfh_dominant == 1 & large_project == 0)), 
      subdivision = as.integer(any(subdivision == 1)),
      
      #unit counts
      hmo_ct = sum(hmo_dens == 1, na.rm = TRUE), 
      office_rental_ct = sum(office_retail == 1, na.rm = TRUE), #same for office/retail to rental conversions
      large_mfh_ct = sum(hmo_dens != 1 & office_retail != 1 & flat_dominant == 1 & large_project == 1, na.rm = TRUE), 
      small_mfh_ct = sum(hmo_dens != 1 & office_retail != 1 & flat_dominant == 1 & large_project == 0, na.rm = TRUE), 
      large_sfh_ct = sum(hmo_dens != 1 & office_retail != 1 & sfh_dominant == 1 & large_project == 1, na.rm = TRUE), 
      small_sfh_ct = sum(hmo_dens != 1 & office_retail != 1 & sfh_dominant == 1 & large_project == 0, na.rm = TRUE), 
      subdivision_ct = sum(subdivision == 1, na.rm = TRUE),
      
      addresses_2013 = sum(all_2013)
    )
  
  dens_grid <- count
  dens_grid <- left_join(dens_grid, grid, by = "grid_id") #re-attach geometry
  dens_grid <- st_as_sf(dens_grid, sf_column_name = "geometry")
  
  st_write(dens_grid, "C:/Users/Vera/Documents/SUBDENSE/Projects/Liverpool_Dembski/R Output/grid_depvar.gpkg")
  