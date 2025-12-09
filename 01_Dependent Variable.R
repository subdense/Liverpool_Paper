#01 SORTING NEW ADDRESSES INTO DENSIFICATION TYPES, AGGREGATE TO HECTARE LEVEL

library(sf)
library(dplyr)
library(dbscan)
library(tidyr)

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
  
  #building footprints from 2013 that exist in 2022 for subdivisions
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
  
  #import LUCS data where J = Offices and K = Retail
  lucs <- st_read("England/LUCS/lucs_liv.gpkg") %>% 
    st_drop_geometry() %>%
    select(c(fid_os, V_FROM_CODE, LUCS_FROM_CODE, COMPLETIONS, CONVERSIONS_TORESIDENTIAL)) %>%
    filter(LUCS_FROM_CODE == "J" | LUCS_FROM_CODE == "K") %>% #filter to conversions
    mutate(office_retail = 1) %>%
    select(-V_FROM_CODE, -LUCS_FROM_CODE)
  
  #00 Read Data Denise----
  setwd("G:/ai_daten/P1047_SUBDENSE/liverpool_paper")
  
  #boundaries: built-up area 2011 and liverpool case boundary
  builtup <- read_sf("01_data_input/Built_up_Areas_Dec_2011_Boundaries_V2_2022_6094869787211526009.gpkg") %>% select(BUA11CD) #built-up area 2011
  metro <- read_sf("01_data_input/liverpool_metropolitan_dissolved.gpkg")
  
  #addresses: joining addresses with classification file
  addb_blpu <- st_read("G:/ai_daten/P1047_SUBDENSE/01_raw_data/UK/OS/AddressBase/AddressBase/0040176195-6425786-1/6425786.gpkg/6425786.gpkg", layer = "blpu")
  addb_clas <- st_read("G:/ai_daten/P1047_SUBDENSE/01_raw_data/UK/OS/AddressBase/AddressBase/0040176195-6425786-1/6425786.gpkg/6425786.gpkg", layer = "classification")
  addresses <- addb_blpu %>% 
    select(uprn, start_date, end_date) %>% 
    left_join(addb_clas %>% 
                transmute(uprn, class_key, classification_code, 
                          start_date, 
                          end_date))
  rm(addb_blpu, addb_clas)
  
  #building footprints from 2013 that exist in 2023 for subdivisions
  stable_bld <- st_read("G:/ai_daten/P1047_SUBDENSE/liverpool_paper/02_data_prep_output/liv_bldg_densetype.gpkg") %>%
    filter(denstype == 'stable')
  
  footprints_2023 <- st_read("G:/ai_daten/P1047_SUBDENSE/liverpool_paper/Projects/Liverpool_Dembski/OneDrive_2025-10-02/Input data for analysis/OS MasterMap/Liverpool City Region/2023/footprints_2023.gpkg")
  
  footprints_2023v2 <- st_read("G:/ai_daten/P1047_SUBDENSE/liverpool_paper/02_data_prep_output/liv_bldg_2023.gpkg")
  
  
#01 Filter to addresses in case area ----
  
  addresses <- addresses[st_within(addresses, metro, sparse = FALSE), ]

#02 Divide into new and existing addresses, distinguish hmo from sfh and mfh (05.2013 - 05.2022) ----
  addresses <- addresses %>%
    mutate(#new flats
           flat_new = ifelse(start_date  >= '2013-05-01' & start_date < '2022-05-01' & (end_date >= '2022-05-01' | is.na(end_date))
                             & classification_code == 'RD06', 1, 0), #new flats
           #new single family
           sfh_new = ifelse(start_date  >= '2013-05-01' & start_date < '2022-05-01' & (end_date >= '2022-05-01' | is.na(end_date))
                            & (classification_code == 'RD02' | classification_code == 'RD03' | classification_code == 'RD04'), 1, 0), 
           #new hmo 
           hmo_new = ifelse(start_date >= '2013-05-01' & start_date < '2022-05-01' & (end_date >= '2022-05-01' | is.na(end_date))
                            & (classification_code == 'RH02'),1 ,0), 
           #all new housing units since 2013 - including hmo's
           hunits_new = ifelse(flat_new == 1 | sfh_new == 1 | hmo_new == 1, 1, 0), 
           
           #housing units that existed in 2013 (this doesnt count RH02, HMO's for some reason but ok. and new housing has no RH01 and RH03)
           hunits_2013 = ifelse(start_date < '2013-05-01' & (end_date >= '2013-05-01' | is.na(end_date))
                                & (classification_code == 'RD02' | classification_code == 'RD03' | classification_code == 'RD04' | classification_code == 'RD06' | 
                                     classification_code == 'RH01' | classification_code == 'RH03' | classification_code == 'X'), 1, 0),
           #housing units that exist in 2022
           hunits_2022 = ifelse(start_date < '2022-05-01' & (end_date >= '2022-05-01' | is.na(end_date))
                                & (classification_code == 'RD02' | classification_code == 'RD03' | classification_code == 'RD04' | classification_code == 'RD06' | 
                                     classification_code == 'RH01' | classification_code == 'RH03' | classification_code == 'X'), 1, 0), 
           #sfh that existed in 2013
           sfh_2013 = ifelse(start_date < '2013-05-01' & (end_date >= '2013-05-01' | is.na(end_date))
                             & (classification_code == 'RD02' | classification_code == 'RD03' | classification_code == 'RD04'), 1, 0),
           #all addresses that existed in 2013 (including non-residential)
           all_2013 = ifelse(start_date < '2013-05-01' & (end_date >= '2013-05-01' | is.na(end_date)), 1, 0),
           
           #factor variables output and process. they get updated throughout and will only include addresses in builtup area. unlike the 1/0 variables, they are not mutually exclusive 
           output = case_when(
             sfh_new == 1 ~ "sfh",
             flat_new == 1 ~ "flat", 
             hmo_new == 1 ~ "hmo",
             TRUE ~ "non-residential"),
           process = "infill" #default. i take out expansion later and will mark subdivision and office rental in the next steps
           )

#03 Subdivisions ----
  #mark dens flats that overlap with 2013 buildings that exist still in 2023 as subdivision == 1
  addresses <- addresses %>%
    mutate(subdivision = ifelse(flat_new == 1 & lengths(st_intersects(., stable_bld)) > 0, 1, 0),
           process = ifelse(subdivision == 1, "subdivision", process), #update process variable
           flat_new = ifelse(subdivision == 1, 0, flat_new)) #a unit is either a flat or a subdivision, not both, but output will still show mfh

#04 Office to rental conversions ----
  #join addresses with building footprint id
  addresses <- st_join(addresses, footprints_2023[, "fid_os"], join = st_intersects, left = TRUE)
  addresses <- distinct(addresses, uprn, .keep_all = TRUE) #some addresses are at the intersection of 2 buildings
  
  #join addresses with conversion info through building footprint
  addresses <- left_join(addresses, lucs %>% select(c(fid_os, office_retail)), by = "fid_os", relationship = "many-to-many")
  addresses <- distinct(addresses, uprn, .keep_all = TRUE) #some addresses are at the intersection of 2 buildings
  
  #office rental conversions must also be a new housing unit between 2013 and 2022
  addresses <- addresses %>% mutate(office_retail = ifelse(hunits_new == 0, 0, office_retail))

  #a new unit that is office retail does not belong to sfh, flat, subdivision. but if it was a conversion to hmo, hmo trumps. 
  #however, the addresses are still marked as hmo, flat or sfh in output variable
  addresses <- addresses %>%
    mutate(
      office_retail = replace_na(office_retail, 0),
      process = ifelse(office_retail == 1, "office_retail", process), #update process, overwrites subdivisions
      flat_new = ifelse(office_retail == 1, 0, flat_new),
      office_retail = ifelse(hmo_new == 1, 0, office_retail),   #hmos that are also office retail are still hmos!
      sfh_new = ifelse(office_retail == 1, 0, sfh_new),   #sfh that are office retail are office retail
      subdivision = ifelse(office_retail == 1, 0, subdivision),   #subdivisions that are office retail are office retail

    )
  
  rm(lucs, footprints_2023)
  
#05 Group new flats and sfh in projects ----
  #we only group new sfh and flats that are not subdivisions or office rental conversions
  addresses_new <- addresses %>% filter(sfh_new == 1 | flat_new == 1) %>% select(uprn)
  
  #we use dbscan to cluster: min size 5, max distance 50m
  coords <- st_coordinates(addresses_new)
  db <- dbscan(coords, eps = 50, minPts = 5) #cluster points
  
  #we attach cluster id and cluster size to the original df
  addresses_new <- addresses_new %>% mutate(cluster_id = db$cluster) #attach cluster id back to original df
  cluster_sizes <- addresses_new %>% group_by(cluster_id) %>% summarise(group_size = n(), .groups = "drop") %>% st_drop_geometry()
  addresses_new <- addresses_new %>% left_join(cluster_sizes, by = "cluster_id") #attach cluster size
  addresses <- addresses %>% left_join(addresses_new %>% st_drop_geometry(), by = "uprn") #join using individual address id
  rm(coords, db, cluster_sizes) #clean up
  
  #individual new flats and sfh have cluster group 0, existing addresses and new other units have cluster group and size = NA
  #for both cases we set cluster group and cluster size to 0
  addresses <- addresses %>%
    mutate(
      cluster_id = replace_na(cluster_id, 0),
      group_size = ifelse(cluster_id == 0, 0, group_size),
      group_size = replace_na(group_size, 0)
    )
  
#06 Take out projects that are (partially) outside built-up area ----
  
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
  
  #all addresses outside the clusters also get a flag if they are in builtup area
  addresses <- addresses %>%
    mutate(
      builtup_2011 = if_else(
        group_size == 0 & lengths(st_intersects(., builtup)) > 0,
        1L,  # set to 1 if condition is met
        builtup_2011  # otherwise keep existing value
      )
    )
  
  rm(intersects_clusters, addresses_new)
  
#07 Mark new flats, sfh, hmos etc if they are densification ----
  #we also update the process and outcome variables and set them to NA if they are outside the builtup area
  #subdi and o-r not addressed separately, i assume if they were created in an existing house, they are densification
  addresses <- addresses %>%
    mutate(process = ifelse(is.na(builtup_2011), NA, process),
           output = ifelse(is.na(builtup_2011), NA, output),
           flat_dens = ifelse(flat_new == 1 & builtup_2011 == 1, 1, 0), 
           sfh_dens = ifelse(sfh_new == 1 & builtup_2011 == 1, 1, 0), 
           hunits_dens = ifelse(hunits_new == 1 & builtup_2011 == 1, 1, 0), 
           hmo_dens = ifelse(hmo_new == 1 & builtup_2011 == 1, 1, 0), 
           hunits_2013_bua = ifelse(hunits_2013 == 1 & builtup_2011 == 1, 1, 0)) #yes, not all housing units of 2013 are in the builtup area of 2011
  
#08 Flag large projects inside BUA ----
  addresses <- addresses %>%
    mutate(
      large_project = ifelse(group_size > 9 & cluster_id != 0 & builtup_2011 == 1, 1, 0))
  
  
#09 Is the project dominated by mfh or sfh ----
  projects <- addresses %>% st_drop_geometry() %>%
    filter(group_size > 0 & builtup_2011 == 1) %>% #projects in builtup area
    group_by(cluster_id) %>%
    summarize(flats = sum(flat_dens == 1),
              sfh = sum(sfh_dens == 1)) %>%
    mutate(flat_dominant = ifelse(flats > sfh, 1, 0),
           sfh_dominant = ifelse(sfh > flats, 1, 0)) %>%
    select(-flats, -sfh)
  
  addresses <- left_join(addresses, projects, by = "cluster_id")
  
  rm(projects)
  
  #Export addresses----
  addresses[is.na(addresses)] <- 0
  st_write(addresses, "C:/Users/Vera/Documents/SUBDENSE/Projects/Liverpool_Dembski/R Output/classified_addresses.gpkg")
  
#10 make grid and join with grid----
  #Create 100m grid using case area polygon
  grid <- st_make_grid(metro, cellsize = 100) #creating the grid with bounding box of case area
  grid <- st_sf(geometry = grid) #convert to sf object
  grid <- grid[st_within(grid, metro, sparse = FALSE), ] #keep only cells that intersect with case area
  grid <- grid %>% mutate(grid_id = row_number())
  
  rm(metro)
  
  #Join address data with grid
  intersections <- st_join(grid, addresses) %>% st_drop_geometry()
  count <- intersections %>% 
    rename(sd_unit = subdivision) %>%
    group_by(grid_id) %>%
    summarize(
      #dichotomous variables
      hmo = as.integer(any(hmo_dens == 1)), #returns 1 if there are any hmo_dens observations in the grid cell
      office_rental = as.integer(any(office_retail == 1)), #same for office/retail to rental conversions
      large_mfh = as.integer(any(flat_dominant == 1 & large_project == 1)), 
      large_sfh = as.integer(any(sfh_dominant == 1 & large_project == 1)), 
      small_mfh = as.integer(any(
        (flat_dominant == 1 & large_project == 0) | #small projects that are flat dominant
        (flat_dens == 1 & group_size == 0))), #or just individual flats
      small_sfh = as.integer(any(
        (sfh_dominant == 1 & large_project == 0) |
        (sfh_dens == 1 & group_size == 0))), 
      subdivision = as.integer(any(sd_unit == 1)),
      
      #unit counts
      hmo_ct = sum(hmo_dens == 1, na.rm = TRUE), 
      office_rental_ct = sum(office_retail == 1, na.rm = TRUE), #same for office/retail to rental conversions
      large_mfh_ct = sum(flat_dominant == 1 & large_project == 1, na.rm = TRUE), 
      large_sfh_ct = sum(sfh_dominant == 1 & large_project == 1, na.rm = TRUE), 
      small_mfh_ct = sum(
        (flat_dominant == 1 & large_project == 0) |
        (flat_dens == 1 & group_size == 0),
        na.rm = TRUE), 
      small_sfh_ct = sum(
        (sfh_dominant == 1 & large_project == 0) |
        (sfh_dens == 1 & group_size == 0),
        na.rm = TRUE), 
      subdivision_ct = sum(sd_unit == 1, na.rm = TRUE),
      
      addresses_2013 = sum(all_2013)
    )
  
  dens_grid <- count
  dens_grid <- left_join(dens_grid, grid, by = "grid_id") #re-attach geometry
  dens_grid <- st_as_sf(dens_grid, sf_column_name = "geometry")
  
  #for comparison, add the office retail conversion count from the lucs data? 
  lucs <- st_read("England/LUCS/lucs_liv.gpkg") %>% 
    select(c(V_FROM_CODE, LUCS_FROM_CODE, COMPLETIONS, CONVERSIONS_TORESIDENTIAL)) %>%
    filter(LUCS_FROM_CODE == "J" | LUCS_FROM_CODE == "K") %>% #filter to conversions
    mutate(count = COMPLETIONS + CONVERSIONS_TORESIDENTIAL) %>%
    select(count)
  
  dens_grid_joined <- aggregate(lucs["count"], dens_grid, sum)
  
  dens_grid_joined <- dens_grid %>%
    mutate(lucs_count = dens_grid_joined$count)
  
  dens_grid <- dens_grid_joined
  
  #export
  st_write(dens_grid, "C:/Users/Vera/Documents/SUBDENSE/Projects/Liverpool_Dembski/R Output/grid_depvar.gpkg")
  
