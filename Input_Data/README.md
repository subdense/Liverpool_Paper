# Input_Data

Raw inputs for the Liverpool densification analysis. None of this folder is tracked in git.

Scripts assume the working directory is the project root (set automatically when `Liverpool_Paper.Rproj` is opened in RStudio) and reference files in this folder using relative paths.

## Present

| file / folder | used by | source |
|---|---|---|
| `Built_up_Areas_Dec_2011_Boundaries_V2_2022_6094869787211526009.gpkg` | 01, 02 | ONS – 2011 Built-up Areas |
| `Liverpool Metropolitan Boundary/liverpool_metropolitan_dissolved.gpkg` | 01, 02, join_lucs_bldg | case-area boundary |
| `liverpool_metropolitan.gpkg` | – | unused by current scripts; non-dissolved variant |
| `liv_bldg_densetype.gpkg` | 01 | derived "stable" 2013↔2023 footprints; treated as input here |
| `LUCS/LUCS_2013-14_addresses.csv` … `LUCS_2021-22_addresses.csv` | join_lucs_bldg | DCLG Land-Use and Land-Use Change |
| `LUCS/lucs_liv.gpkg` | 01 | produced by `join_lucs_bldg.Rmd`; kept here so 01 can run without re-running the join |
| `LUCS/lucs_bri.gpkg` | – | Bristol equivalent, not used by Liverpool flow |
| `LUCS/lucs_eng_2013-22.gpkg` | – | unused by current scripts |
| `OA Classification/2011_OAC.shp` (+ .dbf/.shx/.prj/.sbn/.sbx) | 02 | ONS 2011 Output Area Classification |
| `CTSOP4_1_2011_03_31.csv` | 02 | HMRC Council Tax Statistics – building age 2011 |
| `File_1_ID_2015_Index_of_Multiple_Deprivation.csv` | 02 | IMD 2015 |
| `English Indices of Deprivation_Income_2010.csv` | 02 | IoD income 2010 |
| `ors_carduration_livmanstation.gpkg` | 02 | OpenRouteService car-duration matrix to Liverpool/Manchester (precomputed) |
| `liv_amenities_osm.gpkg` | – (currently) | OSM amenities snapshot; see note below |

## Missing (scripts reference but file is not here)

| expected path | used by | what it is |
|---|---|---|
| `Input_Data/AddressBase/6425786.gpkg` (layers `blpu`, `classification`) | 01 | OS AddressBase Premium for Liverpool City Region — licensed |
| `Input_Data/glcr-lsoa-2011.gpkg` | 02 | 2011 LSOA boundaries clipped to Liverpool City Region |
| `Input_Data/OS OpenMap Local/SJ_RailwayStation.shp` | 02 | OS OpenMap Local rail stations, tile SJ (+ .dbf/.shx/.prj) |
| `Input_Data/OS OpenMap Local/SD_RailwayStation.shp` | 02 | OS OpenMap Local rail stations, tile SD (+ .dbf/.shx/.prj) |
| `Input_Data/U2018_CLC2012_V2020_20u1.gpkg` | 02 | Corine Land Cover 2012 |

## Notes

- **OSM amenities.** Script 02 currently downloads amenities and parks live via the `osmdata` API (lines ~28–57). A snapshot is present here as `liv_amenities_osm.gpkg`. The script has not been modified to load from this file — to use the snapshot instead of the live download, the OSM block in 02 needs to be replaced with `st_read("Input_Data/liv_amenities_osm.gpkg")` plus matching extraction of the amenities/parks objects.
- **Outputs of upstream prep.** `lucs_liv.gpkg` (in `LUCS/`) and `liv_bldg_densetype.gpkg` are technically produced by separate prep steps (`join_lucs_bldg.Rmd` and an upstream building-classification step) but are stored here so that `01_Dependent Variable.R` can run without re-running them.
