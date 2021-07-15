library(magrittr)
library(data.table)
library(sf)
library(raster)
library(fasterize)
library(rgdal)
library(rjson)

# Path Settings
ftype_dir <- '/home/atrusty/NewForests/Projects/Dendro/dendroBasemap/'
data_dir <- '/home/atrusty/github/ftype-to-assessment/data/'

ss_code_dt <- fread(paste0(data_dir,'2015_assessment_area_data_file.csv'), select = c("supersection", 'ss_code')) %>% 
  distinct() %>% 
  as.data.table()

# Read in forest type raster  
ftype_rast <- raster(paste0(ftype_dir, 'CONUS-forest_type-240m.tif'))

# Read in supersection shapefile
supersect_sf <- st_read(paste0(data_dir, 'CAR_supersections/CAR_Supersections.shp')) 

# Fix truncated supersection names in the shapefile
# need <- supersect_sf$SSection[!(supersect_sf$SSection %in% ss_code_dt$supersection)]
supersect_sf$SSection[supersect_sf$SSection == 'Laurentian Mixed Forest Western Superior & Lake'] <- "Laurentian Mixed Forest Western Superior & Lake Plains"
supersect_sf$SSection[supersect_sf$SSection == "MW Broadleaf Forest SC Great Lakes & Lake Whittles"] <- "MW Broadleaf Forest SC Great Lakes & Lake Whittlesey"
supersect_sf$SSection[supersect_sf$SSection == "Prairie Parkland Central Till Plains & Grand"] <- "Prairie Parkland Central Till Plains & Grand Prairies"
supersect_sf$SSection[supersect_sf$SSection == "Central Interior Broadleaf Forest Western Low"] <- "Central Interior Broadleaf Forest Western Low Plateau"
supersect_sf$SSection[supersect_sf$SSection == "Central Interior Broadleaf Forest Central Till"] <- "Central Interior Broadleaf Forest Central Till Plains"
supersect_sf$SSection[supersect_sf$SSection == "Central Interior Broadleaf Forest Eastern Low"] <- "Central Interior Broadleaf Forest Eastern Low Plateau"
supersect_sf$SSection[supersect_sf$SSection == "Eastern Broadleaf Forest Cumberland Plateau"] <- "Eastern Broadleaf Forest Cumberland Plateau & Valley"

supersect_sf <- supersect_sf %>% 
  left_join(ss_code_dt, by=c("SSection"="supersection")) %>% 
  st_transform(crs(ftype_rast))

# Write out a copy of the shapefile with the supersections coded
# st_write(supersect_sf, paste0(data_dir, 'CAR_supersections/CAR_supersections_coded.shp'))

# Convert the polygon to a raster using the forest type extent and spatial resolution
supersect_rast <- fasterize(supersect_sf, ftype_rast, field="ss_code")

# Write out the raster
writeRaster(supersect_rast, filename = paste0(ftype_dir, 'CONUS-supersections-240m.tif'), overwrite=T)
