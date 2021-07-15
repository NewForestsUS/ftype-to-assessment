library(magrittr)
library(data.table)
library(sf)
library(raster)
library(fasterize)
library(rgdal)
library(rjson)

# Path Settings
ftype_dir <- '/home/atrusty/NewForests/Projects/Dendro/dendroBasemap/'
rfia_dir <- '/home/atrusty/github/ftype-to-assessment/data/'

# Read in assessment area crosswalk
assessment_areas <- fromJSON(file=paste0(rfia_dir, 'assessment_area_forest_type_codes.json'))

# Read in forest type raster  
ftype_rast <- raster(paste0(ftype_dir, 'CONUS-forest_type-240m.tif'))

# Read in supersection shapefile
supersect_sf <- st_read(paste0(rfia_dir, 'CAR_Supersections/CAR_Supersections.shp'))

# Map the Supersection ID to each name in the supersection SF
supersect_dt <- lapply(assessment_areas, function(x){
  SSection <- x$supersection_name
  SSID <- x$supersection_id
  AAID <- x$assessment_area_id
  data.table(
    SSection=SSection,
    SSID=SSID,
    AAID=AAID
  )
}) %>% rbindlist() %>% as.data.table()

# Join supersection ID's to supersect SF
supersect_sf_id <- supersect_sf %>% left_join(supersect_dt, by="SSection") %>% st_transform(crs(ftype_rast))

# Convert the polygon to a raster using the forest type extent and spatial resolution
supersect_rast <- fasterize(supersect_sf_id, ftype_rast, field="SSID")

# Write out the raster
writeRaster(supersect_rast, filename = paste0(ftype_dir, 'ss_raster.tif'), overwrite=T)
