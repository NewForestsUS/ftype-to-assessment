library(data.table)
library(sf)
library(rjson)
library(dplyr)
library(raster)
library(fasterize)
library(parallel)

# Set paths
raster_dir <- '/home/atrusty/Projects/Heatmap/SilviaTerra/dendroBasemap/'
data_dir <- '/home/atrusty/github/ftype-to-assessment/data/'
aa_dir <- '/home/atrusty/Projects/Heatmap/SilviaTerra/assessmentArea/'

# Read in forest type to assessment crosswalk
aa_ftype_dt <- fread(paste0(data_dir, "ftype_to_assessment_crosswalk.csv"))

# Read in supersection shapefile
supersect_sf <- st_read(paste0(data_dir, 'CAR_supersections/CAR_supersections_coded.shp'))
supersect_sf$ss_code <- as.character(supersect_sf$ss_code)

# Read in forest type and supersections raster
ss_raster <- raster(paste0(raster_dir, 'CONUS-supersections-240m.tif'))
ftype_raster <- raster(paste0(raster_dir, 'CONUS-forest_type-240m.tif'))

# Stack the rasters
conus_stack <- stack(ss_raster, ftype_raster)

# Create a lookup and reclassification matrix to go from forest type to assessment code
aa_lookup_dt <- aa_ftype_dt[,c("ftype_code", "ss_code", "aa_code")]

# Get a list of unique supersection values - NOTE - the shapefile does not have supersection code 62: Ouachita Mixed Forest
unique_ss <- aa_lookup_dt$ss_code[!aa_lookup_dt$ss_code==62] %>% unique %>% .[!is.na(.)]

# Set up a clusters to compute in parallel
x <- 31
cl <- makeCluster(detectCores())
aa_rast_list <- parLapply(cl, unique_ss, function(x, aa_lookup_dt, supersect_sf, ftype_raster){
  library(sf)
  library(magrittr)
  library(raster)
  library(data.table)
  
  # Grab codes
  ftype_to_aa_dt <- aa_lookup_dt[ss_code==x,]
  reclass_matrix <- ftype_to_aa_dt[,c("ftype_code", "aa_code")]
  
  # ftypes <- ftype_to_aa_dt$ftype_code %>% unique
  # aa_codes <- ftype_to_aa_dt$aa_code %>% unique
  
  # Read in supersection sf and mask rasters to the boundaries
  ss_subset_sf <- supersect_sf[supersect_sf$ss_code==x,] %>% st_transform(crs(ftype_raster))
  
  # ss_sub_rst <- crop(ss_raster, extent(ss_subset_sf))
  ftype_sub_rst <- crop(ftype_raster, extent(ss_subset_sf))
  # ss_masked <- mask(ss_sub_rst, ss_subset_sf)
  # names(ss_masked) <- "SS"
  ftype_masked <- mask(ftype_sub_rst, ss_subset_sf)
  names(ftype_masked) <- "FTYPE"
  
  # Reclassify forest type 
  aa_rast <- reclassify(ftype_masked, reclass_matrix)
  
  # Some combinations of ftype/SS are not in the crosswalk - nullify these
  values(aa_rast)[!(values(aa_rast) %in% ftype_to_aa_dt$aa_code)] = NA
  
  # Return the assessment area raster
  aa_rast
  
}, aa_lookup_dt, supersect_sf, ftype_raster)
stopCluster(cl)

# Define the mosaic function and mosaic the raster list
aa_rast_list$fun <- min
aa_mosaic <- do.call(mosaic, aa_rast_list)

# Write the raster to disk
writeRaster(aa_mosaic, paste0(aa_dir, "CONUS-assessment-areas-240m.tif"), overwrite=TRUE)

# Using the assessment area mosaic, reclassify it to common practice values
aa_to_cp_lo_rcl <- aa_ftype_dt[,c("aa_code", "cp_lo")]
aa_to_cp_hi_rcl <- aa_ftype_dt[,c("aa_code", "cp_hi")]

cp_lo_rast <- reclassify(aa_mosaic, aa_to_cp_lo_rcl)
cp_hi_rast <- reclassify(aa_mosaic, aa_to_cp_hi_rcl)

# Write each to disk
writeRaster(cp_lo_rast, paste0(aa_dir, "CONUS-common-practice-low-240m.tif"), overwrite=TRUE)
writeRaster(cp_hi_rast, paste0(aa_dir, "CONUS-common-practice-hi-240m.tif"), overwrite=TRUE)
