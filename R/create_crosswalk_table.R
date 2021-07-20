library(sf)
library(rjson)
library(dplyr)

# Set paths
raster_dir <- '/home/atrusty/NewForests/Projects/Dendro/dendroBasemap/'
data_dir <- '/home/atrusty/github/ftype-to-assessment/data/'

# Read in supersection shapefile
supersect_sf <- st_read(paste0(data_dir, 'CAR_supersections/CAR_supersections_coded.shp'))
supersect_sf$ss_code <- as.character(supersect_sf$ss_code)

# Read in forest type to common name and format as data table
fortypcmn_json <- fromJSON(file = paste0(data_dir, 'fia_fortyp_to_common_name.json'))
fortypcmn_dt <- fortypcmn_json %>% data.table()
names(fortypcmn_dt) <- as.character("ftype_name")
fortypcmn_dt$ftype_code <- lapply(seq_along(fortypcmn_json), function(i) names(fortypcmn_json)[[i]]) %>% unlist()

# Read in species-forest type codes and format as a data table
fortypcds_json <- fromJSON(file = paste0(data_dir, "arb_fortypcds.json")) # 348
fortypcds_dt <- fortypcds_json %>% data.table()
names(fortypcds_dt) <- as.character("ftype_list")
fortypcds_dt$aa_code <- lapply(seq_along(fortypcds_json), function(i) names(fortypcds_json)[[i]]) %>% unlist()

# Read in assessment area data 
## NOTE: We need to link our forest type data to an aa_code, then we can join to this table to get a common practice value
aa_dt <- fread(paste0(data_dir,'2015_assessment_area_data_file.csv')) %>%
  .[, c("supersection", "assessment_area", "aa_code", "ss_code", "species", "site_class", "common_practice")]

# Update assessment area name
aa_dt$assessment_area <- lapply(aa_dt$assessment_area, function(x){
  ss_name <- aa_dt[assessment_area == x,]$supersection[[1]]
  aa_name <- aa_dt[assessment_area == x,]$assessment_area[[1]]
  ss_words <- strsplit(ss_name, " ") %>% unlist()
  aa_words <- strsplit(aa_name, " ") %>% unlist()

  aa_only <- aa_words[!(aa_words %in% ss_words)]
  paste0(aa_only, collapse = " ")
}) %>% unlist()

aa_uni_dt <- aa_dt[, list(
  ss_name= min(supersection),
  aa_name = min(assessment_area),
  species = unique(species),
  cp_lo = min(common_practice),
  cp_hi = max(common_practice), 
  ss_code = as.character(unique(ss_code)),
  aa_code = as.character(unique(aa_code))
), by=c("assessment_area", "species", "supersection")]

# WHAT ABOUT SITES WITH 'site_class'=='all'???
sc_all <- c("155", "156", "157", "158", "159", "160", "161")
aa_uni_dt <- aa_uni_dt[!(aa_uni_dt$aa_code %in% sc_all),]

# Join the forest type list to assessment data table
aa_ftype_dt <- aa_uni_dt[fortypcds_dt, on = 'aa_code', c("ss_name", "ss_code", "ftype_list", "aa_name", "species", "aa_code", "cp_lo", "cp_hi")]

# Break out the ftypes into individual rowsc
aa_ftype_expanded_dt <- lapply(aa_ftype_dt$aa_code, function(x)  {
  
  dt <- aa_ftype_dt[aa_ftype_dt$aa_code == x, ]
  
  dt_ftype_list <- dt$ftype_list[[1]]

  dt_expanded <- lapply(dt_ftype_list, function(y){
    dt_exp_y <- dt
    dt_exp_y$ftype_code <- as.character(y)
    dt_exp_y
  }) %>% rbindlist()

  dt_expanded
}) %>% rbindlist()

# Join the forest type common name to the data table
aa_ftype_expanded_dt <- fortypcmn_dt[aa_ftype_expanded_dt, on='ftype_code']
aa_ftype_expanded_dt$ftype_name <- as.character(aa_ftype_expanded_dt$ftype_name)

# Trim columns
cols2keep <- c("ftype_code", "ftype_name", "aa_code", "aa_name", "ss_code", "ss_name", "cp_lo", "cp_hi", "species")
aa_crosswalk_dt <- aa_ftype_expanded_dt[,cols2keep, with=FALSE]

# Write out a copy of the forest type to assessment area crosswalk
fwrite(aa_crosswalk_dt, paste0(data_dir, "ftype_to_assessment_crosswalk.csv"), overwrite=T)
