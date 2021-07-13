#############################################
FIA_DIR <- '/home/atrusty/NewForests/Projects/Dendro/dendroBasemap/'
out_dir <- '/home/atrusty/NewForests/Projects/Dendro/dendroBasemap/process_data/'

#
r_code_dir <- '/home/atrusty/github/forest-offsets/R/'

#install.packages('rgeos')
library(rFIA)
library(rgdal)
library(rjson)
library(dplyr)

supersections <- readOGR(paste0(r_code_dir, '/CAR_Supersections/'), layer='CAR_Supersections')

assessment_areas <- fromJSON(file=paste0(r_code_dir, 'rfia_assessment_areas_subset.json'))

process_assessment_area <- function(assessment_area) {

  supersection <- subset(supersections, SSection %in% assessment_area$supersection_name)
  print(assessment_area$assessment_area_id)
  clipped_fia <- clipFIA(readFIA(states = assessment_area$postal_codes,
                                 dir = FIA_DIR, nCores = 12,
                                 tables = c('PLOT', 'TREE', 'COND', 'POP_PLOT_STRATUM_ASSGN','POP_ESTN_UNIT', 'POP_EVAL', 'POP_STRATUM', 'POP_EVAL_TYP', 'POP_EVAL_GRP')
                                ),
                         common=TRUE, matchEval = TRUE, mostRecent = FALSE, nCores=12)
  print('analysis')
  clipped_fia$COND <- clipped_fia$COND %>%
    mutate(site = case_when(is.na(SITECLCD) ~ NA_character_,
                          SITECLCD %in% 1:4 ~ 'high',
                          TRUE ~ 'low'))



  bio <- biomass(clipped_fia,
                 grpBy=c(site, FORTYPCD),
                 areaDomain = OWNGRPCD == 40 & FORTYPCD %in% assessment_area$fortypcds,
                 treeType='live',
                 method = 'TI',
                 variance=TRUE,
                 polys = supersection,
                 component = 'AG',
                 totals=TRUE,
                 nCores=12)

  bio_subset <- subset(bio, CARB_TOTAL > 0, na.rm=TRUE)
  fn <- paste(assessment_area$assessment_area_id, '.csv', sep='')
  write.csv(bio_subset, file=file.path(out_dir, fn))
  gc()
}


for (assessment_area in assessment_areas) {
  process_assessment_area(assessment_area)
}
