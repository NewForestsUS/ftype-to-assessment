# Download FIA Data using rFIA API
library(rFIA)
library(rgdal)
library(rjson)
library(dplyr)

# Set Options
options(timeout=400)

# Set directories
fia_dir <- '/home/atrusty/NewForests/Projects/Dendro/data/'
out_dir <- '/home/atrusty/NewForests/Projects/Dendro/data/'

# All States
# read.csv()
# conus_states <- c()

# Set states to download
states <- c('KY', 'NC', 'TN', 'VA', 'WV', 'MD', 'NJ', 'PA')
fia_states <- getFIA(states = states, common=TRUE, dir = paste0(out_dir, 'FIA/'))

# Some states aren't working, try individually
pa <- getFIA(states = 'PA', dir = paste0(out_dir, 'FIA/'))

# All States
