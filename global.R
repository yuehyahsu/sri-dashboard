# Setup and housekeeping

source("01_load_packages.R")
source("02_functions.R")
source("03_options.R")

data <- readRDS("master_sri_data.rds")

country_shp <- readRDS("country_shp.rds")
country_yearly_shp <- readRDS("country_yearly_shp.rds")