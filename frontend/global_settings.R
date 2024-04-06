# library('rgdal')
# library('spdplyr')
library('leaflet')
library('htmltools')
library('DT')
library('dplyr')
library('tidyr')
library('ggplot2')
library("htmltools")
library('shiny')
library('shinydashboard')
library('leaflet')
library('RColorBrewer')
library(sf)

############# MAP DATA ###############
SG_map = readRDS("frontend/data/SG_map.rds")
all_address = read.csv("frontend/data/hdb_block_details.csv") %>%
  rename(lng = long) %>%
  rename("Primary School" = "primary_schools_1km")
amenities = read.csv("frontend/data/lat_long_for_visualisation.csv")
hawker_centres = read.csv("frontend/data/hawker_centres_geocode.csv")
hospitals = read.csv("frontend/data/hospitals_geocode.csv")
mrt = read.csv("frontend/data/hospitals_geocode.csv")
pri_schs = read.csv("frontend/data/primary_schools_geocode.csv")
supermarkets = read.csv("frontend/data/supermarkets_geocode.csv")


############# PREDICTION DATA ###############
laty = readRDS("frontend/data/lat_long_for_prediction.Rds")
all_address_pred = readRDS("frontend/data/hdb_merged_no_transform.Rds")




