# load the required packages
library(tidyverse)
library(plotly)
library(lubridate)
library(caret)
library(httr) 
library(jsonlite)

# load our cleaned dataset
hdb_resale = read.csv("backend/processed_data/hdb_resale_prices.csv") %>% select(-1)

# Exploratory Data Analysis