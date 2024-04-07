library(readxl)
library(tidyverse)
library(jsonlite)
library(httr)

# Getting the latitude and longitude of each HDB address
geo_code <- function(address) {
  lat <- 1.2996418818103135
  lon <- 103.80010216304007
  x <- 24303.3101027
  y <- 31333.3389857
  postal <- "148812"
  street <- "300 TANGLIN HALT ROAD NEW TOWN CAREHUT SINGAPORE 148812"
  tryCatch({
    street <- address
    address = str_replace_all(address," ","%20")
    base_url <- "https://www.onemap.gov.sg/api/common/elastic/search?searchVal="
    endpoint <- "&returnGeom=Y&getAddrDetails=Y"
    resource_url <- paste0(base_url,address,endpoint)
    res <- GET(resource_url,
               add_headers(Authorization = paste("Bearer", Sys.getenv("ONEMAP_KEY"))),
               accept("application/json"))
    res_list <- content(res, type = "text") %>% fromJSON(flatten = TRUE)
    df <- as_tibble(res_list$results)[1,]
    lat <- as.numeric(df["LATITUDE"])
    lon <- as.numeric(df["LONGITUDE"])
    x <- as.numeric(df["X"])
    y <- as.numeric(df["Y"])
    postal <- as.character(df["POSTAL"])
  }, error = function(e) {
    lat <- 1.2996418818103135
    lon <- 103.80010216304007
    x <- 24303.3101027
    y <- 31333.3389857
    postal <- "148812"
    street <- "300 TANGLIN HALT ROAD NEW TOWN CAREHUT SINGAPORE 148812"
  })
  return(c(lat, lon, postal, x,  y, street))
}

# Creating Geo-coordinates of nearby amenities for each HDB block
get_details <- function(unique_addresses) {
  details <- data.frame(
    lat = numeric(),
    long = numeric(),
    postal = character(),
    x = numeric(),
    y = numeric(),
    street = character(),
    stringsAsFactors = FALSE
  )

  for (address in unique_addresses) {
    coords <- geo_code(address)

    # Check if coords is NULL or has insufficient length
    if (is.null(coords) || length(coords) < 6) {
      warning(paste("Skipping address due to insufficient data:", address))
      next  # Skip to the next iteration
    }

    # Convert data types explicitly
    row <- data.frame(
      lat = as.numeric(coords[1]),
      long = as.numeric(coords[2]),
      postal = as.character(coords[3]),
      x = as.numeric(coords[4]),
      y = as.numeric(coords[5]),
      street = as.character(coords[6]),
      stringsAsFactors = FALSE
    )
    details <- bind_rows(details, row)
  }
  return(details)
}

# HDB Geo Coordinates
data_tidy = readRDS("backend/processed_data/hdb_prices_with_address.Rds") 
lat_long_postal_xy = get_details(unique(data_tidy$address))
lat_long_postal_xy = lat_long_postal_xy %>%
  filter(lat <= 1.299641 | lat >= 1.299642) %>% # Remove the default values 
  na.omit()

# Replace irregular postal code "data "NIL" with postal code in reality
lat_long_postal_xy[1280,]$postal = as.character("680216")
lat_long_postal_xy[7151,]$postal = as.character("680215")

# Saved to RDS to preserve column classes
# saveRDS(lat_long_postal_xy,"backend/processed_data/lat_long_postal_xy.Rds") 

##########################################################################################
# Creating Geo-coordinates of nearby amenities for each HDB block
# Getting the latitude and longitude of each unique address
geo_code_amenities <- function(address) {
  lat <- 1.2996418818103135
  lon <- 103.80010216304007
  x <- 24303.3101027
  y <- 31333.3389857
  postal <- "148812"
  street <- "300 TANGLIN HALT ROAD NEW TOWN CAREHUT SINGAPORE 148812"
  tryCatch({
    address = str_replace_all(address," ","%20")
    base_url <- "https://www.onemap.gov.sg/api/common/elastic/search?searchVal="
    endpoint <- "&returnGeom=Y&getAddrDetails=Y"
    resource_url <- paste0(base_url,address,endpoint)
    res <- GET(resource_url,
               add_headers(Authorization = paste("Bearer", Sys.getenv("ONEMAP_KEY"))),
               accept("application/json"))
    res_list <- content(res, type = "text") %>% fromJSON(flatten = TRUE)
    df <- as_tibble(res_list$results)[1,]
    lat <- as.numeric(df["LATITUDE"])
    lon <- as.numeric(df["LONGITUDE"])
    x <- as.numeric(df["X"])
    y <- as.numeric(df["Y"])
    postal <- as.character(df["POSTAL"])
    street <- as.character(df["ADDRESS"])
  }, error = function(e) {
    lat <- 1.2996418818103135
    lon <- 103.80010216304007
    x <- 24303.3101027
    y <- 31333.3389857
    postal <- "148812"
    street <- "300 TANGLIN HALT ROAD NEW TOWN CAREHUT SINGAPORE 148812"
  })
  return(c(lat, lon, postal, x,  y, street))
}
get_amenity_details <- function(unique_addresses) {
  details <- data.frame(
    lat = numeric(),
    long = numeric(),
    postal = character(),
    x = numeric(),
    y = numeric(),
    street = character(),
    stringsAsFactors = FALSE
  )

  for (address in unique_addresses) {
    coords <- geo_code_amenities(address)

    # Check if coords is NULL or has insufficient length
    if (is.null(coords) || length(coords) < 6) {
      warning(paste("Skipping address due to insufficient data:", address))
      next  # Skip to the next iteration
    }

    # Convert data types explicitly
    row <- data.frame(
      lat = as.numeric(coords[1]),
      long = as.numeric(coords[2]),
      postal = as.character(coords[3]),
      x = as.numeric(coords[4]),
      y = as.numeric(coords[5]),
      street = as.character(coords[6]),
      stringsAsFactors = FALSE
    )
    details <- bind_rows(details, row)
  }
  return(details)
}

# MRT stations Geo Coordinates
mrt = read_excel("backend/raw_data/Train Station Codes and Chinese Names.xls") 
mrt_geocode = get_amenity_details(mrt$stn_code)
mrt_geocode = mrt_geocode %>%
  filter(lat <= 1.299641 | lat >= 1.299642) %>% # Remove the default values 
  unique() %>% 
  na.omit()
# saveRDS(mrt_geocode,"backend/processed_data/mrt_geocode.Rds") 

# Supermarkets Geo Coordinates
supermarkets = read.csv("backend/raw_data/ListofSupermarketLicences.csv")
supermarkets_geocode = get_amenity_details(unique(supermarkets$postal_code))
supermarkets_geocode = supermarkets_geocode %>% 
  filter(lat <= 1.299641 | lat >= 1.299642) %>% # Remove the default values 
  na.omit()
# saveRDS(supermarkets_geocode,"backend/processed_data/supermarkets_geocode.Rds") 

# Hawker centers Geo Coordinates
hawker_centers = read.csv("backend/raw_data/ListofGovernmentMarketsHawkerCentres.csv")
hawkers <- hawker_centers %>% 
  mutate(location_of_centre = str_replace(location_of_centre, ", S\\(", "%")) %>%
  separate(location_of_centre, c("address", "postal"), sep = "%") %>%
  mutate(postal = str_replace(postal, "\\(", "")) %>%
  mutate(postal = str_replace(postal, "\\)", "")) %>% 
  separate_rows(postal, sep = "/")
hawker_centers_geocode = get_amenity_details(unique(hawkers$postal))
hawker_centers_geocode = hawker_centers_geocode %>% 
  filter(lat <= 1.299641 | lat >= 1.299642) %>% # Remove the default values 
  na.omit()
# saveRDS(hawker_centers_geocode,"backend/processed_data/hawker_centers_geocode.Rds") 

# Primary Schools Geo Coordinates
primary_schools = read.csv("backend/raw_data/primary_schools.csv")
primary_schools <- primary_schools %>% mutate(Address = str_replace(Address, ", S", "-")) %>%
  separate(Address, c("address", "postal"), sep = "-")
primary_schools_geocode = get_amenity_details(unique(primary_schools$postal))
primary_schools_geocode = primary_schools_geocode %>% 
  filter(lat <= 1.299641 | lat >= 1.299642) %>% # Remove the default values 
  na.omit()
# saveRDS(primary_schools_geocode,"backend/processed_data/primary_schools_geocode.Rds") 

# # Hospitals Geo Coordinates
# # Web data scraping of list of hospitals 
# library(rvest)
# 
# url <- "https://en.wikipedia.org/wiki/List_of_hospitals_in_Singapore"
# # Find all HTML tables on the page
# tables <- read_html(url) %>% html_elements("table")
# # Number of tables available
# length(tables)
# 
# #convert the second table to a tibble
# records1 = tables[[1]] %>% html_table()
# records2 = tables[[2]] %>% html_table()
# hospitals <- rbind(records1, records2) %>%
#   select(-Opened, -Ownership, -Beds, -Staff) %>%
#   mutate(postal = c(529889, 229899, 544886, 169608, 768828, 308433, 159964, 119074, 609606, 574623,  289891,217562, 258500, 228510, 329563, 427990, 188770, 307677, 547530, 168582, 544835, 768024, 609606, 569766, 329562, 529895, 659674))
# # write.csv(hospitals, "backend/raw_data/hospitals.csv")

hospitals = read.csv("backend/raw_data/hospitals.csv")
hospitals_geocode <- get_amenity_details(unique(hospitals$postal)) %>% 
  filter(lat <= 1.299641 | lat >= 1.299642) %>% # Remove the default values 
  na.omit()
# saveRDS(hospitals_geocode,"backend/processed_data/hospitals_geocode.Rds") 

##########################################################################################
# Getting distance to nearest amenities (Creating the respective tables)
library(geosphere)

find_nearest <- function(house, amenity, radius=1) {
  results <- data.frame(Address = character(), Nearest_Amenity = character(), Distance = numeric(), Amenity_Count = numeric(), stringsAsFactors = FALSE)
  for (index in 1:nrow(house)) {
    flat <- house[index, "street"] # address of hdb resale
    flat_loc <- c(house[index, 'long'], house[index, 'lat']) # long and lat of hdb resale
    nearest_amenity <- c("", 100, 0)
    for (ind in 1:nrow(amenity)) {
      eachloc <- amenity[ind, 'street']
      amenity_loc <- c(amenity[ind, 'long'], amenity[ind, 'lat']) # long and lat of amenities
      distance <- as.numeric(distHaversine(flat_loc, amenity_loc) / 1000)  # in kilometers
      if (distance <= radius) {
        nearest_amenity[3] <- as.numeric(nearest_amenity[3]) + 1
      }
      if (distance < as.numeric(nearest_amenity[2])) {
        nearest_amenity[1] <- eachloc
        nearest_amenity[2] <- distance
      }
    }
    results <- rbind(results, c(flat, nearest_amenity))
  }
  colnames(results) <- c("Address", "Nearest_Amenity", "Distance", "Amenity_Count")
  return(results)
}

lat_long_postal_xy <- readRDS("backend/processed_data/lat_long_postal_xy.Rds") 
mrt_geocode = readRDS("backend/processed_data/mrt_geocode.Rds") 
supermarkets_geocode = readRDS("backend/processed_data/supermarkets_geocode.Rds") 
hawker_centers_geocode = readRDS("backend/processed_data/hawker_centers_geocode.Rds") 
primary_schools_geocode = readRDS("backend/processed_data/primary_schools_geocode.Rds") 
hospitals_geocode = readRDS("backend/processed_data/hospitals_geocode.Rds") 

# Call the find_nearest function with the sample data
nearest_mrt <- find_nearest(lat_long_postal_xy, mrt_geocode)
nearest_mrt <- nearest_mrt %>%
  mutate(Distance = as.numeric(Distance), Amenity_Count = as.numeric(Amenity_Count)) %>%
  rename(c("nearest_mrt" = "Nearest_Amenity", "dist_to_nearest_mrt" = "Distance", 
           "mrt_1km" = "Amenity_Count"))
# saveRDS(nearest_mrt, "backend/processed_data/nearest_mrt.Rds") 

nearest_supermarket <- find_nearest(lat_long_postal_xy, supermarkets_geocode)
nearest_supermarket <- nearest_supermarket %>%
  mutate(Distance = as.numeric(Distance), Amenity_Count = as.numeric(Amenity_Count)) %>%
  rename(c("nearest_supermarket" = "Nearest_Amenity", "dist_to_nearest_supermarket" = "Distance", 
           "supermarket_1km" = "Amenity_Count"))
# saveRDS(nearest_supermarket, "backend/processed_data/nearest_supermarket.Rds") 

nearest_hawkers <- find_nearest(lat_long_postal_xy, hawker_centers_geocode)
nearest_hawkers <- nearest_hawkers %>%
  mutate(Distance = as.numeric(Distance), Amenity_Count = as.numeric(Amenity_Count)) %>%
  rename(c("nearest_hawkers" = "Nearest_Amenity", "dist_to_nearest_hawkers" = "Distance", 
           "hawkers_1km" = "Amenity_Count"))
# saveRDS(nearest_hawkers, "backend/processed_data/nearest_hawkers.Rds") 

nearest_primary_schools <- find_nearest(lat_long_postal_xy, primary_schools_geocode)
nearest_primary_schools <- nearest_primary_schools %>%
  mutate(Distance = as.numeric(Distance), Amenity_Count = as.numeric(Amenity_Count)) %>%
  rename(c("nearest_primary_schools" = "Nearest_Amenity", "dist_to_nearest_primary_schools" = "Distance", 
           "primary_schools_1km" = "Amenity_Count"))
# saveRDS(nearest_primary_schools, "backend/processed_data/nearest_primary_schools.Rds") 

nearest_hospital <- find_nearest(lat_long_postal_xy, hospitals_geocode)
nearest_hospital <- nearest_hospital %>%
  mutate(Distance = as.numeric(Distance), Amenity_Count = as.numeric(Amenity_Count)) %>%
  rename(c("nearest_hospital" = "Nearest_Amenity", "dist_to_nearest_hospital" = "Distance", 
           "hospitals_1km" = "Amenity_Count"))
# saveRDS(nearest_hospital, "backend/processed_data/nearest_hospital.Rds") 

##########################################################################################
# read in the lat_long_postal_xy table we created
lat_long_postal_xy <- readRDS("backend/processed_data/lat_long_postal_xy.Rds")

# Tables of nearest amenities 
nearest_mrt = readRDS("backend/processed_data/nearest_mrt.Rds")
nearest_supermarket = readRDS("backend/processed_data/nearest_supermarket.Rds") 
nearest_hawkers = readRDS("backend/processed_data/nearest_hawkers.Rds")
nearest_primary_schools = readRDS("backend/processed_data/nearest_primary_schools.Rds") 
nearest_hospital = readRDS("backend/processed_data/nearest_hospital.Rds")

# Left join the hdb block details with the nearest amenities 
# MRT, Supermarket, Hawkers, Primary Schools, Hospitals
lat_long_postal_xy = left_join(lat_long_postal_xy, nearest_mrt, by = c("street" = "Address"))
lat_long_postal_xy = left_join(lat_long_postal_xy, nearest_supermarket, by = c("street" = "Address"))
lat_long_postal_xy = left_join(lat_long_postal_xy, nearest_hawkers, by = c("street" = "Address"))
lat_long_postal_xy = left_join(lat_long_postal_xy, nearest_primary_schools, by = c("street" = "Address"))
lat_long_postal_xy = left_join(lat_long_postal_xy, nearest_hospital, by = c("street" = "Address"))

# CBD long and lat
cbd_lat = 1.287953
cbd_long = 103.851784
cbd_coords = c(cbd_long,cbd_lat)

# Adding a column for distance to CBD
lat_long_postal_xy <- lat_long_postal_xy %>%
  mutate(dist_cbd = distHaversine(cbd_coords, cbind(long, lat))/1000) %>% 
  mutate(postal_2dig = as.character(str_sub(as.character(postal),1,2)))

# Merge lat_long_postal_xy dataset with HDB resale flats to get the lease commence date for 
# each block
hdb_lease = readRDS("backend/processed_data/hdb_prices_with_address.Rds") %>% 
  select(address,lease_commence_date) %>%
  unique()

lat_long_postal_xy = lat_long_postal_xy %>% 
  left_join(hdb_lease, by = c("street" = "address"))

# saveRDS(lat_long_postal_xy, "backend/processed_data/lat_long_for_visualisation.Rds")
# lat_long_postal_xy = readRDS("backend/processed_data/lat_long_for_visualisation.Rds")

lat_long_postal_xy = lat_long_postal_xy %>%
  select(-c(starts_with("nearest"),x,y,lat,long))
hdb_town = data_tidy %>% select(town, address)
lat_long_postal_prediction = lat_long_postal_xy %>% 
  left_join(hdb_town, by = c("street" = "address")) %>%
  unique()
# saveRDS(lat_long_postal_prediction, "backend/processed_data/lat_long_for_prediction.Rds")

lat_long_postal_xy = lat_long_postal_xy %>%
  select(-postal)
# saveRDS(lat_long_postal_xy, "backend/processed_data/lat_long_for_analysis.Rds")


##########################################################################################
# Data Cleaning to get the proper MRT and Primary School names to assist visualisations
primary_schools = read.csv("backend/raw_data/primary_schools.csv") %>% select(-1)
primary_schools <- primary_schools %>% mutate(Address = str_replace(Address, ", S", "-")) %>%
  separate(Address, c("address", "postal"), sep = "-") %>% 
  transmute(primary_school_name = Primary.School, postal_primary = postal)

get_onemap_mrt <- function(id) {
  base_url <- "https://www.onemap.gov.sg/api/common/elastic/search?searchVal="
  endpoint <- "&returnGeom=Y&getAddrDetails=Y"
  resource_url <- paste0(base_url, as.character(id), endpoint)
  
  # Make the GET request with the access token in the header
  res <- GET(resource_url,
             add_headers(Authorization = paste("Bearer", Sys.getenv("ONEMAP_KEY"))),
             accept("application/json"))
  
  # Check the status code
  if (res$status_code == 200) {
    # Parse the response content
    res_list <- content(res, type = "text") %>%
      fromJSON(flatten = TRUE)
    
    # Convert response to tibble
    df <- as_tibble(res_list$results)
    mrt = df$BUILDING[grep("\\(", df$BUILDING)]
    if (is.null(mrt)) {
      return("SINGAPORE MRT")
    }
    return(mrt[1])
  } else {
    # cat("Error: Request for ID", id, "failed with status code", res$status_code, "\n")
    return("SINGAPORE MRT")
  }
}

get_onemap_mrt_address <- function(id) {
  base_url <- "https://www.onemap.gov.sg/api/common/elastic/search?searchVal="
  endpoint <- "&returnGeom=Y&getAddrDetails=Y"
  resource_url <- paste0(base_url, as.character(id), endpoint)
  
  # Make the GET request with the access token in the header
  res <- GET(resource_url,
             add_headers(Authorization = paste("Bearer", Sys.getenv("ONEMAP_KEY"))),
             accept("application/json"))
  
  # Check the status code
  if (res$status_code == 200) {
    # Parse the response content
    res_list <- content(res, type = "text") %>%
      fromJSON(flatten = TRUE)
    
    # Convert response to tibble
    df <- as_tibble(res_list$results)
    return(df$ADDRESS[1])
  } else {
    # cat("Error: Request for ID", id, "failed with status code", res$status_code, "\n")
    return("SINGAPORE MRT")
  }
}

mrt = read_excel("backend/raw_data/Train Station Codes and Chinese Names.xls") %>%
  rowwise() %>% 
  mutate(mrt_name = get_onemap_mrt(stn_code), mrt_address = get_onemap_mrt_address(stn_code)) %>% 
  select(mrt_name, mrt_address)
mrt = mrt %>% unique()

hdb_blocks = readRDS("backend/processed_data/lat_long_for_visualisation.Rds")
hdb_blocks = hdb_blocks %>% 
  mutate(postal_code_nearest_primary = substr(nearest_primary_schools,
                                              nchar(nearest_primary_schools) - 5, 
                                              nchar(nearest_primary_schools))) %>%
  mutate(postal_code_nearest_mrt = substr(nearest_mrt,
                                          nchar(nearest_mrt) - 5, 
                                          nchar(nearest_mrt))) %>%
  left_join(primary_schools, by = c("postal_code_nearest_primary" = "postal_primary")) %>%
  left_join(mrt, by = c("nearest_mrt" = "mrt_address")) 

# Mutated the town column
hdb_blocks = hdb_blocks %>% 
  left_join(hdb_town, by = c("street" = "address")) %>%
  unique()
# write.csv(hdb_blocks, "backend/processed_data/unique_hdb_block_details_w_schools_mrt_name.csv") 
# Note: csv is for frontend usage
# saveRDS(hdb_blocks, "backend/processed_data/unique_hdb_block_details_w_schools_mrt_name.Rds")

##########################################################################################
# Test Example of ONEMAP API Call
# Construct the API request URL 
# base_url <- "https://www.onemap.gov.sg/api/common/elastic/search?searchVal="
# endpoint <- "&returnGeom=Y&getAddrDetails=Y"
# resource_url <- paste0(base_url,"406%20ANG%20MO%20KIO%20AVE%2010",endpoint)
# # Make the GET request with the access token in the header
# res <- GET(resource_url,
#            add_headers(Authorization = paste("Bearer", Sys.getenv("ONEMAP_KEY"))),
#            accept("application/json"))
# 
# # Check the status code
# res$status_code
# 
# # Parse the response content
# res_list <- content(res, type = "text") %>%
#   fromJSON(flatten = TRUE)
# 
# # Convert response to tibble
# df <- as_tibble(res_list$results)
# 
# # View the data
# df
##########################################################################################