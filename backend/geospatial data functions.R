library(readxl)
library(tidyverse)

# Getting the latitude and longitude of each HDB address
geo_code <- function(address) {
  lat <- 1.2996418818103135
  lon <- 103.80010216304007
  x <- 24303.3101027
  y <- 31333.3389857
  postal <- 148812
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
    postal <- as.numeric(df["POSTAL"])
  }, error = function(e) {
    lat <- 1.2996418818103135
    lon <- 103.80010216304007
    x <- 24303.3101027
    y <- 31333.3389857
    postal <- 148812
    street <- "300 TANGLIN HALT ROAD NEW TOWN CAREHUT SINGAPORE 148812"
  })
  return(c(lat, lon, postal, x,  y, street))
}

# Creating Geo-coordinates of nearby amenities for each HDB block
get_details <- function(unique_addresses) {
  details <- data.frame(
    lat = numeric(),
    long = numeric(),
    postal = numeric(),
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
      postal = as.numeric(coords[3]),
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
lat_long_postal_xy = get_details(unique(data_tidy$address))
lat_long_postal_xy = lat_long_postal_xy %>%
  filter(lat <= 1.299641 | lat >= 1.299642) %>% # Remove the default values 
  na.omit()
# write.csv(lat_long_postal_xy, "backend/processed_data/lat_long_postal_xy.csv")

##########################################################################################
# Creating Geo-coordinates of nearby amenities for each HDB block
# Getting the latitude and longitude of each unique address
geo_code_amenities <- function(address) {
  lat <- 1.2996418818103135
  lon <- 103.80010216304007
  x <- 24303.3101027
  y <- 31333.3389857
  postal <- 148812
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
    postal <- as.numeric(df["POSTAL"])
    street <- as.character(df["ADDRESS"])
  }, error = function(e) {
    lat <- 1.2996418818103135
    lon <- 103.80010216304007
    x <- 24303.3101027
    y <- 31333.3389857
    postal <- 148812
    street <- "300 TANGLIN HALT ROAD NEW TOWN CAREHUT SINGAPORE 148812"
  })
  return(c(lat, lon, postal, x,  y, street))
}
get_amenity_details <- function(unique_addresses) {
  details <- data.frame(
    lat = numeric(),
    long = numeric(),
    postal = numeric(),
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
      postal = as.numeric(coords[3]),
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
  filter(lat <= 1.299641 | lat >= 1.299642) # Remove the default values
# write.csv(mrt_geocode, "backend/processed_data/mrt_geocode.csv")

# Supermarkets Geo Coordinates
supermarkets = read.csv("backend/raw_data/ListofSupermarketLicences.csv")
supermarkets_geocode = get_amenity_details(supermarkets$postal_code)
supermarkets_geocode = supermarkets_geocode %>% 
  filter(lat <= 1.299641 | lat >= 1.299642) # Remove the default values 
# write.csv(supermarkets_geocode, "backend/processed_data/supermarkets_geocode.csv")

# Hawker centers Geo Coordinates
hawker_centers = read.csv("backend/raw_data/ListofGovernmentMarketsHawkerCentres.csv")
hawkers <- hawker_centers %>% 
  mutate(location_of_centre = str_replace(location_of_centre, ", S\\(", "%")) %>%
  separate(location_of_centre, c("address", "postal"), sep = "%") %>%
  mutate(postal = str_replace(postal, "\\(", "")) %>%
  mutate(postal = str_replace(postal, "\\)", "")) %>% 
  separate_rows(postal, sep = "/")
hawker_centers_geocode = get_amenity_details(hawkers$postal)
hawker_centers_geocode = hawker_centers_geocode %>% 
  filter(lat <= 1.299641 | lat >= 1.299642) # Remove the default values 
# write.csv(hawker_centers_geocode, "backend/processed_data/hawker_centres_geocode.csv")

# Primary Schools Geo Coordinates
primary_schools = read.csv("backend/raw_data/primary_schools.csv")
primary_schools <- primary_schools %>% mutate(Address = str_replace(Address, ", S", "-")) %>%
  separate(Address, c("address", "postal"), sep = "-")
primary_schools_geocode = get_amenity_details(primary_schools$postal)
primary_schools_geocode = primary_schools_geocode %>% 
  filter(lat <= 1.299641 | lat >= 1.299642) # Remove the default values 
# write.csv(primary_schools_geocode, "backend/processed_data/primary_schools_geocode.csv")

# Hospitals Geo Coordinates
# web data scraping of list of hospitals 
library(rvest)

url <- "https://en.wikipedia.org/wiki/List_of_hospitals_in_Singapore"
# Find all HTML tables on the page
tables <- read_html(url) %>% html_elements("table")
# Number of tables available
length(tables)

#convert the second table to a tibble
records1 = tables[[1]] %>% html_table()
records2 = tables[[2]] %>% html_table()
hospitals <- rbind(records1, records2) %>%
  select(-Opened, -Ownership, -Beds, -Staff) %>%
  mutate(postal = c(529889, 229899, 544886, 169608, 768828, 308433, 159964, 119074, 609606, 574623,  289891,217562, 258500, 228510, 329563, 427990, 188770, 307677, 547530, 168582, 544835, 768024, 609606, 569766, 329562, 529895, 659674))
# write.csv(hospitals, "backend/raw_data/hospitals.csv")

hospitals = read.csv("backend/raw_data/hospitals.csv")
hospitals_geocode <- get_amenity_details(hospitals$postal)
# write.csv(hospitals_geocode, "backend/processed_data/hospitals_geocode.csv")

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

lat_long_postal_xy <- read.csv("backend/processed_data/lat_long_postal_xy.csv") %>% 
  select(-1) 
mrt_geocode = read.csv("backend/processed_data/mrt_geocode.csv") %>% select(-1)
supermarkets_geocode = read.csv("backend/processed_data/supermarkets_geocode.csv") %>% select(-1)
hawker_centers_geocode = read.csv("backend/processed_data/hawker_centres_geocode.csv") %>% select(-1)
primary_schools_geocode = read.csv("backend/processed_data/primary_schools_geocode.csv") %>% select(-1)
hospitals_geocode = read.csv("backend/processed_data/hospitals_geocode.csv") %>% select(-1)

# Call the find_nearest function with the sample data
nearest_mrt <- find_nearest(lat_long_postal_xy, mrt_geocode)
nearest_mrt <- nearest_mrt %>%
  rename(c("nearest_mrt" = "Nearest_Amenity", "dist_to_nearest_mrt" = "Distance", 
           "mrt_1km" = "Amenity_Count"))
# write.csv(nearest_mrt, "backend/processed_data/nearest_mrt.csv")

nearest_supermarket <- find_nearest(lat_long_postal_xy, supermarkets_geocode)
nearest_supermarket <- nearest_supermarket %>%
  rename(c("nearest_supermarket" = "Nearest_Amenity", "dist_to_nearest_supermarket" = "Distance", 
           "supermarket_1km" = "Amenity_Count"))
# write.csv(nearest_supermarket, "backend/processed_data/nearest_supermarket.csv")

nearest_hawkers <- find_nearest(lat_long_postal_xy, hawker_centers_geocode)
nearest_hawkers <- nearest_hawkers %>%
  rename(c("nearest_hawkers" = "Nearest_Amenity", "dist_to_nearest_hawkers" = "Distance", 
           "hawkers_1km" = "Amenity_Count"))
# write.csv(nearest_hawkers, "backend/processed_data/nearest_hawkers.csv")

nearest_primary_schools <- find_nearest(lat_long_postal_xy, primary_schools_geocode)
nearest_primary_schools <- nearest_primary_schools %>%
  rename(c("nearest_primary_schools" = "Nearest_Amenity", "dist_to_nearest_primary_schools" = "Distance", 
           "primary_schools_1km" = "Amenity_Count"))
# write.csv(nearest_primary_schools, "backend/processed_data/nearest_primary_schools.csv")

nearest_hospital <- find_nearest(lat_long_postal_xy, hospitals_geocode)
nearest_hospital <- nearest_hospital %>%
  rename(c("nearest_hospital" = "Nearest_Amenity", "dist_to_nearest_hospital" = "Distance", 
           "hospitals_1km" = "Amenity_Count"))
# write.csv(nearest_hospital, "backend/processed_data/nearest_hospital.csv")

##########################################################################################
# read in the lat_long_postal_xy table we created
lat_long_postal_xy <- read.csv("backend/processed_data/lat_long_postal_xy.csv") %>% 
  select(-1) 

# Tables of nearest amenities 
nearest_mrt = read.csv("backend/processed_data/nearest_mrt.csv") %>% select(-1)
nearest_supermarket = read.csv("backend/processed_data/nearest_supermarket.csv") %>% select(-1)
nearest_hawkers = read.csv("backend/processed_data/nearest_hawkers.csv") %>% select(-1)
nearest_primary_schools = read.csv("backend/processed_data/nearest_primary_schools.csv") %>% select(-1)
nearest_hospital = read.csv("backend/processed_data/nearest_hospital.csv") %>% select(-1)

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
# write.csv(lat_long_postal_xy, "backend/processed_data/lat_long_for_visualisation.csv")

# lat_long_postal_xy = read.csv("backend/processed_data/lat_long_for_visualisation.csv") %>% select(-1)
lat_long_postal_xy = lat_long_postal_xy %>%
  select(-c(starts_with("nearest"),x,y,lat,long,postal))
# write.csv(lat_long_postal_xy, "backend/processed_data/lat_long_for_analysis.csv")
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