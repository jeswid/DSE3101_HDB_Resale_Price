# library(httr)
# library(jsonlite)
# library(tidyverse)
# library(readxl)
# 
# # Getting the latitude and longitude of each unique address
# geo_code <- function(address) {
#   lat <- 1.2996418818103135
#   lon <- 103.80010216304007
#   x <- 24303.3101027
#   y <- 31333.3389857
#   postal <- 148812
#   street <- "300 TANGLIN HALT ROAD NEW TOWN CAREHUT SINGAPORE 148812"
#   tryCatch({
#     address = str_replace_all(address," ","%20")
#     base_url <- "https://www.onemap.gov.sg/api/common/elastic/search?searchVal="
#     endpoint <- "&returnGeom=Y&getAddrDetails=Y"
#     resource_url <- paste0(base_url,address,endpoint)
#     res <- GET(resource_url,
#                add_headers(Authorization = paste("Bearer", Sys.getenv("ONEMAP_KEY"))),
#                accept("application/json"))
#     res_list <- content(res, type = "text") %>% fromJSON(flatten = TRUE)
#     df <- as_tibble(res_list$results)[1,]
#     lat <- as.numeric(df["LATITUDE"])
#     lon <- as.numeric(df["LONGITUDE"])
#     x <- as.numeric(df["X"])
#     y <- as.numeric(df["Y"])
#     postal <- as.numeric(df["POSTAL"])
#     street <- as.character(df["ADDRESS"])
#   }, error = function(e) {
#     lat <- 1.2996418818103135
#     lon <- 103.80010216304007
#     x <- 24303.3101027
#     y <- 31333.3389857
#     postal <- 148812
#     street <- "300 TANGLIN HALT ROAD NEW TOWN CAREHUT SINGAPORE 148812"
#   })
#   return(c(lat, lon, postal, x,  y, street))
# }
# 
# # Define the function
# get_details <- function(unique_addresses) {
#   details <- data.frame(
#     lat = numeric(),
#     long = numeric(),
#     postal = numeric(),
#     x = numeric(),
#     y = numeric(),
#     street = character(),
#     stringsAsFactors = FALSE
#   )
#   
#   for (address in unique_addresses) {
#     coords <- geo_code(address)
#     
#     # Check if coords is NULL or has insufficient length
#     if (is.null(coords) || length(coords) < 6) {
#       warning(paste("Skipping address due to insufficient data:", address))
#       next  # Skip to the next iteration
#     }
#     
#     # Convert data types explicitly
#     row <- data.frame(
#       lat = as.numeric(coords[1]),
#       long = as.numeric(coords[2]),
#       postal = as.numeric(coords[3]),
#       x = as.numeric(coords[4]),
#       y = as.numeric(coords[5]),
#       street = as.character(coords[6]),
#       stringsAsFactors = FALSE
#     )
#     
#     details <- bind_rows(details, row)
#   }
#   
#   return(details)
# }
# 
# 
# # HDB Geo Coordinates
# # write.csv(get_lat_long_postal_xy(unique(data_tidy$address)), "backend/lat_long_postal_xy.csv")
# 
# # MRT stations Geo Coordinates
# mrt = read_excel("backend/Train Station Codes and Chinese Names.xls")
# mrt_geocode = get_details(mrt$stn_code)
# #write.csv(mrt_geocode, "backend/mrt_geocode.csv")
# 
# # Supermarkets Geo Coordinates
# supermarkets = read.csv("backend/ListofSupermarketLicences.csv")
# supermarkets_geocode = get_details(supermarkets$postal_code)
# write.csv(supermarkets_geocode, "backend/supermarkets_geocode.csv")
# 
# # Hawker centers Geo Coordinates
# hawker_centers = read.csv("backend/ListofGovernmentMarketsHawkerCentres.csv")
# 
# #isolate the postal code from the address column of the hawker centre
# hawkers <- hawker_centers %>% mutate(location_of_centre = str_replace(location_of_centre, ", S\\(", "-")) %>%
#   separate(location_of_centre, c("address", "postal"), sep = "-") %>%
#   mutate(postal = str_replace(postal, "\\(", ""))%>%
#   mutate(postal = str_replace(postal, "\\)", ""))
# 
# hawker_centers_geocode = get_details(hawkers$postal)
# write.csv(hawker_centers_geocode, "backend/hawker_centres_geocode.csv")
# 
# primary_schools = read.csv("backend/primary_schools.csv")
# primary_schools <- primary_schools %>% mutate(Address = str_replace(Address, ", S", "-")) %>%
#   separate(Address, c("address", "postal"), sep = "-")
# 
# primary_schools_geocode = get_details(primary_schools$postal)
# write.csv(primary_schools_geocode, "backend/primary_schools_geocode.csv")
##########################################################################################
# Test Example of ONEMAP API Call
# #Construct the API request URL
# base_url <- "https://www.onemap.gov.sg/api/common/elastic/search?searchVal="
# endpoint <- "&returnGeom=Y&getAddrDetails=Y"
# resource_url <- paste0(base_url,"148812",endpoint)
# # Make the GET request with the access token in the header
# res <- GET(resource_url,
#            add_headers(Authorization = paste("Bearer", Sys.getenv("ONEMAP_KEY"))),
#            accept("application/json"))
# # 
# # Check the status code
# res$status_code
# 
# # Parse the response content
# res_list <- content(res, type = "text") %>%
#   fromJSON(flatten = TRUE)
# 
# # Convert response to tibble
# df <- as_tibble(res_list$results)
# df["ADDRESS"]
# # View the data
# df
##########################################################################################
# #Web data scraping of list of primary schools using CSS selectors
# # install.packages("rvest")
# library(rvest)
# 
# url <- "https://cloverhome.sg/primary-school-list/"
# # Find all HTML tables on the page
# tables <- read_html(url) %>% html_elements("table")
# # Number of tables available
# length(tables)
# 
# #convert the second table to a tibble
# records = tables[[1]] %>% html_table()
# primary_schools = write.csv(records, "backend/primary_schools.csv")