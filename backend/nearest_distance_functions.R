# Install and load necessary packages
install.packages("geosphere")
library(geosphere)

# Latitude and longitude of the building
building_lat <- 1.234567  # Replace with actual latitude
building_long <- 2.345678  # Replace with actual longitude

# Sample data of MRT stations, shopping malls, and primary schools
# Replace with your actual datasets
mrt_stations <- data.frame(
  lat = c(1.111, 2.222, 3.333),  # Example latitudes
  lon = c(4.444, 5.555, 6.666)   # Example longitudes
)

shopping_malls <- data.frame(
  lat = c(7.777, 8.888, 9.999),  # Example latitudes
  lon = c(1.111, 2.222, 3.333)   # Example longitudes
)

primary_schools <- data.frame(
  lat = c(4.444, 5.555, 6.666),  # Example latitudes
  lon = c(7.777, 8.888, 9.999)   # Example longitudes
)

# Function to calculate distances
calculate_distance <- function(lat1, lon1, lat2, lon2) {
  dist <- distHaversine(c(lon1, lat1), c(lon2, lat2))
  return(dist)
}

# Calculate distances to nearest amenities
nearest_mrt <- min(apply(mrt_stations, 1, function(x) calculate_distance(building_lat, building_long, x[1], x[2])))
nearest_mall <- min(apply(shopping_malls, 1, function(x) calculate_distance(building_lat, building_long, x[1], x[2])))
nearest_school <- min(apply(primary_schools, 1, function(x) calculate_distance(building_lat, building_long, x[1], x[2])))

# Print the results
print(paste("Nearest MRT distance:", round(nearest_mrt, 2), "km"))
print(paste("Nearest mall distance:", round(nearest_mall, 2), "km"))
print(paste("Nearest school distance:", round(nearest_school, 2), "km"))