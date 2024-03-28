# load the required packages
library(tidyverse)
library(plotly)
library(lubridate)
library(caret)
library(httr) 
library(jsonlite)

# load our cleaned dataset
hdb_resale = read.csv("backend/processed_data/hdb_resale_prices.csv") %>% select(-1)

#load our cleaned dataset with only continuous variables
hdb_resale_cont <- read.csv("backend/processed_data/hdb_resale_continuous.csv") %>% select(-1)
  
# Exploratory Data Analysis

#build a correlation heatmap to test for correlation between continuous variables
correlation_matrix <- cor(hdb_resale_cont)


# Create a plotly heatmap with labels
plot_ly(z = correlation_matrix, type = "heatmap",
        x = colnames(correlation_matrix), y = colnames(correlation_matrix),
        text = paste("Variable 1: ", colnames(correlation_matrix), "<br>",
                     "Variable 2: ", rownames(correlation_matrix), "<br>",
                     "Correlation: ", round(correlation_matrix, 2), "<br>",
                     sep = "")) %>%
  layout(title = "Correlation Heatmap of Continuous Variables",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

#find the highest correlated variables:

# Flatten the upper triangle of the correlation matrix (excluding the diagonal)
correlation_values <- as.vector(correlation_matrix[upper.tri(correlation_matrix)])

# Find the indices of the highest correlation values
top_correlation_indices <- order(correlation_values, decreasing = TRUE)[1:10]  # Adjust the number as needed

# Extract corresponding variable pairs and their correlation values
top_variable_pairs <- expand.grid(Variables_1 = colnames(correlation_matrix),
                                  Variables_2 = rownames(correlation_matrix))[top_correlation_indices, ]
top_correlation_values <- correlation_values[top_correlation_indices]

# Combine variable pairs and correlation values
top_correlation_results <- data.frame(top_variable_pairs, Correlation_Value = top_correlation_values)

# Print the top correlated variable pairs
print(top_correlation_results)

#plot the HDB resale prices (density plot) to see the skewness of prices n whether we need to transform the data
# Plot histogram of resale prices
ggplot(hdb_resale, aes(x = resale_price)) +
  geom_histogram(binwidth = 10000, fill = "skyblue", color = "black") +
  labs(title = "Histogram of HDB Resale Prices",
       x = "Resale Price (SGD)",
       y = "Frequency") +
  theme_minimal()

#the prices are highly skewed to the left (right-tailed), so must do log transformation of HDB prices
# Log transformation of resale prices
hdb_resale$log_resale_price <- log(hdb_resale$resale_price)

# Plot histogram of log-transformed resale prices
ggplot(hdb_resale, aes(x = log_resale_price)) +
  geom_histogram(binwidth = 0.2, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Log-Transformed HDB Resale Prices",
       x = "Log(Resale Price)",
       y = "Frequency") +
  theme_minimal()

#we do log transformation bc this tends to compress higher values more than lower values, hence reducing the right skewness.


#plot median HDB resale prices by town, facet wrap by other categories
# Assuming you have a dataset named `hdb_data` with columns `town`, `resale_price`, and other categorical variables for facet wrapping
# Assuming ggplot2 is already installed, if not, install it using install.packages("ggplot2")
library(ggplot2)

#read in non transformed data
hdb_ori <- read.csv("backend/processed_data/hdb_original.csv")

# Calculate median resale price by town
median_prices <- aggregate(resale_price ~ town, hdb_ori, median)

# Reorder town based on median resale prices (from highest to lowest)
hdb_ori$town <- factor(hdb_ori$town, levels = median_prices[order(median_prices$resale_price, decreasing = TRUE), "town"])

#create another column called median_prices
# Calculate median resale price for each town
median_prices <- hdb_ori %>%
  group_by(town) %>%
  summarise(median_price = median(resale_price, na.rm = TRUE))

median_prices$town <- reorder(median_prices$town, -median_prices$median_price)

# Plot barplot of median prices vs. town and facet wrap by flat_type
ggplot(median_prices, aes(x = town, y = median_price)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Median HDB Resale Prices by Town",
       x = "Town",
       y = "Median Resale Price (SGD)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = ))
