# This R-Code is using Data Aggregation to handle the dataset which is too large 
# for the Partitioning Around Medoids (PAM) method I have been using. 
# Here we create a versoin of picking a specific cube_id

install.packages("TSclust")

# Load required libraries
library(dplyr)
library(ggplot2)
library(cluster)
library(TSclust)
library(lubridate)

# Load the dataset without headers and specify column names
column_names <- c('cube_id','Timestamp','Tick','Value','total_order_volume','inverter_production','to_grid','to_community','to_supplier','from_grid', 'from_community','from_supplier', paste0("V", 1:11))
data <- read.csv("/data/res202212.csv", sep=";", header=FALSE, col.names=column_names)

# Parse the timestamp
data$Timestamp <- ymd_hms(data$Timestamp)

# Filter the data for a specific cube_id, e.g., cube_id = 103
specific_cube_id <- 103
filtered_data <- data %>% filter(cube_id == specific_cube_id)

# Selecting relevant columns for clustering
filtered_data <- filtered_data %>% select(Timestamp, Value)

# Convert filtered data to a time series object
time_series <- ts(filtered_data$Value, frequency = 4)

# Perform clustering
k <- 1 # number of clusters changeable
clustering_result <- pam(time_series, k)

# Add cluster information to filtered data
filtered_data$Cluster <- clustering_result$clustering

# Plotting the clusters
ggplot(filtered_data, aes(x = Timestamp, y = Value, color = as.factor(Cluster))) +
  geom_line() +
  theme_minimal() +
  labs(title = paste("Time Series Cluster Analysis for cube_id", specific_cube_id), x = "Time", y = "Production/Consumption", color = "Cluster")

