# This R-Code is using Data Aggregation to handle the dataset which is too large 
# for the Partitioning Around Medoids (PAM) method I have been using. 
# Here we create a version of sampling down to 0,1% of the data to be below 65k observations

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

# Selecting relevant columns for clustering
data <- data %>% select(Timestamp, Value)

# Sample the data (e.g., 0,1% of the data)
set.seed(123) # for reproducibility
sampled_data <- data %>% sample_frac(0.001)

# Convert sampled data to a time series object
time_series <- ts(sampled_data$Value, frequency = 4)

# Perform clustering
k <- 4 # number of clusters changeable
clustering_result <- pam(time_series, k)

# Add cluster information to sampled data
sampled_data$Cluster <- clustering_result$clustering

# Plotting the clusters
ggplot(sampled_data, aes(x = Timestamp, y = Value, color = as.factor(Cluster))) +
  geom_line() +
  theme_minimal() +
  labs(title = "Time Series Cluster Analysis (Sampled Data)", x = "Time", y = "Production / Consumption", color = "Cluster")
