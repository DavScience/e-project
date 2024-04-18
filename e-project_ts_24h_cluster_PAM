# This R-Code is using Timestamp simplificatiopn and Data Aggregation to handle the dataset which is too large 
# for the Partitioning Around Medoids (PAM) method I have been using. 
# Here we create a version of sampling of the data to be below 65k observations

# Load required libraries
library(dplyr)
library(ggplot2)
library(cluster)
library(TSclust)
library(lubridate)

# Load the dataset without headers and specify column names
column_names <- c('cube_id','Timestamp','Tick','Value','total_order_volume','inverter_production','to_grid','to_community','to_supplier','from_grid', 'from_community','from_supplier', paste0("V", 1:11))
data <- read.csv("/data/res202203.csv", sep=";", header=FALSE, col.names=column_names)

# Parse the timestamp and set it to 24-hour intervals
data$Timestamp <- ymd_hms(data$Timestamp)
data <- data %>% mutate(Timestamp = floor_date(Timestamp, "24 hours"))

# Aggregating Value data for each cube_id over each 24-hour period
data_aggregated <- data %>% 
  group_by(cube_id, Timestamp) %>% 
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  ungroup()

# Increase sampling rate if the number of observations is greater than 65536
if (nrow(data_aggregated) > 65536) {
  set.seed(123) # for reproducibility
  sampled_data <- data_aggregated %>% sample_frac(65536 / nrow(data_aggregated))
} else {
  sampled_data <- data_aggregated
}

### Convert sampled data to a time series object
time_series <- ts(sampled_data$Value, frequency = 4)

### Perform clustering
#k <- 4 # number of clusters changeable
#clustering_result <- pam(time_series, k)

### Add cluster information to sampled data
#sampled_data$Cluster <- clustering_result$clustering

# Perform clustering using K-Means
k <- 4 # number of clusters changeable
clustering_result <- kmeans(time_series, centers = k)

# Add cluster information to sampled data
sampled_data$Cluster <- clustering_result$cluster

# Merge the Cluster information with the aggregated data
data_aggregated <- merge(data_aggregated, sampled_data[c("cube_id", "Timestamp", "Cluster")], by = c("cube_id", "Timestamp"), all.x = TRUE)

# Plotting the clusters
ggplot(data_aggregated, aes(x = Timestamp, y = Value, color = as.factor(Cluster))) +
  geom_line() +
  theme_minimal() +
  labs(title = "Time Series Cluster Analysis (Aggregated Data)", x = "Time", y = "Production / Consumption", color = "Cluster")



### CREATE a table with the cluster information
save_to_csv <- function(data_aggregated, filename = "tbl_cluster_res202203.csv") {
  # Write data to CSV file
  write.csv(data_aggregated, filename, row.names = FALSE)
}
# Using the function to save data_aggregated
save_to_csv(data_aggregated)

