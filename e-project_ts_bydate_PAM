# This R-Code is using Data Aggregation to handle the dataset which is too large 
# for the Partitioning Around Medoids (PAM) method I have been using. 
# Here we create a version of picking a specific date

install.packages("TSclust")

# Load required libraries
library(dplyr)
library(ggplot2)
library(cluster)
library(TSclust)
library(lubridate)
library(broom)

# Load the dataset without headers and specify column names
column_names <- c('cube_id','Timestamp','Tick','Value','total_order_volume','inverter_production','to_grid','to_community','to_supplier','from_grid', 'from_community','from_supplier', paste0("V", 1:11))
data <- read.csv("/data/res202212.csv", sep=";", header=FALSE, col.names=column_names)

# Parse the timestamp
data$Timestamp <- ymd_hms(data$Timestamp)

# Filter the data for a specific date, e.g., "2022-12-24"
specific_date <- as.Date("2022-12-24")
filtered_data <- data %>% filter(as.Date(Timestamp) == specific_date)

# Check if the number of observations is greater than 65536 and sample if necessary
if (nrow(filtered_data) > 65536) {
  set.seed(123) # for reproducibility
  filtered_data <- sample_n(filtered_data, 25536)
}

# Selecting relevant columns for clustering
filtered_data <- filtered_data %>% select(Timestamp, Value)

# Convert filtered data to a time series object
time_series <- ts(filtered_data$Value, frequency = 4)

### INCLUDE SCREEPLOT SECTION USING K-MEANS ###
# Create Screeplot
multi_clust <- data.frame(k = 1:10) %>% 
  group_by(k) %>% 
  do(clust = kmeans(time_series, .$k, nstart = 25))
sumsq_clust <- multi_clust %>% 
  group_by(k) %>% 
  do(glance(.$clust[[1]]))

scree_plot <- ggplot(sumsq_clust, aes(k, tot.withinss)) + 
  geom_line() + 
  geom_point() +
  labs(title = "Scree Plot using K-means", x = "Number of Clusters", y = "Total Within-Cluster Sum of Squares")

print(scree_plot)
#### END OF SCREEPLOT SECTION ###

# Perform clustering with selected k value
k <- 5 #  This value is based on scree plot observation above
clustering_result <- pam(time_series, k)

# Add cluster information to filtered data
filtered_data$Cluster <- clustering_result$clustering

# Plotting the clusters
cluster_plot <- ggplot(filtered_data, aes(x = Timestamp, y = Value, color = as.factor(Cluster))) +
  geom_line() +
  theme_minimal() +
  labs(title = paste("Time Series Cluster Analysis for Date", specific_date), x = "Time", y = "Production / Consumption", color = "Cluster")

print(cluster_plot)
