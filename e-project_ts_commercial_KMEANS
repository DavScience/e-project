# FINAL VERSION OF TIMESERIES CUSTOMER CLUSTERING OVER 2 YEARS DATA USING KMEANS CLUSTERING METHOD
# IN THIS VERSION FOCUSING ON COMMERCIAL CUSTOMERS THAT HAVE HAND-PICKED BY THEIR PATTERN
# Load required libraries
library(dplyr)
library(ggplot2)
library(cluster)
library(lubridate)
library(broom)

# Specify column names
column_names <- c('cube_id','Timestamp','Tick','Value','total_order_volume','inverter_production','to_grid','to_community','to_supplier','from_grid', 'from_community','from_supplier', paste0("V", 1:11))

# Function to load and process each file
load_and_process <- function(file_path, skip_first_line = FALSE) {
  skip_lines <- ifelse(skip_first_line, 1, 0)
  data <- read.csv(file_path, sep=";", header=FALSE, col.names=column_names, skip=skip_lines)
  data$Timestamp <- ymd_hms(data$Timestamp)
  data <- data %>% mutate(Timestamp = floor_date(Timestamp, "1 hours")) %>%
    group_by(cube_id, Timestamp) %>% 
    summarise(Value = sum(Value, na.rm = TRUE)) %>%
    ungroup()
  return(data)
}

# Load and process each file
data1 <- load_and_process("/data/res202201.csv")
data2 <- load_and_process("/data/res202202.csv")
data3 <- load_and_process("/data/res202203.csv")
data4 <- load_and_process("/data/res202204.csv")
data5 <- load_and_process("/data/res202205.csv")
data6 <- load_and_process("/data/res202206.csv")
data7 <- load_and_process("/data/res202207.csv")
data8 <- load_and_process("/data/res202208.csv")
data9 <- load_and_process("/data/res202209.csv")
data10 <- load_and_process("/data/res202210.csv")
data11 <- load_and_process("/data/res202211.csv")
data12 <- load_and_process("/res202212.csv")
data13 <- load_and_process("data/res2023.csv", skip_first_line = TRUE)

# Combine the datasets
combined_data <- bind_rows(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, data11, data12, data13)

# Specify only the cube_ids to be analyzed
included_ids <- c(920, 199, 1285, 1495, 1488, 1597, 1635, 1724, 51, 331, 538, 860, 973, 75, 1727)
combined_data <- combined_data %>% filter(cube_id %in% included_ids)

# Aggregate data by cube_id
agg_data <- combined_data %>%
  group_by(cube_id) %>%
  summarise(Agg_Value = sum(Value, na.rm = TRUE))

# Ensure the aggregated data is in a suitable format for K-Means
time_series_matrix <- as.matrix(agg_data$Agg_Value)

# Calculate total within-cluster sum of squares for different cluster numbers
wss <- sapply(1:10, function(k) {
  kmeans(time_series_matrix, centers = k, nstart = 25, iter.max = 100)$tot.withinss
})

# Create Screeplot
scree_plot <- ggplot(data.frame(k = 1:10, wss = wss), aes(x = k, y = wss)) + 
  geom_line() + 
  geom_point() +
  labs(title = "Scree Plot using K-means", x = "Number of Clusters", y = "Total Within-Cluster Sum of Squares")
print(scree_plot)

# Perform clustering using K-Means
k <- 3 # number of clusters
clustering_result <- kmeans(time_series_matrix, centers = k)

# Add cluster information to aggregated data
agg_data$Cluster <- clustering_result$cluster

# Merge cluster information back to the combined data
combined_data <- combined_data %>% 
  left_join(agg_data, by = "cube_id")

# Plotting the clusters
ggplot(combined_data, aes(x = Timestamp, y = Value, color = as.factor(Cluster))) +
  geom_line() +
  theme_minimal() +
  labs(title = "Time Series Cluster Analysis (Aggregated Data)", x = "Time", y = "Production / Consumption", color = "Cluster")



############## PLOTTING just specific cluster pick date/range
# Function to plot a specific cluster within a specific timeframe
plot_specific_cluster <- function(data, cluster_number, start_date, end_date, line_color = "blue") {
  data_to_plot <- data %>%
    filter(Cluster == cluster_number, Timestamp >= as.Date(start_date) & Timestamp <= as.Date(end_date))
  
  ggplot(data_to_plot, aes(x = Timestamp, y = Value)) +
    geom_line(color = line_color) +
    theme_minimal() +
    labs(title = paste("Time Series Cluster Analysis - Cluster", cluster_number, "from", start_date, "to", end_date),
         x = "Time", y = "Production / Consumption")
}

# Call the function to plot cluster X from June 1, 2023, to July 1, 2023
plot_specific_cluster(combined_data, 3, "2022-01-01", "2023-12-31", "blue")

############# Single Cluster Plot full timeframe
### Function to plot a specific cluster with a specified color
#plot_specific_cluster <- function(data, cluster_number, line_color = "blue") {
#  data_to_plot <- data %>% filter(Cluster == cluster_number)
# 
#  ggplot(data_to_plot, aes(x = Timestamp, y = Value)) +  # Removed color from aes()
#    geom_line(color = line_color) +  # Set the line color
#    theme_minimal() +
#    labs(title = paste("Time Series Cluster Analysis - Cluster", cluster_number),
#         x = "Time", y = "Production / Consumption")
#}
## Call the function to plot cluster 1 in blue
#plot_specific_cluster(combined_data, 1, "blue")
########


############  STATISTICS ##############
############## PLOTTING for a specific cube_id within a specified date range
# Function to plot time series for a specific cube_id within a specific timeframe
plot_time_series_for_cube_id <- function(data, cube_id, start_date, end_date, line_color = "green") {
  data_to_plot <- data %>%
    filter(cube_id == cube_id, Timestamp >= as.Date(start_date) & Timestamp <= as.Date(end_date))
  
  ggplot(data_to_plot, aes(x = Timestamp, y = Value)) +
    geom_line(color = line_color) +
    theme_minimal() +
    labs(title = paste("Time Series for Cube ID", cube_id, "from", start_date, "to", end_date),
         x = "Time", y = "Production / Consumption")
}

# Call the function to plot the time series for cube_id 1639 from October 29, 2023, to October 30, 2023
plot_time_series_for_cube_id(combined_data, 1727, "2022-01-01", "2023-12-31", "green")

###########


### Function to create and print a table of cube_ids with their summarized Values for a specified cluster where there a peaks
get_summarized_values_by_cluster <- function(data, cluster_number) {
  cluster_data <- data %>%
    filter(Cluster == cluster_number) %>%
    group_by(cube_id) %>%
    summarise(Summed_Value = sum(Value, na.rm = TRUE))
  return(cluster_data)
}
### Getting summarized Values for cube_ids in cluster 1
summarized_values_cluster_1 <- get_summarized_values_by_cluster(combined_data, 1)
### Print the result
print(summarized_values_cluster_1)

######
### Function to find the highest peak for each cube_id in a specified cluster
get_highest_peaks_by_cluster <- function(data, cluster_number) {
  highest_peaks <- data %>%
    filter(Cluster == cluster_number) %>%
    group_by(cube_id) %>%
    summarise(Highest_Peak = max(Value, na.rm = TRUE))
  return(highest_peaks)
}
### Getting highest peaks for cube_ids in a specific cluster (e.g., cluster 1)
highest_peaks_cluster <- get_highest_peaks_by_cluster(combined_data, 2)
### Print the result
print(highest_peaks_cluster)

###########
######
### Function to find the highest and lowest peak for each cube_id in a specified cluster
get_peaks_by_cluster <- function(data, cluster_number) {
  peaks <- data %>%
    filter(Cluster == cluster_number) %>%
    group_by(cube_id) %>%
    summarise(
      Highest_Peak = max(Value, na.rm = TRUE),
      Lowest_Peak = min(Value, na.rm = TRUE)
    )
  return(peaks)
}
### Getting highest and lowest peaks for cube_ids in a specific cluster (e.g., cluster 2)
peaks_cluster <- get_peaks_by_cluster(combined_data, 1)
### Print the result
print(peaks_cluster)




#####
# Function to count the number of cube_ids in each cluster
count_cube_ids_by_cluster <- function(data) {
  count_table <- data %>%
    group_by(Cluster) %>%
    summarise(Count = n_distinct(cube_id))
  return(count_table)
}

# Call the function and print the result
count_table <- count_cube_ids_by_cluster(combined_data)
print(count_table)



############## STATISTICS END ##############

# Function to save specific data (only cube_id and Cluster) to CSV
save_to_csv <- function(data, filename = "tbl_kmeans_commercial_22-23_expeaks_1h.csv") {
  data_to_save <- data %>% 
    select(cube_id, Cluster) %>%
    distinct() # To remove duplicate rows
  write.csv(data_to_save, filename, row.names = FALSE)
}

# Save the specific data (only cube_id and Cluster)
save_to_csv(combined_data)
