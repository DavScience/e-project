# THIS IS A TEST USING ARIMA TO FORECAST THE CONSUMPTION OF CUSTOMERS USING HISTORY DATA
# Install necessary packages
install.packages("forecast")
install.packages("tseries")

# Load required libraries
library(dplyr)
library(ggplot2)
library(cluster)
library(TSclust)
library(lubridate)
library(broom)
library(forecast)
library(tseries)

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

# Convert filtered data to a time series object
time_series <- ts(filtered_data$Value, frequency = 4)

# Checking for stationarity
adf_test_result <- adf.test(time_series, alternative = "stationary")

# If the data is not stationary, difference it
if (adf_test_result$p.value > 0.05) {
  # Data is not stationary, differencing
  time_series_diff <- diff(time_series, differences = 1)
} else {
  time_series_diff <- time_series
}

# Fit an ARIMA model
arima_model <- auto.arima(time_series_diff)
summary(arima_model)

# Check diagnostics
checkresiduals(arima_model)

# Forecasting future values
forecast_values <- forecast(arima_model, h = 10) # 'h' is the number of periods you want to forecast
plot(forecast_values)
