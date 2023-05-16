#R Code Used for Initial Graphs:

library(forecast)
library(ggplot2)
library(readxl)

# Read the data from the Excel file
data <- read_excel("D:/RProb/Coastal-Andhra-Rainfall-Dataset.xlsx")

# Convert the "YEAR" column to Date format
data$YEAR <- as.Date(paste0(data$YEAR, "-01-01"))

# Create a new column for labeling the period
data$Period <- ifelse(data$YEAR >= as.Date("2016-01-01"), "Forecast", "Historical")

# Plot a line graph of annual rainfall over years
ggplot(data, aes(x = YEAR, y = ANNUAL, group = Period, color = Period)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE) +  # Add smoothed line
  geom_vline(xintercept = as.numeric(as.Date("2016-01-01")), linetype = "dashed", color = "red") +
  labs(x = "Year", y = "Annual (mm)") +
  theme_minimal()


#R CODE Used for prediction 

# Load the necessary libraries
library(forecast)
library(ggplot2)
library(moments)
library(readxl)

# Read the rainfall data from the Excel file
data <- read_excel("D:/RProb/new ap4.xlsx")

# Modify the data frame to include the next year
new_row <- data.frame(YEAR = 2017, JAN = 0, FEB = 0, MAR = 0, APR = 0, MAY = 0, JUN = 0, JUL = 0, AUG = 0, SEP = 0, OCT = 0, NOV = 0, DEC = 0)
names(new_row) <- names(data)
data <- rbind(data, new_row)

# Extract the monthly data columns
monthly_data <- data[, -1]

# Convert the monthly data to a matrix
data_matrix <- as.matrix(monthly_data)

# Set the start date and frequency
start_date <- as.Date(paste0(data$YEAR[1], "-01-01"))
frequency <- 12

# Create the time series object
ts_data <- ts(data_matrix, start = c(year(start_date), month(start_date)), frequency = frequency)

# Create empty vectors to store statistical measures and forecasted values
mean_values <- vector("numeric", 12)
median_values <- vector("numeric", 12)
variance_values <- vector("numeric", 12)
skewness_values <- vector("numeric", 12)
forecasted_values <- vector("list", 12)

# Calculate statistical measures and forecasts for each month
for (i in 1:12) {
  month_data <- ts_data[, i]
  
  mean_values[i] <- mean(month_data)
  median_values[i] <- median(month_data)
  variance_values[i] <- var(month_data)
  skewness_values[i] <- skewness(month_data)
  
  model <- auto.arima(month_data)
  forecast <- forecast(model, h = 12)
  forecasted_values[[i]] <- forecast$mean[12]
  
}

# Print the statistical measures and forecasted rainfall values
for (i in 1:12) {
  cat("Month:", colnames(ts_data)[i], "\n")
  cat("Mean:", mean_values[i], "\n")
  cat("Median:", median_values[i], "\n")
  cat("Variance:", variance_values[i], "\n")
  cat("Skewness:", skewness_values[i], "\n")
  cat("Forecasted rainfall:", forecasted_values[[i]], "\n\n")
}

#R code for predicted graphs: 
  
library(forecast)
library(ggplot2)
library(readxl)

# Read the data from the Excel file
data <- read_excel("D:/RProb/Coastal-Andhra-Rainfall-Dataset.xlsx")

# Convert the "YEAR" column to Date format
data$YEAR <- as.Date(paste0(data$YEAR, "-01-01"))

# Create a new column for labeling the period
data$Period <- ifelse(data$YEAR >= as.Date("2016-01-01"), "Forecast", "Historical")

# Plot a line graph of annual rainfall over years
ggplot(data, aes(x = YEAR, y = ANNUAL, group = Period, color = Period)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE) +  # Add smoothed line
  geom_vline(xintercept = as.numeric(as.Date("2016-01-01")), linetype = "dashed", color = "red") +
  labs(x = "Year", y = "Annual 
