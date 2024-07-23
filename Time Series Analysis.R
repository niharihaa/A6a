# List of necessary packages
packages <- c("tidyverse", "dplyr", "lubridate", "imputeTS", "ggplot2", "cowplot", "zoo", "forecast", "nnet", "randomForest", "rpart", "caret", "glmnet", "gbm")

# Install any packages that are not already installed
install.packages(setdiff(packages, rownames(installed.packages())))

# Load the libraries
lapply(packages, library, character.only = TRUE)

# Load the dataset
file_path <- "C:/Users/nihar/OneDrive/Desktop/Bootcamp/SCMA 632/DataSet/APSE Historical Data.csv"
data <- read.csv(file_path)

# View the first few rows of the dataset
head(data)

# Convert Date column to Date type in yyyy-mm-dd format
data$Date <- as.Date(mdy(data$Date))

# Check the structure of the dataset
str(data)

# Check for missing values
sum(is.na(data))

# Ensure that the Price column is numeric
data$Price <- as.numeric(gsub(",", "", data$Price))

# Convert other relevant columns to numeric after removing non-numeric characters
data$Open <- as.numeric(gsub(",", "", data$Open))
data$High <- as.numeric(gsub(",", "", data$High))
data$Low <- as.numeric(gsub(",", "", data$Low))
data$Change <- as.numeric(gsub("%", "", data$Change..))

# Clean the Vol. column by removing any non-numeric characters and handle both M and K values
data$Vol <- sapply(data$Vol., function(x) {
  if (grepl("M", x)) {
    as.numeric(gsub("M", "", x)) * 1e6
  } else if (grepl("K", x)) {
    as.numeric(gsub("K", "", x)) * 1e3
  } else {
    NA
  }
})

# Check for NAs in Vol. after conversion
sum(is.na(data$Vol))

# Impute the NA value in Vol. column with the median
data$Vol[is.na(data$Vol)] <- median(data$Vol, na.rm = TRUE)

# Check for NAs in Vol. after imputation
sum(is.na(data$Vol))

# Check the structure of the dataset to confirm changes
str(data)

# Identify outliers using IQR method for the Price column
Q1 <- quantile(data$Price, 0.25)
Q3 <- quantile(data$Price, 0.75)
IQR <- Q3 - Q1
outliers <- data %>% filter(Price < (Q1 - 1.5 * IQR) | Price > (Q3 + 1.5 * IQR))

# Plot outliers
ggplot(data, aes(x = Date, y = Price)) +
  geom_line(color = "blue") +
  geom_point(data = outliers, aes(x = Date, y = Price), color = "red") +
  ggtitle("Stock Price with Outliers Highlighted")

# Plot a line graph for the price of all ten years
ggplot(data, aes(x = Date, y = Price)) +
  geom_line(color = "purple") +
  ggtitle("Stock Price Over 10 Years")

# Create and store individual year plots in a list
plots <- data %>%
  mutate(Year = year(Date)) %>%
  split(.$Year) %>%
  map(~ ggplot(.x, aes(x = Date, y = Price)) + 
        geom_line(color = "darkgreen") + 
        ggtitle(paste("Stock Price in Year", unique(.x$Year))) +
        theme_minimal())

# Display each plot separately
for (i in 1:length(plots)) {
  print(plots[[i]])
}

# Split the data into train and test sets
set.seed(123) # For reproducibility
train_indices <- sample(1:nrow(data), size = 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Check the structure of the train and test datasets
str(train_data)
str(test_data)

# Aggregate the data to monthly frequency
monthly_data <- data %>%
  mutate(Month = floor_date(Date, "month")) %>%
  group_by(Month) %>%
  summarise(
    Price = mean(Price, na.rm = TRUE),
    Open = mean(Open, na.rm = TRUE),
    High = mean(High, na.rm = TRUE),
    Low = mean(Low, na.rm = TRUE),
    Vol = sum(Vol, na.rm = TRUE),
    Change = mean(Change, na.rm = TRUE)
  )

# Check the structure of the monthly_data
str(monthly_data)

# Convert to time series object
ts_monthly <- ts(monthly_data$Price, start = c(year(min(monthly_data$Month)), month(min(monthly_data$Month))), frequency = 12)

# Fit Holt-Winters model to the monthly data
hw_model_monthly <- HoltWinters(ts_monthly)

# Create a data frame for plotting with ggplot2
monthly_data_ts <- data.frame(
  Date = as.Date(monthly_data$Month),
  Price = as.numeric(ts_monthly)
)

# Plot the monthly time series
ggplot(monthly_data_ts, aes(x = Date, y = Price)) +
  geom_line(color = "darkorange") +
  ggtitle("Monthly Average Stock Price") +
  xlab("Time") +
  ylab("Price") +
  theme_minimal()

# Decompose the time series using additive model
decomposed_additive <- decompose(ts_monthly, type = "additive")

# Convert decomposed components to a data frame for plotting
decomposed_additive_df <- data.frame(
  Date = as.Date(monthly_data$Month),
  Observed = as.numeric(decomposed_additive$x),
  Trend = as.numeric(decomposed_additive$trend),
  Seasonal = as.numeric(decomposed_additive$seasonal),
  Random = as.numeric(decomposed_additive$random)
)

# Filter out rows with NA values
decomposed_additive_df <- decomposed_additive_df %>%
  filter(!is.na(Trend) & !is.na(Random))

# Plot the additive decomposition
ggplot(decomposed_additive_df, aes(x = Date)) +
  geom_line(aes(y = Observed, color = "Observed")) +
  geom_line(aes(y = Trend, color = "Trend")) +
  geom_line(aes(y = Seasonal, color = "Seasonal")) +
  geom_line(aes(y = Random, color = "Random")) +
  ggtitle("Additive Decomposition of Monthly Stock Price") +
  ylab("Price") +
  xlab("Time") +
  scale_color_manual(values = c("Observed" = "blue", "Trend" = "red", "Seasonal" = "green", "Random" = "purple")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Decompose the time series using multiplicative model
decomposed_multiplicative <- decompose(ts_monthly, type = "multiplicative")

# Convert decomposed components to a data frame for plotting
decomposed_multiplicative_df <- data.frame(
  Date = as.Date(monthly_data$Month),
  Observed = as.numeric(decomposed_multiplicative$x),
  Trend = as.numeric(decomposed_multiplicative$trend),
  Seasonal = as.numeric(decomposed_multiplicative$seasonal),
  Random = as.numeric(decomposed_multiplicative$random)
)

# Filter out rows with NA values
decomposed_multiplicative_df <- decomposed_multiplicative_df %>%
  filter(!is.na(Trend) & !is.na(Random))

# Plot the multiplicative decomposition
ggplot(decomposed_multiplicative_df, aes(x = Date)) +
  geom_line(aes(y = Observed, color = "Observed")) +
  geom_line(aes(y = Trend, color = "Trend")) +
  geom_line(aes(y = Seasonal, color = "Seasonal")) +
  geom_line(aes(y = Random, color = "Random")) +
  ggtitle("Multiplicative Decomposition of Monthly Stock Price") +
  ylab("Price") +
  xlab("Time") +
  scale_color_manual(values = c("Observed" = "blue", "Trend" = "red", "Seasonal" = "green", "Random" = "purple")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Forecast for the next 12 months (1 year)
hw_forecast_1 <- forecast(hw_model_monthly, h = 12)

# Plot the forecast for the first year
plot(hw_forecast_1, main = "Holt-Winters Forecast for Monthly Stock Price - Year 1", xlab = "Time", ylab = "Price")

# Update the model with the new data and forecast for the next 12 months (second year)
hw_model_monthly_2 <- HoltWinters(ts(c(ts_monthly, hw_forecast_1$mean), frequency = 12, start = start(ts_monthly)))
hw_forecast_2 <- forecast(hw_model_monthly_2, h = 12)

# Plot the forecast for the second year
plot(hw_forecast_2, main = "Holt-Winters Forecast for Monthly Stock Price - Year 2", xlab = "Time", ylab = "Price")

# Update the model with the new data and forecast for the next 12 months (third year)
hw_model_monthly_3 <- HoltWinters(ts(c(ts_monthly, hw_forecast_1$mean, hw_forecast_2$mean), frequency = 12, start = start(ts_monthly)))
hw_forecast_3 <- forecast(hw_model_monthly_3, h = 12)

# Plot the forecast for the third year
plot(hw_forecast_3, main = "Holt-Winters Forecast for Monthly Stock Price - Year 3", xlab = "Time", ylab = "Price")

# Fit ARIMA model to the daily data and perform diagnostic checks
ts_daily <- ts(data$Price, frequency = 365, start = c(year(min(data$Date)), yday(min(data$Date))))

# Fit an ARIMA model to the daily data
arima_model_daily <- auto.arima(ts_daily)

# Perform diagnostic checks on the ARIMA model
tsdiag(arima_model_daily)
acf(residuals(arima_model_daily))
pacf(residuals(arima_model_daily))

# Check if a Seasonal-ARIMA (SARIMA) fits better
sarima_model_daily <- auto.arima(ts_daily, seasonal = TRUE)

# Compare AIC values of ARIMA and SARIMA models
cat("AIC of ARIMA model:", AIC(arima_model_daily), "\n")
cat("AIC of SARIMA model:", AIC(sarima_model_daily), "\n")

# Forecast the series for the next three months (90 days)
forecast_sarima <- forecast(sarima_model_daily, h = 90)

# Plot the SARIMA forecast
plot(forecast_sarima, main = "SARIMA Forecast for Daily Stock Price", xlab = "Time", ylab = "Price")

# Forecasting for the next three months using SARIMA
forecast_sarima_3months <- forecast(sarima_model_daily, h = 3 * 30) # 3 months

# Plot the SARIMA forecast for the next three months
plot(forecast_sarima_3months, main = "SARIMA Forecast for Daily Stock Price - Next 3 Months", xlab = "Time", ylab = "Price")

# Fit ARIMA model to the monthly series
arima_model_monthly <- auto.arima(ts_monthly)

# Print the summary of the ARIMA model
summary(arima_model_monthly)

# Perform diagnostic checks on the ARIMA model
checkresiduals(arima_model_monthly)

# Fit SARIMA model to the monthly series
sarima_model_monthly <- auto.arima(ts_monthly, seasonal = TRUE)

# Print the summary of the SARIMA model
summary(sarima_model_monthly)

# Compare AIC values of ARIMA and SARIMA models
cat("AIC of ARIMA model:", AIC(arima_model_monthly), "\n")
cat("AIC of SARIMA model:", AIC(sarima_model_monthly), "\n")

# Choose the best model based on AIC
if (AIC(sarima_model_monthly) < AIC(arima_model_monthly)) {
  best_model <- sarima_model_monthly
} else {
  best_model <- arima_model_monthly
}

# Forecast the series for the next three months using the best model
forecast_monthly <- forecast(best_model, h = 3)

# Plot the forecast
autoplot(forecast_monthly) +
  ggtitle("Forecast for Monthly Stock Price") +
  xlab("Time") +
  ylab("Price") +
  theme_minimal()

# Print the forecasted values
print(forecast_monthly)

# Save the model summaries
hw_model_summary <- summary(hw_model_monthly)
arima_model_summary <- summary(arima_model_daily)
sarima_model_summary <- summary(sarima_model_daily)
arima_model_monthly_summary <- summary(arima_model_monthly)
sarima_model_monthly_summary <- summary(sarima_model_monthly)

# Print model summaries
print(hw_model_summary)
print(arima_model_summary)
print(sarima_model_summary)
print(arima_model_monthly_summary)
print(sarima_model_monthly_summary)

# Install and load necessary packages
install.packages(c("nnet", "randomForest", "rpart", "ggplot2", "tidyverse", "caret", "glmnet", "gbm"))
library(nnet)
library(randomForest)
library(rpart)
library(ggplot2)
library(tidyverse)
library(caret)
library(glmnet)
library(gbm)

# Normalize the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data_norm <- data %>%
  mutate(across(c(Price, Open, High, Low, Vol, Change), normalize))

# Split data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(data_norm), size = 0.7 * nrow(data_norm))
train_data <- data_norm[train_indices, ]
test_data <- data_norm[-train_indices, ]

# Prepare the dataset for neural network
train_x <- train_data %>% select(-Date, -Price)
train_y <- train_data$Price
test_x <- test_data %>% select(-Date, -Price)
test_y <- test_data$Price

str(train_x)
str(train_y)

# Convert Vol. and Change.. columns to numeric
train_x <- train_x %>%
  mutate(Vol. = as.numeric(gsub("[^0-9.]", "", Vol.)),
         Change.. = as.numeric(gsub("[^0-9.-]", "", Change..)))

train_x <- as.matrix(train_x)
test_x <- as.matrix(test_x)
train_y <- as.numeric(train_y)
test_y <- as.numeric(test_y)

# Check for missing and infinite values again
sum(is.na(train_x))
sum(is.na(train_y))
sum(is.infinite(train_x))
sum(is.infinite(train_y))

# Impute missing values with the median
train_x <- apply(train_x, 2, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))

# Train neural network model
set.seed(123)
nn_model <- nnet(train_x, train_y, size = 10, linout = TRUE)

# Check for missing and infinite values in test_x
sum(is.na(test_x))
sum(is.infinite(test_x))

str(test_x)

# Identify non-numeric values in each column of test_x
non_numeric_test_x <- apply(test_x, 2, function(x) which(is.na(as.numeric(x))))
non_numeric_test_x

# Convert test_x back to a data frame
test_x <- as.data.frame(test_x)

# Clean the Vol. and Change.. columns in test_x
test_x$Vol. <- as.numeric(gsub("[^0-9.]", "", test_x$Vol.))
test_x$Change.. <- as.numeric(gsub("[^0-9.-]", "", test_x$Change..))

# Check for NAs in the cleaned columns
sum(is.na(test_x$Vol.))
sum(is.na(test_x$Change..))

# Inspect the first few rows of test_data to see the raw values in Vol. and Change.. columns
head(test_data$Vol.)
head(test_data$Change..)

# Inspect unique values in Vol. and Change.. columns
unique(test_data$Vol.)
unique(test_data$Change..)

# Clean the Vol. and Change.. columns in test_data
test_data$Vol. <- as.numeric(gsub("M", "", gsub("K", "e3", gsub("[^0-9.]", "", test_data$Vol.))))
test_data$Change.. <- as.numeric(gsub("%", "", test_data$Change..))

# Impute NA values with the median of the respective columns if necessary
if(sum(is.na(test_data$Vol.)) > 0) {
  test_data$Vol.[is.na(test_data$Vol.)] <- median(test_data$Vol., na.rm = TRUE)
}

if(sum(is.na(test_data$Change..)) > 0) {
  test_data$Change..[is.na(test_data$Change..)] <- median(test_data$Change.., na.rm = TRUE)
}

# Prepare test_x again after cleaning
test_x <- test_data %>% select(-Date, -Price)

# Convert test_x to a matrix
test_x <- as.matrix(test_x)

# Check the structure of test_x to confirm the conversion
str(test_x)

# Make predictions using the neural network model
nn_predictions <- predict(nn_model, test_x)

# Visualize the results
plot_df <- data.frame(
  Date = test_data$Date,
  Actual = test_y,
  Predicted = nn_predictions
)

# Plot the actual vs predicted values with improved visualization
ggplot(plot_df, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), linewidth = 1, linetype = "dashed") +
  ggtitle("Neural Network Model Predictions") +
  ylab("Stock Price (Normalized)") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "right")

# Train Random Forest model
set.seed(123)
rf_model <- randomForest(train_x, train_y, ntree = 500)

# Make predictions using the Random Forest model
rf_predictions <- predict(rf_model, test_x)

# Visualize the Random Forest model results
plot_df_rf <- data.frame(
  Date = test_data$Date,
  Actual = test_y,
  Predicted = rf_predictions
)

# Plot the actual vs predicted values for the Random Forest model
ggplot(plot_df_rf, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), linewidth = 1, linetype = "dashed") +
  ggtitle("Random Forest Model Predictions") +
  ylab("Stock Price (Normalized)") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "right")

# Train Decision Tree model
dt_model <- rpart(train_y ~ ., data = data.frame(train_x, train_y), method = "anova")

# Convert test_x to a data frame
test_x_df <- as.data.frame(test_x)

# Make predictions using the Decision Tree model
dt_predictions <- predict(dt_model, test_x_df)

# Visualize the Decision Tree model results
plot_df_dt <- data.frame(
  Date = test_data$Date,
  Actual = test_y,
  Predicted = dt_predictions
)

# Plot the actual vs predicted values for the Decision Tree model
ggplot(plot_df_dt, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), linewidth = 1, linetype = "dashed") +
  ggtitle("Decision Tree Model Predictions") +
  ylab("Stock Price (Normalized)") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "right")

