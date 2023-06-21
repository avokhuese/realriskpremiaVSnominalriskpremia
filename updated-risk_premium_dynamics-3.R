library(tidyverse)
library(readxl)
library(stringr)
library(psych)

# Read the final data and weekly data for SD computation
fin_data <- read_excel("Downloads/data (computations)mains.xlsx", 
                       skip = 2)

# Convert date columns to proper date format
fin_data$Month <- as.Date(fin_data$Month, format = "%m/%d/%Y")
# weekly_sd_comp$Week <- as.Date(weekly_sd_comp$Week, format = "%b %d, %Y")

# Convert the return columns to numeric
fin_data <- fin_data %>%
  mutate(across(-1, as.numeric))

# Remove rows with NA values
fin_data <- fin_data %>%
  na.omit()


## Data Imputation

remove_outliers <- function(x) {
  # Calculate the interquartile range
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  # Define the upper and lower bounds for outlier detection
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Remove outliers
  x_no_outliers <- x[!is.na(x) & x >= lower_bound & x <= upper_bound]
  
  return(x_no_outliers)
}

remove_outliers <- function(data) {
  # Initialize an empty logical vector to track rows with outliers
  rows_with_outliers <- logical(nrow(data))
  
  # Iterate over each column except the first one (assuming it's the date column)
  for (col in names(data)[-1]) {
    # Calculate the interquartile range
    Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    
    # Define the upper and lower bounds for outlier detection
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    # Identify rows with outliers in the current column
    outliers <- !is.na(data[[col]]) & (data[[col]] < lower_bound | data[[col]] > upper_bound)
    
    # Update the rows_with_outliers vector
    rows_with_outliers <- rows_with_outliers | outliers
  }
  
  # Remove rows with outliers from the dataframe
  data_no_outliers <- data[!rows_with_outliers, ]
  
  return(data_no_outliers)
}

# This is the approach that will be used
replace_outliers_median <- function(data, columns = 2:5) {
  # Iterate over each column
  for (col in columns) {
    # Calculate the interquartile range
    Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
    Q2 <- median(data[[col]], na.rm = TRUE)   # median
    Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    
    # Define the upper and lower bounds for outlier detection
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    # Replace outlier values with the median value
    outliers <- !is.na(data[[col]]) & (data[[col]] < lower_bound | data[[col]] > upper_bound)
    data[[col]][outliers] <- Q2
  }
  
  return(data)
}



# Step 2: Filter relevant columns
returns_cols <- grep("_Returns$", names(fin_data), value = TRUE)
returns_data <- fin_data[, c("Month", returns_cols)]

View(returns_data)

# Grouping and calculating row means for returns_data for 4 equities, 4 etfs and 2 bonds 
USD <- rowMeans(returns_data[, 2:11])
plot(returns_data$Month, USD, xlab = "Year", main = "Indices Returns based on US Currency Region")

EURO <- rowMeans(returns_data[, 12:21])
plot(returns_data$Month, EURO, xlab = "Year", main = "Indices Returns based on EURO Currency Region")


GBP <- rowMeans(returns_data[, 22:31])
plot(returns_data$Month, GBP, xlab = "Year", ylab = "GBP", main = "Indices Returns based on UK Currency Region")

JPY <- rowMeans(returns_data[, 32:41])
plot(returns_data$Month, JPY, xlab = "Year", main = "Indices Returns based on JAPAN Currency Region")

#Creating a dataframe for the returns of indices for all currency regions
returns_data <- data.frame(Month = returns_data$Month, USD, EURO, GBP, JPY)


#Describe and plot the correlation of each currency region agaisnt the other with outliers 
describe(returns_data[, 2:5])
plot(returns_data[, 2:5])

## Remove outliers
#Describe and plot the correlation of each currency region agaisnt the other w/out outliers 
returns_data <- replace_outliers_median(returns_data)
describe(returns_data[, 2:5])
plot(returns_data[, 2:5])

# Step 3: Filter relevant columns for the risk premia of each indices
rp_cols <- grep("_RP$", names(fin_data), value = TRUE)
risk_premia <- select(fin_data, Month, all_of(rp_cols))
#risk_premia <- na.omit(risk_premia)
View(risk_premia)

# Grouping and calculating row means for risk_premia of each indices based on currency region
USD_rp <- rowMeans(risk_premia[, 2:11])
plot(returns_data$Month, USD_rp, col = 2:11, xlab = "Year", ylab = "Risk Premia in USD", main = "Risk Premia for indices based on US Currency Region")
legend("topright", legend = rp_cols[1:10], col = 1:10, lty = 1:10, cex = 0.4)


EURO_rp <- rowMeans(risk_premia[, 12:21])
plot(returns_data$Month, EURO_rp, xlab = "Year", col = 12:21, ylab = "Risk Premia in EURO", main = "Risk Premia for indices based on EURO Currency Region")
legend("topleft", legend = rp_cols[11:20], col = 11:20, lty = 11:20, cex = 0.4)

GBP_rp <- rowMeans(risk_premia[, 22:31])
plot(returns_data$Month, GBP_rp, xlab = "Year", col = 22:31, ylab = "Risk Premia in GBP", main = "Risk Premia for indices based on GBP Currency Region")
legend("topleft", legend = rp_cols[21:30], col = 21:30, lty = 21:30, cex = 0.4)


JPY_rp <- rowMeans(risk_premia[, 32:41])
plot(returns_data$Month, JPY_rp, xlab = "Year", col = 32:41, ylab = "Risk Premia in JPY", main = "Risk Premia for indices based on JPY Currency Region")
legend("topleft", legend = rp_cols[31:40], col = 31:40, lty = 31:40, cex = 0.4)

#Creating a dataframe for the risk premia of indices for all currency regions
risk_premia <- data.frame(Month = risk_premia$Month, USD_rp, EURO_rp, GBP_rp, JPY_rp)

#Describe and plot the risk premia combining each currency region agaisnt the other with outliers 
describe(risk_premia[, 2:5])
plot(risk_premia[, 2:5])

## Remove outliers
#Describe and plot the risk premia combining each currency region agaisnt the other without outliers 
risk_premia <-  replace_outliers_median(risk_premia)
describe(risk_premia[, 2:5])
plot(risk_premia[, 2:5])

# Step 4: Filter relevant columns for volatilities of each indices
volatility_cols <- grep("_SD$", names(fin_data), value = TRUE)
volatility <- fin_data[, c("Month", volatility_cols)]

# Grouping and calculating row means for volatilities of each indices
USD_vol <- rowMeans(volatility[, 2:11])
plot(returns_data$Month, USD_vol, xlab = "Year", col = 2:11, ylab = "Volatility in USD", main = "Volatilities for indices based on US Currency Region")
legend("topleft", legend = rp_cols[1:10], col = 1:10, lty = 1:10, cex = 0.4)

EURO_vol <- rowMeans(volatility[, 12:21])
plot(returns_data$Month, EURO_vol, xlab = "Year", col = 12:21, ylab = "Volatility in EURO", main = "Volatilities for indices based on EURO Currency Region")
legend("topleft", legend = rp_cols[11:20], col = 11:20, lty = 11:20, cex = 0.4)

GBP_vol <- rowMeans(volatility[, 22:31])
plot(returns_data$Month, GBP_vol, col = 22:31, xlab = "Year", ylab = "Volatility in GBP", main = "Volatilities for indices based on  GBP Currency Region")
legend("topleft", legend = rp_cols[21:30], col = 21:30, lty = 21:30, cex = 0.4)


JPY_vol <- rowMeans(volatility[, 32:41])
plot(returns_data$Month, JPY_vol, xlab = "Year", col = 32:41, ylab = "Volatility in JPY", main = "Volatilities for indices based on JAPAN Currency Region")
legend("topleft", legend = rp_cols[31:40], col = 31:40, lty = 31:40, cex = 0.4)

#Creating a dataframe for the volatilities of indices for all currency regions
volatility <- data.frame(Month = volatility$Month, USD_vol, EURO_vol, GBP_vol, JPY_vol)
describe(volatility[, 2:5])
plot(volatility[, 2:5])

## Remove outliers
#Describe and plot the volatilities combining each currency region agaisnt the other without outliers 
volatility <- replace_outliers_median(volatility)
describe(volatility[, 2:5])
plot(volatility[, 2:5])

# Step 5: Filter relevant columns for the risk premia adjusted for inflations for each indices
rp_inflation_cols <- grep("_RPIA$", names(fin_data), value = TRUE)
real_rp <- select(fin_data, Month, all_of(rp_inflation_cols))
#real_rp <- na.omit(real_rp)

# Grouping and calculating row means for risk_premia adjusted for inflation
USD_rp <- rowMeans(real_rp[, 2:11])
plot(returns_data$Month , USD_rp, xlab = "Year", col = 2:11, ylab = "RRP Adjusted for Inflation", main = "Real Risk Premia Adj. for inflation for US Currency Region")
legend("topleft", legend = rp_cols[1:10], col = 1:10, lty = 1:10, cex = 0.4)

EURO_rp <- rowMeans(real_rp[, 12:21])
plot(returns_data$Month , EURO_rp, col = 12:21, xlab = "Year", ylab = "RRP Adjusted for Inflation", main = "Real Risk Premia Adj. for inflation for EURO Currency Region")
legend("topleft", legend = rp_cols[11:20], col = 11:20, lty = 11:20, cex = 0.4)

GBP_rp <- rowMeans(real_rp[, 22:31])
plot(returns_data$Month , GBP_rp, col = 22:31, xlab = "Year", ylab = "RRP Adjusted for Inflation", main = "Real Risk Premia Adj. for inflation for GBP Currency Region")
legend("topleft", legend = rp_cols[21:30], col = 21:30, lty = 21:30, cex = 0.4)


JPY_rp <- rowMeans(real_rp[, 32:41])
plot(returns_data$Month , JPY_rp, col = 32:41, xlab = "Year", ylab = "RRP Adjusted for Inflation", main = "Real Risk Premia Adj. for inflation for JPY Currency Region")
legend("topleft", legend = rp_cols[31:40], col = 31:40, lty = 31:40, cex = 0.4)

#Creating a dataframe for real risk premia adjusted for inflation of indices for all currency regions
real_rpai <- data.frame(Month = real_rp$Month, USD_rp, EURO_rp,GBP_rp,  JPY_rp)

#Describe and plot real risk premia combining each currency region agaisnt the other with outliers 
describe(real_rpai[, 2:5])
plot(real_rpai[, 2:5])


## Remove outliers
#Describe and plot real risk premia combining each currency region agaisnt the other without outliers 
real_rpai <-replace_outliers_median(real_rpai)
describe(real_rpai[, 2:5])
plot(real_rpai[, 2:5])
# Create a vector of colors for the plot
col_colors <- c("blue", "red", "green", "purple")

# Plot risk premia 
plot.ts(risk_premia[, 2:5], plot.type = "single", xy.labels = TRUE, col = col_colors,
        xlab = "Time", ylab = "Risk Premia")
legend("topleft", legend = c("USD", "EU", "GBP", "JPY"), fill = col_colors, lty = "dashed", cex = 0.8)
title("Nominal Risk Premia for all 10 indices of each currency region")

# Plot volatility
plot.ts(volatility[, 2:5], plot.type = "single", xy.labels = TRUE, col = col_colors,
        xlab = "Time", ylab = "Volatility")
legend("topleft", legend = c("USD", "EU", "GBP", "JPY"), fill = col_colors, lty = "dashed", cex = 0.8)
title("Volatility for all 10 indices of each currency region")

# Plot inflation-adjusted risk premia
plot.ts(real_rp[, 2:5], plot.type = "single", xy.labels = TRUE, col = col_colors,
        xlab = "Time", ylab = "Inflation-Adjusted Risk Premia")
legend("topleft", legend = c("USD", "EU", "GBP", "JPY"), fill = col_colors, lty = "dashed", cex = 0.8)
title("Real (Inflation-Adjusted) Risk Premia for all 10 indices of each currency region")




#Plot of Real Risk Premia vs. Nominal Risk Premia
plot(real_rpai$Month, real_rpai$USD_rp, col = 2:11, xlab = "Year", ylab = "Real Risk Premia", main = "Real Risk Premai with Inflation for USD") # Real with adjusted for inflation
legend("topleft", legend = rp_cols[1:10], col = 1:10, lty = 1:10, cex = 0.4)
plot(risk_premia$Month, risk_premia$USD_rp, col = 2:11, xlab = "Year", ylab = "Nominal Risk Premia", main = "Nominal Risk Premia w/out Inflation for USD") #Nominal without inflation
legend("topleft", legend = rp_cols[1:10], col = 1:10, lty = 1:10, cex = 0.4)

plot(real_rpai$GBP_rp, real_rpai$GBP_rp, col = 22:31, xlab = "Year", ylab = "Real Risk Premia", main = "Real Risk Premia with Inflation for GBP") # Real
legend("topleft", legend = rp_cols[21:30], col = 21:30, lty = 21:30, cex = 0.4)
plot(risk_premia$Month, risk_premia$GBP_rp, col = 22:31, xlab = "Year", ylab = "Nominal Risk Premia", main="Nominal Risk Premia w/out inflation for GBP") # Nominal
legend("topleft", legend = rp_cols[21:30], col = 21:30, lty = 21:30, cex = 0.4)

plot(real_rpai$EURO_rp, real_rpai$EURO_rp, col = 12:21, xlab = "Year", ylab = "Real Risk Premia", main = "Real Risk Premia with Inflation for EURO") # Real
legend("topleft", legend = rp_cols[11:20], col = 11:20, lty = 11:20, cex = 0.4)
plot(risk_premia$Month, risk_premia$EURO_rp, col = 12:21, xlab = "Year", ylab = "Nominal Risk Premia", main="Nominal Risk Premia w/out inflation for EURO") # Nominal
legend("topleft", legend = rp_cols[11:20], col = 11:20, lty = 11:20, cex = 0.4)

plot(real_rpai$JPY_rp, real_rpai$JPY_rp, col = 32:41, xlab = "Year", ylab = "Real Risk Premia", main = "Real Risk Premia with Inflation for JPY") # Real
legend("topleft", legend = rp_cols[31:40], col = 31:40, lty = 31:40, cex = 0.4)
plot(risk_premia$Month, risk_premia$JPY_rp, col = 32:41, xlab = "Year", ylab = "Nominal Risk Premia", main="Nominal Risk Premia w/out inflation for JPY") # Nominal
legend("topleft", legend = rp_cols[31:40], col = 31:40, lty = 31:40, cex = 0.4)


# Mean-variance optimization model
mean_variance <- function(data) {
  returns <- data %>%
    select(-Month) %>%
    summarise_all(~ mean(.)) %>%
    t() # transpose
  
  cov_matrix <- cov(data %>% select(-Month))
  
  # Check if the covariance matrix is singular
  if (is.null(eigen(cov_matrix)$values[1])) {
    stop("Covariance matrix is singular. Please check for multicollinearity or consider regularization.")
  }
  
  # Add a small positive value to the diagonal elements to avoid singularity
  cov_matrix <- cov_matrix + diag(1e-10, nrow(cov_matrix))
  
  n <- nrow(returns)
  weights <- solve(cov_matrix) %*% rep(1, n) / sum(solve(cov_matrix) %*% rep(1, n))
  
  return(list(returns = returns, cov_matrix = cov_matrix, weights = weights))
}

# 1/n policy
one_over_n <- function(data) {
  returns <- data %>%
    select(-Month) %>%
    summarise_all(~ mean(.)) %>%
    t() # transpose
  
  n <- nrow(returns)
  weights <- rep(1/n, n)
  
  return(list(returns = returns, weights = weights))
}

# Extremal ratio
extremal_ratio <- function(data) {
  returns <- data %>%
    select(-Month) %>%
    summarise_all(~ mean(.)) %>%
    t() # transpose
  
  max_returns <- apply(data %>% select(-Month), 2, max)
  min_returns <- apply(data %>% select(-Month), 2, min)
  
  weights <- (returns - min_returns) / (max_returns - min_returns)
  weights <- weights / sum(weights)
  
  return(list(returns = returns, weights = weights))
}

## Rolling Windows

# Step 6: Divide the period into rolling windows
# Create a vector of dates for the period
dates <- seq(as.Date("2016-01-01"), as.Date("2022-12-31"), by = "month")

# Create an empty list to store the windows
windows <- list()

# Loop through the windows
for (i in 1:11) {
  # Determine the starting and ending indexes for the window
  start_index <- (i - 1) * 6 + 1
  end_index <- start_index + 11
  
  # Extract the dates for the window
  window_dates <- dates[start_index:end_index]
  
  # Add the window to the list
  windows[[i]] <- window_dates
}

# Print the windows
for (i in 1:11) {
  cat("Window", i, "(", format(windows[[i]][1], "%b%y"), "-", format(windows[[i]][12], "%b%y"), ")\n")
}

# Step 7: Perform optimization models for each window
perform_optimization <- function(window_data, optimization_function) {
  model_result <- optimization_function(window_data)
  return(model_result)
}


# Step 8: Allocate assets based on optimization results
allocate_assets <- function(results) {
  weights_list <- lapply(results, function(result) result$weights)
  weights_df <- data.frame(do.call(cbind, weights_list))
  
  returns_list <- lapply(results, function(result) result$returns)
  returns_df <- data.frame(do.call(cbind, returns_list))
  
  window_names <- character(length(results))
  for (i in 1:length(results)) {
    window_names[i] <- paste0("(", format(windows[[i]][1], "%b%y"), "-", format(windows[[i]][12], "%b%y"), ")")
  }
  
  colnames(weights_df) <- window_names
  colnames(returns_df) <- window_names
  
  weights_df$Asset <- row.names(weights_df)
  return(weights_df)
}


asset_returns <- function(results) {
  returns_list <- lapply(results, function(result) result$returns)
  returns_df <- data.frame(do.call(cbind, returns_list))
  
  window_names <- character(length(results))
  for (i in 1:length(results)) {
    window_names[i] <- paste0("(", format(windows[[i]][1], "%b%y"), "-", format(windows[[i]][12], "%b%y"), ")")
  }
  colnames(returns_df) <- window_names
  return(returns_df)
}

# Step 9: Plot the stacked bar plot
plot_stacked_bar <- function(weights_df, title) {
  data_long <- tidyr::gather(weights_df, key = "Window", value = "Weight", -Asset)
  ggplot(data_long, aes(x = Window, y = Weight, fill = Asset)) +
    geom_bar(stat = "identity") +
    labs(title = title, x = "Window", y = "Weight") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}

# Perform optimization models for each window
## Mean-Variance Optimisation
results <- lapply(windows, function(window_dates) {
  window_data <- returns_data %>%
    filter(Month >= window_dates[1] & Month <= window_dates[12])
  perform_optimization(window_data, mean_variance)
})

# Allocate assets based on optimization results
weights_df <- allocate_assets(results)

view(weights_df)

# Plot the stacked bar plot
plot_stacked_bar(weights_df, "Mean Variance Optimization Allocations")

# Extract the returns from the optimization results
returns_df <- asset_returns(results)
view(returns_df)
# Descriptive stats of the returns based on the windows
describe(returns_df)

# Descriptive stats of the returns based on the assets
describe(t(returns_df))

## 1/n Policy
results <- lapply(windows, function(window_dates) {
  window_data <- returns_data %>%
    filter(Month >= window_dates[1] & Month <= window_dates[12])
  perform_optimization(window_data, one_over_n)
})

# Allocate assets based on optimization results
weights_df <- allocate_assets(results)

view((weights_df))
describe(weights_df)
# Plot the stacked bar plot
plot_stacked_bar(weights_df, "1/n policy Allocations")

# Extract the returns from the optimization results
returns_df <- asset_returns(results)

view(returns_df)
# Descriptive stats of the returns based on the windows
describe(returns_df)

# Descriptive stats of the returns based on the assets
describe(t(returns_df))


## Extremal ratio
results <- lapply(windows, function(window_dates) {
  window_data <- returns_data %>%
    filter(Month >= window_dates[1] & Month <= window_dates[12])
  perform_optimization(window_data, extremal_ratio)
})

# Allocate assets based on optimization results
weights_df <- allocate_assets(results)

view(weights_df)
describe(weights_df)
# Plot the stacked bar plot
plot_stacked_bar(weights_df, "Extremal Ratio Allocations")

# Extract the returns from the optimization results
returns_df <- asset_returns(results)

# Descriptive stats of the returns based on the windows
describe(returns_df)

# Descriptive stats of the returns based on the assets
describe(t(returns_df))


# Additional analyses requirements
# Function to format the regression equation
format_regression_equation <- function(slope, intercept) {
  equation <- paste0("y = ", round(slope, 2), "x + ", round(intercept, 2))
  return(equation)
}

# Function to format the R-squared value
format_r_squared <- function(r_squared) {
  r_squared_text <- paste0("R-squared = ", round(r_squared, 2))
  return(r_squared_text)
}

# Regression lines for real risk premia vs volatility
plot(USD_vol, real_rpai$USD_rp, col = 2:11, xlab = "Volatility", ylab = "Real Risk Premia in USD",
     main = "Real Risk Premia vs Volatility - USD Currency Area")
legend("topright", inset=c(-0.35,0), legend = rp_cols[1:10], col = 1:10, lty = 1:10, cex = 0.5)
abline(lm(real_rpai$USD_rp ~ USD_vol), col = "red")

slope_USD_real_rp <- coef(lm(real_rpai$USD_rp ~ USD_vol))[2]
r_squared_USD_real_rp <- summary(lm(real_rpai$USD_rp ~ USD_vol))$r.squared
equation_USD_real_rp <- format_regression_equation(slope_USD_real_rp, coef(lm(real_rpai$USD_rp ~ USD_vol))[1])
r_squared_text_USD_real_rp <- format_r_squared(r_squared_USD_real_rp)
text(0.8 * max(USD_vol), 0.8 * max(real_rpai$USD_rp), equation_USD_real_rp, pos = 3)
text(0.8 * max(USD_vol), 0.7 * max(real_rpai$USD_rp), r_squared_text_USD_real_rp, pos = 3)

plot(EURO_vol, real_rpai$EURO_rp, col = 12:21, xlab = "Volatility", ylab = "Real Risk Premia in EURO",
     main = "Real Risk Premia vs Volatility - EURO Currency Area")
legend("topright", inset=c(-0.35,0), legend = rp_cols[11:20], col = 11:20, lty = 11:20, cex = 0.4)
abline(lm(real_rpai$EURO_rp ~ EURO_vol), col = "red")

slope_EURO_real_rp <- coef(lm(real_rpai$EURO_rp ~ EURO_vol))[2]
r_squared_EURO_real_rp <- summary(lm(real_rpai$EURO_rp ~ EURO_vol))$r.squared
equation_EURO_real_rp <- format_regression_equation(slope_EURO_real_rp, coef(lm(real_rpai$EURO_rp ~ EURO_vol))[1])
r_squared_text_EURO_real_rp <- format_r_squared(r_squared_EURO_real_rp)
text(0.8 * max(EURO_vol), 0.8 * max(real_rpai$EURO_rp), equation_EURO_real_rp, pos = 3)
text(0.8 * max(EURO_vol), 0.7 * max(real_rpai$EURO_rp), r_squared_text_EURO_real_rp, pos = 3)

plot(GBP_vol, real_rpai$GBP_rp, col = 12:21, xlab = "Volatility", ylab = "Real Risk Premia in GBP",
     main = "Real Risk Premia vs Volatility - GBP Currency Area")
legend("topright", inset=c(-0.35,0), legend = rp_cols[11:20], col = 11:20, lty = 11:20, cex = 0.4)
abline(lm(real_rpai$GBP_rp ~ GBP_vol), col = "red")

slope_GBP_real_rp <- coef(lm(real_rpai$GBP_rp ~ GBP_vol))[2]
r_squared_GBP_real_rp <- summary(lm(real_rpai$GBP_rp ~ GBP_vol))$r.squared
equation_GBP_real_rp <- format_regression_equation(slope_GBP_real_rp, coef(lm(real_rpai$GBP_rp ~ GBP_vol))[1])
r_squared_text_GBP_real_rp <- format_r_squared(r_squared_GBP_real_rp)
text(0.8 * max(GBP_vol), 0.8 * max(real_rpai$GBP_rp), equation_GBP_real_rp, pos = 3)
text(0.8 * max(GBP_vol), 0.7 * max(real_rpai$GBP_rp), r_squared_text_GBP_real_rp, pos = 3)

plot(JPY_vol, real_rpai$JPY_rp, col = 12:21, xlab = "Volatility", ylab = "Real Risk Premia in JPY",
     main = "Real Risk Premia vs Volatility - JPY Currency Area")
legend("topright", inset=c(-0.35,0), legend = rp_cols[11:20], col = 11:20, lty = 11:20, cex = 0.4)

abline(lm(real_rpai$JPY_rp ~ JPY_vol), col = "red")
slope_JPY_real_rp <- coef(lm(real_rpai$JPY_rp ~ JPY_vol))[2]
r_squared_JPY_real_rp <- summary(lm(real_rpai$JPY_rp ~ JPY_vol))$r.squared
equation_JPY_real_rp <- format_regression_equation(slope_JPY_real_rp, coef(lm(real_rpai$JPY_rp ~ JPY_vol))[1])
r_squared_text_JPY_real_rp <- format_r_squared(r_squared_JPY_real_rp)
text(0.8 * max(JPY_vol), 0.8 * max(real_rpai$JPY_rp), equation_JPY_real_rp, pos = 3)
text(0.8 * max(JPY_vol), 0.7 * max(real_rpai$JPY_rp), r_squared_text_JPY_real_rp, pos = 3)


# Regression lines for nominal risk premia vs volatility
plot(USD_vol, risk_premia$USD_rp, col = 12:21, xlab = "Volatility", ylab = "Nominal Risk Premia in USD",
     main = "Nominal Risk Premia vs Volatility - USD Currency Area")
legend("topright", inset=c(-0.35,0), legend = rp_cols[11:20], col = 11:20, lty = 11:20, cex = 0.4)
abline(lm(risk_premia$USD_rp ~ USD_vol), col = "red")
slope_USD_nominal_rp <- coef(lm(risk_premia$USD_rp ~ USD_vol))[2]
r_squared_USD_nominal_rp <- summary(lm(risk_premia$USD_rp ~ USD_vol))$r.squared
equation_USD_nominal_rp <- format_regression_equation(slope_USD_nominal_rp, coef(lm(risk_premia$USD_rp ~ USD_vol))[1])
r_squared_text_USD_nominal_rp <- format_r_squared(r_squared_USD_nominal_rp)
text(0.8 * max(USD_vol), 0.8 * max(risk_premia$USD_rp), equation_USD_nominal_rp, pos = 3)
text(0.8 * max(USD_vol), 0.7 * max(risk_premia$USD_rp), r_squared_text_USD_nominal_rp, pos = 3)

plot(EURO_vol, risk_premia$EURO_rp, col = 12:21, xlab = "Volatility", ylab = "Nominal Risk Premia in EURO",
     main = "Nominal Risk Premia vs Volatility - EURO Currency Area")
legend("topright", inset=c(-0.35,0), legend = rp_cols[11:20], col = 11:20, lty = 11:20, cex = 0.4)
abline(lm(risk_premia$EURO_rp ~ EURO_vol), col = "red")
slope_EURO_nominal_rp <- coef(lm(risk_premia$EURO_rp ~ EURO_vol))[2]
r_squared_EURO_nominal_rp <- summary(lm(risk_premia$EURO_rp ~ EURO_vol))$r.squared
equation_EURO_nominal_rp <- format_regression_equation(slope_EURO_nominal_rp, coef(lm(risk_premia$EURO_rp ~ EURO_vol))[1])
r_squared_text_EURO_nominal_rp <- format_r_squared(r_squared_EURO_nominal_rp)
text(0.8 * max(EURO_vol), 0.8 * max(risk_premia$EURO_rp), equation_EURO_nominal_rp, pos = 3)
text(0.8 * max(EURO_vol), 0.7 * max(risk_premia$EURO_rp), r_squared_text_EURO_nominal_rp, pos = 3)

plot(GBP_vol, risk_premia$GBP_rp, col = 12:21, xlab = "Volatility", ylab = "Nominal Risk Premia in GBP",
     main = "Nominal Risk Premia vs Volatility - GBP Currency Area")
legend("topright", inset=c(-0.35,0), legend = rp_cols[11:20], col = 11:20, lty = 11:20, cex = 0.4)

abline(lm(risk_premia$GBP_rp ~ GBP_vol), col = "red")
slope_GBP_nominal_rp <- coef(lm(risk_premia$GBP_rp ~ GBP_vol))[2]
r_squared_GBP_nominal_rp <- summary(lm(risk_premia$GBP_rp ~ GBP_vol))$r.squared
equation_GBP_nominal_rp <- format_regression_equation(slope_GBP_nominal_rp, coef(lm(risk_premia$GBP_rp ~ GBP_vol))[1])
r_squared_text_GBP_nominal_rp <- format_r_squared(r_squared_GBP_nominal_rp)
text(0.8 * max(GBP_vol), 0.8 * max(risk_premia$GBP_rp), equation_GBP_nominal_rp, pos = 3)
text(0.8 * max(GBP_vol), 0.7 * max(risk_premia$GBP_rp), r_squared_text_GBP_nominal_rp, pos = 3)

plot(JPY_vol, risk_premia$JPY_rp, col = 12:21, xlab = "Volatility", ylab = "Nominal Risk Premia in JPY",
     main = "Nominal Risk Premia vs Volatility - JPY Currency Area")
legend("topright", inset=c(-0.35,0), legend = rp_cols[11:20], col = 11:20, lty = 11:20, cex = 0.4)

abline(lm(risk_premia$JPY_rp ~ JPY_vol), col = "red")
slope_JPY_nominal_rp <- coef(lm(risk_premia$JPY_rp ~ JPY_vol))[2]
r_squared_JPY_nominal_rp <- summary(lm(risk_premia$JPY_rp ~ JPY_vol))$r.squared
equation_JPY_nominal_rp <- format_regression_equation(slope_JPY_nominal_rp, coef(lm(risk_premia$JPY_rp ~ JPY_vol))[1])
r_squared_text_JPY_nominal_rp <- format_r_squared(r_squared_JPY_nominal_rp)
text(0.8 * max(JPY_vol), 0.8 * max(risk_premia$JPY_rp), equation_JPY_nominal_rp, pos = 3)
text(0.8 * max(JPY_vol), 0.7 * max(risk_premia$JPY_rp), r_squared_text_JPY_nominal_rp, pos = 3)
