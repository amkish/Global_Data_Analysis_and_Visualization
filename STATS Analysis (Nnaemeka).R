# Libraries used
library(tidyverse)
library(Hmisc)
library(corrplot)
library(dplyr)
library(moments)
library(e1071)
library(broom)
library(ggplot2)
library(purrr)
library(datarium)
library(rcompanion)
library(xts)
library(forecast)
library(ggfortify)
library(TTR)
library(car)
library(caret)
library(mgcv)

# Loading my dataset
dataset <- read.csv("ASDV_DF_World_Bank_new.csv")

# Preprocessing 
# Removing unwanted columns 
dataset <- dataset %>%
  select(-Country_Code, -Time_Code)

names(dataset) <- c("Time", "Country_name", "GDP_per_capita", "GDP_growth", "FDI_pct_GDP", "CO2_emissions_kt", "Forest_area_sqkm", "REC_pct_TFEC","Rule_of_Law","Control_of_Corruption","Government_Effectiveness", "Exports_of_GS", "Manufactures_exports_pct_ME", "Merchandise_exports_USdol", "Tax_revenue_pct_GDP","Trade_pct_GDP", "Life_expectancy_at_birth", "DGGHE", "Physicians")

# Check for missing values in the dataset
missing_values <- colSums(is.na(dataset))
total_missing <- sum(missing_values)

print(total_missing) # Total number of missing values
print(missing_values) # Missing values count for each column

# Impute missing values within each country segment using mean
dataset <- dataset %>%
  group_by(Country_name) %>%
  mutate(across(everything(), ~ ifelse(is.na(as.numeric(.)), mean(as.numeric(.), na.rm = TRUE), as.numeric(.)))) %>%
  ungroup()

View(dataset)



# -------------------------------------------------------------------------------------------------------------
# EDA ---------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------


head(dataset)
summary(dataset)

# Subset the dataset to include only numeric variables
numeric_dataset <- dataset %>% select_if(is.numeric)

# Boxplot to check for outliers
boxplot(numeric_dataset, outline = TRUE, col = "lightblue", main = "Outliers Detection")

# Box plot for a numerical variable by country
ggplot(dataset, aes(x = Country_name, y = GDP_growth)) +
  geom_boxplot() +
  labs(title = "Comparative Analysis of GDP Growth", x = "Country", y = "GDP growth")



# -------------------------------------------------------------------------------------------------------------
# OBJECTIVE 1: ECONOMIC GROWTH AND DEVELOPMENT ----------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------



# will be applying Descriptive Statistical Analysis
# Descriptive Statistical Analysis

# Columns of interest
objective1_cols <- c("GDP_per_capita", "FDI_pct_GDP", "Tax_revenue_pct_GDP") #indicators related to Objective 1

# Subset the dataset to include only the columns of interest
subset_data <- dataset[, c("Country_name", objective1_cols)]

# Function to calculate mode
calculate_mode <- function(x) {
  unique_values <- unique(x)
  frequencies <- tabulate(match(x, unique_values))
  mode_index <- which.max(frequencies)
  return(unique_values[mode_index])
}

# Function to perform descriptive analysis by country 
objective1_stats <- function(dataset, columns) {
  dataset %>%
    group_by(Country_name) %>%
    summarise(
      across(all_of(columns), list(
        Mean = ~mean(.),
        Median = ~median(.),
        Mode = ~calculate_mode(.),
        SD = ~sd(.),
        Skewness = ~skewness(.),
        Kurtosis = ~kurtosis(.),
        Kurtosis_Interpretation = ~interpret_kurtosis(kurtosis(.))
      ), .names = "{col}_{fn}")
    )
}

# descriptive analysis for each indicator grouped by country
analysis_result <- objective1_stats(dataset, objective1_cols)

# Display the results
print(analysis_result)

# Function to create bar plot for mean and SD
create_bar_plot <- function(df, indicator) {
  ggplot(df, aes(x = Country_name, y = .data[[paste0(indicator, "_Mean")]])) +
    geom_bar(stat = "identity", fill = "skyblue", position = "dodge") +
    geom_errorbar(
      aes(ymin = .data[[paste0(indicator, "_Mean")]] - .data[[paste0(indicator, "_SD")]],
          ymax = .data[[paste0(indicator, "_Mean")]] + .data[[paste0(indicator, "_SD")]]),
      width = 0.2,
      position = position_dodge(0.9)
    ) +
    labs(title = paste("Mean and Standard Deviation of", indicator),
         y = "Value") +
    theme_minimal()
}

# Create and print bar plots for each indicator
for (col in objective1_cols) {
  single_bar_plot <- create_bar_plot(analysis_result, col)
  print(single_bar_plot)
}


# Density plot of indicators
for (i in seq_along(objective1_cols)) {
  col <- objective1_cols[i]
  col_skewness <- skewness(dataset[[col]])
  
  if (col_skewness > 0) {
    message <- paste(col, "is positively skewed.")
  } else if (col_skewness < 0) {
    message <- paste(col, "is negatively skewed.")
  } else {
    message <- paste(col, "is symmetric.")
  }
  
  cat(message, "\n")
  
  # Calculate mean
  col_mean <- mean(dataset[[col]], na.rm = TRUE)
  
  # Create density plot
  density_plot <- ggplot(dataset, aes(x = .data[[col]])) +
    geom_density(fill = "skyblue", color = "black") +
    geom_vline(xintercept = col_mean, color = "#FF0000", linetype = "dashed", size = 1) +
    labs(title = paste("Density Plot of", col),
         x = col,
         y = "Density") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))
  
  # Print the plot
  print(density_plot)
}



# -------------------------------------------------------------------------------------------------------------
# OBJECTIVE 2: ENVIRONMENTAL SUSTAINABILITY -------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------



# will be applying Regression & Correlation Analysis
# Correlation Analysis

# Calculating the correlation matrix
cor_matrix <- cor(numeric_dataset)
# Visualize the correlation matrix
corrplot(cor_matrix, method = "circle")

#checking for outliers
boxplot(dataset$CO2_emissions_kt, dataset$REC_pct_TFEC, names=c("CO2 Emmission", "REC"), xlab = "Indicators", ylab = "values", main = "Boxplot of CO2 Emissions and Renewable Energy Consumption")

# Create a scatter plot with regression line
ggplot(data = dataset, aes(x = CO2_emissions_kt, y = REC_pct_TFEC)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "CO2 emissions (kt)", y = "REC % of TFEC") +
  ggtitle("Scatter plot: CO2 emissions vs. REC % of TFEC") +
  theme_minimal()

# Test for Normality
shapiro.test(dataset$CO2_emissions_kt)
shapiro.test(dataset$REC_pct_TFEC)

# correlation between CO2 Emmision and Renewable Energy Consumption
cor(dataset$CO2_emissions_kt, dataset$REC_pct_TFEC, method = "spearman")

# Create a scatter plot with regression line
ggplot(data = dataset, aes(x = CO2_emissions_kt, y = REC_pct_TFEC)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "CO2 emissions (kt)", y = "REC % of TFEC") +
  ggtitle("Scatter plot: CO2 emissions vs. REC % of TFEC") +
  theme_minimal()

# using Multiple continuous variables
# Test for Normality
shapiro.test(dataset$CO2_emissions_kt)
shapiro.test(dataset$Merchandise_exports_USdol)
shapiro.test(dataset$Manufactures_exports_pct_ME)
shapiro.test(dataset$Trade_pct_GDP)

# Correlation between using multiple variables
multi_cor <- dataset[ ,c("CO2_emissions_kt", "REC_pct_TFEC", "Merchandise_exports_USdol", "Manufactures_exports_pct_ME", "Trade_pct_GDP")]
multi_cor_test <-round(cor(multi_cor, method = "spearman"), digits = 2)

#viewing the results
corrplot(multi_cor_test, method = "shade", type = "upper")


# Regression Analysis
# Specifing the dependent variable and independent variables
# dep_var - CO2_emissions_kt
# indep_var - Manufactures_exports_pct_ME, REC_pct_TFEC, Forest_area_sqkm 

# Test 1: multi-linear regression analysis
# Perform multiple linear regression 
model1 <- lm(CO2_emissions_kt ~ Manufactures_exports_pct_ME + REC_pct_TFEC + Forest_area_sqkm, data = dataset)

# Print the regression results
print(summary(model1))

# checking is our model fits the MLR assumptions:
# Linearity
pairs(dataset[, c("CO2_emissions_kt", "Manufactures_exports_pct_ME", "REC_pct_TFEC", "Forest_area_sqkm")], lower.panel = NULL, pch = 19, cex = 0.2)
# Residuals’ Independence
plot(model1, 1)
# Normality of residuals:
plot(model1, 2)
# Homoscedasticity
plot(model1, 3)
# multicollinearity
vif(model1)

# NB: our model failed to pass all the assumptions


# Test 2: Future engineering 

# Perform feature engineering
dataset$Interaction <- dataset$Manufactures_exports_pct_ME * dataset$REC_pct_TFEC
dataset$Manufactures_exports_sq <- dataset$Manufactures_exports_pct_ME^2

# Create a feature engineering model
model2 <- train(CO2_emissions_kt ~ Manufactures_exports_pct_ME + REC_pct_TFEC + Forest_area_sqkm + Interaction + Manufactures_exports_sq,
                                  data = dataset,
                                  method = "lm",
                                  trControl = trainControl(method = "cv"))

# Print the regression results
print(summary(model2$finalModel))




# -------------------------------------------------------------------------------------------------------------
# OBJECTIVE 3: HEALTH AND WELL-BEING --------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------


# will be applying two Hypotheses testing 
# Hypotheses test 1


# Function to create Q-Q plot
qq_plot <- function(data, dependent_variable, independent_variable) {
  ggplot(data, aes(sample = .data[[independent_variable]])) +
    geom_qq() +
    geom_qq_line(color = "red") +
    labs(title = paste("Q-Q Plot for", independent_variable, "vs", dependent_variable),
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_minimal()
}

# Function to perform Shapiro-Wilk test and print the result
shapiro_test_summary <- function(data, dependent_variable, independent_variable) {
  shapiro_test_result <- shapiro.test(data[[independent_variable]])
  cat("Shapiro-Wilk test for", independent_variable, "vs", dependent_variable, ":\n")
  cat("  W statistic:", shapiro_test_result$statistic, "\n")
  cat("  p-value:", shapiro_test_result$p.value, "\n\n")
}

# Function to normalize multiple columns in a dataset
z_score <- function(dataset, cols) {
  normalized_data <- dataset
  for (col in cols) {
    mean_val <- mean(dataset[[col]], na.rm = TRUE)
    standard_dev <- sd(dataset[[col]], na.rm = TRUE)
    normalized_data[[col]] <- (dataset[[col]] - mean_val) / standard_dev
  }
  return(normalized_data)
}

# Specify the dependent and independent variables
dependent_variable <- "Life_expectancy_at_birth"
independent_variables <- c("DGGHE", "Physicians")

# Normalize the dataset
normalized_dataset <- z_score(dataset, c(dependent_variable, independent_variables))

# Assessing the Normality of the Data
# Create Q-Q plots for each independent variable
for (variable in independent_variables) {
  print(qq_plot(normalized_dataset, dependent_variable, variable))
}  

# Perform Shapiro-Wilk test
shapiro_test_summary(normalized_dataset, dependent_variable, variable)

# Perform Shapiro-Wilk test for the dependent variable
shapiro_test_summary(normalized_dataset, dependent_variable, dependent_variable)

# my hypothesis
# Null Hypothesis (H0): The average life expectancy in the United Kingdom remains above 60 years.
# H0 > 60

# Alternative Hypothesis (H1): The average life expectancy in the United Kingdom has experienced a significant decrease and is now 60 years or less.
# H1 <= 60


hypothesized_mean <- 60

# Mann-Whitney U Test
# Filter data for the United Kingdom and another country (e.g., United States)
uk_life_expectancy <- normalized_dataset$Life_expectancy_at_birth[normalized_dataset$Country_name == "United Kingdom"]


# Performing Mann-Whitney U test
mwu_test_result <- wilcox.test(uk_life_expectancy, mu = hypothesized_mean, alternative = "less")
# note our mann whiney test failed, suggesting that there are ties in your data, making it challenging to compute an exact p-value, however we will employ Monte Carlo simulation to get an accurate p-value

# Performing Mann-Whitney U test with Monte Carlo simulation
mwu_test_result <- wilcox.test(uk_life_expectancy, mu = hypothesized_mean, alternative = "less", exact = FALSE, simulate.p.value = TRUE, B = 10000)

# Print the result
print(mwu_test_result)

# Plot a histogram
hist(uk_life_expectancy, 
     main = "Life expectancy in the UK", 
     xlab = "life_expectancy (kt)",
     col = "skyblue",
     border = "black")



# Test 2

# Specify the dependent and independent variables
dependent_variable3 <- "Life_expectancy_at_birth"
independent_variables3 <- c("Rule_of_Law", "Control_of_Corruption", "GDP_per_capita")

# Null Hypothesis (H0):
# There is no significant difference in the mean "Life_expectancy_at_birth" among the conditions of "Physicians"

# Alternative Hypothesis (H1):
# There is a significant difference in the mean "Life_expectancy_at_birth" among the conditions of "Physicians"

# Shapiro-Wilk test for normality
shapiro_kru_Gov <- shapiro.test(dataset$Life_expectancy_at_birth)
shapiro_kru_rule <- shapiro.test(dataset$Physicians)

shapiro_kru_Gov
shapiro_kru_rule

# Perform Kruskal-Wallis test
kruskal_test_result <- kruskal.test(Life_expectancy_at_birth ~ Physicians, data = normalized_dataset)
kruskal_test_result

# Create a bar plot to visualize the Kruskal-Wallis test result
barplot(c(0.07633), main = "Kruskal-Wallis Test Result",
        xlab = "Kruskal-Wallis chi-squared", ylab = "p-value",
        ylim = c(0, 0.1), col = "lightblue")

# Add the p-value as text on the plot
text(0.1, 0.07633, labels = "0.07633", pos = 3)

# Adding a horizontal line at the significance level
abline(h = 0.05, lty = 2)

# Add a legend for the significance level
legend("topright", legend = c("p-value", "Significance Level"), fill = c("lightblue", "white"), 
       border = "black", bty = "n", bg = "white", cex = 0.8, lty = c(0, 2))





# -------------------------------------------------------------------------------------------------------------
# OBJECTIVE 4: EXPORT GAINS AND TRADE BALANCE -----------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------



# Time Series

# Loading my dataset
dataset_ts <- read.csv("ASDV_DF_World_Bank_Time_Series2.csv")

# Preprocessing 
# Removing unwanted columns 
dataset_ts <- dataset_ts %>%
  select(-Country_Code, -Time_Code)

names(dataset_ts) <- c("Time", "Country_name", "Exports_of_GS")

# Check for missing values in the dataset
missing_values_ts <- colSums(is.na(dataset_ts))
total_missing_ts <- sum(missing_values)

print(total_missing_ts) # Total number of missing values
print(missing_values_ts) # Missing values count for each column

#creating a new subset
SA_data <- subset(dataset_ts, Country_name == "South Africa", select = c("Exports_of_GS"))

# Display the new dataset
view(SA_data)

# Create a time series object
time_series <- SA_data$Exports_of_GS

display <- ts(SA_data, start = 1983)
display
# Plotting the time series data with a title
plot.ts(display, main = "Exports of GS in South Africa (1983 - 2022)", xlab = "Year", ylab = "Exports of GS")

# smoothing
smooth_data <- SMA(display, n=3)
plot.ts(smooth_data)

# Plotting the smooth data
plot.ts(smooth_data, main = "Smoothed Exports of GS in South Africa", xlab = "Year", ylab = "Exports of GS")

#forcasting
SA_forcast <- HoltWinters(display, gamma = FALSE)
SA_forcast

SA_data$SSE
# visualizing the forcast
plot(SA_forcast)

# setting h to forcast
SA_forcast_new <- forecast(SA_forcast, h = 10)
plot(SA_forcast_new)
SA_forcast_new

# Visualizing the model
acf(SA_forcast_new$residuals, lag.max = 20, na.action = na.pass)
Box.test(SA_forcast_new$residuals, lag = 20, type = "Ljung-Box")


# Examing if the forecast errors exhibit consistent variance and follow a normal distribution.
plot.ts(SA_forcast_new$residuals)

# creating a hist function 
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

# distribution of forecast errors
SA_forcast_new$residuals <- SA_forcast_new$residuals[!is.na(SA_forcast_new$residuals)]
plotForecastErrors(SA_forcast_new$residuals)


# THE END **********************************************************************************

