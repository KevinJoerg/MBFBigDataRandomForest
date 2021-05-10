library(data.table)
library(ff)
library(ffbase)
library(ffbase2)
# devtools::install_github("edwindj/ffbase2")
library(pryr)
library(dplyr)
library(tidyr)
library(biglm)
library(stringr)
library(lobstr)
library(lmtest)
library(sandwich)
library(olsrr)
library(ggplot2)
library(corrgram)
library(tictoc)
library(gputools)

rm(list = ls())

### SETUP ### ------------------------------------------------

# # Alternative approach to read in data
# df <- fread('./data/used_cars_data.csv', verbose = FALSE)
# pryr::object_size(df)
# str(df)


# set wd to where the source file is
# make sure you have the datafiles in a /data/ folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the clean data
load.ffdf(dir='./ffdfClean2')

### Some analysis ### -----------------------------------------

num_variables = c('city_fuel_economy', 'horsepower', 'length', 'maximum_seating', 'mileage', 'price', 'DemRepRatio', 'StateDemRepRatio')

carListings.onlynum <- data.frame(carListingsClean) %>% select(num_variables)
names(carListings.onlynum) <- c('Fuel Economy', 'HP', 'Length', 'Seats', 'Mileage', 'Price', 'Dem-Rep County', 'Dem-Rep State')


# correlation matrix
cor(na.omit(data.frame(carListings.onlynum)))

# Create a correlogram (Slow)
# corrgram(carListings.onlynum, lower.panel=panel.shade,
#         upper.panel=panel.pie, main="Correlation numerical variables")

# Scale variables for regression
carListings.df <- as.data.frame(carListingsClean)
for (column in colnames(carListings.df)){
  if (is.numeric(carListings.df[, column])){
    print(column)
    carListings.df[,column] <- scale(carListings.df[,column])
  }
}

# Split into two df, one with known county dem rep ratio, one without
carListings.df.train <- carListings.df[!is.na(carListings.df$DemRepRatio), ]
carListings.df.forecast <- carListings.df[is.na(carListings.df$DemRepRatio), ]

# For OLS, drop state and county
countyTrain <- carListings.df.train$county
countyForecast <- carListings.df.forecast$county
carListings.df.train$state <- NULL
carListings.df.train$county <- NULL
carListings.df.forecast$state <- NULL
carListings.df.forecast$county <- NULL


# Regression
f <- 'DemRepRatio ~ .'
tic()
olsgpu <- gpuLm(f, carListings.df.train)
toc()
tic()
ols <- lm(f, carListings.df.train)
toc()

summary(olsgpu)
summary(ols)

# plot residuals
plot(ols$residuals)

# Breusch Pagan test for heteroscedasticity
# H0: Homoscedasticity
# H1: Heteroscedasticity
bptest(ols)
# There is heteroscedasticity.

# robust standard errors
ols.robust <- coeftest(ols, vcov = vcovHC(ols, type = "HC0"))
ols.robust

# Plot residuals with qqplot
# The Q-Q plot, or quantile-quantile plot, is a graphical tool to help us assess if a set of data plausibly came from some theoretical distribution such as a Normal or exponential
qqnorm(ols$residuals, pch = 1, frame = FALSE)
qqline(ols$residuals, col = "steelblue", lwd = 2)

# check multicollinearity
ols_vif_tol(ols)

# Relative importance of independent variables in determining Y. How much
# each variable uniquely contributes to R2 over and above that which can be
# accounted for by the other predictors.
ols_correlations(ols)

# Test forecast on existing data ***********************************************
forecast_evaluate <- predict(ols, carListings.df.train)
forecast_evaluate <- as.data.frame(cbind(as.character(countyTrain), forecast_evaluate))

# Scale back
mu <- attr(carListings.df$DemRepRatio,"scaled:center")
std <- attr(carListings.df$DemRepRatio,"scaled:scale")
forecast_evaluate$forecast <- (as.numeric(forecast_evaluate$forecast)*std) + mu
forecast_evaluate <- na.omit(forecast_evaluate)

# Count number of car listings per state
forecast_evaluate.count <- forecast_evaluate %>%
  group_by(V1) %>%
  summarise(forecast_evaluate = length(forecast_evaluate)) %>%
  ungroup

# Average value as forecast
forecast_evaluate <- forecast_evaluate %>%
  group_by(V1) %>%
  summarise(forecast = mean(forecast, na.rm = TRUE)) %>%
  ungroup

# Only keep forecasts that are based on at least 100 observations
forecast_evaluate <- forecast_evaluate[forecast_evaluate.count$forecast > 100, ]
names(forecast_evaluate) <- c('county', 'forecast')

# Add true values
forecast_evaluate <- left_join(forecast_evaluate, carListings.df[,c('county', 'DemRepRatio')])
forecast_evaluate <- unique(forecast_evaluate)

# Scale back
forecast_evaluate$DemRepRatio <- (as.numeric(forecast_evaluate$DemRepRatio)*std) + mu
olsTest <- lm('DemRepRatio ~ forecast', data = forecast_evaluate)
summary(olsTest)

# Forecast *********************************************************************
forecast <- predict(ols, carListings.df.forecast)
forecast <- as.data.frame(cbind(as.character(countyForecast), forecast))

# Scale back
mu <- attr(carListings.df$DemRepRatio,"scaled:center")
std <- attr(carListings.df$DemRepRatio,"scaled:scale")
forecast$forecast <- (as.numeric(forecast$forecast)*std) + mu
forecast <- na.omit(forecast)

# Count number of car listings per state
forecast.count <- forecast %>%
  group_by(V1) %>%
  summarise(forecast = length(forecast)) %>%
  ungroup

# Average value as forecast
forecast <- forecast %>%
  group_by(V1) %>%
  summarise(forecast = mean(forecast, na.rm = TRUE)) %>%
  ungroup

# Only keep forecasts that are based on at least 100 observations
forecast <- forecast[forecast.count$forecast > 100, ]

# Scale with coefficients for optimal forecast
forecast$forecast <- olsTest$coefficients[1] + olsTest$coefficients[2] * forecast$forecast


















