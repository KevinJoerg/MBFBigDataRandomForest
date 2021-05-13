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
library(Metrics)
library(mapview)

rm(list = ls())

# Set wd to where the source file is
# Make sure you have the datafiles in a /data/ folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Load Data ------------------------------------------------------------------

# Load the clean data
load.ffdf(dir='./ffdfClean2')

### Prepare data ---------------------------------------------------------------

# Scale numeric variables for regression
carListings.df <- as.data.frame(carListingsClean)
for (column in colnames(carListings.df)){
  if (is.numeric(carListings.df[, column])){
    print(column)
    carListings.df[,column] <- scale(carListings.df[,column])
  }
}

# Split into two df, one with known county dem rep ratio, one without
carListings.df.withCounty <- carListings.df[!is.na(carListings.df$DemRepRatio), ]
carListings.df.forecast <- carListings.df[is.na(carListings.df$DemRepRatio), ]

# For OLS, drop state and county, but save for evaluation
countyTrain <- carListings.df.withCounty$county
countyForecast <- carListings.df.forecast$county
stateTrain <- carListings.df.withCounty$state
stateForecast <- carListings.df.forecast$state
carListings.df.withCounty$state <- NULL
carListings.df.withCounty$county <- NULL
carListings.df.forecast$state <- NULL
carListings.df.forecast$county <- NULL

# Now split the data with observations into train and test

# set the seed to make your partition reproducible
set.seed(123)
smp_size <- floor(0.75 * nrow(carListings.df.withCounty)) ## 75% of the sample size
train_ind <- base::sample(seq_len(nrow(carListings.df.withCounty)), size = smp_size)

# Split the data into train and test
carListings.df.withCounty.train <- carListings.df.withCounty[train_ind,]
carListings.df.withCounty.test <- carListings.df.withCounty[-train_ind, ]

countyTrain.train <- countyTrain[train_ind]
stateTrain.train <- stateTrain[train_ind]
countyTrain.test <- countyTrain[-train_ind]
stateTrain.test <- stateTrain[-train_ind]

### Regression -----------------------------------------------------------------

# Regression formula
f <- 'DemRepRatio ~ .'

# Standard 
tic()
ols <- lm(f, carListings.df.withCounty.train)
toc()
tic()
olsgpu <- gpuLm(f, carListings.df.withCounty.train)
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
ols.robust.short <- ols.robust[c(1,2,4,5,6,7,11,12,17,18,33,19,21,22,23,24,28,35), 1:4]
ols.robust.short
saveRDS(ols.robust.short, file='Pictures_presentation/OLSOutput.rds')

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

# In sample
forecast_evaluate <- predict(ols, carListings.df.withCounty.train)
forecast_evaluate <- as.data.frame(cbind(cbind(as.character(stateTrain.train), as.character(countyTrain.train)), forecast_evaluate))

# Scale back
mu <- attr(carListings.df$DemRepRatio,"scaled:center")
std <- attr(carListings.df$DemRepRatio,"scaled:scale")
forecast_evaluate$forecast <- (as.numeric(forecast_evaluate$forecast)*std) + mu
forecast_evaluate <- na.omit(forecast_evaluate)

# Count number of car listings per state
forecast_evaluate.count <- forecast_evaluate %>%
  group_by(V1, V2) %>%
  summarise(forecast = length(forecast_evaluate)) %>%
  ungroup

# Average value as forecast
forecast_evaluate <- forecast_evaluate %>%
  group_by(V1, V2) %>%
  summarise(forecast = mean(forecast, na.rm = TRUE)) %>%
  ungroup

# Only keep forecasts that are based on at least 100 observations
forecast_evaluate <- forecast_evaluate[forecast_evaluate.count$forecast > 100, ]
names(forecast_evaluate) <- c('state', 'county', 'forecast')

# Add true values
forecast_evaluate <- merge(forecast_evaluate, carListings.df[,c('state', 'county', 'DemRepRatio')],
                           by.x = c('state', 'county'), by.y = c("state", "county"),
                           all.x = TRUE)
forecast_evaluate <- unique(forecast_evaluate)

# Scale back
forecast_evaluate$DemRepRatio <- (as.numeric(forecast_evaluate$DemRepRatio)*std) + mu

# Evaluate with OLS
olsTest <- lm('DemRepRatio ~ forecast', data = forecast_evaluate)
summary(olsTest)

# Save to evaluate performance
fwrite(forecast_evaluate, 'models/OLS_DemRepRatiosEvaluateForecastInSample.csv')


# Out of sample **********************************
forecast_evaluate <- predict(ols, carListings.df.withCounty.test)
forecast_evaluate <- as.data.frame(cbind(cbind(as.character(stateTrain.test), as.character(countyTrain.test)), forecast_evaluate))

# Scale back
mu <- attr(carListings.df$DemRepRatio,"scaled:center")
std <- attr(carListings.df$DemRepRatio,"scaled:scale")
forecast_evaluate$forecast <- (as.numeric(forecast_evaluate$forecast)*std) + mu
forecast_evaluate <- na.omit(forecast_evaluate)

# Count number of car listings per state
forecast_evaluate.count <- forecast_evaluate %>%
  group_by(V1, V2) %>%
  summarise(forecast = length(forecast_evaluate)) %>%
  ungroup

# Average value as forecast
forecast_evaluate <- forecast_evaluate %>%
  group_by(V1, V2) %>%
  summarise(forecast = mean(forecast, na.rm = TRUE)) %>%
  ungroup

# Only keep forecasts that are based on at least 100 observations
forecast_evaluate <- forecast_evaluate[forecast_evaluate.count$forecast > 100, ]
names(forecast_evaluate) <- c('state', 'county', 'forecast')

# Add true values
forecast_evaluate <- merge(forecast_evaluate, carListings.df[,c('state', 'county', 'DemRepRatio')],
                           by.x = c('state', 'county'), by.y = c("state", "county"),
                           all.x = TRUE)
forecast_evaluate <- unique(forecast_evaluate)

# Scale back
forecast_evaluate$DemRepRatio <- (as.numeric(forecast_evaluate$DemRepRatio)*std) + mu

# Evaluate with OLS
olsTest <- lm('DemRepRatio ~ forecast', data = forecast_evaluate)
summary(olsTest)

# Save to evaluate performance
fwrite(forecast_evaluate, 'models/OLS_DemRepRatiosEvaluateForecast.csv')

# Forecast *********************************************************************
forecast <- predict(ols, carListings.df.forecast)
forecast <- as.data.frame(cbind(cbind(as.character(stateForecast), as.character(countyForecast)), forecast))

# Scale back
mu <- attr(carListings.df$DemRepRatio,"scaled:center")
std <- attr(carListings.df$DemRepRatio,"scaled:scale")
forecast$forecast <- (as.numeric(forecast$forecast)*std) + mu
forecast <- na.omit(forecast)

# Count number of car listings per state
forecast.count <- forecast %>%
  group_by(V1, V2) %>%
  summarise(forecast = length(forecast)) %>%
  ungroup

# Average value as forecast
forecast <- forecast %>%
  group_by(V1, V2) %>%
  summarise(forecast = mean(forecast, na.rm = TRUE)) %>%
  ungroup

# Only keep forecasts that are based on at least 100 observations
forecast <- forecast[forecast.count$forecast > 100, ]

# Scale with coefficients for optimal forecast
forecast$forecast <- olsTest$coefficients[1] + olsTest$coefficients[2] * forecast$forecast
forecast <- as.data.frame(forecast)
names(forecast) <- c('state', 'county', 'forecast')

# Save to evaluate performance
fwrite(forecast, 'models/OLS_DemRepRatiosForecast.csv')

# Save available Dem Rep ratios for visualization ******************************

# Create df of all existing county dem rep ratios
availableDemRepRatios <- unique(as.data.frame(cbind(cbind(as.character(stateTrain), as.character(countyTrain)), carListings.df.withCounty$DemRepRatio)))
names(availableDemRepRatios) <- c('state', 'county', 'DemRepRatio')
availableDemRepRatios$DemRepRatio <- as.numeric(availableDemRepRatios$DemRepRatio)

# Scale back
availableDemRepRatios$DemRepRatio <- (as.numeric(availableDemRepRatios$DemRepRatio)*std) + mu

# Write CSV
fwrite(availableDemRepRatios, 'models/DemRepRatiosAvailable.csv')




