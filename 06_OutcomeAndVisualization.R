library(data.table)
library(leaflet)
library(raster)

# set wd to where the source file is
# make sure you have the datafiles in a /data/ folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Load the intermediate data

# OLS

# Dem Rep Ratios that are available
DemRepRatiosAvailable <- fread('models/DemRepRatiosAvailable.csv')

# Data to evaluate OLS forecasts in sample
DemRepRatiosOLSEvaluateInSample <- fread('models/OLS_DemRepRatiosEvaluateForecastInSample.csv')

# Data to evaluate OLS forecasts out of sample
DemRepRatiosOLSEvaluate <- fread('models/OLS_DemRepRatiosEvaluateForecast.csv')

# OLS forecasts
DemRepRatiosOLSForecast <- fread('models/OLS_DemRepRatiosForecast.csv')

# XGBOOST

# Data to evaluate XGB forecasts
DemRepRatiosXGBEvaluate <- fread('models/xgb_pred_test.csv')

# XGB Forecast
DemRepRatiosXGBForecast <- fread('models/xgb_forecast.csv')

# Evaluate performance *********************************************************

# Data of evaluate set
n_observations <- 455
n_variables <- 11

R2 <- function(actual, predicted){
  return(cor(actual, predicted) ^ 2)
}

adjusted_R2 <- function(actual, predicted, number_observations, number_variables){
  return(1 - ((1 - R2(actual, predicted)) * (number_observations - 1)) / (number_observations - number_variables - 1))
}

rmse <- function(actual, predicted){
  return(sqrt(mean((predicted - actual)^2)))
}

# OLS in sample
r2_ols_in_sample <- R2(DemRepRatiosOLSEvaluateInSample$DemRepRatio, DemRepRatiosOLSEvaluateInSample$forecast)
r2_adjusted_ols_in_sample <- adjusted_R2(DemRepRatiosOLSEvaluateInSample$DemRepRatio, DemRepRatiosOLSEvaluateInSample$forecast, n_observations, n_variables)
rmse_ols_in_sample <- rmse(DemRepRatiosOLSEvaluateInSample$DemRepRatio, DemRepRatiosOLSEvaluateInSample$forecast)

# OLS out of sample
r2_ols <- R2(DemRepRatiosOLSEvaluate$DemRepRatio, DemRepRatiosOLSEvaluate$forecast)
r2_adjusted_ols <- adjusted_R2(DemRepRatiosOLSEvaluate$DemRepRatio, DemRepRatiosOLSEvaluate$forecast, n_observations, n_variables)
rmse_ols <- rmse(DemRepRatiosOLSEvaluate$DemRepRatio, DemRepRatiosOLSEvaluate$forecast)

# XGB in sample
# r2_xgb <- R2(DemRepRatiosXGBEvaluate$)

# Visualization ****************************************************************

plotUSVotingData <- function(dataset){
  # Get USA polygon data
  USA <- getData("GADM", country = "usa", level = 2)
  USA@data$NAME_0 <- as.character(lapply(USA@data$NAME_0, tolower))
  USA@data$NAME_1 <- as.character(lapply(USA@data$NAME_1, tolower))
  USA@data$NAME_2 <- as.character(lapply(USA@data$NAME_2, tolower))
  
  # Append data
  temp <- merge(USA, DemRepRatiosAvailable,
                by.x = c("NAME_1", "NAME_2"), by.y = c("state", "county"),
                all.x = TRUE)
  
  # Create a color range for the markers
  pal.quantile <- colorQuantile("RdYlBu", domain =  c(0.1,0.9), reverse = FALSE, n = 10)
  mypal <- pal.quantile(temp$DemRepRatio)
  
  # Create the leaflet map
  map <- leaflet() %>% 
    addProviderTiles("OpenStreetMap.Mapnik") %>%
    setView(lat = 39.8283, lng = -98.5795, zoom = 4) %>%
    addPolygons(data = USA, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
                fillColor = mypal,
                popup = paste("Region: ", temp$NAME_2, "<br>",
                              "Value: ", round(temp$DemRepRatio,3), "<br>")) %>%
    addLegend(position = "bottomleft", pal = pal.quantile, values = c(0.1,0.9),
              title = "Value",
              opacity = 1)

# Return map
return(map)
}

# First a map of only the observed Dem Rep Ratios ******************************
m <- plotUSVotingData(DemRepRatiosAvailable)

# Show map
m

# Export
mapshot(m,'plots/MapAvailableCountyVotingOutcome.html', file='plots/MapAvailableCountyVotingOutcome.png')

# Map of only OLS forecasted counties ******************************************

m <- plotUSVotingData(DemRepRatiosAvailable)

# Show map
m

# Export
mapshot(m,'plots/MapAvailableCountyVotingOutcome.html', file='plots/MapAvailableCountyVotingOutcome.png')




