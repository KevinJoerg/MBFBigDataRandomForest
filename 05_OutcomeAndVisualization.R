### AGGREGATION AND VISUALIZATION ###
### Authors: Tim Graf, Kevin Jörg, Moritz Dänliker ###

library(data.table)
library(leaflet)
library(mapview)
library(raster)
library(ggplot2)

rm(list = ls())

# set wd to where the source file is
# make sure you have the datafiles in a /data/ folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Load the intermediate data -------------------------------------------------

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

# Data to evaluate XGB forecasts in sample
DemRepRatiosXGBEvaluateInSample <- fread('models/xgb_pred_train_withState.csv')

# Data to evaluate XGB forecasts out of sample
DemRepRatiosXGBEvaluate <- fread('models/xgb_pred_test_withState.csv')

# XGB Forecast
DemRepRatiosXGBForecast <- fread('models/xgb_forecast.csv')

### Get performance metrics of OLS and XGB -------------------------------------

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
ols_metrics_in_sample <- round(c(r2_ols_in_sample, r2_adjusted_ols_in_sample, rmse_ols_in_sample),digits = 5)

# OLS out of sample
r2_ols <- R2(DemRepRatiosOLSEvaluate$DemRepRatio, DemRepRatiosOLSEvaluate$forecast)
r2_adjusted_ols <- adjusted_R2(DemRepRatiosOLSEvaluate$DemRepRatio, DemRepRatiosOLSEvaluate$forecast, n_observations, n_variables)
rmse_ols <- rmse(DemRepRatiosOLSEvaluate$DemRepRatio, DemRepRatiosOLSEvaluate$forecast)
ols_metrics_out_of_sample <- round(c(r2_ols, r2_adjusted_ols, rmse_ols),digits = 5)
ols_computation_time <- '1.9 Seconds'

# XGB in sample
r2_xgb_in_sample <- R2(DemRepRatiosXGBEvaluateInSample$actual, DemRepRatiosXGBEvaluateInSample$forecast)
r2_adjusted_xgb_in_sample <- adjusted_R2(DemRepRatiosXGBEvaluateInSample$actual, DemRepRatiosXGBEvaluateInSample$forecast, n_observations, n_variables)
rmse_xgb_in_sample <- rmse(DemRepRatiosXGBEvaluateInSample$actual, DemRepRatiosXGBEvaluateInSample$forecast)
xgb_metrics_in_sample <- round(c(r2_xgb_in_sample, r2_adjusted_xgb_in_sample, rmse_xgb_in_sample),digits = 5)

# XGB out of sample
r2_xgb <- R2(DemRepRatiosXGBEvaluate$actual, DemRepRatiosXGBEvaluate$forecast)
r2_adjusted_xgb <- adjusted_R2(DemRepRatiosXGBEvaluate$actual, DemRepRatiosXGBEvaluate$forecast, n_observations, n_variables)
rmse_xgb <- rmse(DemRepRatiosXGBEvaluate$actual, DemRepRatiosXGBEvaluate$forecast)
xgb_metrics_out_of_sample <- round(c(r2_xgb, r2_adjusted_xgb, rmse_xgb),digits = 5)
xgb_computation_time <- '63.9 Minutes'

# Create a dataframe of the observations
performance_metrics <- data.frame(matrix(c(ols_metrics_in_sample, ols_metrics_out_of_sample, ols_computation_time, xgb_metrics_in_sample, xgb_metrics_out_of_sample, xgb_computation_time), ncol=2))
names(performance_metrics) <- c('OLS', 'XGB')
row.names(performance_metrics) <- c('R2 in Sample', 'Adjusted R2 in Sample', 'RMSE in Sample', 'R2 out of Sample', 'Adjusted R2 out of Sample', 'RMSE out of Sample', 'Computation time')

# Save for presentation
saveRDS(performance_metrics, 'Pictures_presentation/performance_metrics.rds')

# OLS actual vs predicted
# merge dataframes
merged_df <- DemRepRatiosOLSEvaluate[,c('forecast', 'DemRepRatio')]
colnames(merged_df) <- c('predicted', 'actual')
merged_df <- merged_df[order(merged_df$actual),]
row.names(merged_df) <- NULL

# Plot predicted vs. actual 
colors <- c("actual" = "red", "predicted" = "blue")
plot_ols <- ggplot(data = merged_df, aes(x = as.numeric(row.names(merged_df)))) +
  geom_point(aes(y = predicted, color = 'predicted')) +
  geom_point(aes(y = actual, color = 'actual')) +
  ggtitle('Actual vs. predicted values OLS') + 
  scale_color_manual(values = colors) +
  labs(x = 'Index', y = 'DemRepRatio')
plot_ols

ggsave('plot_ols_actual_prediction.png', path = './Plots/', plot = plot_ols, device = 'png')

### Visualization --------------------------------------------------------------

plotUSVotingData <- function(dataset){
  # Get USA polygon data
  USA <- getData("GADM", country = "usa", level = 2)
  USA@data$NAME_0 <- as.character(lapply(USA@data$NAME_0, tolower))
  USA@data$NAME_1 <- as.character(lapply(USA@data$NAME_1, tolower))
  USA@data$NAME_2 <- as.character(lapply(USA@data$NAME_2, tolower))
  
  # Append data
  temp <- merge(USA, dataset,
                by.x = c("NAME_1", "NAME_2"), by.y = c("state", "county"),
                all.x = TRUE)
  
  # Create a color range for the markers
  pal.quantile <- colorQuantile("RdYlBu", domain =  c(0,1), reverse = FALSE, n = 10)
  mypal <- pal.quantile(temp$DemRepRatio)
  
  # Create the leaflet map
  map <- leaflet() %>% 
    addProviderTiles("OpenStreetMap.Mapnik") %>%
    setView(lat = 39.8283, lng = -98.5795, zoom = 4) %>%
    addPolygons(data = USA, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
                fillColor = mypal,
                popup = paste("Region: ", temp$NAME_2, "<br>",
                              "Value: ", round(temp$DemRepRatio,3), "<br>")) %>%
    addLegend(position = "bottomleft", pal = pal.quantile, values = c(0,1),
              title = "Value",
              opacity = 1)

# Return map
return(map)
}

#### First a map of only the observed Dem Rep Ratios ---------------------------
m <- plotUSVotingData(DemRepRatiosAvailable)

# Show map
m

# Export
mapshot(m,'plots/MapAvailableCountyVotingOutcome.html', file='plots/MapAvailableCountyVotingOutcome.png')

#### Map of only OLS forecasted counties ---------------------------------------
names(DemRepRatiosOLSForecast) <- c('state', 'county', 'DemRepRatio')
DemRepRatiosOLSForecast$DemRepRatio <- as.numeric(DemRepRatiosOLSForecast$DemRepRatio)
m <- plotUSVotingData(DemRepRatiosOLSForecast)

# Show map
m

# Export
mapshot(m,'plots/OLSForecast.html', file='plots/OLSForecast.png')

#### Map of only XGB forecasted counties ---------------------------------------
names(DemRepRatiosXGBForecast) <- c('state', 'county', 'DemRepRatio')
DemRepRatiosXGBForecast$DemRepRatio <- as.numeric(DemRepRatiosXGBForecast$DemRepRatio)
m <- plotUSVotingData(DemRepRatiosXGBForecast)

# Show map
m

# Export
mapshot(m,'plots/XGBForecast.html', file='plots/XGBForecast.png')

#### Map of observed and forecasted counties -----------------------------------
names(DemRepRatiosXGBForecast) <- c('state', 'county', 'DemRepRatio')

# Combine observed with forecasts
DemRepRatiosFullMap <- rbind(DemRepRatiosXGBForecast[,c('state', 'county', 'DemRepRatio')], DemRepRatiosAvailable)

m_full <- plotUSVotingData(DemRepRatiosFullMap)

# Show map
m_full

# Save map for presentation
saveRDS(m_full, file='Pictures_presentation/MapComplete.rds')

# Export
mapshot(m,'plots/FullMap.html', file='plots/FullMap.png')














