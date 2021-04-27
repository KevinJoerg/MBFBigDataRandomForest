library(data.table)
library(ff)
library(ffbase)
library(pryr)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(tidyr)
library(maps)

# Package to map lat long to county
# source("https://raw.githubusercontent.com/MangoTheCat/remotes/master/install-github.R")$value("mangothecat/remotes")
# remotes::install_github("JVAdams/jvamisc")
library(jvamisc)

rm(list = ls())

wd_kevin <- '~/Documents/MBF/2. Semester/BigData/MBFBigDataRandomForest'

# Set wd
setwd(wd_kevin)


# Prepare for loading with ff, read in Used Car CSV ****************************

# Original file
carListings.path <- 'data/used_cars_data.csv'

# create directory for ff chunks, and assign directory to ff 
system("mkdir ffdf")
options(fftempdir = "ffdf")

# Read in the file. Only 3'000'000, as there is an issue in the last 40 rows
carListings <- read.csv.ffdf(file= carListings.path,
                             nrows=3000000,
                             VERBOSE=TRUE,
                             header=TRUE,
                             next.rows=100000,
                             colClasses=NA)

# Saving it this way names the files by colnames
save.ffdf(carListings, dir = './ffdf', overwrite = TRUE)

# From here actually start if data are prepared ********************************

# Load prepared car data (to continue from here)
load.ffdf(dir='./ffdf')

# Load voting data
votingdata.path <- 'data/PRESIDENT_precinct_general.csv'
votingdata <- fread(votingdata.path)

# Aggregate voting data to county level democratic / republican ratio **********

# Add up votes of counties
votingdata.bycounty <- votingdata %>% group_by(county_name, party_simplified) %>% 
  summarise(votes = sum(votes)) %>% ungroup

# Unaffiliated candidates have empty label, change to NOPARTY
votingdata.bycounty[votingdata.bycounty[, 'party_simplified'] == "", 'party_simplified'] <- 'NOPARTY'

# Pivot the data to have columns for the different parties
votingdata.bycounty.byparty <- pivot_wider(votingdata.bycounty, id_cols = 'county_name', names_from ='party_simplified', values_from = 'votes', names_repair = "check_unique")

# NA entry means 0 votes
votingdata.bycounty.byparty[is.na(votingdata.bycounty.byparty)] <- 0

# Calculate ratio between democratic and republican votes
votingdata.ratio <- data.frame(matrix(c(votingdata.bycounty.byparty$county_name, votingdata.bycounty.byparty$DEMOCRAT / (votingdata.bycounty.byparty$DEMOCRAT + votingdata.bycounty.byparty$REPUBLICAN)), ncol = 2)) %>%
  'names<-'(c('county', 'DemocratRepublicanRatio'))

# Change county names to lower
votingdata.ratio$county <- unlist(lapply(votingdata.ratio$county, tolower))

# Assign county to used car observations ***************************************

# Create normal data frame with coordinates of listings
lat <- carListings[['latitude']][]
long <- carListings[['longitude']][]
coordinates <- data.frame(matrix(c(long, lat), ncol = 2)) %>%
  'names<-'(c('long', 'lat'))

# Use function latlong2 of jvamisc package to get county of coordinates
counties_of_coordinates <- latlong2(coordinates, to = 'county')
counties_of_coordinates <- data.frame(counties_of_coordinates) %>%
  'names<-'(c('state', 'county'))

# Join voting ratio to car data by county, append columns to ffdf
counties_of_cord_with_ratio <- left_join(counties_of_coordinates, votingdata.ratio, by='county')
carListings$state <- as.ff(factor(counties_of_cord_with_ratio[, 1]))
carListings$county <- as.ff(factor(counties_of_cord_with_ratio[, 2]))
carListings$DemRepRatio <- as.ff(as.numeric(counties_of_cord_with_ratio[, 3]))

# Clean ffdf that only contains listings combined with voting data
carListingsClean <- carListings[!is.na(carListings$DemRepRatio)]

# Save this clean ffdf
system("mkdir ffdfClean")
save.ffdf(carListingsClean, dir = './ffdfClean', overwrite = TRUE)

## Analysis ********************************************************************

# Load the clean data
rm(list=ls())
load.ffdf(dir='./ffdfClean')

# Create a map of the listings, to show the distribution

# Leaflet needs numeric vectors
map.lat <- carListingsClean[['latitude']][]
map.long <- carListingsClean[['longitude']][]
map.ratios <- carListingsClean[['DemRepRatio']][]

class(map.ratios)

# Only show 10k random listings, for performance
samp <- sample(1:length(map.lat), 10000)
map.lat <- map.lat[samp]
map.long <- map.long[samp]
map.ratios <- map.ratios[samp]

# Create a color range for the markers
pal.quantile <- colorQuantile("RdYlBu", 
                              domain =  map.ratios, reverse = FALSE, n = 10)
colors.quant <- pal.quantile(map.ratios)

# Simple visualization
map <- leaflet() %>%
  # Set view on center of listings
  setView(lng = (max(map.long) + min(map.long)) / 2, lat = (max(map.lat) + min(map.lat)) / 2, zoom = 3) %>%
  # Add a custom base map
  addProviderTiles(providers$Stamen.TonerLite)

# Add the data points as circles on the map, also add a legend
map <- addCircles(map, lng = map.long,
                  lat = map.lat,
                  radius = 400,
                  stroke = F, fillOpacity = 0.45, fill = T, 
                  fillColor =  colors.quant) %>%
  addLegend(pal = pal.quantile, values = map.ratios, opacity = 1, 
            title = "Dem to Rep Vote Ratio")

# Show the map
map




