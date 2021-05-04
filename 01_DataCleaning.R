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

# set wd to where the source file is
# make sure you have the datafiles in a /data/ folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


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

# From here actually start if data is prepared ********************************

# Load prepared car data (to continue from here)
load.ffdf(dir='./ffdf')

head(carListings)

# Load voting data
votingdata.path <- 'data/PRESIDENT_precinct_general.csv'
votingdata <- fread(votingdata.path)

# Aggregate voting data to county level democratic / republican ratio **********

# Add up votes of counties
votingdata.bycounty <- votingdata %>% 
  group_by(county_name, party_simplified) %>% 
  summarise(votes = sum(votes)) %>%
  ungroup

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
load.ffdf(dir='./ffdfClean')

#*******************************************************************************
# Data cleaning with ffdf

# convert all "" to NAs
carListingsClean <- update(carListingsClean, as.ffdf(carListingsClean[] %>% mutate_all(na_if,"")))

# quick plot of NAs
count_nas <- (as.numeric(lapply(physical(carListingsClean), FUN=function(x) sum(is.na(x))))/nrow(carListingsClean)) %>%
  'names<-'(names(carListingsClean))
sorted <- rev(sort(count_nas))
barplot(sorted, cex.names = 0.5, las = 2)
title(main = '% NAs in used cars data')

# drop all variables for which we have less than 80% observations
omit <- names(which(sorted>=0.2))
for (column in omit) {carListingsClean[[column]] <- NULL}

# let us clean and convert the variables correctly
colnames <- c(vin = 'character', 
              back_legroom = 'numeric',
              body_type = 'factor', 
              city = 'character', 
              city_fuel_economy = 'numeric', 
              daysonmarket = 'numeric', 
              dealer_zip = 'numeric',
              description = 'character',
              engine_cylinders = 'factor', # should be separated
              engine_displacement = 'numeric', # ?
              engine_type = 'numeric', # ?
              exterior_color = 'factor', # should be broken down
              franchise_dealer = 'boolean', 
              franchise_make = 'factor', # either this or make_name
              front_legroom  = 'numeric', 
              fuel_tank_volume  = 'numeric', 
              fuel_type = 'factor', 
              height = 'numeric', 
              highway_fuel_economy = 'numeric', 
              horsepower = 'numeric', 
              interior_color = 'character', # should be broken down
              is_new  = 'boolean', 
              latitude  = 'numeric', 
              length = 'numeric', 
              listed_date = 'date', 
              listing_color  = 'factor', # could be taken as alternatives for colors
              listing_id  = 'numeric', 
              longitude = 'numeric', 
              main_picture_url = 'character', 
              major_options = 'character', # should be separated
              make_name = 'factor', # either this or franchise_make
              maximum_seating = 'numeric', 
              mileage = 'numeric', 
              model_name = 'character', # too many
              power = 'character', # should be split in multiple
              price = 'numeric', 
              savings_amount = 'numeric', # ?
              seller_rating = 'numeric', 
              sp_id = 'numeric', 
              sp_name = 'character', 
              torque = 'numeric', 
              transmission = 'factor', 
              transmission_display = 'character', # can be omitted as we already have transmission 
              trimId = 'numeric', 
              trim_name = 'character', 
              wheel_system = 'factor', 
              wheel_system_display = 'factor', # can be omitted 
              wheelbase = 'character', # needs to be separated
              width = 'numeric', 
              year = 'numeric', 
              state = 'factor', 
              county = 'character', # too many
              DemRepRatio = 'numeric'
)

# make conversions. Some need physical input []
for (i in colnames(carListingsClean)) {
  if (colnames[i][[1]] == 'numeric') {
    carListingsClean[[i]] <- as.ff(as.numeric(carListingsClean[[i]][]))
  } else if (colnames[[i]] == 'factor'){
    carListingsClean[[i]] <- as.ff(as.factor(carListingsClean[[i]][]))
  } else if (colnames[[i]] == 'boolean'){
    carListingsClean[[i]] <- as.ff(as.boolean(carListingsClean[[i]][]))
  } else if (colnames[[i]] == 'character'){
    # Character does not exist, needs to be factor
    carListingsClean[[i]] <- as.ff(as.factor(carListingsClean[[i]][]))
  } else if (colnames[[i]] == 'date'){
    carListingsClean[[i]] <- as.ff(as.Date(carListingsClean[[i]][]))
  }
}

# check if it worked
str(carListingsClean[])

# let us omit more variables we don't want
omit <- c('vin', 'description', 'dealer_zip', 'listing_id', 'main_picture_url', 'sp_id', 'sp_name', 
          'transmission_display',  'trimId', 'trim_name', 'wheel_system_display', 
          'exterior_color', 'interior_color', # as we already account for other color
          'franchise_make', # as we already account for the brand
          'major_options', # too detailed
          'model_name' # too many options
)
for (column in omit) {carListingsClean[[column]] <- NULL}

# rename columns to make it clear where the data comes from
n <- colnames(carListingsClean)
n[n=='DemRep_state'] <- 'state'
n[n=='DemRep_county'] <- 'county'
names(carListingsClean) <- n
remove(n)

# separate variable "power" into "hp" and into "RPM"
list <- (str_split(carListingsClean$power[], " "))
carListingsClean$hp <- as.ff(as.numeric(lapply(list, '[[', 1)))

# but these are almost the same, so we can delete $hp
identical(carListingsClean[['horsepower']],carListingsClean[['hp']])
tail(carListingsClean$horsepower)
tail(carListingsClean$hp)
head(carListingsClean$horsepower)
head(carListingsClean$hp)
carListingsClean$hp <- NULL

# as lapply doesn't work with errors, we write our own function to ignore those errors
testFunction <- function (x) {
  return(tryCatch("[["(x, 4), error=function(e) NULL))}

# Get RPM from the list of the power column
rpm <- lapply(list, testFunction)
carListingsClean$rpm <- as.ff(as.numeric(str_replace(rpm, ",", "")))
carListingsClean$power <- NULL

# separate number from ".. in" in the variable "wheelbase"
head(carListingsClean$wheelbase)
list <- str_split(carListingsClean$wheelbase[], " in")
carListingsClean$wheelbase <- as.ff(as.numeric(lapply(list, '[[', 1)))

# simplify engine_cylinders by allowing for less variations
head(carListingsClean$engine_cylinders)
list <- str_split(carListingsClean$engine_cylinders[], " ")
carListingsClean$engine_cylinders <- as.ff(as.factor(as.character(lapply(list, '[[', 1))))

# from Date to Month and Year
carListingsClean$month <- as.ff(month(carListingsClean$listed_date[]))
carListingsClean$year <- as.ff(year(carListingsClean$listed_date[]))

# remove unnecessary variables
remove(list, rpm, column, colnames, count_nas, i, omit, sorted, testFunction)

## Analysis ********************************************************************
# 
# # Load the clean data
# rm(list=ls())
# load.ffdf(dir='./ffdfClean')
# 
# # Create a map of the listings, to show the distribution
# 
# # Leaflet needs numeric vectors
# map.lat <- carListingsClean[['latitude']][]
# map.long <- carListingsClean[['longitude']][]
# map.ratios <- carListingsClean[['DemRepRatio']][]
# 
# class(map.ratios)
# 
# # Only show 10k random listings, for performance
# samp <- sample(1:length(map.lat), 10000)
# map.lat <- map.lat[samp]
# map.long <- map.long[samp]
# map.ratios <- map.ratios[samp]
# 
# # Create a color range for the markers
# pal.quantile <- colorQuantile("RdYlBu", 
#                               domain =  map.ratios, reverse = FALSE, n = 10)
# colors.quant <- pal.quantile(map.ratios)
# 
# # Simple visualization
# map <- leaflet() %>%
#   # Set view on center of listings
#   setView(lng = (max(map.long) + min(map.long)) / 2, lat = (max(map.lat) + min(map.lat)) / 2, zoom = 3) %>%
#   # Add a custom base map
#   addProviderTiles(providers$Stamen.TonerLite)
# 
# # Add the data points as circles on the map, also add a legend
# map <- addCircles(map, lng = map.long,
#                   lat = map.lat,
#                   radius = 400,
#                   stroke = F, fillOpacity = 0.45, fill = T, 
#                   fillColor =  colors.quant) %>%
#   addLegend(pal = pal.quantile, values = map.ratios, opacity = 1, 
#             title = "Dem to Rep Vote Ratio")
# 
# # Show the map
# map




