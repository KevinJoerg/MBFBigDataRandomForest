library(data.table)
library(ff)
library(ffbase)
library(pryr)
library(leaflet)
library(leaflet.extras)

# Set wd
setwd("~/Documents/MBF/2. Semester/BigData/MBFBigDataRandomForest")

# Change the original data *****************************************************

# There is an issue with reading the full file with read.csv.ffdf, removing the last 40 lines helps. Should look at this again

# # Original file
# carListings.path <- 'data/used_cars_data.csv'

# carListings <- fread(carListings.path)
# carListingsSample <- carListings[1:3000000, ]
# write.csv(carListingsSample, 'data/used_cars_data2.csv')

# Load the file with ff, prepare for loading with ff ***************************

# Load the file that is 40 lines shorter
carListings.path <- 'data/used_cars_data2.csv'

# create directory for ff chunks, and assign directory to ff 
system("mkdir ffdf")
options(fftempdir = "ffdf")

# Read in the file
carListings <- read.csv.ffdf(file= carListings.path,
                             VERBOSE=TRUE,
                             header=TRUE,
                             next.rows=100000,
                             colClasses=NA)

# Saving it this way names the files by colnames
save.ffdf(carListings, dir = './ffdf')

# Load voting data
votingdata.path <- 'data/PRESIDENT_precinct_general.csv'
# votingdata <-

# From here actually start *****************************************************
load.ffdf(dir='./ffdf')''
colnames(carListings)

mem_used()

# Create a map to show distributions of car listings 

# Leaflet needs numeric vectors
lat <- carListings[['latitude']][]
long <- carListings[['longitude']][]

samp <- sample(1:3000000, 10000)
lat <- lat[samp]
long <- long[samp]

# Simple visualization
map <- leaflet() %>%
  # Set view on center of properties
  setView(lng = mean(long), lat = mean(lat), zoom = 4) %>%
  # Add a custom base map
  addProviderTiles(providers$Stamen.TonerLite)

# Add the data points as circles on the map, also add a legend
map <- addCircles(map, lng = long,
                  lat = lat,
                  radius = 400,
                  stroke = F, fillOpacity = 0.45, fill = T, 
                  fillColor =  "red")

# Show the map
map













