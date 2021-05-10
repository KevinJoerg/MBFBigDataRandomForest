library(data.table)
library(ff)
library(ffbase)
library(pryr)
library(dplyr)
library(tidyr)
library(biglm)
library(stringr)
library(lobstr)


### SETUP ### ------------------------------------------------

rm(list = ls())
gc()

# set wd to where the source file is
# make sure you have the datafiles in a /data/ folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Load the clean data
ffbase::load.ffdf(dir='./ffdfClean')


### DATA CLEANING WITH FFDF ### ------------------------------------------------

# convert all "" to NAs
carListingsClean <- update(carListingsClean, as.ffdf(carListingsClean[] %>% mutate_all(na_if,"")))

# quick plot of NAs
count_nas <- (as.numeric(lapply(physical(carListingsClean), FUN=function(x) sum(is.na(x))))/nrow(carListingsClean)) %>%
  'names<-'(names(carListingsClean))
sorted <- rev(sort(count_nas))
barplot(sorted, cex.names = 0.5, las = 2)
title(main = '% NAs in used cars data')

# drop all variables for which we have less than 80% observations (just not DemRepRatio)
omit <- names(which(sorted>=0.2))
omit <- omit[omit != 'DemRepRatio']
for (column in omit) {carListingsClean[[column]] <- NULL}
rm(omit, column, count_nas, sorted)

# let us omit more variables we don't want
omit <- c('vin', 
          'city', 
          'description', 
          'dealer_zip', 
          'franchise_dealer',
          'franchise_make', # as we already account for the brand
          'longitude', 'latitude', # as we already account for it in the dependent variable
          'listing_id',
          'main_picture_url', 
          'sp_id', 
          'sp_name', 
          'transmission_display', 
          'trimId', 
          'trim_name',
          'wheel_system_display', 
          'exterior_color', 'interior_color', # as we already account for other color
          'major_options', # too detailed
          'model_name' # too many options
)
for (column in omit) {carListingsClean[[column]] <- NULL}


# let us clean and convert the variables correctly
colnames <- c(back_legroom = 'numeric',
              body_type = 'factor', 
              city_fuel_economy = 'numeric', 
              daysonmarket = 'numeric', 
              engine_cylinders = 'factor', # should be separated
              engine_displacement = 'numeric', # ?
              engine_type = 'factor', # This should be factor !!!!!!!!!!!!!!!!!
              front_legroom  = 'numeric', 
              fuel_tank_volume  = 'numeric', 
              fuel_type = 'factor', 
              height = 'numeric', 
              highway_fuel_economy = 'numeric', 
              horsepower = 'numeric', 
              is_new  = 'boolean', 
              length = 'numeric', 
              listed_date = 'date', 
              listing_color  = 'factor', # could be taken as alternatives for colors
              make_name = 'factor', # either this or franchise_make
              maximum_seating = 'numeric', 
              mileage = 'numeric', 
              power = 'character', # should be split in multiple
              price = 'numeric', 
              savings_amount = 'numeric', # ?
              seller_rating = 'numeric', 
              torque = 'numeric', 
              transmission = 'factor', 
              wheel_system = 'factor', 
              wheelbase = 'character', # needs to be separated
              width = 'numeric', 
              year = 'numeric', 
              state = 'factor', 
              county = 'character', # too many, can we break it down?
              DemRepRatio = 'numeric',
              StateDemRepRatio = 'numeric'
)


# make conversions. Some need physical input []
for (i in colnames(carListingsClean)) {
  if (colnames[[i]] == 'numeric') {
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
rm(colnames, i)

# rename columns to make it clear where the data comes from
n <- colnames(carListingsClean)
n[n=='DemRep_state'] <- 'state'
n[n=='DemRep_county'] <- 'county'
names(carListingsClean) <- n
rm(n)

# check if it worked
str(carListingsClean[])

# filter out listings with listed date before 2020
carListingsClean <- (subset.ffdf(carListingsClean, listed_date > as.Date("2020-01-01"), drop = TRUE))

# filter out listings with days older than 300, we don't need to as the range is between 0 and 259
range(carListingsClean[['daysonmarket']])

# omit both columns
omit <- c('daysonmarket', 'listed_date')
for (column in omit) {carListingsClean[[column]] <- NULL}

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

# reduce number of engine types
head(carListingsClean$engine_type)
list <- str_split(carListingsClean$engine_type[], " ")
carListingsClean$engine_type <- as.ff(as.factor(as.character(lapply(list, '[[', 1))))

# remove unnecessary variables
remove(list, rpm, column, omit, testFunction)

# check if it worked
str(carListingsClean[])
nrow(carListingsClean)


### FILTER OUTLIERS ### --------------------------------------------

# define variables which are numeric
num_variables = c('back_legroom', 'city_fuel_economy', 'engine_displacement', 'front_legroom', 'fuel_tank_volume', 'height', 'highway_fuel_economy', 
                  'horsepower', 'length', 'maximum_seating', 'mileage', 'price', 'savings_amount', 'seller_rating', 'torque', 'wheelbase', 'width', 
                  'year', 'DemRepRatio', 'StateDemRepRatio', 'rpm')

carListings.df <- data.frame(carListingsClean) %>% select(num_variables)

# plot histograms with graphics
for (i in num_variables) {
  hist(carListings.df[[i]], main = paste0(i, ' Histogram'), xlab = i)
}

# filter outliers in savings_amount, prices, horsepower, and more. Keep observations with NA
carListingsClean <- subset.ffdf(carListingsClean, city_fuel_economy < 70 | is.na(city_fuel_economy), drop = TRUE)
carListingsClean <- subset.ffdf(carListingsClean, highway_fuel_economy < 60 | is.na(highway_fuel_economy), drop = TRUE)
carListingsClean <- subset.ffdf(carListingsClean, horsepower < 600 | is.na(horsepower), drop = TRUE)
carListingsClean <- subset.ffdf(carListingsClean, price < 200000, drop = TRUE)
carListingsClean <- subset.ffdf(carListingsClean, mileage < 300000, drop = TRUE)
carListingsClean <- subset.ffdf(carListingsClean, rpm > 2000 | is.na(rpm), drop = TRUE)
# carListingsClean <- subset.ffdf(carListingsClean, savings_amount < 2500, drop = TRUE) We don't care about this, is just advertising
carListingsClean <- subset.ffdf(carListingsClean, year > 1900, drop = TRUE)

# hist.ff only works with some??
hist(carListings.df[['back_legroom']])
hist.ff(carListingsClean[['back_legroom']]) # doesn't work
hist.ff(carListingsClean[['rpm']]) # works only after cleaning
hist.ff(carListingsClean[['year']])
hist.ff(carListingsClean[['price']]) 

# plot again histograms with graphics
carListings.df <- data.frame(carListingsClean)
for (i in num_variables) {
  hist(carListings.df[[i]], main = paste0(i, ' Histogram'), xlab = i)
}

# Now only select the relevant variables
variablesOfInterest <- c('DemRepRatio','StateDemRepRatio', 'is_new', 'mileage', 'price', 'city_fuel_economy', 'horsepower', 'length', 'maximum_seating', 'body_type', 'make_name', 'state')
carListingsClean <- carListingsClean[variablesOfInterest]

# New df is small enough to run in RAM
carListings.df <- as.data.frame(carListingsClean)

# Find percentage of cases of the factor variables
pct <- lapply(carListings.df, function(x) table(x) / length(x))

# Function to add level "Other"
addFactorOther <- function(x){
  if(is.factor(x)) return(factor(x, levels=c(levels(x), "Other")))
  return(x)
}

# Apply the function above to all columns
carListings.df <- as.data.frame(lapply(carListings.df, addFactorOther))

# For each factor column, summarize cases that occur with less than 1% frequency as Other
for (column in colnames(carListings.df)){
  if (is.factor(carListings.df[, column])){
    drop <- pct[[column]]
    drop <- names(drop[drop < 0.01])
    carListings.df[is.element(carListings.df[,column], drop), column] <- "Other"
  }
}

update(carListingsClean, carListings.df)

# Save this new clean2 ffdf
system("mkdir ffdfClean2")
save.ffdf(carListingsClean, dir = './ffdfClean2', overwrite = TRUE)



