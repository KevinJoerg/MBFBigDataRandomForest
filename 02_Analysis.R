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
              engine_type = 'factor', # This should be factor !!!!!!!!!!!!!!!!!
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

### Regression ### -----------------------------------------
library(biglm)
library(bigmemory)
library(biganalytics)
library(bigmemoryExtras)
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
# BiocManager::install("gputools")
# install.packages("gputools")

# Change to bigmemory type
carListings.df <- as.data.frame(carListingsClean)
carListings <- as.big.matrix(carListings.df, backingfile = 'carListings.bin', descriptorfile = 'carListings.bin.desc')

colnames(carListings)
# Variables that work alone
# back_legroom + body_type + city_fuel_economy + daysonmarket + engine_cylinders + engine_displacement + 
# franchise_dealer + front_legroom + fuel_tank_volume + fuel_type + height + highway_fuel_economy + horsepower + is_new + make_name + maximum_seating
# listing_color + mileage + price + savings_amount + torque + transmission + wheel_system + wheelbase + state + rpm
# factors c('body_type', 'engine_cylinders', 'fuel_type', 'listing_color', 'make_name', 'transmission', 'wheel_system', 'state')
# Variables that are super slow
# city, county

f <- 'DemRepRatio ~ back_legroom + body_type + city_fuel_economy + daysonmarket + engine_cylinders + engine_displacement + franchise_dealer + front_legroom + fuel_tank_volume + fuel_type + height + highway_fuel_economy + horsepower + is_new + make_name + maximum_seating + listing_color + mileage + price + savings_amount + torque + transmission + wheel_system + wheelbase + state + rpm'
f <- 'DemRepRatio ~ back_legroom + body_type + city_fuel_economy + daysonmarket + engine_cylinders + engine_displacement + franchise_dealer'

a <- biglm.big.matrix('DemRepRatio ~ body_type', data = carListings, fc = c('body_type'))
summary(a)

# carListings.df <- carListings.df %>% mutate_if(is.factor, as.character) 

c <- as.matrix(carListings.df)
carListings <- as.big.matrix(carListings.df, backingfile = 'carListings.bin', descriptorfile = 'carListings.bin.desc')
carListings <- filebacked.big.matrix()
b <- BigMatrixFactor(c, backingfile = 'carListingsFactor.bin')
# rm(carListingsClean, carListings.df)
gc()
colnames(carListings)
carListings[, 3]

a <- biglm.big.matrix('DemRepRatio ~ back_legroom + body_type', data = carListings)
summary(a)

# Remove rows with NA. Not needed for normal lm
carListings.short <- as.ffdf(na.omit(as.data.frame(carListingsClean)))

colnames(carListingsClean)

# Try a regression with many variables
summary(bigglm.ffdf(DemRepRatio ~ back_legroom + body_type + city, data = carListingsClean))

lm(DemRepRatio ~ ., data = carListings.short)

?bigglm.ffdf

ols <- bigglm(DemRepRatio ~ price + back_legroom + body_type + city, data = carListingsClean[], chunksize = 100000)

bigglm.ffdf(back_legroom ~ body_type, data = carListings.short)

data(trees)
x <- as.ffdf(trees)
a <- bigglm(log(Volume)~log(Girth)+log(Height), 
            data=x[], chunksize=10, sandwich=TRUE)




