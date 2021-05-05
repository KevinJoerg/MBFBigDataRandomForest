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
rm(omit, column, count_nas, sorted)

# let us omit more variables we don't want
omit <- c('vin', 
          'city', 
          'description', 
          'dealer_zip', 
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
          'franchise_make', # as we already account for the brand
          'major_options', # too detailed
          'model_name' # too many options
)
for (column in omit) {carListingsClean[[column]] <- NULL}
  

# let us clean and convert the variables correctly
colnames <- c(back_legroom = 'numeric',
              body_type = 'factor', 
              city = 'character', 
              city_fuel_economy = 'numeric', 
              daysonmarket = 'numeric', 
              engine_cylinders = 'factor', # should be separated
              engine_displacement = 'numeric', # ?
              engine_type = 'factor', # This should be factor !!!!!!!!!!!!!!!!!
              franchise_dealer = 'boolean', 
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
carListingsClean <- (subset.ffdf(carListingsClean, listed_date > "2020-01-01", drop = TRUE))

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

# reduce numer of engine types
head(carListingsClean$engine_type)
list <- str_split(carListingsClean$engine_type[], " ")
carListingsClean$engine_type <- as.ff(as.factor(as.character(lapply(list, '[[', 1))))

# remove unnecessary variables
remove(list, rpm, column, colnames, count_nas, i, omit, sorted, testFunction)

# check if it worked
str(carListingsClean[])
nrow(carListingsClean)

num_variables = c('back_legroom', 'city_fuel_economy', 'engine_displacement', 'front_legroom', 'fuel_tank_volume', 'height', 'highway_fuel_economy', 
         'horsepower', 'length', 'maximum_seating', 'mileage', 'price', 'savings_amount', 'seller_rating', 'torque', 'wheelbase', 'width', 
         'year', 'DemRepRatio', 'rpm')
corr.df <- subset.ffdf(carListingsClean, 
                                 select = num_variables)

cor(na.omit(data.frame(corr.df)))



### Regression ### -----------------------------------------
library(tictoc)
library(gputools)
# gputools:
# Download from https://cran.r-project.org/src/contrib/Archive/gputools/
# Install manually
# Needs Nvidia GPU with installed CUDA toolkit

# The clean data are small enough to analyze in memory
carListings.df <- as.data.frame(carListingsClean)

# In order to avoid multicolinearity issues, summarize uncommon factors as "Other"

# First find the pct occurrence of all unique values
pct <- lapply(carListings.df, function(x) table(x) / length(x))

# Function that adds the factor "Other" to all columns that are factors
addFactorOther <- function(x){
  if(is.factor(x)) return(factor(x, levels=c(levels(x), "Other")))
  return(x)
}

# Apply function
carListings.df <- as.data.frame(lapply(carListings.df, addFactorOther))

# Change factor values that occur in less than 1% of cases to "Other"
for (column in colnames(carListings.df)){
  if (is.factor(carListings.df[, column])){
    drop <- pct[[column]]
    drop <- names(drop[drop < 0.01])
    carListings.df[is.element(carListings.df[,column], drop), column] <- "Other"
  }
  else{
    # Scale non factor columns
    carListings.df[,column] <- scale(carListings.df[,column])
  }

}

# Drop some variables
carListings.df$city <- NULL
carListings.df$county <- NULL

# Drop unused levels
carListings.df <- droplevels(carListings.df)

# Free memory
rm(carListingsClean, pct)
gc()

# Here run the regressions *****************************************************

# Regression equation
f <- 'DemRepRatio ~ .'

# Regression with CUDA GPU computing
tic()
olsgpu <- gpuLm(f, carListings.df)
toc()

# Normal CPU based regression
tic()
ols <- lm(f, carListings.df)
toc()

# Compare the two outputs
summary(olsgpu)
summary(ols)


# Check for multicolinearity
alias(ols)
alias(olsgpu)


# OLD, Backup, keep incase other solution than GPU based regression is desired
# 
# library(biglm)
# library(bigmemory)
# library(biganalytics)
# library(bigmemoryExtras)
# #if (!requireNamespace("BiocManager", quietly = TRUE))
# #  install.packages("BiocManager")
# # BiocManager::install("gputools")
# # install.packages("gputools")
# 
# # Change to bigmemory type
# carListings.df <- as.data.frame(carListingsClean)
# carListings <- as.big.matrix(carListings.df, backingfile = 'carListings.bin', descriptorfile = 'carListings.bin.desc')
# 
# colnames(carListings)
# # Variables that work alone
# # back_legroom + body_type + city_fuel_economy + daysonmarket + engine_cylinders + engine_displacement + 
# # franchise_dealer + front_legroom + fuel_tank_volume + fuel_type + height + highway_fuel_economy + horsepower + is_new + make_name + maximum_seating
# # listing_color + mileage + price + savings_amount + torque + transmission + wheel_system + wheelbase + state + rpm
# # factors c('body_type', 'engine_cylinders', 'fuel_type', 'listing_color', 'make_name', 'transmission', 'wheel_system', 'state')
# # Variables that are super slow
# # city, county
# 
# f <- 'DemRepRatio ~ back_legroom + body_type + city_fuel_economy + daysonmarket + engine_cylinders + engine_displacement + franchise_dealer + front_legroom + fuel_tank_volume + fuel_type + height + highway_fuel_economy + horsepower + is_new + make_name + maximum_seating + listing_color + mileage + price + savings_amount + torque + transmission + wheel_system + wheelbase + state + rpm'
# f <- 'DemRepRatio ~ back_legroom + body_type + city_fuel_economy + daysonmarket + engine_cylinders + engine_displacement + franchise_dealer'
# 
# a <- biglm.big.matrix('DemRepRatio ~ body_type', data = carListings, fc = c('body_type'))
# summary(a)
# 
# # carListings.df <- carListings.df %>% mutate_if(is.factor, as.character) 
# 
# c <- as.matrix(carListings.df)
# carListings <- as.big.matrix(carListings.df, backingfile = 'carListings.bin', descriptorfile = 'carListings.bin.desc')
# carListings <- filebacked.big.matrix()
# b <- BigMatrixFactor(c, backingfile = 'carListingsFactor.bin')
# # rm(carListingsClean, carListings.df)
# gc()
# colnames(carListings)
# carListings[, 3]
# 
# a <- biglm.big.matrix('DemRepRatio ~ back_legroom + body_type', data = carListings)
# summary(a)
# 
# # Remove rows with NA. Not needed for normal lm
# carListings.short <- as.ffdf(na.omit(as.data.frame(carListingsClean)))
# 
# colnames(carListingsClean)
# 
# # Try a regression with many variables
# summary(bigglm.ffdf(DemRepRatio ~ back_legroom + body_type + city, data = carListingsClean))
# 
# lm(DemRepRatio ~ ., data = carListings.short)
# 
# ?bigglm.ffdf
# 
# ols <- bigglm(DemRepRatio ~ price + back_legroom + body_type + city, data = carListingsClean[], chunksize = 100000)
# 
# bigglm.ffdf(back_legroom ~ body_type, data = carListings.short)
# 
# data(trees)
# x <- as.ffdf(trees)
# a <- bigglm(log(Volume)~log(Girth)+log(Height), 
#             data=x[], chunksize=10, sandwich=TRUE)
# 



