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

### Filter outliers ### --------------------------------------------

num_variables = c('back_legroom', 'city_fuel_economy', 'engine_displacement', 'front_legroom', 'fuel_tank_volume', 'height', 'highway_fuel_economy', 
                  'horsepower', 'length', 'maximum_seating', 'mileage', 'price', 'savings_amount', 'seller_rating', 'torque', 'wheelbase', 'width', 
                  'year', 'DemRepRatio', 'rpm')

carListings.small <- subset.ffdf(carListingsClean, 
                                 select = num_variables)

carListings.df <- data.frame(carListingsClean)

# plot histograms with graphics
for (i in num_variables) {
  hist(carListings.df[[i]], main = paste0(i, ' Histogram'), xlab = i)
}

# filter outliers in savings_amount, prices, horsepower, and more
carListingsClean <- subset.ffdf(carListingsClean, c(savings_amount < 2500), drop = TRUE)
carListingsClean <- subset.ffdf(carListingsClean, price < 200000, drop = TRUE)
carListingsClean <- subset.ffdf(carListingsClean, mileage < 300000, drop = TRUE)
carListingsClean <- subset.ffdf(carListingsClean, horsepower < 600, drop = TRUE)
carListingsClean <- subset.ffdf(carListingsClean, highway_fuel_economy < 60, drop = TRUE)
carListingsClean <- subset.ffdf(carListingsClean, city_fuel_economy < 70, drop = TRUE)

# plot again histograms with graphics
carListings.df <- data.frame(carListingsClean)
for (i in num_variables) {
  hist(carListings.df[[i]], main = paste0(i, ' Histogram'), xlab = i)
}


### Some analysis ### -----------------------------------------



# hist.ff doesn't work?
hist.ff(carListingsClean[['back_legroom']], breaks = min(100, 1000))

# correlation matrix
cor(na.omit(data.frame(carListings.small)))

# simple regression 
carListings.df <- as.data.frame(carListingsClean)
regr <- lm(DemRepRatio ~ price + horsepower + height + factor(state), data = carListings.df)
summary(regr)

# plot residuals
plot(regr$residuals)

# Breusch Pagan test for heteroscedasticity
# If the test statistic has a p-value below an appropriate threshold (e.g. p < 0.05) 
# then the null hypothesis of homoskedasticity is rejected and heteroskedasticity assumed
bptest(regr)

# robust standard erros
regr.robust <- coeftest(regr, vcov = vcovHC(regr, type = "HC0"))

#plot residuals with qqplot
# The Q-Q plot, or quantile-quantile plot, is a graphical tool to help us assess if a set of data plausibly came from some theoretical distribution such as a Normal or exponential
qqnorm(regr$residuals, pch = 1, frame = FALSE)
qqline(regr$residuals, col = "steelblue", lwd = 2)

# check multicollinearity
ols_vif_tol(regr)

# Relative importance of independent variables in determining Y. How much
# each variable uniquely contributes to R2 over and above that which can be
# accounted for by the other predictors.
ols_correlations(regr)


### Regression ### -----------------------------------------
library(gputools)
library(tictoc)
# library(ade4)

carListings.df <- as.data.frame(carListingsClean)
# # carListings.df <- na.omit(carListings.df)
# carListings.df <- droplevels(carListings.df)
# 
# test.df <- acm.disjonctif(carListings.df[,c('body_type', 'fuel_type')])
# test.df$horsepower <- carListings.df$horsepower
# test.df$DemRepRatio <- carListings.df$DemRepRatio
# 
# # Find pct
# pct <- colSums(test.df) / nrow(test.df)
# 
# test.df[,pct < 0.01] <- NULL
# 
# # test.df$body_type. <- NULL
# # test.df$body_type.Convertible <- NULL
# # test.df$fuel_type. <- NULL
# # test.df$`fuel_type.Compressed Natural Gas` <- NULL
# # test.df$fuel_type.Propane <- NULL
# # test.df$fuel_type.Hybrid <- NULL
# 
# test.df <- droplevels(test.df)
# 
# # Other method
# # test.df <- carListings.df[,c('DemRepRatio', 'body_type', 'fuel_type', 'horsepower')]
test.df <- carListings.df

pct <- lapply(test.df, function(x) table(x) / length(x))

addFactorOther <- function(x){
  if(is.factor(x)) return(factor(x, levels=c(levels(x), "Other")))
  return(x)
}

test.df <- as.data.frame(lapply(test.df, addFactorOther))

for (column in colnames(test.df)){
  if (is.factor(test.df[, column])){
    drop <- pct[[column]]
    drop <- names(drop[drop < 0.005])
    test.df[is.element(test.df[,column], drop), column] <- "Other"
  }
  else{
    test.df[,column] <- scale(test.df[,column])
  }

}


# test.df$engine_type <- NULL
# test.df$engine_cylinders <- NULL
# test.df$state <- NULL
# test.df$fuel_type <- NULL
# test.df$fuel_tank_volume <- NULL
# test.df$longitude <- NULL
# test.df$latitude <- NULL
# test.df$listed_date <- NULL
# test.df$maximum_seating <- NULL
# test.df$month <- NULL
# test.df$year <- NULL
# test.df$city_fuel_economy <- NULL
# test.df$highway_fuel_economy <- NULL
# test.df$wheelbase <- NULL
# test.df$front_legroom <- NULL
# test.df$back_legroom <- NULL
# test.df$height <- NULL
# test.df$width <- NULL

# Drop some variables
test.df$city <- NULL
test.df$county <- NULL

test.df <- droplevels(test.df)

# rm(carListingsClean, carListings.df, pct)
# gc()


f <- 'DemRepRatio ~ .'
tic()
olsgpu <- gpuLm(f, test.df)
toc()

tic()
ols <- lm(f, test.df)
toc()

summary(olsgpu)
summary(ols)


# Check for multicolinearity
alias(ols)
alias(olsgpu)

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




