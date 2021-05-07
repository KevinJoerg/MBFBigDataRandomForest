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
load.ffdf(dir='./ffdfClean2')

### Some analysis ### -----------------------------------------

num_variables = c('city_fuel_economy', 'horsepower', 'length', 'maximum_seating', 'mileage', 'price', 'DemRepRatio')

carListings.onlynum <- data.frame(carListingsClean) %>% select(num_variables)


# correlation matrix
tic()
b <- cor(na.omit(data.frame(carListings.onlynum)))
toc()


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
gpuLm.defaultTol(useSingle = FALSE)

carListings.df <- as.data.frame(carListingsClean)

variablesOfInterest <- c('DemRepRatio', 'is_new', 'mileage', 'price', 'city_fuel_economy', 'horsepower', 'length', 'maximum_seating', 'body_type', 'make_name', 'state')

carListings.analyze <- carListings.df %>% select(variablesOfInterest)

pct <- lapply(carListings.analyze, function(x) table(x) / length(x))

addFactorOther <- function(x){
  if(is.factor(x)) return(factor(x, levels=c(levels(x), "Other")))
  return(x)
}

carListings.analyze <- as.data.frame(lapply(carListings.analyze, addFactorOther))

for (column in colnames(carListings.analyze)){
  if (is.factor(carListings.analyze[, column])){
    
  }
  else{
    carListings.analyze[,column] <- scale(carListings.analyze[,column])
  }
  
}

f <- 'DemRepRatio ~ .'
tic()
olsgpu <- gpuLm(f, carListings.analyze)
toc()
tic()
ols <- lm(f, carListings.analyze)
toc()

summary(olsgpu)
summary(ols)






pct <- lapply(carListings.analyze, function(x) table(x) / length(x))

addFactorOther <- function(x){
  if(is.factor(x)) return(factor(x, levels=c(levels(x), "Other")))
  return(x)
}

carListings.analyze <- as.data.frame(lapply(carListings.analyze, addFactorOther))

for (column in colnames(carListings.analyze)){
  if (is.factor(carListings.analyze[, column])){
    drop <- pct[[column]]
    drop <- names(drop[drop < 0.005])
    carListings.analyze[is.element(carListings.analyze[,column], drop), column] <- "Other"
  }
  else{
    carListings.analyze[,column] <- scale(carListings.analyze[,column])
  }
  
}


# carListings.analyze$engine_type <- NULL
# carListings.analyze$engine_cylinders <- NULL
# carListings.analyze$state <- NULL
# carListings.analyze$fuel_type <- NULL
# carListings.analyze$fuel_tank_volume <- NULL
# carListings.analyze$longitude <- NULL
# carListings.analyze$latitude <- NULL
# carListings.analyze$listed_date <- NULL
# carListings.analyze$maximum_seating <- NULL
# carListings.analyze$month <- NULL
# carListings.analyze$year <- NULL
# carListings.analyze$city_fuel_economy <- NULL
# carListings.analyze$highway_fuel_economy <- NULL
# carListings.analyze$wheelbase <- NULL
# carListings.analyze$front_legroom <- NULL
# carListings.analyze$back_legroom <- NULL
# carListings.analyze$height <- NULL
# carListings.analyze$width <- NULL

# Drop some variables
carListings.analyze$city <- NULL
carListings.analyze$county <- NULL

carListings.analyze <- droplevels(carListings.analyze)

# rm(carListingsClean, carListings.df, pct)
# gc()


f <- 'DemRepRatio ~ .'
tic()
olsgpu <- gpuLm(f, carListings.analyze)
toc()

tic()
ols <- lm(f, carListings.analyze)
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



