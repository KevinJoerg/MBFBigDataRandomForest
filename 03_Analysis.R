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
load.ffdf(dir='./ffdfClean2')

### Some analysis ### -----------------------------------------


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




