library(data.table)
library(ff)
library(ffbase)
library(pryr)

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


# From here actually start *****************************************************
load.ffdf(dir='./ffdf')


mem_used()


