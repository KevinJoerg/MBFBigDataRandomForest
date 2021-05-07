### XGBOOST ###
### Authors: Tim Graf, Kevin Jörg, Moritz Dänliker ###

"note: 
Xgboost manages only numeric vectors. Hence we convert all factors to spare matrix with binary format

For many machine learning algorithms, using correlated features is not a good idea. 
It may sometimes make prediction less accurate, and most of the time make interpretation of the model 
almost impossible. GLM, for instance, assumes that the features are uncorrelated.
Fortunately, decision tree algorithms (including boosted trees) are very robust to these features. 
Therefore we have nothing to do to manage this situation.

Decision trees do not require normalization of their inputs; 
and since XGBoost is essentially an ensemble algorithm comprised of decision trees, 
it does not require normalization for the inputs either.
"

library(libomp)
library(xgboost)
library(Matrix)
library(mlr)
library(parallel)
library(parallelMap) 
library(randomForest)
library(data.table)
library(dplyr)
library(tidyverse)
library(tictoc)
library(ff)
library(ffbase)
library(ffbase2)



### SETUP ### ----------------------------------------------

tic()

rm(list = ls())

# set wd to where the source file is
# make sure you have the datafiles in a /data/ folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the cleaned data
load.ffdf(dir='./ffdfClean2')

# convert to df
data <- data.frame(carListingsClean)

### DATA CLEANING ### ----------------------------------------------

# smaller dataset for trial and omit NA rows, that will else produce errors in the algorithm
data <- na.omit(data)

# transform
data$is_new = as.factor(data$is_new)   

data <- data[1:10000,]


### SPLIT TRAINING AND TESTING DATASET ### ----------------------------------------------

# set the seed to make your partition reproducible
set.seed(123)
smp_size <- floor(0.75 * nrow(data)) ## 75% of the sample size
train_ind <- base::sample(seq_len(nrow(data)), size = smp_size)


# Split the data into train and test
train <- data[train_ind,]
test <- data[-train_ind, ]

# define training label = dependent variable
train_target = as.matrix((train[,'DemRepRatio']))
test_target = as.matrix((test[,'DemRepRatio']))

# convert categorical factor into dummy variables using one-hot encoding
sparse_matrix_train <- sparse.model.matrix((DemRepRatio)~.-1, data = train)
sparse_matrix_test <- sparse.model.matrix((DemRepRatio)~.-1, data = test)

# check the dimnames crated by the one-hot encoding
sparse_matrix_train@Dimnames[[2]]

# Create a dense matrix for XGBoost
dtrain <- xgb.DMatrix(data = sparse_matrix_train, label = train_target)
dtest <- xgb.DMatrix(data = sparse_matrix_test, label=test_target)



# write a libsvm
write.libsvm <- function(data, target, filename="out.dat") {
  out <- file(filename)
  writeLines(paste(target, apply(data, 1, function( X ) paste(apply(cbind(which(X!=0), X[which(X!=0)]), 1, paste, collapse=":"), collapse=" "))), out)
  close( out )
}