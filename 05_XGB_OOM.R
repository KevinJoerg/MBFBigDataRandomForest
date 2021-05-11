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

# first install libomp by using the termin and the following command: 
# brew install libomp

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

# start timer
tic()

rm(list = ls())

# set wd to where the source file is
# make sure you have the datafiles in a /data/ folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the cleaned data
load.ffdf(dir='./ffdfClean2')


### DATA CLEANING ### ----------------------------------------------

# delete columns we don't need for the regression
carListingsClean$county <- NULL
carListingsClean$state <- NULL
carListingsClean$StateDemRepRatio <- NULL

# omit the NAs for XGBoost
carListingsClean <- na.omit(carListingsClean)

carListingsClean <- carListingsClean[1:10000,]

### SPLIT TRAINING AND TESTING DATASET ### ----------------------------------------------
# to clean and modify the data we can load it into RAM as it is small enough
# for XGBoost we will do the computation out of memory

# set the seed to make your partition reproducible
set.seed(123)
smp_size <- floor(0.75 * nrow(carListingsClean)) ## 75% of the sample size
train_ind <- base::sample(seq_len(nrow(carListingsClean)), size = smp_size)

# Split the data into train and test
train <- carListingsClean[train_ind,]
test <- carListingsClean[-train_ind, ]

# convert to factor
train$is_new <- as.factor(train$is_new)
test$is_new <- as.factor(test$is_new)

# define training label = dependent variable
train_target = as.matrix((train[,'DemRepRatio']))
test_target = as.matrix((test[,'DemRepRatio']))

# convert categorical factor into dummy variables using one-hot encoding
matrix_train <- model.matrix(~.-1, data = train)
matrix_test <- model.matrix(~.-1, data = test)

# save the new sparse matrix as csv
fwrite(matrix_train, file = './data/train.csv', row.names = FALSE)
fwrite(matrix_test, file = './data/test.csv', row.names = FALSE)




### XGBOOST ### ----------------------------------------------

# remove already present cache
file.remove('dtrain.cache.row.page')
file.remove('dtrain.cache')
file.remove('dtest.cache.row.page')
file.remove('dtest.cache')
gc()

# load data as out-of-memory for XGBoost
# the #dtrain.cache specifies that it should be loaded out of memory
dtrain = xgb.DMatrix(data = './data/train.csv?format=csv&label_column=0#dtrain.cache') 
dtest = xgb.DMatrix(data = './data/test.csv?format=csv&label_column=0#dtest.cache') 

# set the parameter
params_xgb <- list(booster = 'dart', 
                   objective = "reg:squarederror",
                   max_depth = 5,
                   eta= 0.1, 
                   eval_metric = "rmse"
                   ) 

# train xgboost
xgb <- xgb.train(data = dtrain, 
                 params = params_xgb,
                 nrounds = 100L, 
                 maximize = F, 
                 print_every_n = 1, 
                 watchlist = list(train = dtrain, test = dtest), 
                 early_stopping_rounds = 50,
                 verbose = 2, 
                 tree_method = 'hist')  


### PLOTS ### --------------------------------------------------

# from wide to long dataframe needed for plotting
log <- xgb$evaluation_log %>% gather(key = 'dataset', value = 'RMSE', -iter)

# plot improvement 
plot_rmse <- ggplot(data = log, aes(x = iter, y = RMSE, color = dataset)) +
  geom_point() +
  xlab('iteration') +
  ggtitle('Return Mean Squared Error over iterations')
plot_rmse

# check importance plot
importance <- xgb.importance(feature_names = colnames(matrix_train), model = xgb)
xgb_importance <- xgb.plot.importance(importance_matrix = importance, top_n = 15)
plot_xgb_importance <- xgb_importance %>%
  mutate(Feature = fct_reorder(Feature, Importance)) %>%
  ggplot(aes(x=Feature, y=Importance)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw() +
  ggtitle('Feature Importance Plot for XG-Boost')
plot_xgb_importance


# end timer
toc()
