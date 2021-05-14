### XGBOOST Out-of-memory approach ###
### Authors: Tim Graf, Kevin Jörg, Moritz Dänliker ###

"Note: 
This approach uses an out-of-memory strategy, if the dataset is too large to load into RAM. 
However, some functions such as the parameter search and cross-validation is not possible. 
Therefore, this file needs input of the optimal parameters of the previous files (04_01_XGB_traintest). Please run that one first. 
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

# only for testing purposes
carListingsClean <- carListingsClean[1:1000,]


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
train_target = as.matrix(train[,'DemRepRatio'])
test_target = as.matrix(test[,'DemRepRatio'])

# convert categorical factor into dummy variables using one-hot encoding
matrix_train <- model.matrix(~.-1, data = train)
matrix_test <- model.matrix(~.-1, data = test)

# save the new sparse matrix as csv
fwrite(matrix_train, file = './data/train.csv', row.names = FALSE)
fwrite(matrix_test, file = './data/test.csv', row.names = FALSE)


### XGBOOST ### ----------------------------------------------
# Note that as we work with an out of memory approach, we cannot do cross-validation or a hyperparameter tuning as we did in 04_XGBoost. 
# we will just use the optimal parameters found from 04_XGBoost

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

# load parameters from 04_XGBoost
load('./models/xgb_params.RData')

# take the parameters of the loaded file
params <- list(booster = params_xgb$booster, 
                   objective = "reg:squarederror",
                   eta=params_xgb$eta, # learning rate, usually between 0 and 1. makes the model more robust by shrinking the weights on each step
                   gamma=params_xgb$gamma, # regularization (prevents overfitting), higher means more penalty for large coef. makes the algo more conservative
                   subsample= params_xgb$subsample, # fraction of observations taken to make each tree. the lower the more conservative and more underfitting, less overfitting. 
                   max_depth = params_xgb$max_depth, # max depth of trees, the more deep the more complex and overfitting
                   min_child_weight = params_xgb$min_child_weight, # min number of instances per child node, blocks potential feature interaction and thus overfitting
                   colsample_bytree = params_xgb$colsample_bytree # number of variables per tree, typically between 0.5 - 0.9
) 

# load best iteration
load('./models/xgb_best_iteration.RData')

# train xgboost
xgb <- xgb.train(data = dtrain, 
                 params = params,
                 nrounds = xgb_best_iteration, 
                 maximize = F, 
                 print_every_n = 1, 
                 watchlist = list(train = dtrain, test = dtest), 
                 early_stopping_rounds = 50,
                 verbose = 2, 
                 tree_method = 'hist')  

### TESTING THE MODEL ###--------------------------------------------

train_target <- getinfo(dtrain, name = 'label')
test_target <- getinfo(dtest, name = 'label')

# predict
xgb_pred_train <- predict(xgb, dtrain)
xgb_pred_test <- predict(xgb, dtest)

# metrics for train
rmse_xgb_train <- sqrt(mean((xgb_pred_train - train_target)^2))
r2_xgb_train <- 1 - ( sum((train_target-xgb_pred_train)^2) / sum((train_target-mean(train_target))^2) )
adj_r2_xgb_train <- 1 - ((1 - r2_xgb_train) * (length(train_target) - 1)) / (length(train_target) - ncol(matrix_train) - 1)

# metrics for test
rmse_xgb_test <- sqrt(mean((xgb_pred_test - test_target)^2))
r2_xgb_test <- 1 - ( sum((test_target-xgb_pred_test)^2) / sum((test_target-mean(test_target))^2) )
adj_r2_xgb_test <- 1 - ((1 - r2_xgb_test) * (length(test_target) - 1)) / (length(test_target) - ncol(matrix_test) - 1)

# combining results
results_xgb_train <- rbind(rmse_xgb_train, r2_xgb_train, adj_r2_xgb_train)
results_xgb_test <- rbind(rmse_xgb_test, r2_xgb_test, adj_r2_xgb_test)
results_xgb <- data.frame(cbind(results_xgb_train, results_xgb_test))
colnames(results_xgb) <- c("train_xgb", "test_xgb")
rownames(results_xgb) <- c("RMSE", "R2", "ADJ_R2")

errors_xgb <- xgb_pred_test - test_target

# print results
results_xgb


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
