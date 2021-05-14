### XGBOOST forecast ###
### Authors: Tim Graf, Kevin Jörg, Moritz Dänliker ###

"Note: 
Here we train and predict the data for counties where we currently do not have any DemRepRatios observed. 
At the time being of this paper, the data of Democrat / Republicans Ratio on a county level hasn't been published for every county in the US. 
Thus, we cannot assess the predictive power of the algorithm and need to wait for the official results to be publish. 
However, we therefore include the DemRepRatio on a state-level and use is as training for the algorithm and forecast. 
"

# if you're using a MAC OS X
# first install libomp by using the termin and the following command: 
# brew install libomp

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
library(ggplot2)
library(tidyr)
library(caret)
library(doParallel)


### SETUP ### ----------------------------------------------

tic()

rm(list = ls())

# set wd to where the source file is
# make sure you have the datafiles in a /data/ folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the cleaned data
load.ffdf(dir='./ffdfClean2')


### DATA CLEANING ### ----------------------------------------------

# we load here only data for which we don't yet observe the DemRepRatio on a county level
carListingsClean.forecast <- carListingsClean[is.na(carListingsClean$DemRepRatio), ]

# for performance reasons
carListingsClean <- carListingsClean[1:1000,]

# omit the NAs for XGBoost
carListingsClean <- na.omit(carListingsClean)

### SPLIT TRAINING AND TESTING DATASET ### ----------------------------------------------

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

# keep county for later
county_train <- data.frame(as.numeric(rownames(train)), as.character(train$state), as.character(train$county))
colnames(county_train) = c('index', 'state', 'county')
county_test <- data.frame(as.numeric(rownames(test)), as.character(test$state), as.character(test$county))
colnames(county_test) = c('index', 'state', 'county')

# convert categorical factor into dummy variables using one-hot encoding
sparse_matrix_train <- model.matrix(county~.-1, data = train)
sparse_matrix_test <- model.matrix(county~.-1, data = test)

# define training label = dependent variable
train_target = as.matrix((sparse_matrix_train[,'DemRepRatio']))
test_target = as.matrix((sparse_matrix_test[,'DemRepRatio']))

# omit DemRepRatio
sparse_matrix_train <- sparse_matrix_train[,-1 ]
sparse_matrix_test <- sparse_matrix_test[,-1 ]

# Create a dense matrix for XGBoost
dtrain <- xgb.DMatrix(data = sparse_matrix_train, label = train_target)
dtest <- xgb.DMatrix(data = sparse_matrix_test, label = test_target)

rm(train_ind, carListingsClean)
gc()


### MODEL 1: FIND OPTIMAL PARAMETERS - WITH CARET ###  -----------------------------
"Note: This method uses a lot of memory, thus we do hyperparameter tuning on a subsample
change n to make it work on computers with less power"

# make a subsample
n = 0.10
smp_size <- floor(n * nrow(sparse_matrix_train)) 
train_ind <- base::sample(seq_len(nrow(sparse_matrix_train)), size = smp_size)
sparse_matrix_train_subsample <- sparse_matrix_train[train_ind,]
train_target_subsample <- as.numeric(train_target[train_ind,])

stopCluster()
cl <- parallel::makePSOCKcluster(detectCores()-1)
parallel::clusterEvalQ(cl, library(foreach))
doParallel::registerDoParallel(cl)

xgb_trcontrol <- caret::trainControl(
  method = "cv",
  number = 2,
  allowParallel = TRUE,
  verboseIter = TRUE,
  returnData = FALSE,
  search = 'random', 
  trim = TRUE
)


xgbGrid <- base::expand.grid(nrounds = 100L, 
                             max_depth = c(4, 6, 8, 10),
                             colsample_bytree = c(0.1, 0.3, 0.5),
                             eta = c(0.05, 0.1, 0.5),
                             gamma= c(0.1, 0.3, 0.5),
                             min_child_weight = c(0.2, 0.4, 1),
                             subsample = c(0.1, 0.4, 0.7, 1)
)

set.seed(0)

xgb_model = caret::train(
  sparse_matrix_train_subsample, as.double(train_target_subsample),
  trControl = xgb_trcontrol,
  tuneGrid = xgbGrid,
  method = "xgbTree",
  tree_method = 'hist',
  objective = "reg:squarederror", 
  tuneLength = 10000L, 
)


stopCluster(cl)

### MODEL 1: FIND OPTIMAL PARAMETERS - WITH MLR ###  -----------------------------
"Note: This worked on a MacBook. However, it still consumes a lot of RAM and computing time"
# 
# # make the subsample
# n = 0.01
# smp_size <- floor(n * nrow(train)) 
# train_ind <- base::sample(seq_len(nrow(train)), size = smp_size)
# train_subsample <- train[train_ind,]
# train_target_subsample <- train_target[train_ind,]
# 
# 
# set.seed(123)
# 
# # create tasks for learner
# traintask <- makeRegrTask(data = data.frame(train), target = 'DemRepRatio')
# 
# # create dummy features, as classif.xgboost does not support factors
# traintask <- createDummyFeatures(obj = traintask)
# 
# # create learner
# # fix number of rounds and eta
# lrn <- makeLearner("regr.xgboost", predict.type = "response")
# lrn$par.vals <- list(objective="reg:squarederror",
#                      eval_metric="rmse",
#                      nrounds=100L,
#                      eta = 0.1)
# 
# # set parameter space
# # for computational reasons we only optimize the most important variables with are the booster type and the max depth per tree
# params_xgb <- makeParamSet(makeDiscreteParam("booster", values = c("gbtree", "dart")), # gbtree and dart - use tree-based models, while glinear uses linear models
#                            makeIntegerParam("max_depth",lower = 3L,upper = 10L),
#                            makeNumericParam("min_child_weight",lower = 1L,upper = 10L),
#                            makeNumericParam("subsample",lower = 0.2,upper = 1),
#                            makeNumericParam("colsample_bytree",lower = 0.1,upper = 1),
#                            makeDiscreteParam("eta", values = c(0.05, 0.1, 0.2)),
#                            makeDiscreteParam("gamma", values = c(0, 0.2, 0.5, 0.7))
# )
# 
# # set resampling strategy
# # If you have many classes for a classification type predictive modeling problem or the classes are imbalanced (there are a lot more instances for one class than another),
# # it can be a good idea to create stratified folds when performing cross validation.
# # however stratification is not supported for regression tasks so we set it to false
# rdesc <- makeResampleDesc("CV",stratify = F, iters=5L)
# 
# # search strategy
# # instead of a grid search we use a random search strategy to find the best parameters.
# ctrl <- makeTuneControlRandom(maxit = 100L) # maxit is the number of iterations for random search
# 
# # set parallel backend
# parallelStartSocket(cpus = detectCores(), level = "mlr.tuneParams")
# 
# # parameter tuning
# mytune <- tuneParams(learner = lrn,
#                      task = traintask,
#                      resampling = rdesc,
#                      par.set = params_xgb,
#                      control = ctrl,
#                      show.info = TRUE)
# 
# parallelStop()
# 
# # print the optimal parameters
# mytune
# 
# gc()
# 
# # take the parameters of mytune
# params_xgb <- list(booster = mytune$x$booster,
#                    objective = "reg:squarederror",
#                    eta=mytune$x$eta, # learning rate, usually between 0 and 1. makes the model more robust by shrinking the weights on each step
#                    gamma=mytune$x$gamma, # regularization (prevents overfitting), higher means more penalty for large coef. makes the algo more conservative
#                    subsample= mytune$x$subsample, # fraction of observations taken to make each tree. the lower the more conservative and more underfitting, less overfitting.
#                    max_depth = mytune$x$max_depth, # max depth of trees, the more deep the more complex and overfitting
#                    min_child_weight = mytune$x$min_child_weight, # min number of instances per child node, blocks potential feature interaction and thus overfitting
#                    colsample_bytree = mytune$x$colsample_bytree # number of variables per tree, typically between 0.5 - 0.9
#                    )

### MODEL 2: FIND OPTIMAL ITERATIONS ###  -----------------------------

# take the parameters of xgb_model
params_xgb_withStateDemRatio <- list(booster = 'dart',
                   objective = "reg:squarederror",
                   eta=xgb_model$bestTune$eta, # learning rate, usually between 0 and 1. makes the model more robust by shrinking the weights on each step
                   gamma= xgb_model$bestTune$gamma, # regularization (prevents overfitting), higher means more penalty for large coef. makes the algo more conservative
                   subsample= xgb_model$bestTune$subsample, # fraction of observations taken to make each tree. the lower the more conservative and more underfitting, less overfitting.
                   max_depth = xgb_model$bestTune$max_depth, # max depth of trees, the more deep the more complex and overfitting
                   min_child_weight = xgb_model$bestTune$min_child_weight, # min number of instances per child node, blocks potential feature interaction and thus overfitting
                   colsample_bytree = xgb_model$bestTune$colsample_bytree # number of variables per tree, typically between 0.5 - 0.9
)

# # using cross-validation to find optimal nrounds parameter
# xgbcv <- xgb.cv(params = params_xgb_withStateDemRatio,
#                 data = dtrain, 
#                 nrounds = 150, 
#                 nfold = 5,
#                 showsd = T, # whether to show standard deviation of cv
#                 stratified = F, 
#                 print_every_n = 1, 
#                 early_stopping_rounds = 50, # stop if we don't see much improvement
#                 maximize = F, # should the metric be maximized?
#                 verbose = 2, 
#                 tree_method = 'hist')
# 
# # Result of best iteration
# xgb_best_iteration_withStateDemRatio <- xgbcv$best_iteration


### MODEL 3: RUN WITH OPTIMAL PARAMETERS ###  -----------------------------
'Xgboost doesnt run multiple trees in parallel like you noted, you need predictions after each tree to update gradients.
Rather it does the parallelization WITHIN a single tree by using openMP to create branches independently'

# training with optimized nrounds and params
# best is to let out the num threads, as xgboost takes all by default
xgb_withStateDemRatio <- xgb.train(params = params_xgb_withStateDemRatio, 
                 data = dtrain, 
                 nrounds = 250L, 
                 watchlist = list(test = dtest, train = dtrain), 
                 maximize = F, 
                 early_stopping_rounds = 50L, # stop if we don't see much improvement
                 eval_metric = "rmse", 
                 tree_method = 'hist') # this accelerates the process 


### TESTING THE MODEL ###--------------------------------------------

# predict
xgb_pred_train <- predict(xgb_withStateDemRatio, dtrain)
xgb_pred_test <- predict(xgb_withStateDemRatio, dtest)

# metrics for train
rmse_xgb_train <- sqrt(mean((xgb_pred_train - train_target)^2))
r2_xgb_train <- 1 - ( sum((train_target-xgb_pred_train)^2) / sum((train_target-mean(train_target))^2) )
adj_r2_xgb_train <- 1 - ((1 - r2_xgb_train) * (nrow(train_target) - 1)) / (nrow(train_target) - ncol(train_target) - 1)

# metrics for test
rmse_xgb_test <- sqrt(mean((xgb_pred_test - test_target)^2))
r2_xgb_test <- 1 - ( sum((test_target-xgb_pred_test)^2) / sum((test_target-mean(test_target))^2) )
adj_r2_xgb_test <- 1 - ((1 - r2_xgb_test) * (nrow(test_target) - 1)) / (nrow(test_target) - ncol(test_target) - 1)

# combining results
results_xgb_train <- rbind(rmse_xgb_train, r2_xgb_train, adj_r2_xgb_train)
results_xgb_test <- rbind(rmse_xgb_test, r2_xgb_test, adj_r2_xgb_test)
results_xgb_withStateDemRatio <- data.frame(cbind(results_xgb_train, results_xgb_test))
colnames(results_xgb_withStateDemRatio) <- c("train_xgb", "test_xgb")
rownames(results_xgb_withStateDemRatio) <- c("RMSE", "R2", "ADJ_R2")

# print results
results_xgb_withStateDemRatio

str(train_target)
str(xgb_pred_train)

# merge predicted dataframe by the initial index
xgb_pred_train.df <- data.frame(train_target, xgb_pred_train)
xgb_pred_train.df$index <- as.numeric(rownames(xgb_pred_train.df))
xgb_pred_train.df <- left_join(xgb_pred_train.df, county_train, by = 'index')
xgb_pred_train.df <- xgb_pred_train.df %>% select(-index)
colnames(xgb_pred_train.df) <- c('actual', 'predicted', 'state', 'county')

# count number of forecast listings per state
forecast.count <- xgb_pred_train.df %>%
  group_by(state, county) %>%
  summarise(obs_per_forecast = length(predicted)) %>%
  ungroup

# Average the forecast values on a state and county level
xgb_pred_train.df <- xgb_pred_train.df %>%
  group_by(state, county) %>%
  summarise(forecast = mean(predicted, na.rm = TRUE), actual = mean(actual, na.rm = TRUE)) %>%
  ungroup

xgb_pred_train.df <- xgb_pred_train.df[forecast.count$obs_per_forecast > 100, ]


# merge predicted dataframe by the initial index
xgb_pred_test.df <- data.frame(test_target, xgb_pred_test)
xgb_pred_test.df$index <- as.numeric(rownames(xgb_pred_test.df))
xgb_pred_test.df <- left_join(xgb_pred_test.df, county_test, by = 'index')
xgb_pred_test.df <- xgb_pred_test.df %>% select(-index)
colnames(xgb_pred_test.df) <- c('actual', 'predicted', 'state', 'county')

# count number of forecast listings per state
forecast.count <- xgb_pred_test.df %>%
  group_by(state, county) %>%
  summarise(obs_per_forecast = length(predicted)) %>%
  ungroup

# Average the forecast values on a state and county level
xgb_pred_test.df <- xgb_pred_test.df %>%
  group_by(state, county) %>%
  summarise(forecast = mean(predicted, na.rm = TRUE), actual = mean(actual, na.rm = TRUE)) %>%
  ungroup

xgb_pred_test.df <- xgb_pred_test.df[forecast.count$obs_per_forecast > 100, ]


# PLOTS --------------------------------------------------

# from wide to long dataframe needed for plotting
log <- xgb_withStateDemRatio$evaluation_log %>% gather(key = 'dataset', value = 'RMSE', -iter)

# plot RMSE improvement 
plot_rmse <- ggplot(data = log, aes(x = iter, y = RMSE, color = dataset)) +
  geom_point() +
  xlab('iteration') +
  ggtitle('Return Mean Squared Error over iterations')
plot_rmse


# Plot importance
importance <- xgb.importance(feature_names = colnames(sparse_matrix_train), model = xgb_withStateDemRatio)
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

# define top 3 relevant variables
variable1 = xgb_importance$Feature[1]
variable2 = xgb_importance$Feature[2]
variable3 = xgb_importance$Feature[3]

# merge dataframes
merged_df <- data.frame(cbind(xgb_pred_test, test_target)) 
colnames(merged_df) <- c('predicted', 'actual')
merged_df <- merged_df[order(merged_df$actual),]
row.names(merged_df) <- NULL

# Plot predicted vs. actual 
colors <- c("actual" = "red", "predicted" = "blue")
plot_xgb <- ggplot(data = merged_df, aes(x = as.numeric(row.names(merged_df)))) +
  geom_point(aes(y = predicted, color = 'predicted')) +
  geom_point(aes(y = actual, color = 'actual')) +
  ggtitle('Actual vs. predicted values') + 
  scale_color_manual(values = colors) +
  labs(x = 'Index', y = 'DemRepRatio')
plot_xgb

# create datafrome for plotting
test_sparse <- model.matrix(~.-1, data = test)

# Plot most Top 1 variable vs. actual 
plot_v1 <- ggplot(data = data.frame(test_sparse)) +
  geom_point(aes(x = !!ensym(variable1), y = DemRepRatio)) +
  ggtitle(paste0('DemRepRatio vs. ', variable1))
plot_v1

# Plot most Top 2 variable vs. actual 
plot_v2 <- ggplot(data = data.frame(test_sparse)) +
  geom_point(aes(x = !!ensym(variable2), y = DemRepRatio)) +
  ggtitle(paste0('DemRepRatio vs. ', variable2))
plot_v2

# Plot most Top 3 variable vs. actual 
plot_v3 <- ggplot(data = data.frame(test_sparse)) +
  geom_point(aes(x = !!ensym(variable3), y = DemRepRatio)) +
  ggtitle(paste0('DemRepRatio vs. ', variable3))
plot_v3


### TESTING THE MODEL ON DATA WITH NO OBSERVATIONS FOR DEM-REP-RATIOS ###--------------------------------------------

gc()

# prepare dataframe before prediction
carListingsClean.forecast.dt <- data.table(data.frame(carListingsClean.forecast))
index <- as.numeric(seq(1:nrow(carListingsClean.forecast.dt)))
state <- as.character(carListingsClean.forecast.dt$state)
county <- as.character(carListingsClean.forecast.dt$county)
df <- data.frame(cbind(index, state, county))
df$index <- as.numeric(index)

# add and delete variables in ffdf
carListingsClean.forecast$index <- as.ff(index)
carListingsClean.forecast$county <- NULL
carListingsClean.forecast$DemRepRatio <- NULL

# create a sparse matrix, which we need as input for prediction
# note that we keep track of the index with "index_new" as the model.matrix omits factors for which there are no observations
# this results in a smaller dataframe
matrix_forecast <- model.matrix(~.-1, data = carListingsClean.forecast)
colnames(matrix_forecast)
index_new <- matrix_forecast[,'index']
matrix_forecast <- matrix_forecast[,-ncol(matrix_forecast)] 

# predict values
forecast <- data.table(predict(xgb_withStateDemRatio, matrix_forecast))
forecast$index <- index_new

# check if we get equal length
identical(nrow(matrix_forecast), length(index_new), nrow(forecast))

# merge both dataframes for later
xgb_forecast <- inner_join(forecast, df, by = 'index')
colnames(xgb_forecast) = c('forecast', 'index', 'state', 'county')

# drop index, as we don't need it anymore
xgb_forecast$index <- NULL

# count number of forecast listings per state
forecast.count <- xgb_forecast %>%
  group_by(state, county) %>%
  summarise(obs_per_forecast = length(forecast)) %>%
  ungroup

# Average the forecast values on a state and county level
xgb_forecast <- xgb_forecast %>%
  group_by(state, county) %>%
  summarise(forecast = mean(forecast, na.rm = TRUE)) %>%
  ungroup

# Only keep forecasts that are based on at least 100 observations
xgb_forecast <- xgb_forecast[forecast.count$obs_per_forecast > 100, ]

### SAVE ###--------------------------------------------


# make dir
dir.create('./plots/')
dir.create('./models/')

# save plot
ggsave('plot_rmse_withState.png', path = './Plots/', plot = plot_rmse, device = 'png')
ggsave('plot_xgb_v1_withState.png', path = './Plots/', plot = plot_v1, device = 'png')
ggsave('plot_xgb_v2_withState.png', path = './Plots/', plot = plot_v2, device = 'png')
ggsave('plot_xgb_v3_withState.png', path = './Plots/', plot = plot_v3, device = 'png')
ggsave('plot_xgb_withState.png', path = './Plots/', plot = plot_xgb, device = 'png')
ggsave('plot_xgb_importance_withState.png', path = './Plots/', plot = plot_xgb_importance, device = 'png')

# save model to local file
xgb.save(xgb_withStateDemRatio, "./models/xgboost_withState.model")

# save results
fwrite(xgb_forecast_train, file = "./models/xgb_pred_train_withState.csv", row.names = FALSE)
fwrite(xgb_forecast_test, file = "./models/xgb_pred_test_withState.csv", row.names = FALSE)
# save xgb model
xgb.save(xgb_withStateDemRatio, './models/xgb_model_withStateDemRatio')

# save xgb files
save(xgb_best_iteration_withStateDemRatio, file = './models/xgb_best_iteration_withStateDemRatio.RData')
save(xgb_withStateDemRatio, file = './models/xgb_withStateDemRatio.RData')
save(params_xgb_withStateDemRatio, file = "./models/xgb_params_withStateDemRatio.RData")
save(results_xgb_withStateDemRatio, file = "./models/xgb_results_withStateDemRatio.RData")

# save output
fwrite(xgb_forecast, file = './models/xgb_forecast.csv', row.names = FALSE)


toc()
