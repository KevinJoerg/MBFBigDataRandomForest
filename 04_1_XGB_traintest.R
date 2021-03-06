### XGBOOST ###
### Authors: Tim Graf, Kevin Jörg, Moritz Dänliker ###

"Note on running: 
This script may not work on computers with little RAM, as the hyperparameter tuning and xgboost use up alot of RAM. 
If it crashes please try the out-of-memory approach stated in 04_03_XGB_OOM. 

Difference to 04_02: 
This method does not include the variable state for predictions.

Note on XGBoost: 
* Xgboost manages only numeric vectors. Hence we convert all factors to spare matrix with binary format
* For many machine learning algorithms, using correlated features is not a good idea. 
It may sometimes make prediction less accurate, and most of the time make interpretation of the model 
almost impossible. GLM, for instance, assumes that the features are uncorrelated.
Fortunately, decision tree algorithms (including boosted trees) are very robust to these features. 
Therefore we have nothing to do to manage this situation.
* Decision trees do not require normalization of their inputs; 
and since XGBoost is essentially an ensemble algorithm comprised of decision trees, 
it does not require normalization for the inputs either.
"

# if you're using a MAC OS X
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
library(DiagrammeR)
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

carListingsClean <- data.frame(carListingsClean)

# for performance reasons
#carListingsClean <- carListingsClean[1:10000,]

# delete columns we don't need for the regression
carListingsClean$StateDemRepRatio <- NULL

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

# define training label = dependent variable
train_target = as.matrix((train[,c('DemRepRatio', 'county', 'state')]))
test_target = as.matrix((test[,c('DemRepRatio', 'county', 'state')]))

# omit variables we don't need
train$county <- NULL
test$county <- NULL
train$state <- NULL
test$state <- NULL

# convert categorical factor into dummy variables using one-hot encoding
sparse_matrix_train <- model.matrix((DemRepRatio)~.-1, data = train)
sparse_matrix_test <- model.matrix((DemRepRatio)~.-1, data = test)

# Create a dense matrix for XGBoost
dtrain <- xgb.DMatrix(data = sparse_matrix_train, label = train_target[, 'DemRepRatio'])
dtest <- xgb.DMatrix(data = sparse_matrix_test, label = test_target[, 'DemRepRatio'])

rm(train_ind, carListingsClean, smp_size)
gc()

colnames(sparse_matrix_train)
colnames(sparse_matrix_test)


### MODEL 1: FIND OPTIMAL PARAMETERS - WITH CARET ###  -----------------------------
"Note: This method uses a lot of memory, thus we do hyperparameter tuning on a subsample
change n to make it work on computers with less power"

# make a subsample
set.seed(123)
n = 0.10
smp_size <- floor(n * nrow(sparse_matrix_train)) 
train_ind <- base::sample(seq_len(nrow(sparse_matrix_train)), size = smp_size)
sparse_matrix_train_subsample <- sparse_matrix_train[train_ind,]
train_target_subsample <- as.numeric(train_target[train_ind, 'DemRepRatio'])


# set up parallelization
cl <- parallel::makePSOCKcluster(detectCores()-1)
parallel::clusterEvalQ(cl, library(foreach))
doParallel::registerDoParallel(cl)

# set how hyperparameters should be tested
xgb_trcontrol <- caret::trainControl(
  method = "cv",
  number = 2,
  allowParallel = TRUE,
  verboseIter = TRUE,
  returnData = FALSE,
  search = 'random', 
  trim = TRUE
)

# create the grid to test hyperparameters
xgbGrid <- base::expand.grid(nrounds = 100L, 
                             max_depth = c(4, 6, 8),
                             colsample_bytree = c(0.1, 0.3, 0.5),
                             eta = c(0.05, 0.1, 0.5),
                             gamma= 0.5,
                             min_child_weight = c(0.2, 0.4, 1),
                             subsample = c(0.1, 0.4, 0.7, 1)
)


# train the model with different hyperparameters
xgb_model = caret::train(
  sparse_matrix_train_subsample, as.double(train_target_subsample),
  trControl = xgb_trcontrol,
  tuneGrid = xgbGrid,
  method = "xgbTree",
  tree_method = 'hist',
  objective = "reg:squarederror", 
  tuneLength = 10000, 
)

# stop parallelization
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
params_xgb <- list(booster = 'dart',
                   objective = "reg:squarederror",
                   eta=xgb_model$bestTune$eta, # learning rate, usually between 0 and 1. makes the model more robust by shrinking the weights on each step
                   gamma= 0.8, # regularization (prevents overfitting), higher means more penalty for large coef. makes the algo more conservative
                   subsample= xgb_model$bestTune$subsample, # fraction of observations taken to make each tree. the lower the more conservative and more underfitting, less overfitting.
                   max_depth = xgb_model$bestTune$max_depth, # max depth of trees, the more deep the more complex and overfitting
                   min_child_weight = xgb_model$bestTune$min_child_weight, # min number of instances per child node, blocks potential feature interaction and thus overfitting
                   colsample_bytree = xgb_model$bestTune$colsample_bytree # number of variables per tree, typically between 0.5 - 0.9
)

# # using cross-validation to find optimal nrounds parameter (optional)
# xgbcv <- xgb.cv(params = params_xgb,
#                 data = dtrain, 
#                 nrounds = 150L, 
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
# xgb_best_iteration <- xgbcv$best_iteration


### MODEL 3: RUN WITH OPTIMAL PARAMETERS ###  -----------------------------
'Xgboost doesnt run multiple trees in parallel like you noted, you need predictions after each tree to update gradients.
Rather it does the parallelization WITHIN a single tree by using openMP to create branches independently'

# training with optimized nrounds and params
# best is to let out the num threads, as xgboost takes all by default
xgb <- xgb.train(params = params_xgb, 
                 data = dtrain, 
                 nrounds = 1000L, 
                 watchlist = list(test = dtest, train = dtrain), 
                 maximize = F, 
                early_stopping_rounds = 50, # stop if we don't see much improvement
                 eval_metric = "rmse", 
                 tree_method = 'hist') # this accelerates the process 


### TESTING THE MODEL ###--------------------------------------------

# predict
xgb_pred_train <- data.table(predict(xgb, dtrain))
xgb_pred_train <- cbind(xgb_pred_train, train_target)
colnames(xgb_pred_train) <- c('predicted', 'actual', 'county', 'state')

xgb_pred_test <- data.table(predict(xgb, dtest))
xgb_pred_test <- cbind(xgb_pred_test, test_target)
colnames(xgb_pred_test) <- c('predicted', 'actual', 'county', 'state')

# metrics for train
act <- as.numeric(train_target[,'DemRepRatio'])
pred <- xgb_pred_train$predicted
rmse_xgb_train <- sqrt(mean((pred - act)^2))
r2_xgb_train <- 1 - ( sum((act-pred)^2) / sum((act-mean(act))^2) )

# metrics for test
act <- as.numeric(test_target[,'DemRepRatio'])
pred <- xgb_pred_test$predicted
rmse_xgb_test <- sqrt(mean((pred - act)^2))
r2_xgb_test <- 1 - ( sum((act-pred)^2) / sum((act-mean(act))^2) )


### PLOTS --------------------------------------------------

# from wide to long dataframe needed for plotting
log <- xgb$evaluation_log %>% gather(key = 'dataset', value = 'RMSE', -iter)

# plot RMSE improvement 
plot_rmse <- ggplot(data = log, aes(x = iter, y = RMSE, color = dataset)) +
  geom_point() +
  xlab('iteration') +
  ggtitle('Return Mean Squared Error over iterations')
plot_rmse


# Plot importance
importance <- xgb.importance(feature_names = colnames(sparse_matrix_train), model = xgb)
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
merged_df <- data.frame(cbind(xgb_pred_test, test_target)) #by 0 merges based on index
merged_df <- merged_df[order(merged_df$DemRepRatio),]
head(merged_df)
merged_df <- merged_df %>% select(predicted, actual)
merged_df$actual <- as.numeric(merged_df$actual)
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


# SAVE MODELS AND PLOTS ----------------------------

# make dir
dir.create('./plots/')
dir.create('./models/')

# save plot
ggsave('plot_rmse.png', path = './Plots/', plot = plot_rmse, device = 'png')
ggsave('plot_xgb_v1.png', path = './Plots/', plot = plot_v1, device = 'png')
ggsave('plot_xgb_v2.png', path = './Plots/', plot = plot_v2, device = 'png')
ggsave('plot_xgb_v3.png', path = './Plots/', plot = plot_v3, device = 'png')
ggsave('plot_xgb.png', path = './Plots/', plot = plot_xgb, device = 'png')
ggsave('plot_xgb_importance.png', path = './Plots/', plot = plot_xgb_importance, device = 'png')

# save model to local file
xgb.save(xgb, "./models/xgboost.model")

# save results
fwrite(xgb_pred_train, file = "./models/xgb_pred_train.csv", row.names = FALSE)
fwrite(xgb_pred_test, file = "./models/xgb_pred_test.csv", row.names = FALSE)

# save parameters
save(params_xgb, file = "./models/xgb_params.RData")
save(xgb_best_iteration, file = "./models/xgb_best_iteration.RData")

# stop the timer
toc()
