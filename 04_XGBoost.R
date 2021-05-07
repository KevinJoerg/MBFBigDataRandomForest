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

# data <- data[1:10000,]


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

rm(data, train_ind, carListingsClean)
gc()


### MODEL 1: FIND OPTIMAL PARAMETERS ###  -----------------------------

set.seed(123)

# make a list of the optimal parameters, which we can aggregate later
params.list <- list()

# take a random sample of 10% of the original dataset for hyperparameter tuning
# we do this to avoid memory issues with huge grid sizes and limited computing power
train_subset = sample_n(train, 0.01*nrow(train))

# convert characters to factors
fact_col <- colnames(train_subset)[sapply(train_subset,is.character)]
for(i in fact_col) set(train_subset,j=i,value = factor(train_subset[[i]]))

# create tasks for learner
traintask <- makeRegrTask(data = data.frame(train_subset), target = 'DemRepRatio')

# create dummy features, as classif.xgboost does not support factors
traintask <- createDummyFeatures(obj = traintask)

# create learner
# fix number of rounds and eta 
lrn <- makeLearner("regr.xgboost", predict.type = "response")
lrn$par.vals <- list(objective="reg:squarederror",
                     eval_metric="rmse", 
                     nrounds=100L, 
                     eta = 0.1)

# set parameter space
# for computational reasons we only optimize the most important variables with are the booster type and the max depth per tree
params_xgb <- makeParamSet(makeDiscreteParam("booster", values = c("gbtree", "dart")), # gbtree and dart - use tree-based models, while glinear uses linear models
                           makeIntegerParam("max_depth",lower = 3L,upper = 10L), 
                           makeNumericParam("min_child_weight",lower = 1L,upper = 10L), 
                           makeNumericParam("subsample",lower = 0.2,upper = 1), 
                           makeNumericParam("colsample_bytree",lower = 0.1,upper = 1), 
                           makeDiscreteParam("eta", values = c(0.05, 0.1, 0.2)),
                           makeDiscreteParam("gamma", values = c(0, 0.2, 0.5, 0.7))
)

# set resampling strategy
# If you have many classes for a classification type predictive modeling problem or the classes are imbalanced (there are a lot more instances for one class than another), 
# it can be a good idea to create stratified folds when performing cross validation.
# however stratification is not supported for regression tasks so we set it to false
rdesc <- makeResampleDesc("CV",stratify = F, iters=5L)

# search strategy
# instead of a grid search we use a random search strategy to find the best parameters. 
ctrl <- makeTuneControlRandom(maxit = 10L) #maxit is the number of iterations for random search

# set parallel backend
parallelStartSocket(cpus = detectCores(), level = "mlr.tuneParams")

# parameter tuning
mytune <- tuneParams(learner = lrn, 
                     task = traintask, 
                     resampling = rdesc, 
                     par.set = params_xgb, 
                     control = ctrl, 
                     show.info = TRUE)

parallelStop()

# print the optimal parameters
mytune

gc()


### MODEL 2: FIND OPTIMAL ITERATIONS ###  -----------------------------

# take the parameters of mytune
params_xgb <- list(booster = mytune$x$booster, 
                   objective = "reg:squarederror",
                   eta=mytune$x$eta, # learning rate, usually between 0 and 1. makes the model more robust by shrinking the weights on each step
                   gamma=mytune$x$gamma, # regularization (prevents overfitting), higher means more penalty for large coef. makes the algo more conservative
                   subsample= mytune$x$subsample, # fraction of observations taken to make each tree. the lower the more conservative and more underfitting, less overfitting. 
                   max_depth = mytune$x$max_depth, # max depth of trees, the more deep the more complex and overfitting
                   min_child_weight = mytune$x$min_child_weight, # min number of instances per child node, blocks potential feature interaction and thus overfitting
                   colsample_bytree = mytune$x$colsample_bytree # number of variables per tree, typically between 0.5 - 0.9
                   ) 


# # test core methods
# xgbcore.time = list()
# cores = c(-1, 1, 16)
# for (i in 1:length(cores)) {
#   xgbcore.time[[i]] = system.time({
#       xgbcv <- xgb.cv(params = params_xgb,
#                     data = dtrain, 
#                     nrounds = 10L, 
#                     nfold = 5,
#                     showsd = T, # whether to show standard deviation of cv
#                     stratified = F, 
#                     print_every_n = 1, 
#                     n_jobs = cores[i],
#                     early_stopping_rounds = 50, # stop if we don't see much improvement
#                     maximize = F, # should the metric be maximized?
#                     verbose = 2, 
#                     tree_method = 'hist')
#     })
# }
# xgbcore.time

# # test tree-methods
# # note that there exists also "gpu_hist" but this only runs on CUDA-enable GPUs by NVIDIA
# tree_method = c('hist', 'exact')
# xgbtree.time = list()
# for (i in 1:length(tree_method)) {
#   xgbtree.time[[i]] = system.time({
#     xgbcv <- xgb.cv(params = params_xgb,
#                     data = dtrain, 
#                     nrounds = 10L, 
#                     nfold = 5,
#                     showsd = T, # whether to show standard deviation of cv
#                     stratified = F, 
#                     print_every_n = 1, 
#                     early_stopping_rounds = 50, # stop if we don't see much improvement
#                     maximize = F, # should the metric be maximized?
#                     verbose = 2, 
#                     tree_method = tree_method[i])
#   })
# }
# xgbtree.time

# using cross-validation to find optimal nrounds parameter
xgbcv <- xgb.cv(params = params_xgb,
                data = dtrain, 
                nrounds = 1000L, 
                nfold = 5,
                showsd = T, # whether to show standard deviation of cv
                stratified = F, 
                print_every_n = 1, 
                early_stopping_rounds = 50, # stop if we don't see much improvement
                maximize = F, # should the metric be maximized?
                verbose = 2, 
                tree_method = 'hist')

# Result of best iteration
xgb_best_iteration <- xgbcv$best_iteration

### MODEL 2: WITH CARET ###  -----------------------------
# 
# xgb_trcontrol = caret::trainControl(
#   method = "cv",
#   number = 2,  
#   allowParallel = TRUE,
#   verboseIter = TRUE,
#   returnData = FALSE,
#   search = 'random'
# )
# 
# xgbGrid <- expand.grid(nrounds = c(100,200),  # this is n_estimators in the python code above
#                        max_depth = c(10, 15, 20, 25),
#                        colsample_bytree = seq(0.5, 0.9, length.out = 5),
#                        ## The values below are default values in the sklearn-api. 
#                        eta = 0.1,
#                        gamma=0,
#                        min_child_weight = 1,
#                        subsample = 1
# )
# 
# set.seed(0) 
# 
# 
# xgb_model = caret::train(
#   dtrain, as.double(train_target),  
#   trControl = xgb_trcontrol,
#   tuneGrid = xgbGrid,
#   method = "xgbTree", 
#   objective = "reg:squarederror"
# )
# 
# 
# library(doParallel)
# cl <- makePSOCKcluster(5)
# registerDoParallel(cl)
# 
# parallelStartSocket(cpus = detectCores())
# parallelStop()
# 
# xgb_model2 = caret::train(
#   dtrain, as.double(train_target),  
#   trControl = xgb_trcontrol,
#   tuneGrid = xgbGrid,
#   method = "xgbTree", 
#   objective = "reg:squarederror", 
#   nthread = 16
# )
# 
# stopCluster(cl)

### MODEL 3: RUN WITH OPTIMAL PARAMETERS ###  -----------------------------
'Xgboost doesnt run multiple trees in parallel like you noted, you need predictions after each tree to update gradients.
Rather it does the parallelization WITHIN a single tree my using openMP to create branches independently'


# training with optimized nrounds and params
# best is to let out the num threads, as xgboost takes all by default
xgb3 <- xgb.train(params = params_xgb, 
                  data = dtrain, 
                  nrounds = xgb_best_iteration, 
                  watchlist = list(test = dtest, train = dtrain), 
                  maximize = F, 
                  eval_metric = "rmse", 
                  tree_method = 'hist') # this accelerates the process 



### ALTERNATIVE APPROACH ###--------------------------------------------

# # Create 10,000 rows with random hyperparameters
# 
# set.seed(20)
# 
# # Create empty lists
# lowest_error_list = list()
# parameters_list = list()
# 
# # create grid
# for (iter in 1:10){
#   param <- list(booster = "gbtree",
#                 objective = "reg:squarederror",
#                 max_depth = base::sample(3:10, 1),
#                 eta = runif(1, .01, .3),
#                 subsample = runif(1, .7, 1),
#                 colsample_bytree = runif(1, .6, 1),
#                 min_child_weight = base::sample(0:10, 1), 
#                 tree_method = 'hist'
#   )
#   parameters <- as.data.frame(param)
#   parameters_list[[iter]] <- parameters
# }
# 
# # Create object that contains all randomly created hyperparameters
# parameters_df = do.call(rbind, parameters_list)
# 
# # Use randomly created parameters to create 10,000 XGBoost-models
# for (row in 1:nrow(parameters_df)){
#   set.seed(20)
#   mdcv <- xgb.train(data=dtrain,
#                     booster = "gbtree",
#                     objective = "reg:squarederror",
#                     max_depth = parameters_df$max_depth[row],
#                     eta = parameters_df$eta[row],
#                     subsample = parameters_df$subsample[row],
#                     colsample_bytree = parameters_df$colsample_bytree[row],
#                     min_child_weight = parameters_df$min_child_weight[row],
#                     nrounds= 300,
#                     eval_metric = "error",
#                     early_stopping_rounds= 50,
#                     print_every_n = 10,
#                     watchlist = list(train= dtrain, val= dtest)
#   )
#   lowest_error <- as.data.frame(1 - min(mdcv$evaluation_log$val_error))
#   lowest_error_list[[row]] <- lowest_error
# }

### TESTING THE MODEL ###--------------------------------------------

# predict
xgb_pred_train <- predict(xgb3, dtrain)
xgb_pred_test <- predict(xgb3, dtest)

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
results_xgb <- data.frame(cbind(results_xgb_train, results_xgb_test))
colnames(results_xgb) <- c("train_xgb", "test_xgb")
rownames(results_xgb) <- c("RMSE", "R2", "ADJ_R2")

errors_xgb <- xgb_pred_test - test_target

# print results
results_xgb


# PLOTS --------------------------------------------------

# plot an example tree of all
xgb.plot.tree(feature_names = names(dtrain), 
              model = xgb3, 
              trees = 1)

# Plot importance
importance <- xgb.importance(feature_names = colnames(sparse_matrix_train), model = xgb3)
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
colnames(merged_df) <- c('xgb_pred_test', 'DemRepRatio')
merged_df <- merged_df[order(merged_df$DemRepRatio),]
merged_df$initialindex <- row.names(merged_df)
row.names(merged_df) <- NULL

# Plot predicted vs. actual 
colors <- c("actual" = "red", "predicted" = "blue")
plot_xgb <- ggplot(data = merged_df, aes(x = as.numeric(row.names(merged_df)))) +
  geom_point(aes(y = xgb_pred_test, color = 'predicted')) +
  geom_point(aes(y = DemRepRatio, color = 'actual')) +
  ggtitle('Actual vs. predicted values') + 
  scale_color_manual(values = colors) +
  labs(x = 'Index', y = 'DemRepRatio')
plot_xgb

# Plot most Top 1 variable vs. actual 
plot_v1 <- ggplot(data = data.frame(matrix_test)) +
  geom_point(aes(x = !!ensym(variable1), y = DemRepRatio)) +
  ggtitle(paste0('DemRepRatio vs. ', variable1))
plot_v1

# Plot most Top 2 variable vs. actual 
plot_v2 <- ggplot(data = data.frame(matrix_test)) +
  geom_point(aes(x = !!ensym(variable2), y = DemRepRatio)) +
  ggtitle(paste0('DemRepRatio vs. ', variable2))
plot_v2

# Plot most Top 3 variable vs. actual 
plot_v3 <- ggplot(data = data.frame(matrix_test)) +
  geom_point(aes(x = !!ensym(variable3), y = DemRepRatio)) +
  ggtitle(paste0('DemRepRatio vs. ', variable3))
plot_v3


# SAVE MODELS AND PLOTS -----------------------------
# 
# # save plot
# ggsave('plot_xgb_v1.png', path = './Plots/', plot = plot_v1, device = 'png')
# ggsave('plot_xgb_v2.png', path = './Plots/', plot = plot_v2, device = 'png')
# ggsave('plot_xgb_v3.png', path = './Plots/', plot = plot_v3, device = 'png')
# ggsave('plot_xgb.png', path = './Plots/', plot = plot_xgb, device = 'png')
# ggsave('plot_xgb_importance.png', path = './Plots/', plot = plot_xgb_importance, device = 'png')
# 
# # save model to local file
# xgb.save(xgb, "./Models/xgboost.model")
# 
# # save results
# save(results_xgb,file="./Models/results_xgboost.RData")
# 
# # save errors
# save(errors_xgb, file = "./Models/errors_xgb.RData")
# 
# # save parameters
# save(params_xgb, file = "./Models/params_xgb.RData")
# save(xgb_best_iteration, file = "./Models/xgb_best_iteration.RData")
# 
# 

toc()
