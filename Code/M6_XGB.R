#-------------------------------
#Gradient Boosting
#-------------------------------
source("../code/fe.R")

x_train$income <- y_train
x_test$income <- y_test

########################################################
# Hyper parameter tuning with cross validation
########################################################
xgb_params_adult = list(
  colsample_bytree = 0.7,
  subsample = 0.7,
  eta = 0.075,
  objective = 'binary:logistic',
  max_depth = 6,
  num_parallel_tree = 1,
  min_child_weight = 1
)
res = xgb.cv(xgb_params_adult,
             dtrain,
             nrounds=700,
             nfold=3,
             early_stopping_rounds=15,
             print_every_n = 10,
             verbose=1)

# xgb.viz.cv(res)
# xgb.viz.cv(res[450:550, ])
# xgb.viz.cv(res[680:700, ])
# res[680:700, ]
# 
xgb.fit = xgb.train(xgb_params_adult, dtrain, 490)

# model <- xgb.dump(xgb.fit, with_stats = T)
# model[1:10] #This statement prints top 10 nodes of the model
# 
# feature.names <- colnames(x_train)[-13]
# importance_matrix <- xgb.importance(feature.names, model = xgb.fit)
# importance_matrix
# xgb.plot.importance(importance_matrix[1:10,])

# Confusion Matrix
# x_test[['income']] <- NULL
preds <- ifelse(predict(xgb.fit, newdata=as.matrix(x_test)) >= 0.5, 1, 0)
XGB.pred = prediction(preds, y_test)
XGB.perf = performance(XGB.pred, "tpr", "fpr")

caret::confusionMatrix(y_test, preds)

#Calculate the AUC value
perf_AUC=performance(XGB.pred,"auc") 
AUC=perf_AUC@y.values[[1]]
AUC


########################################################
# Hyper parameter tuning with cross validation
########################################################
best_param = list()
best_seednumber = 1234
best_auc = 0
best_auc_index = 0

for (iter in 1:100) {
  param <- list(objective = "binary:logistic",
                eval_metric = "auc",
                max_depth = sample(6:10, 1),
                eta = runif(1, .01, .3),
                gamma = runif(1, 0.0, 0.2), 
                subsample = runif(1, .6, .9),
                colsample_bytree = runif(1, .5, .8), 
                min_child_weight = sample(1:40, 1),
                max_delta_step = sample(1:10, 1)
  )
  cv.nround = 2000
  cv.nfold = 10
  seed.number = sample.int(10000, 1)[[1]]
  set.seed(seed.number)
  mdcv <- xgb.cv(data=as.matrix(train_new),label=as.matrix(target), params = param, nthread=10, 
                 nfold=cv.nfold, nrounds=cv.nround,
                 verbose = T, early_stopping_rounds=15, maximize=TRUE)
  
  max_auc = max(mdcv$evaluation_log[,test_auc_mean])
  max_auc_index = which.max(mdcv$evaluation_log[,test_auc_mean])
  print(paste("iteration",iter," started"))
  if (max_auc > best_auc) {
    best_auc = max_auc
    best_auc_index = max_auc_index
    best_seednumber = seed.number
    print(param)
    best_param = param
  }
}

nround = best_auc_index
best_param
best_seednumber
