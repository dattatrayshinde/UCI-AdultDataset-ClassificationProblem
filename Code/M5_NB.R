#-------------------------------
#Gradient Boosting
#-------------------------------
source("../code/fe.R")

x_train$income <- y_train
x_test$income <- y_test


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
x_test[['income']] <- NULL
preds <- ifelse(predict(xgb.fit, newdata=as.matrix(x_test)) >= 0.5, 1, 0)
caret::confusionMatrix(y_test, preds)

# Overfitting?
xgb.fit = xgb.train(xgb_params_adult, dtrain, 700)
preds <- ifelse(predict(xgb.fit, newdata=as.matrix(x_test)) >= 0.5, 1, 0)
caret::confusionMatrix(y_test, preds, mode = "prec_recall")

##Cross Validation
cv <- xgb.cv(data = dtrain, nrounds = 3, nthread = 2, nfold = 5, metrics = list("rmse","auc"),
             max_depth = 3, eta = 1, objective = "binary:logistic")
print(cv)
print(cv, verbose=TRUE)
#--------------------------------------------------------------------------------
# Submission
#--------------------------------------------------------------------------------
test_dataset = read.csv("../data/test.csv")
head(test_dataset)
prob <- predict(xgb.fit,newdata = test_dataset)
predicted_trend <- ifelse(prob > 0.5,1,0)

filename  = paste('xgb_submission','.csv')
df =data.frame(test_dataset$Loan_ID,predicted_trend)
colnames(df)=c("Loan_Id","Loan_Status")
write.csv(df, file = filename)
#--------------------------------------------------------------------------------

