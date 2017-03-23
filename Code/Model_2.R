source("../code/fe.R")

## Random Forest
x_train[['income']] <- NULL
x_test[['income']] <- NULL

# Tuning takes factors as target variables
bestmtry <- tuneRF(x_train, as.factor(y_train),
                   ntreeTry=100, stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)

x_train$income <- as.factor(y_train)
x_test$income <- as.factor(y_test)
rf.fit <- randomForest(income ~ ., data=x_train,
                       mtry=2, ntree=1000, keep.forest=TRUE, importance=TRUE, test=x_test)
rf.preds = predict(rf.fit, type="prob", newdata=x_test)[,2]

rf.pred = prediction(rf.preds, x_test$income)
rf.perf = performance(rf.pred, "tpr", "fpr")

#importance(rf.fit)
varImpPlot(rf.fit)

# Confusion Matrix
preds <- ifelse(predict(rf.fit, type="prob", newdata=x_test)[,2] >= 0.5, 1, 0)
table(x_test$income, preds)
caret::confusionMatrix(x_test$income, preds, mode = "prec_recall")

################################## CART Trees
set.seed(50)
mycontrol = rpart.control(cp = 0, xval = 10)
tree.fit = rpart(income ~ ., method = "class", data = x_train, control = mycontrol)
tree.fit$cptable
tree.cptarg = sqrt(tree.fit$cptable[8,1]*tree.fit$cptable[9,1])
tree.prune = prune(tree.fit, cp=tree.cptarg)
tree.preds = predict(tree.prune, newdata=x_test, type="prob")[,2]
tree.pred = prediction(tree.preds, x_test$income)
tree.perf = performance(tree.pred, "tpr", "fpr")

# Confusion Matrix
caret::confusionMatrix(x_test$income, predict(tree.fit, newdata=x_test, type="class"),
                       mode = "prec_recall")


################################## Neural Network
nnet.fit = nnet(income ~ ., data=x_train, size=20, maxit=10000, decay=.001)
nnet.preds = predict(nnet.fit, newdata=x_test, type="raw")
nnet.pred = prediction(nnet.preds, x_test$income)
nnet.perf = performance(nnet.pred, "tpr", "fpr")
# A good rule of thumb is to set it between the number of input nodes and number of output nodes.

# Confusion Matrix
table(x_test$income, predict(nnet.fit, newdata=x_test, type="class"))
caret::confusionMatrix(x_test$income, predict(nnet.fit, newdata=x_test, type="class"),
                       mode = "prec_recall")

plot()
plot( nnet.perf,col=2)


####################################Gradient Boosting
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

xgb.viz.cv(res)
xgb.viz.cv(res[450:550, ])
xgb.viz.cv(res[680:700, ])
res[680:700, ]

xgb.fit = xgb.train(xgb_params_adult, dtrain, 490)

model <- xgb.dump(xgb.fit, with.stats = T)
model[1:10] #This statement prints top 10 nodes of the model

feature.names <- colnames(x_train)[-13]
importance_matrix <- xgb.importance(feature.names, model = xgb.fit)
importance_matrix
xgb.plot.importance(importance_matrix[1:10,])

# Confusion Matrix
x_test[['income']] <- NULL
preds <- ifelse(predict(xgb.fit, newdata=as.matrix(x_test)) >= 0.5, 1, 0)
caret::confusionMatrix(y_test, preds, mode = "prec_recall")

# Overfitting?
xgb.fit = xgb.train(xgb_params_adult, dtrain, 700)
preds <- ifelse(predict(xgb.fit, newdata=as.matrix(x_test)) >= 0.5, 1, 0)
caret::confusionMatrix(y_test, preds, mode = "prec_recall")
