## ensemble of DT - Random forest


source("../code/fe.r")
#------------------------------------------------
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

length(x_test$income)
length(preds)

caret::confusionMatrix(x_test$income, preds)
