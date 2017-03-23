# Neural Network

source("../code/fe.r")

x_train$income <- as.factor(y_train)
x_test$income <- as.factor(y_test)

nnet.fit = nnet(income ~ ., data=x_train, size=20, maxit=10000, decay=.001)

nnet.preds = predict(nnet.fit, newdata=x_test, type="raw")
nnet.pred = prediction(nnet.preds, x_test$income)
nnet.perf = performance(nnet.pred, "tpr", "fpr")
# A good rule of thumb is to set it between the number of input nodes and number of output nodes.

# Confusion Matrix
table(x_test$income, predict(nnet.fit, newdata=x_test, type="class"))
caret::confusionMatrix(x_test$income, predict(nnet.fit, newdata=x_test, type="class"))
plot(nnet.perf,col=2)

