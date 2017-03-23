# CART Trees
source("../code/fe.r")

x_train$income <- as.factor(y_train)
x_test$income <- as.factor(y_test)

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
confusionMatrix(x_test$income, predict(tree.fit, newdata=x_test, type="class"))

