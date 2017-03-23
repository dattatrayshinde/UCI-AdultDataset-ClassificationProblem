#------------------------------------------------
## Naive Bayes
# error of coehersion - only one record present
#------------------------------------------------
source("data.r")

data = read.table("../data/adult.data.txt",
                  sep=",",header=F,col.names=c("age", "type_employer", "fnlwgt", "education",
                                               "education_num","marital", "occupation", "relationship", "race","sex",
                                               "capital_gain", "capital_loss", "hr_per_week","country", "income"),
                  fill=FALSE,strip.white=T)

data <- as.data.frame(rbind(as.matrix(data),as.matrix(data)))

dat <- datasplit(data)
train <- dat$train
test <- dat$validation

y_train = train[ , TARGET]
y_test = test[ , TARGET]

train[ , TARGET] <- NULL
test[ , TARGET] <- NULL

x_train <- train
x_test <- test
features = names(x_train)

x_train$income=ifelse(y_train==">50K", 1, 0)
x_test$income=ifelse(y_test==">50K", 1, 0)
features=colnames(x_train)
x_test$country[x_test$country=='Holand-Netherlands']  = 'United-States'

model<-naiveBayes(income~.,data=x_train)
# summary(model)
pred=predict(model,newdata=x_test)
#Confusion Matrix
preds=ifelse(pred[,"0"] >=0.9999975,1,0)
confusionMatrix(data = preds, reference = x_test$income)
auc.tmp <- performance(pred,"auc");
auc <- as.numeric(auc.tmp@y.values)
auc
#------------------------------------------------
## GLM
#------------------------------------------------
source("data.r")
x_train$income=ifelse(y_train==">50K", 1, 0)
x_test$income=ifelse(y_test==">50K", 1, 0)

x_test$country[x_test$country=='Holand-Netherlands']  = 'United-States'
sum(x_test$country=='Holand-Netherlands')
formula = "income~."
logit.fit=glm(formula,family=binomial(logit),data=x_train)
## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
logit.preds=predict(logit.fit,newdata=x_test,type ="response") 
pred <- prediction(logit.preds,x_test$income)
perf <- performance(pred,"tpr","fpr")
plot(perf)

## precision/recall curve (x-axis: recall, y-axis: precision)
perf1 <- performance(pred, "prec", "rec")
perf1
plot(perf1)

## sensitivity/specificity curve (x-axis: specificity,
## y-axis: sensitivity)
perf1 <- performance(pred, "sens", "spec")
plot(perf1)

#Confusion Matrix
preds=ifelse(predict(logit.fit,newdata=x_test,type="response")>=0.5,1,0)
table(x_test$income,preds)
confusionMatrix(data = preds, reference = x_test$income)
auc.tmp <- performance(pred,"auc");
auc <- as.numeric(auc.tmp@y.values)
auc
#------------------------------------------------
library('ROCR')
roc = performance(pred, "tpr", "fpr")
plot(roc, lwd=2, colorize=TRUE)
lines(x=c(0, 1), y=c(0, 1), col="black", lwd=1)

auc = performance(pred, "auc")
auc = unlist(auc@y.values)
auc
#------------------------------------------------
## Random Forest : required all the features as chanacter
#------------------------------------------------
source("../code/library.R")
source("../code/helper.R")
TARGET = "income"

data = read.table("../data/adult.data.txt",
                  sep=",",header=F,col.names=c("age", "type_employer", "fnlwgt", "education",
                                               "education_num","marital", "occupation", "relationship", "race","sex",
                                               "capital_gain", "capital_loss", "hr_per_week","country", "income"),
                  fill=FALSE,strip.white=T)


for (f in features) {
  if (class(data[[f]])=="character") {
    #cat("VARIABLE : ",f,"\n")
    levels <- unique(data[[f]])
    data[[f]] <- as.numeric(as.integer(factor(data[[f]], levels=levels)))
  }
}
dat <- datasplit(data)
train <- dat$train
test <- dat$validation

y_train = train[ , TARGET]
y_test = test[ , TARGET]

train[ , TARGET] <- NULL
test[ , TARGET] <- NULL

x_train <- train
x_test <- test
features = names(x_train)

x_train[['income']] <- NULL
x_test[['income']] <- NULL

# Tuning takes factors as target variables
bestmtry <- tuneRF(x_train, as.factor(y_train),
                   ntreeTry=100, stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)

x_train$income <- as.factor(ifelse(y_train==">50K", 1, 0))
x_test$income <- as.factor(ifelse(y_test==">50K", 1, 0))

rf.fit <- randomForest(income ~ ., data=x_train,
          mtry=2, ntree=1000, keep.forest=TRUE, importance=TRUE, test=x_test)

rf.preds = predict(rf.fit, type="prob", newdata=x_test)[,2]
rf.pred = prediction(rf.preds, x_test$income)

#importance(rf.fit)
varImpPlot(rf.fit)

caret::confusionMatrix(x_test$income, rf.preds)
roc = performance(rf.pred, "tpr", "fpr")
plot(roc, lwd=2, colorize=TRUE)
lines(x=c(0, 1), y=c(0, 1), col="black", lwd=1)
auc = performance(rf.pred, "auc")
auc = unlist(auc@y.values)
auc
#------------------------------------------------
## 3. Cart
#------------------------------------------------
head(x_train)
x_train$income <- as.factor(ifelse(y_train==">50K", 1, 0))
x_test$income <- as.factor(ifelse(y_test==">50K", 1, 0))

set.seed(50)
mycontrol = rpart.control(cp = 0, xval = 10)
tree.fit = rpart(income ~ ., method = "class", data = x_train, control = mycontrol)
tree.fit$cptable
tree.cptarg = sqrt(tree.fit$cptable[8,1]*tree.fit$cptable[9,1])
tree.prune = prune(tree.fit, cp=tree.cptarg)
tree.preds = predict(tree.prune, newdata=x_test, type="prob")[,2]
tree.pred = prediction(tree.preds, x_test$income)
# Confusion Matrix
confusionMatrix(x_test$income, predict(tree.fit, newdata=x_test, type="class"))

roc = performance(tree.pred, "tpr", "fpr")
plot(roc, lwd=2, colorize=TRUE)
lines(x=c(0, 1), y=c(0, 1), col="black", lwd=1)
auc = performance(tree.pred, "auc")
auc = unlist(auc@y.values)
auc
#------------------------------------------------
## 4. Neural network : 
# Error - nnet too many weights
# Either increase MaxNWts to something that will accommodate the size of your model,
# or reduce size to make your model smaller.
#country has new levels Holand-Netherlands
#------------------------------------------------
x_train$income <- as.factor(ifelse(y_train==">50K", 1, 0))
x_test$income <- as.factor(ifelse(y_test==">50K", 1, 0))

nnet.fit = nnet(income ~ ., data=x_train, size=10, maxit=10000, decay=.001)

nnet.preds = predict(nnet.fit, newdata=x_test, type="raw")
nnet.pred = prediction(nnet.preds, x_test$income)
nnet.perf = performance(nnet.pred, "tpr", "fpr")
# A good rule of thumb is to set it between the number of input nodes and number of output nodes.

# Confusion Matrix
table(x_test$income, predict(nnet.fit, newdata=x_test, type="class"))
caret::confusionMatrix(x_test$income, predict(nnet.fit, newdata=x_test, type="class"))
plot(nnet.perf,col=2)

roc = performance(nnet.pred, "tpr", "fpr")
plot(roc, lwd=2, colorize=TRUE)
lines(x=c(0, 1), y=c(0, 1), col="black", lwd=1)
auc = performance(nnet.pred, "auc")
auc = unlist(auc@y.values)
auc