#------------------------------------------------
## GLM
#------------------------------------------------
setwd("D:/Projects/Kaggle/UCI-AdultDataset-Classification/Code")
source("../code/fe.R")

x_train$income=y_train
x_test$income=y_test

formula = "income~."
logit.fit=glm(formula,family=binomial(logit),data=x_train)

print("GLM fit:")
summary(logit.fit)

features=names(x_train)
print("GLM vif for colliniarity(more than 2.5 is not good):")
vif.fit = vif(logit.fit)
print(vif.fit)

print("GLM Performance:")
## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
logit.preds=predict(logit.fit,newdata=x_test,type ="response") 
pred <- prediction(logit.preds,x_test$income)
perf <- performance(pred,"tpr","fpr")
plot(perf)

## precision/recall curve (x-axis: recall, y-axis: precision)
perf1 <- performance(pred, "prec", "rec")
plot(perf1)

## sensitivity/specificity curve (x-axis: specificity,
## y-axis: sensitivity)
perf1 <- performance(pred, "sens", "spec")
plot(perf1)

#Confusion Matrix
preds=ifelse(predict(logit.fit,newdata=x_test,type="response")>=0.5,1,0)
table(x_test$income,preds)
confusionMatrix(data = preds, reference = x_test$income)

#Calculate the AUC value
perf_AUC=performance(pred,"auc") 
AUC=perf_AUC@y.values[[1]]
AUC

 