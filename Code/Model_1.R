x_train$income=y_train
x_test$income=y_test

logit.fit=glm(income~.,family=binomial(logit),data=x_train)
summary(logit.fit)
features=names(x_train)
vif(logit.fit)

logit.preds=predict(logit.fit,newdata=x_test,type="response")
logit.pred=prediction(logit.preds,x_test$income)
logit.perf=performance(logit.pred,"tpr","fpr")

#Confusion Matrix
preds=ifelse(predict(logit.fit,newdata=x_test,type="response")>=0.5,1,0)
table(x_test$income,preds)
caret::confusionMatrix(x_test$income,preds,mode="prec_recall")

start.time=Sys.time()

##Without relationship
logit.fit=glm(income~. ~relationship,family=binomial(logit),data=x_train)
summary(logit.fit)
features=names(x_train)
vif(logit.fit)

logit.preds=predict(logit.fit,newdata=x_test,type="response")
logit.pred=prediction(logit.preds,x_test$income)
logit.perf=performance(logit.pred,"tpr","fpr")
end.time=Sys.time()

time=end.time-start.time
time

#Confusion Matrix
preds=ifelse(predict(logit.fit,newdata=x_test,type="response")>=0.5,1,0)
table(x_test$income,preds)
caret::confusionMatrix(x_test$income,preds,mode="prec_recall")