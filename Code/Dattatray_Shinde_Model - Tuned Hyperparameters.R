# -------------------------------------------------------------------------
#  1. Data loading with libraries
# -------------------------------------------------------------------------
## setting working directory (edit your path before running script)
path <- "D:/Projects/Kaggle/UCI-AdultDatasetClassification/UCI-AdultDatasetKaggleClass/Code"
setwd(path)

## loading libraries
source("library.R")
library(data.table)
library(plyr)
library(xgboost)
library(dplyr)
library(nnet)
library(mice)
library(car)
rm(list=ls())

#get the data replace empty values with NA
train = read.csv("train.csv", header=T, na.strings=c("","NA"))
summary(train)
test = read.csv("test.csv", header=T, na.strings=c("","NA"))
summary(test)
ntrain=nrow(train)
ntest=nrow(test)


boxplot(train,col = c('red'))

col_names=c(
  "Id"
  ,"age"
  ,"workclass"
  ,"fnlwgt"
  ,"education"     
  ,"education_num"
  ,"marital_status"
  ,"occupation"     
  ,"relationship"   
  ,"race"           
  ,"sex"           
  ,"capital_gain"   
  ,"capital_loss"   
  ,"hr_per_week"    
  ,"native_country" 
  ,"income")

test[["income"]] = 0
train_test=rbind(train,test)
train_test[["X"]]=NULL
train_test[["education_num"]]=NULL
train_test[["fnlwgt"]]=NULL
summary(train_test)

train_test$workclass <- as.character(train_test$workclass)
train_test$workclass[train_test$workclass == "?"] <- NA
train_test$workclass[train_test$workclass == " ?"] <- NA
train_test$native_country <- as.character(train_test$native_country)
train_test$native_country[train_test$native_country == "?"] <- NA
train_test$native_country[train_test$native_country == " ?"] <- NA
train_test$workclass <- as.factor(train_test$workclass)
train_test$native_country <- as.factor(train_test$native_country)
summary(train_test)

#3.Missing value treatment
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(train_test,2,pMiss)
md.pattern(train_test)
whatis(train_test)

##-------------------------------------------------------------------------------------------
table(train_test$workclass, useNA = "always")
# Given "Never worked" and "Without-Pay" are both very small groups, and they are
# likely very similar, we can combine them to form a "Not Working" Category.
# In a similar vein, we can combine government employee categories, and self-employed
# categories. This allows us to reduce the number of categories significantly.
train_test$workclass = gsub("^Federal-gov","Federal-Govt",train_test$workclass)
train_test$workclass = gsub("^Local-gov","Other-Govt",train_test$workclass)
train_test$workclass = gsub("^State-gov","Other-Govt",train_test$workclass)
train_test$workclass = gsub("^Private","Private",train_test$workclass)
train_test$workclass = gsub("^Self-emp-inc","Self-Employed",train_test$workclass)
train_test$workclass = gsub("^Self-emp-not-inc","Self-Employed",train_test$workclass)
train_test$workclass = gsub("^Without-pay","Not-Working",train_test$workclass)
train_test$workclass = gsub("^Never-worked","Not-Working",train_test$workclass)
class(train_test$workclass)
train_test$workclass=as.factor(train_test$workclass)
levels(train_test$workclass)

table(train_test$occupation, useNA = "always")
# On occupation, a simple way to block the categories would include blue collar versus white
# collar. Separate out service industry, and other occupations that are not fitting well with
# the other groups into their own group. It's unfortunate that Armed Forces won't fit well
# with any of the other groups. In order to get it properly represented, we can try up-sampling
# it when we train the model.
train_test$occupation = gsub("^Adm-clerical","Admin",train_test$occupation)
train_test$occupation = gsub("^Armed-Forces","Military",train_test$occupation)
train_test$occupation = gsub("^Craft-repair","Blue-Collar",train_test$occupation)
train_test$occupation = gsub("^Exec-managerial","White-Collar",train_test$occupation)
train_test$occupation = gsub("^Farming-fishing","Blue-Collar",train_test$occupation)
train_test$occupation = gsub("^Handlers-cleaners","Blue-Collar",train_test$occupation)
train_test$occupation = gsub("^Machine-op-inspct","Blue-Collar",train_test$occupation)
train_test$occupation = gsub("^Other-service","Service",train_test$occupation)
train_test$occupation = gsub("^Priv-house-serv","Service",train_test$occupation)
train_test$occupation = gsub("^Prof-specialty","Professional",train_test$occupation)
train_test$occupation = gsub("^Protective-serv","Other-Occupations",train_test$occupation)
train_test$occupation = gsub("^Sales","Sales",train_test$occupation)
train_test$occupation = gsub("^Tech-support","Other-Occupations",train_test$occupation)
train_test$occupation = gsub("^Transport-moving","Blue-Collar",train_test$occupation)

table(train_test$native_country, useNA = "always")
train_test$native_country=as.character(train_test$native_country)
# The variable country presents a small problem. Obviously the United States
# represents the vast majority of observations, but some of the groups have
# such small numbers that their contributions might not be significant. A way
# around this would be to block the countries.
# Use a combination of geographical location, political organization, and economic
# zones. Euro_1 is countries within the Eurozone that are considered more affluent,
# and therefore people from there are probably going to be more affluent. Euro_2
# includes countries within the Eurozone that are considered less affluent. These
# included countries that are financially troubled like Spain and Portugal, but also
# the Slavic countries and those formerly influenced by the USSR like Poland. Formerly
# British holdings that are still closely economically aligned with Britain are included
# under the British-Commonwealth.
train_test$native_country[train_test$native_country=="Cambodia"] = "SE-Asia"
train_test$native_country[train_test$native_country=="Canada"] = "British-Commonwealth"  
train_test$native_country[train_test$native_country=="China"] = "China"   
train_test$native_country[train_test$native_country=="Columbia"] = "South-America"  
train_test$native_country[train_test$native_country=="Cuba"] = "Other"    
train_test$native_country[train_test$native_country=="Dominican-Republic"] = "Latin-America"
train_test$native_country[train_test$native_country=="Ecuador"] = "South-America"  
train_test$native_country[train_test$native_country=="El-Salvador"] = "South-America"
train_test$native_country[train_test$native_country=="England"] = "British-Commonwealth"
train_test$native_country[train_test$native_country=="France"] = "Euro_1"
train_test$native_country[train_test$native_country=="Germany"] = "Euro_1"
train_test$native_country[train_test$native_country=="Greece"] = "Euro_2"
train_test$native_country[train_test$native_country=="Guatemala"] = "Latin-America"
train_test$native_country[train_test$native_country=="Haiti"] = "Latin-America"
train_test$native_country[train_test$native_country=="Holand-Netherlands"] = "Euro_1"
train_test$native_country[train_test$native_country=="Honduras"] = "Latin-America"
train_test$native_country[train_test$native_country=="Hong"] = "China"
train_test$native_country[train_test$native_country=="Hungary"] = "Euro_2"
train_test$native_country[train_test$native_country=="India"] = "British-Commonwealth"
train_test$native_country[train_test$native_country=="Iran"] = "Other"
train_test$native_country[train_test$native_country=="Ireland"] = "British-Commonwealth"
train_test$native_country[train_test$native_country=="Italy"] = "Euro_1"
train_test$native_country[train_test$native_country=="Jamaica"] = "Latin-America"
train_test$native_country[train_test$native_country=="Japan"] = "Other"
train_test$native_country[train_test$native_country=="Laos"] = "SE-Asia"
train_test$native_country[train_test$native_country=="Mexico"] = "Latin-America"
train_test$native_country[train_test$native_country=="Nicaragua"] = "Latin-America"
train_test$native_country[train_test$native_country=="Outlying-US(Guam-USVI-etc)"] = "Latin-America"
train_test$native_country[train_test$native_country=="Peru"] = "South-America"
train_test$native_country[train_test$native_country=="Philippines"] = "SE-Asia"
train_test$native_country[train_test$native_country=="Poland"] = "Euro_2"
train_test$native_country[train_test$native_country=="Portugal"] = "Euro_2"
train_test$native_country[train_test$native_country=="Puerto-Rico"] = "Latin-America"
train_test$native_country[train_test$native_country=="Scotland"] = "British-Commonwealth"
train_test$native_country[train_test$native_country=="South"] = "Euro_2"
train_test$native_country[train_test$native_country=="Taiwan"] = "China"
train_test$native_country[train_test$native_country=="Thailand"] = "SE-Asia"
train_test$native_country[train_test$native_country=="Trinadad&Tobago"] = "Latin-America"
train_test$native_country[train_test$native_country=="United-States"] = "United-States"
train_test$native_country[train_test$native_country=="Vietnam"] = "SE-Asia"
train_test$native_country[train_test$native_country=="Yugoslavia"] = "Euro_2"

table(train_test$education, useNA = "always")
# Block all the dropouts together. Block high school graduates and those that
# attended some college without receiving a degree as another group. Those college
# graduates who receive an associates are blocked together regardless of type of
# associates. Those who graduated college with a Bachelors, and those who went on to
# graduate school without receiving a degree are blocked as another group. Most
# everything thereafter is separated into its own group.
train_test$education = gsub("^10th","Dropout",train_test$education)
train_test$education = gsub("^11th","Dropout",train_test$education)
train_test$education = gsub("^12th","Dropout",train_test$education)
train_test$education = gsub("^1st-4th","Dropout",train_test$education)
train_test$education = gsub("^5th-6th","Dropout",train_test$education)
train_test$education = gsub("^7th-8th","Dropout",train_test$education)
train_test$education = gsub("^9th","Dropout",train_test$education)
train_test$education = gsub("^Assoc-acdm","Associates",train_test$education)
train_test$education = gsub("^Assoc-voc","Associates",train_test$education)
train_test$education = gsub("^Bachelors","Bachelors",train_test$education)
train_test$education = gsub("^Doctorate","Doctorate",train_test$education)
train_test$education = gsub("^HS-Grad","HS-Graduate",train_test$education)
train_test$education = gsub("^Masters","Masters",train_test$education)
train_test$education = gsub("^Preschool","Dropout",train_test$education)
train_test$education = gsub("^Prof-school","Prof-School",train_test$education)
train_test$education = gsub("^Some-college","HS-Graduate",train_test$education)

# Similarly
train_test$marital_status=as.character(train_test$marital_status)

train_test$marital_status[train_test$marital_status=="Never-married"] = "Never-Married"
train_test$marital_status[train_test$marital_status=="Married-AF-spouse"] = "Married"
train_test$marital_status[train_test$marital_status=="Married-civ-spouse"] = "Married"
train_test$marital_status[train_test$marital_status=="Married-spouse-absent"] = "Not-Married"
train_test$marital_status[train_test$marital_status=="Separated"] = "Not-Married"
train_test$marital_status[train_test$marital_status=="Divorced"] = "Not-Married"
train_test$marital_status[train_test$marital_status=="Widowed"] = "Widowed"

train_test$race=as.character(train_test$race)

train_test$race[train_test$race=="White"] = "White"
train_test$race[train_test$race=="Black"] = "Black"
train_test$race[train_test$race=="Amer-Indian-Eskimo"] = "Amer-Indian"
train_test$race[train_test$race=="Asian-Pac-Islander"] = "Asian"
train_test$race[train_test$race=="Other"] = "Other"
##-------------------------------------------------------------------------------------------
#Missing value tratement
mean(train_test$age,na.rm=TRUE)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
train_test$workclass[is.na(train_test$workclass)]=Mode(train_test$workclass)
train_test$native_country[is.na(train_test$native_country)]=Mode(train_test$native_country)

##-------------------------------------------------------------------------------------------
for (f in col_names) {
  if(!f=="income")
  {
    if (class(train_test[[f]])=="character") {
      #cat("VARIABLE : ",f,"\n")
      levels <- unique(train_test[[f]])
      train_test[[f]] <- as.numeric(as.integer(factor(train_test[[f]], levels=levels)))
    }
    if (class(train_test[[f]])=="factor") {
      #cat("VARIABLE : ",f,"\n")
      levels <- unique(train_test[[f]])
      train_test[[f]] <- as.numeric(as.integer(factor(train_test[[f]], levels=levels)))
    }
  }
}
whatis(train_test)
##-------------------------------------------------------------------------------------------
train_new = train_test[1:ntrain, ]
test_new = train_test[(ntrain+1):nrow(train_test), ]
y_train = train_new[["income"]] 
y_train
target = as.numeric(ifelse(y_train==">50K", 1, 0))
summary(target)

test_Id = test_new[["Id"]]

train_new[["Id"]]=NULL
train_new[["income"]]=NULL
test_new[["Id"]]=NULL
test_new[["income"]]=NULL

# Preparing for xgboost
# dtrain = xgb.DMatrix(as.matrix(train_new), label=y_train)
# dtest = xgb.DMatrix(as.matrix(test_new))

##-------------------------------------------------------------------------------------------
##find best XGB params run it once
# XGBoost hyper parameter tuning
##-------------------------------------------------------------------------------------------
dtrain = xgb.DMatrix(as.matrix(train_new), label=target)
nrow(train_new)
class(dtrain)
whatis(train_new)
#dtrain = xgb.DMatrix(train_new)
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
  cv.nround = 1500
  cv.nfold = 10
  seed.number = sample.int(10000, 1)[[1]]
  set.seed(seed.number)
  mdcv <- xgb.cv(data=as.matrix(train_new),label=as.matrix(target), params = param, nthread=6, 
                 nfold=cv.nfold, nrounds=cv.nround,
                 verbose = T, early_stopping_rounds=10, maximize=TRUE)
  
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
load("../data/bestModelParams.rda")
save(nround, best_seednumber,best_param, file="../data/bestModelParams.rda")
##-------------------------------------------------------------------------------------------
# Predtictions
##-------------------------------------------------------------------------------------------
set.seed(best_seednumber)
target = as.numeric(target)
best_xgb_model <- xgb.train(data=dtrain, params=best_param, nrounds=nround, nthread=6)
pred <- predict(best_xgb_model, as.matrix(test_new))
# submission
submit <- data.frame("Id" = as.character(test_Id), "Prediction" = pred)
write.csv(submit, "submit1.csv", row.names=F)
##-------------------------------------------------------------------------------------------
