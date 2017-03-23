setwd("D:/Projects/Kaggle/UCI-AdultDataset-Classification/Code")
source("../code/library.R")
source("../code/helper.R")
TARGET = "income"

data = read.table("../data/adult.data.txt",
                  sep=",",header=F,col.names=c("age", "type_employer", "fnlwgt", "education",
                                               "education_num","marital", "occupation", "relationship", "race","sex",
                                               "capital_gain", "capital_loss", "hr_per_week","country", "income"),
                  fill=FALSE,strip.white=T)


# data=read.csv("../data/train.csv")
colnames(data)
data[["X"]]=NULL


library(VIM)
is.na(data) = data=='?'
is.na(data) = data==' ?'

## Missing Value Treatment
is.na(data) = data=='?'
is.na(data) = data==' ?'
data = na.omit(data)


aggr_plot <- aggr(data, col=c('green','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(data), cex.axis=.7, gap=2, 
                  ylab=c("Histogram of missing data","Pattern"))

aggr_plot <- aggr(data)
summary(data)
table(is.na(data))

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(data,2,pMiss)

#--------------------------------------------
# check feature importance with the boruta
#--------------------------------------------
colnames(data)
set.seed(123)
boruta.train <- Boruta(income~., data = data, doTrace = 2)
print(boruta.train)

plot(boruta.train, xlab = "", xaxt = "n")

lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])

names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
getSelectedAttributes(boruta.train, withTentative = F)
boruta.df <- attStats(boruta.train)
print(boruta.df)

#--------------------------------------------
# data[["education_num"]]=NULL
# data[["fnlwgt"]]=NULL

# Factor to Character
#Convert factor variables to character
fctr.cols <- sapply(data, is.factor)
data[, fctr.cols] <- sapply(data[, fctr.cols], as.character)
#str(data)
ordered(unique(data$education_num))
unique(data$education)
#--------------------------------------------
# Box plots
#--------------------------------------------
# library(corrgram)
# corrgram(data, order=TRUE, lower.panel=panel.shade,
#          upper.panel=panel.pie, text.panel=panel.txt,
#          main="Car Milage Data in PC2/PC1 Order")
install.packages("corrplot") 
install.packages("Hmisc") 
library(corrplot)
library("Hmisc")

num.cols <- sapply(data, is.numeric)
res <- cor(data[,num.cols])
res
corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
#--------------------------------------------


#--------------------------------------------
#correlation between numerical variables
#--------------------------------------------
# library(corrgram)
# corrgram(data, order=TRUE, lower.panel=panel.shade,
#          upper.panel=panel.pie, text.panel=panel.txt,
#          main="Car Milage Data in PC2/PC1 Order")
install.packages("corrplot") 
install.packages("Hmisc") 
library(corrplot)
library("Hmisc")

num.cols <- sapply(data, is.numeric)
res <- cor(data[,num.cols])
res
corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
#--------------------------------------------
whatis(data)
# age
# type_employer
# fnlwgt
# education
# education_num
# marital
# occupation
# relationship
# race
# sex
# capital_gain
# capital_loss
# hr_per_week
# country
# income

# ----------------------
# Numerical variables
# ----------------------
# boxplot of weights vs Item type
source("library.R")
whatis(data)
ggplot(data, aes(income,type_employer )) +
  geom_histogram() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item Type") + 
  ylab("Item Weight") + 
  ggtitle("Item Weight vs Item Type")
# ----------------------
# categorical variables
# ----------------------
# type_employer
# education
# marital
# occupation
# relationship
# race
# sex
# country
# income
library("ggplot2")
# ggplot(data, aes(x=income,y = type_employer)) +geom_bar(stat = "identity") 
#   theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
#   xlab("Income") + 
#   ylab("Employer Type") + 
#   ggtitle("Income vs Employer Type")

qplot(factor(type_employer), data=data, geom="bar", fill=factor(type_employer))
qplot(factor(education), data=data, geom="bar", fill=factor(education))
qplot(factor(marital), data=data, geom="bar", fill=factor(marital))
qplot(factor(occupation), data=data, geom="bar", fill=factor(occupation))
qplot(factor(relationship), data=data, geom="bar", fill=factor(relationship))
qplot(factor(race), data=data, geom="bar", fill=factor(race))
qplot(factor(sex), data=data, geom="bar", fill=factor(sex))
qplot(factor(country), data=data, geom="bar", fill=factor(country))

#--------------------------------------------
is.na(data) = data=='?'
is.na(data) = data==' ?'
data = na.omit(data)

dat <- datasplit(data)
train <- dat$train
test <- dat$validation

y_train = train[ , TARGET]
y_test = test[ , TARGET]

train[ , TARGET] <- NULL
test[ , TARGET] <- NULL

x_train <- train
x_test <- test

save(x_train, x_test, y_train, y_test, file="../data/intermediate.rda")
print("Loading datasets complete.")
