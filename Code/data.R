setwd("D:/Projects/Kaggle/UCI-AdultDataset-Classification/Code")
source("../code/library.R")
source("../code/helper.R")

TARGET = "income"
data = read.table("../data/adult.data.txt",
                  sep=",",header=F,col.names=c("age", "type_employer", "fnlwgt", "education",
                                               "education_num","marital", "occupation", "relationship", "race","sex",
                                               "capital_gain", "capital_loss", "hr_per_week","country", "income"),
                  fill=FALSE,strip.white=T)
data[["X"]]=NULL

library(VIM)

# Removing the missing data
is.na(data) = data=='?'
is.na(data) = data==' ?'
data = na.omit(data)

# Factor to Character
#Convert factor variables to character
fctr.cols <- sapply(data, is.factor)
data[, fctr.cols] <- sapply(data[, fctr.cols], as.character)
# ordered(unique(data$education_num))
# unique(data$education)

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
