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
  #Box plots
  #--------------------------------------------
  
  
  #--------------------------------------------
  #correlation between numerical variables
  #--------------------------------------------
  # library(corrgram)
  # corrgram(data, order=TRUE, lower.panel=panel.shade,
  #          upper.panel=panel.pie, text.panel=panel.txt,
  #          main="Car Milage Data in PC2/PC1 Order")
  # install.packages("corrplot") 
  # install.packages("Hmisc") 
  library(corrplot)
  library("Hmisc")
  
  num.cols <- sapply(data, is.numeric)
  res <- cor(data[,num.cols])
  res
  corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
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
