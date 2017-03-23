source("../code/library.r")
#Split the data and return train and test datasets
datasplit <- function(data, p=0.7){
  idx <- sample(2, nrow(data), replace=TRUE, prob = c(p, 1-p))
  trainSet <- data[idx==1, ]
  validationSet <- data[idx==2, ]
  return(list(train=trainSet, validation=validationSet, idx=idx))
}

datasplit.xgb <- function(x_train, y_train, p=0.7){
  idx <- sample(2, nrow(x_train), replace=TRUE, prob = c(p, 1-p))
  
  trainSet <- x_train[idx==1, ]
  validationSet <- x_train[idx==2, ]
  
  y_trainSet <- y_train[idx==1]
  y_validationSet <- y_train[idx==2]
  
  dtrain = xgb.DMatrix(as.matrix(trainSet), label=y_trainSet)
  dvalidation = xgb.DMatrix(as.matrix(validationSet), label=y_validationSet)
  
  return(list(train=dtrain, validation=dvalidation, idx=idx))
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


xg_eval_mae <- function (yhat, dtrain) {
  y = getinfo(dtrain, "label")
  err= mae(exp(y),exp(yhat) )
  return (list(metric = "error", value = err))
}

xgb.viz.cv <- function(dataset) {
  dataset$iteration <- as.integer(rownames(dataset))
  p <- ggplot(dataset, aes(x = iteration)) + 
    geom_line(aes(y = train.error.mean), colour="blue") + 
    geom_line(aes(y = test.error.mean), colour = "red") + 
    #geom_line(aes(y = train.error.mean + train.error.std), colour="black") +
    #geom_line(aes(y = train.error.mean - train.error.std), colour="black") +
    ylab(label="Error (MAE)") + 
    xlab("Iteration") + 
    ggtitle("Test vs Train") +
    scale_colour_manual(name="Dataset", values=c(test="red", train="blue")) 
  return(p)
}

submission <- function(pred_obj, test_dataset, output_fname, path_sample=SUBMISSION_FILE) {
  submission = fread(path_sample, colClasses = c("integer", "numeric"))
  submission$loss = exp(predict(pred_obj, test_dataset))
  write.csv(submission, output_fname, row.names = FALSE)
}

#GLM
my_glm = function(formula){
 
}





