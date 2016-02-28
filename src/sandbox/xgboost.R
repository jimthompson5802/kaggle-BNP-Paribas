###
# experiment xgboost
###
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(xgboost)
# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.
train <- read.table("./data/train.csv", header=T, sep=",") 

idx <- sample(nrow(train),0.5*nrow(train))
train <- train[idx,]

y <- train[, 'target']
train <- train[, -2]
test <- read.table("./data/test.csv", header=T, sep=",") 
train[is.na(train)] <- -1
test[is.na(test)] <- -1

# Find factor variables and translate to numeric
f <- c()
for(i in 1:ncol(train)) {
    if (is.factor(train[, i])) f <- c(f, i)
}

f.t <- c()
for(i in 1:ncol(test)) {
    if (is.factor(test[, i])) f.t <- c(f.t, i)
}

ttrain <- rbind(train, test)
for (i in f) {
    ttrain[, i] <- as.numeric(ttrain[, i]) 
}
train <- ttrain[1:nrow(train), ]
test <- ttrain[(nrow(train)+1):nrow(ttrain), ]

doTest <- function(y, train, test, param0, iter) {
    n<- nrow(train)
    xgtrain <- xgb.DMatrix(as.matrix(train), label = y)
    xgval = xgb.DMatrix(as.matrix(test))
    watchlist <- list('train' = xgtrain)
    model = xgb.train(
        nrounds = iter
        , params = param0
        , data = xgtrain
        , watchlist = watchlist
        , print.every.n = 100
        , nthread = 8 
    )
    p <- predict(model, xgval)
    rm(model)
    gc()
    p
}

param0 <- list(
    # general , non specific params - just guessing
    "objective"  = "binary:logistic"
    , "eval_metric" = "logloss"
    , "eta" = 0.01
    , "subsample" = 0.8
    , "colsample_bytree" = 0.8
    , "min_child_weight" = 1
    , "max_depth" = 10
)

# total analysis
#submission <- read.table("../input/sample_submission.csv", header=TRUE, sep=',')
ensemble <- rep(0, nrow(test))
# change to 1:5 to get result
for (i in 1:3) {
    p <- doTest(y, train, test, param0, 500) 
    # change to 1300 or 1200, test by trial and error, have to add to local check which suggests 900, 
    # but have another 20% training data to concider which gives longer optimal training time
    ensemble <- ensemble + p
}
# submission$PredictedProb <- ensemble/i
# write.csv(submission, "submission.csv", row.names=F, quote=F)
