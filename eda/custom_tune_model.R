###
# xgboost - custom tune parameters
###

library(data.table)
library(caret)
# add any model specific package library commands
library(xgboost)
library(ggplot2)

# set working directory
WORK.DIR <- "./eda"  # modify to specify directory to contain model artififacts

# Common Functions and Global variables
source("./src/CommonFunctions.R")

MODEL.SPECIFIC.PARMS <- list(verbose=FALSE) #NULL # Other model specific parameters

FRACTION.TRAIN.DATA <- 1.0

PREPARE.MODEL.DATA <- prepL0FeatureSetAll

MODEL.COMMENT <- "Only Class_1 probabilites as features"

# get training data
train.df <- fread(paste0(DATA.DIR,"/train.csv"))
setkey(train.df,ID)

if (FRACTION.TRAIN.DATA != 1.0) {
    # extract subset for inital training
    set.seed(29)
    idx <- createDataPartition(train.df$target,p=FRACTION.TRAIN.DATA,list=FALSE)
    train.df <- train.df[idx,]
}

# prepare data for training
train.data <- PREPARE.MODEL.DATA(train.df)

library(doMC)
registerDoMC(cores = 7)


# train the model
Sys.time()
set.seed(825)

param <- list(booster="gbtree",
              objecive="binary:logistic",
              eval_metric="logloss",
              max.depth=8,
              eta=0.01,
              gamma=0,
              nthread=7, 
              min_child_weight=1,
              subsample=1,
              colsample_bytree=1,
              seed=13)


dTrain <- xgb.DMatrix(as.matrix(train.data$predictors),
                      label=as.numeric(train.data$response=="Class_1"))
set.seed(13)
res <- xgb.cv(params=param, 
              data=dTrain,
              nrounds=2000,
              nfold=5,
              early.stop.round=30,
              prediction=TRUE)


best.round <- which(res$dt$test.logloss.mean == min(res$dt$test.logloss.mean))

tune.df <- data.frame(best.round=best.round,
                      train.error = res$dt$train.logloss.mean[best.round],
                      test.error = res$dt$test.logloss.mean[best.round],
                      param)
write.table(tune.df,file=paste0(WORK.DIR,"/hyper_parameter_tune_log.tsv"),
            append=TRUE,sep="\t",row.names=FALSE,col.names=FALSE)

# plot 
p <- ggplot(res$dt) +
    geom_line(aes(1:nrow(res$dt),res$dt$train.logloss.mean),color="blue") +
    geom_line(aes(1:nrow(res$dt),res$dt$test.logloss.mean),color="red") +
    ylab("LogLoss Erroor") +
    xlab("nrounds")
print(p)



