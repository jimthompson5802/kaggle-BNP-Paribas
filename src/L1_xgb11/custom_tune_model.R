###
# xgboost - custom tune parameters
###

library(data.table)
library(caret)
# add any model specific package library commands
library(xgboost)
library(ggplot2)

# set working directory
WORK.DIR <- "./src/L1_xgb11"  # modify to specify directory to contain model artififacts

# Common Functions and Global variables
source("./src/CommonFunctions.R")




MODEL.SPECIFIC.PARMS <- list(verbose=FALSE) #NULL # Other model specific parameters

PREPARE.MODEL.DATA <- prepL1FeatureSet1  # need for processing test data set

MODEL.COMMENT <- "Only Class_1 probabilites as features"

LEVEL0.MODELS <- c("L0_gbm21",
                   "L0_gbm41",
                   "L0_xtc11",
                   "L0_xtc21",
                   "L0_xtc31",
                   #"L0_xtc4",  did not improve score
                   #"L0_xtc5",
                   #"L0_nnet1",
                   "L0_xgb21",
                   "L0_xgb31")


# get training data
train.data <- prepL1FeatureSet3(LEVEL0.MODELS)  # used only for training

library(doMC)
registerDoMC(cores = 7)


# train the model
Sys.time()
set.seed(825)

param <- list(booster="gbtree",
              objecive="binary:logistic",
              eval_metric="logloss",
              max.depth=6,
              eta=0.01,
              gamma=0,
              nthread=2, 
              min_child_weight=1,
              subsample=0.8,
              colsample_bytree=0.8,
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



