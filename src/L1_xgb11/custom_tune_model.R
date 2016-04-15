###
# xgboost - custom tune parameters
###

library(data.table)
library(caret)
# add any model specific package library commands


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

param <- list(boster=gbtree,
              max.depth=2,
              eta=0.3,
              gamma=0,
              nthread=2, 
              min_child_weight=1,
              subsample=1,
              colsample_bytree=1,
              nround=2)

res <- xgb.cv(params=param, data=train.data$predictors, 
              label=train.data$response,
              nround=2,
              prediction=TRUE)


time.data
mdl.fit
# stopCluster(cl)

score <- mean(mdl.fit$resample$LogLoss)
score




