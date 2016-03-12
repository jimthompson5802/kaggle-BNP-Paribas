###
# For this Level 1 model, create calibration data for determining weights for Level 2
###

library(caret)
library(data.table)
# add model specific libraries
library(caTools)

# set working directory
WORK.DIR <- "./src/L1_gbm2"   # directory where model artifacts are stored

# Common Functions and Global variables
source("./src/CommonFunctions.R")

# retrive generated model-name created in training run
model.file.name <- readLines(paste0(WORK.DIR,"/this_model"))
load(paste0(WORK.DIR,"/",model.file.name))

# read kaggle submission data
load(paste0(DATA.DIR,"/train_calib_test.RData"))

# data used for determining optimal weights
new.df <- calib.raw
#save id vector
id <- new.df$ID

# prep the data for prediction model
train.data <- PREPARE.MODEL.DATA(LEVEL0.MODELS,new.df,includeResponse=TRUE)

# predict class probabilities
pred.probs <- predict(mdl.fit,newdata = train.data$predictors,type = "prob")

# augment with identifier and target variable
calib.pred.probs <- cbind(ID=id,pred.probs,target=train.data$response)

# data used for testing optimal weights
new.df <- test.raw
#save id vector
id <- new.df$ID

# prep the data for prediciton model
train.data <- PREPARE.MODEL.DATA(LEVEL0.MODELS,new.df,includeResponse=TRUE)

# predict class probabilities
pred.probs <- predict(mdl.fit,newdata = train.data$predictors,type = "prob")

# augment with identifier and target variable
test.pred.probs <- cbind(ID=id,pred.probs,target=train.data$response)

# save data for calibrating Level 2 model weights
save(calib.pred.probs,test.pred.probs,file=paste0(WORK.DIR,"/data_for_level2_optimization.RData"))




