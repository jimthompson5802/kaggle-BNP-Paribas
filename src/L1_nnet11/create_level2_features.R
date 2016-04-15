###
# For this Level 1 model, create calibration data for determining weights for Level 2
###

library(caret)
library(data.table)
# add model specific libraries
library(caTools)

# set working directory
WORK.DIR <- "./src/L1_nnet11"   # directory where model artifacts are stored

# Common Functions and Global variables
source("./src/CommonFunctions.R")

# retrive generated model-name created in training run
model.file.name <- readLines(paste0(WORK.DIR,"/this_model"))
load(paste0(WORK.DIR,"/",model.file.name))

# get training data
train.raw <- fread(paste0(DATA.DIR,"/train.csv"))
setkey(train.raw,ID)

#save id vector
id <- train.raw$ID

library(doMC)
registerDoMC(cores = 7)

# prep the data for prediction model
train.data <- PREPARE.MODEL.DATA(LEVEL0.MODELS,train.raw,includeResponse=TRUE)

pred.probs <- predict(mdl.fit,data=train.data$predictors,type="prob")


level2.data <- cbind(ID=id,pred.probs,response=train.data$response)

# save data for calibrating Level 2 model weights
save(level2.data,file=paste0(WORK.DIR,"/level2_features.RData"))




