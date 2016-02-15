###
# For model stacking generate the Level 1 training data set
###

library(caret)
library(data.table)
# add model specific libraries
library(caTools)

# set working directory
WORK.DIR <- "./src/L0_skeleton_model"   # directory where model artifacts are stored

# Common Functions and Global variables
source("./src/CommonFunctions.R")

PREPARE.MODEL.DATA <- prepL0SkltnModelData

# read kaggle submission data
load(paste0(DATA.DIR,"/train_calib_test.RData"))
new.df <- train1.raw

#save id vector
id <- new.df$ID

# prep the data for submission
submission <- PREPARE.MODEL.DATA(new.df,includeResponse=TRUE)

# retrive generated model-name created in training run
load(Sys.readlink(paste0(WORK.DIR,"/this_model.RData")))

# predict class probabilities
pred.probs <- predict(mdl.fit,newdata = submission$predictors,type = "prob")

# Attribute Level 0 model to the created predictions
file.name <- Sys.readlink(paste0(WORK.DIR,"/this_model.RData"))
file.name.parts <- unlist(strsplit(file.name,"/"))
model.level <- file.name.parts[length(file.name.parts) - 1]
new.names <- paste0(model.level,".",names(pred.probs))
names(pred.probs) <- new.names

L1.training.data <- data.table(ID=id,pred.probs,target=submission$response)

save(model.level,L1.training.data,file=paste0(WORK.DIR,"/L1_training_data.RData"))


