###
# generate submission for  model
###

library(caret)
# add model specific libraries
library(caTools)

# set working directory
WORK.DIR <- "./src/skeleton_model"   # directory where model artifacts are stored

# Common Functions and Global variables
source("./src/CommonFunctions.R")
source(paste0(WORK.DIR,"/ModelCommonFunctions.R"))

PREPARE.MODEL.DATA <- prepGBMModelData

# read kaggle submission data
new.df <- read.csv(paste0(DATA.DIR,"/test.csv"),stringsAsFactors=FALSE)

#save id vector
id <- new.df$ID

# prep the data for submission
submission <- PREPARE.MODEL.DATA(new.df,only.predictors=TRUE)

# retrive generated model-name created in training run
load(Sys.readlink(paste0(WORK.DIR,"/this_model.RData")))

# predict class probabilities
pred.probs <- predict(mdl.fit,newdata = submission$predictors,type = "prob")

#create kaggle submission file
write.csv(data.frame(ID=id,PredictedProb=pred.probs[,"Yes"]),file=paste0(WORK.DIR,"/submission.csv"),
          row.names=FALSE)




