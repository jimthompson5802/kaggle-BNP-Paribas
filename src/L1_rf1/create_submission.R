###
# generate submission for  model
###

library(caret)
library(data.table)
# add model specific libraries
library(caTools)

# set working directory
WORK.DIR <- "./src/L1_rf1"   # directory where model artifacts are stored

# Common Functions and Global variables
source("./src/CommonFunctions.R")

# retrive generated model-name created in training run
load(Sys.readlink(paste0(WORK.DIR,"/this_model.RData")))

# read kaggle submission data
new.df <- fread(paste0(DATA.DIR,"/test.csv"))

#save id vector
id <- new.df$ID

# prep the data for submission
submission <- PREPARE.MODEL.DATA(new.df,includeResponse=FALSE)



# predict class probabilities
pred.probs <- predict(mdl.fit,newdata = submission$predictors,type = "prob")

#create kaggle submission file
write.csv(data.frame(ID=id,PredictedProb=pred.probs[,"Class_1"]),file=paste0(WORK.DIR,"/submission.csv"),
          row.names=FALSE)




