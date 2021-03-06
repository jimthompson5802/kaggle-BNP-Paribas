###
# generate submission for  model
###

library(caret)
library(data.table)
# add model specific libraries
library(caTools)

library(doMC)
registerDoMC(7)

# set working directory
WORK.DIR <- "./src/L1_nnet11"   # directory where model artifacts are stored

# Common Functions and Global variables
source("./src/CommonFunctions.R")

# retrive generated model-name created in training run
model.file.name <- readLines(paste0(WORK.DIR,"/this_model"))
load(paste0(WORK.DIR,"/",model.file.name))

# read kaggle submission data
new.df <- fread(paste0(DATA.DIR,"/test.csv"))

#save id vector
id <- new.df$ID

# prep the data for submission
submission <- PREPARE.MODEL.DATA(LEVEL0.MODELS,new.df,includeResponse=FALSE)


# predict class probabilities
pred.probs <- predict(mdl.fit,newdata = submission$predictors,type = "prob")

#create kaggle submission file
write.csv(data.frame(ID=id,PredictedProb=pred.probs[,"Class_1"]),file=paste0(WORK.DIR,"/submission.csv"),
          row.names=FALSE)




