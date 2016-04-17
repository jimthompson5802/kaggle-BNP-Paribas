###
#  create ensemble model combining selected models
###

library(data.table)
library(caret)


# import global variabels and common functions
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/L2_blnd3"

# retrive generated blending weights data structure
model.file.name <- readLines(paste0(WORK.DIR,"/this_model"))
load(paste0(WORK.DIR,"/",model.file.name))

# retrieve Level 1 submissions
# L1_gbm2
gbm2.pred.probs <- read.csv("./src/L1_gbm2/submission.csv")

#L1_nnet1
nnet1.pred.probs <- read.csv("./src/L1_nnet1/submission.csv")

# combine model probabilities into a single matrix
probs.mat <- cbind(gbm2=gbm2.pred.probs[,"PredictedProb"],
                   nnet1=nnet1.pred.probs[,"PredictedProb"])


# compute overall probablities using invidual model class weights
pred.probs <- probs.mat %*% blending.weights

#create kaggle submission file
write.csv(data.frame(ID=gbm2.pred.probs[,"ID"],PredictedProb=pred.probs),file=paste0(WORK.DIR,"/submission.csv"),
          row.names=FALSE)

