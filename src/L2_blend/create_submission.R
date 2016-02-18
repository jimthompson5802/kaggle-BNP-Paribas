###
#  create ensemble model combining selected models
###

library(data.table)
library(caret)


# import global variabels and common functions
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/L2_blend"

# read kaggle submission data
new.df <- fread(paste0(DATA.DIR,"/test.csv"))

id <- new.df$ID

# training data for blending
submission <- prepL2FeatureSet(new.df,includeResponse = FALSE)


#
# Average the individual probablities
#

pred.probs <- apply(submission$predictors,1,sum)/ncol(submission$predictors)


# # 
# # use optimal weights calculated
# #
# 
# # combine three models probabilities into a single matrix
# probs.mat <- cbind(rf2.probs,gbm2.probs,gbm4.probs)
# 
# #set up weight matrix to combine the individual weights
# wmat <- rbind(diag(opt.wts$par[1:9]),diag(opt.wts$par[10:18]),diag(opt.wts$par[19:27]))
# 
# # compute overall probablities using invidual model class weights
# pred.probs <- probs.mat %*% wmat
# colnames(pred.probs) <- paste0("Class_",1:9)
#     

#create kaggle submission file
write.csv(data.frame(ID=id,PredictedProb=pred.probs),file=paste0(WORK.DIR,"/submission.csv"),
          row.names=FALSE)

