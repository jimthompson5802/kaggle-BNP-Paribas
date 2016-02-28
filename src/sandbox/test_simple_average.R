###
#  test of simple average of Level 1 models
###

l1_gbm2 <- read.csv("./src/L1_gbm2/submission.csv")
l1_nnet1 <- read.csv("./src/L1_nnet1/submission.csv")


cor(l1_gbm2[,"PredictedProb"],l1_nnet1[,"PredictedProb"])

PredictedProb <- apply(cbind(l1_gbm2[,"PredictedProb"],l1_nnet1[,"PredictedProb"]),1,sum)/2


write.csv(cbind(ID=l1_gbm2[,"ID"],PredictedProb),
          file="./src/sandbox/average_test_submission.csv",
          row.names = FALSE)

