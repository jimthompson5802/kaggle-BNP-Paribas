###
#  sandbox for testing Level 2 blending models
###

library(caret)

source("./src/CommonFunctions.R")

level1.models <- c("L1_gbm2","L1_nnet1")


prepL2FeatureSet <- function(level1.models,df,...){
    #prepL2FeatureSet
    #level1.models: vector of Level 1 model ids 
    #df: data set to prepare
    
    # For each Level 1 model generate feature set
    
    
}