###
# Model configuration parameters
###


# set caret training parameters
CARET.TRAIN.PARMS <- list(method="nnet")   # Replace MODEL.METHOD with appropriate caret model

CARET.TUNE.GRID <- expand.grid(size=9, decay=0.1)  # training


PREPARE.MODEL.DATA <- prepL0FeatureSet7

MODEL.SPECIFIC.PARMS <- list(verbose=FALSE,MaxNWts=7000) #NULL # Other model specific parameters

MODEL.COMMENT <- "prepL0FeatureSet7, 5-fold training"