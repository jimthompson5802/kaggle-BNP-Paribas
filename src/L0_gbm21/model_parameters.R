###
# Model configuration parameters
###


# set caret training parameters
CARET.TRAIN.PARMS <- list(method="gbm")   # Replace MODEL.METHOD with appropriate caret model

CARET.TUNE.GRID <- expand.grid(interaction.depth=5,n.trees=1000,
                               shrinkage=0.01,n.minobsinnode=10)

PREPARE.MODEL.DATA <- prepL0FeatureSet2

MODEL.SPECIFIC.PARMS <- list(verbose=FALSE) #NULL # Other model specific parameters
