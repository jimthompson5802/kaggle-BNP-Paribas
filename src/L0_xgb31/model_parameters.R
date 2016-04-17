###
# Model configuration parameters
###


# set caret training parameters
CARET.TRAIN.PARMS <- list(method="xgbTree")   # Replace MODEL.METHOD with appropriate caret model

CARET.TUNE.GRID <-  expand.grid(nrounds=500, 
                                max_depth=10, 
                                eta=0.01, 
                                gamma=0.1, 
                                colsample_bytree=0.8, 
                                min_child_weight=1)

PREPARE.MODEL.DATA <- prepL0FeatureSet2a

MODEL.SPECIFIC.PARMS <- list(verbose=0) #NULL # Other model specific parameters
