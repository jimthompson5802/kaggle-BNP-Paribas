###
# neural network model
###

library(data.table)
library(caret)
# add any model specific package library commands
library(nnet)

# set working directory
WORK.DIR <- "./src/L1_nnet11"  # modify to specify directory to contain model artififacts

# Common Functions and Global variables
source("./src/CommonFunctions.R")

# set caret training parameters
CARET.TRAIN.PARMS <- list(method="nnet")   # Replace MODEL.METHOD with appropriate caret model

CARET.TUNE.GRID <-  NULL  # NULL provides model specific default tuning parameters

# user specified tuning parameters
#CARET.TUNE.GRID <- expand.grid(nIter=c(100))

# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=1,
                                 verboseIter=FALSE,
                                 classProbs=TRUE,
                                 summaryFunction=caretLogLossSummary)

CARET.TRAIN.OTHER.PARMS <- list(trControl=CARET.TRAIN.CTRL,
                            maximize=FALSE,
                           tuneGrid=CARET.TUNE.GRID,
                           tuneLength=5,
                           metric="LogLoss")

MODEL.SPECIFIC.PARMS <- list(verbose=FALSE) #NULL # Other model specific parameters

PREPARE.MODEL.DATA <- function(data){return(data)}  #default data prep
PREPARE.MODEL.DATA <- prepL1FeatureSet1

MODEL.COMMENT <- "Only Class_1 probabilites as features"

LEVEL0.MODELS <- c("L0_gbm21",
                   #"L0_gbm4",
                   #"L0_xtc1",
                   #"L0_xtc2",
                   #"L0_xtc3",
                   #"L0_xtc4",  did not improve score
                   #"L0_xtc5",
                   #"L0_nnet1",
                   "L0_xgb21",
                   "L0_xgb31")

# amount of data to train
FRACTION.TRAIN.DATA <- 1.0

# force recording model flag
FORCE_RECORDING_MODEL <- FALSE

# Extract pre-computed Level1 features for each specified Level0 models
prepL1FeatureSet3 <- function(level0.models,includeResponse=TRUE){
    # l0_models: character vector of Level 0 models to include the Level 1 Feature set
    # if includeResponse is TRUE then return list(predictors,response)
    # if includeResponse is FALSE then return list(predictors)
    
    
    # predictors <- foreach(mdl.dir=level0.models,.combine=cbind) %dopar%
    #     createLevel1Features(mdl.dir,df,includeResponse)
    
    # for each Level0 model retrieve pre-computed Level1 Feature
    ll <- lapply(level0.models, function(model.dir){
        cat("retrieving Level1 features from",model.dir,"\n")
        flush.console()
        
        # retrieve name of Level1 features data set
        level1.features.file.name <- readLines(paste0("./src/",model.dir,"/this_level1_features"))
        
        # create environment to hold Level 0 Model data structures
        l0.env <- new.env()
        
        # retrieve Level1 features
        load(paste0("./src/",model.dir,"/",level1.features.file.name[1]),envir=l0.env)
        
        return(l0.env$level1.features[,c("Class_1","response")])
        
        
    })
    
    predictors <- do.call(cbind,ll)
    colnames(predictors) <- paste0(level0.models,c(".Class_1","response"))
    
    if (includeResponse) {
        
        response <- factor(ifelse(df$target == 1,"Class_1","Class_0"),
                           levels=c("Class_1","Class_0"))
        ans <- list(predictors=predictors,response=response)
        
    } else {
        
        ans <- list(predictors=predictors)
        
    }
    
    return(ans)
}

# get training data




# prepare data for training
train.data <- prepL1FeatureSet3(LEVEL0.MODELS,FALSE)


# # extract subset for inital training
# set.seed(29)
# idx <- createDataPartition(train.df$target,p=FRACTION.TRAIN.DATA,list=FALSE)
# train.df <- train.df[idx,]
# 
# library(doMC)
# registerDoMC(cores = 7)
# 
# # library(doSNOW)
# # cl <- makeCluster(5,type="SOCK")
# # registerDoSNOW(cl)
# # clusterExport(cl,list("logLossEval"))
# 
# # train the model
# Sys.time()
# set.seed(825)
# 
# time.data <- system.time(mdl.fit <- do.call(train,c(list(x=train.data$predictors,
#                                                          y=train.data$response),
#                                                     CARET.TRAIN.PARMS,
#                                                     MODEL.SPECIFIC.PARMS,
#                                                     CARET.TRAIN.OTHER.PARMS)))
# 
# time.data
# mdl.fit
# # stopCluster(cl)
# 
# # prepare data for training
# test.data <- PREPARE.MODEL.DATA(LEVEL0.MODELS,test.raw)
# pred.probs <- predict(mdl.fit,newdata = test.data$predictors,type = "prob")
# 
# score <- logLossEval(pred.probs[,1],test.data$response)
# score
# 
# # record Model performance
# modelPerf.df <- read.delim(paste0(WORK.DIR,"/model_performance.tsv"),
#                          stringsAsFactors=FALSE)
# # determine if score improved
# improved <- ifelse(score < min(modelPerf.df$score),"Yes","No")
# 
# recordModelPerf(paste0(WORK.DIR,"/model_performance.tsv"),
#                               mdl.fit$method,
#                               time.data,
#                               train.data$predictors,
#                               score,
#                               improved=improved,
#                               bestTune=flattenDF(mdl.fit$bestTune),
#                               tune.grid=flattenDF(CARET.TUNE.GRID),
#                               model.parms=paste(names(MODEL.SPECIFIC.PARMS),
#                                                 as.character(MODEL.SPECIFIC.PARMS),
#                                                 sep="=",collapse=","),
#                               comment=paste0(MODEL.COMMENT,":",paste0(LEVEL0.MODELS,collapse=", ")))
# 
# modelPerf.df <- read.delim(paste0(WORK.DIR,"/model_performance.tsv"),
#                          stringsAsFactors=FALSE)
# 
# 
# #display model performance record for this run
# tail(modelPerf.df[,1:10],1)
# 
# # if last score recorded is better than previous ones save model object
# last.idx <- length(modelPerf.df$score)
# if (last.idx == 1 || improved == "Yes" || FORCE_RECORDING_MODEL) {
#     cat("found improved model, saving...\n")
#     flush.console()
#     #yes we have improvement or first score, save generated model
#     file.name <- paste0("model_",mdl.fit$method,"_",modelPerf.df$date.time[last.idx],".RData")
#     file.name <- gsub(" ","_",file.name)
#     file.name <- gsub(":","_",file.name)
#     
#     save(LEVEL0.MODELS,PREPARE.MODEL.DATA,mdl.fit,file=paste0(WORK.DIR,"/",file.name))
#     
#     # estalish pointer to current model
#     writeLines(file.name,paste0(WORK.DIR,"/this_model"))
# } else {
#     cat("no improvement!!!\n")
#     flush.console()
# }
# 


