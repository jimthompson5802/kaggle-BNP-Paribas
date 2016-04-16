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
                           tuneLength=7,
                           metric="LogLoss")

MODEL.SPECIFIC.PARMS <- list(verbose=FALSE) #NULL # Other model specific parameters

PREPARE.MODEL.DATA <- prepL1FeatureSet1

MODEL.COMMENT <- "Only Class_1 probabilites as features"

LEVEL0.MODELS <- c("L0_gbm21",
                   "L0_gbm41",
                   "L0_xtc11",
                   "L0_xtc21",
                   "L0_xtc31",
                   #"L0_xtc4",  did not improve score
                   "L0_xtc51",
                   "L0_nnet11",
                   "L0_xgb21",
                   "L0_xgb31")

# amount of data to train
FRACTION.TRAIN.DATA <- 1.0

# force recording model flag
FORCE_RECORDING_MODEL <- FALSE


# get training data
train.data <- prepL1FeatureSet3(LEVEL0.MODELS)


# # create the partitions
# set.seed(13)
# data.folds <- createFolds(raw$target, k=5)

library(doMC)
registerDoMC(cores = 7)


# train the model
Sys.time()
set.seed(825)

time.data <- system.time(mdl.fit <- do.call(train,c(list(x=train.data$predictors,
                                                         y=train.data$response),
                                                    CARET.TRAIN.PARMS,
                                                    MODEL.SPECIFIC.PARMS,
                                                    CARET.TRAIN.OTHER.PARMS)))

time.data
mdl.fit
# stopCluster(cl)

score <- mean(mdl.fit$resample$LogLoss)
score

# record Model performance
modelPerf.df <- read.delim(paste0(WORK.DIR,"/model_performance.tsv"),
                         stringsAsFactors=FALSE)
# determine if score improved
improved <- ifelse(score < min(modelPerf.df$score),"Yes","No")

recordModelPerf(paste0(WORK.DIR,"/model_performance.tsv"),
                              mdl.fit$method,
                              time.data,
                              train.data$predictors,
                              score,
                              improved=improved,
                              bestTune=flattenDF(mdl.fit$bestTune),
                              tune.grid=flattenDF(CARET.TUNE.GRID),
                              model.parms=paste(names(MODEL.SPECIFIC.PARMS),
                                                as.character(MODEL.SPECIFIC.PARMS),
                                                sep="=",collapse=","),
                              comment=paste0(MODEL.COMMENT,":",paste0(LEVEL0.MODELS,collapse=", ")))

modelPerf.df <- read.delim(paste0(WORK.DIR,"/model_performance.tsv"),
                         stringsAsFactors=FALSE)


#display model performance record for this run
tail(modelPerf.df[,1:10],1)

# if last score recorded is better than previous ones save model object
last.idx <- length(modelPerf.df$score)
if (last.idx == 1 || improved == "Yes" || FORCE_RECORDING_MODEL) {
    cat("found improved model, saving...\n")
    flush.console()
    #yes we have improvement or first score, save generated model
    # file.name <- paste0("model_",mdl.fit$method,"_",modelPerf.df$date.time[last.idx],".RData")
    file.name <- paste0("model_",mdl.fit$method,"_",as.character(Sys.time()),".RData")
    file.name <- gsub(" ","_",file.name)
    file.name <- gsub(":","_",file.name)

    save(LEVEL0.MODELS,PREPARE.MODEL.DATA,mdl.fit,file=paste0(WORK.DIR,"/",file.name))

    # estalish pointer to current model
    writeLines(file.name,paste0(WORK.DIR,"/this_model"))
} else {
    cat("no improvement!!!\n")
    flush.console()
}



