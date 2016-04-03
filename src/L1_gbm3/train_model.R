###
# training skeleton for training Level 1 model
###

library(data.table)
library(caret)
# add any model specific package library commands
library(gbm)

# set working directory
WORK.DIR <- "./src/L1_gbm3"  # modify to specify directory to contain model artififacts

# Common Functions and Global variables
source("./src/CommonFunctions.R")

# set caret training parameters
CARET.TRAIN.PARMS <- list(method="gbm")   # Replace MODEL.METHOD with appropriate caret model

CARET.TUNE.GRID <- expand.grid(interaction.depth=5,n.trees=1000,
                                shrinkage=0.01,n.minobsinnode=10)

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
PREPARE.MODEL.DATA <- prepL1FeatureSet2

MODEL.COMMENT <- "prepL1FeatureSet2"

LEVEL0.MODELS <- c("L0_gbm2",
                   "L0_gbm4",
                   "L0_xtc1",
                   "L0_xtc2",
                   "L0_xtc3",
                   "L0_xgb2",
                   "L0_xgb3")

# amount of data to train
FRACTION.TRAIN.DATA <- 0.25

# force recording model flag
FORCE_RECORDING_MODEL <- FALSE

# get training data
load(paste0(DATA.DIR,"/train_calib_test.RData"))
train.df <- train1.raw

# extract subset for inital training
set.seed(29)
idx <- createDataPartition(train.df$target,p=FRACTION.TRAIN.DATA,list=FALSE)
train.df <- train.df[idx,]

library(doMC)
registerDoMC(cores = 7)


# prepare data for training
train.data <- PREPARE.MODEL.DATA(LEVEL0.MODELS,train.df)


# library(doSNOW)
# cl <- makeCluster(5,type="SOCK")
# registerDoSNOW(cl)
# clusterExport(cl,list("logLossEval"))

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

# prepare data for training
test.data <- PREPARE.MODEL.DATA(LEVEL0.MODELS,test.raw)
pred.probs <- predict(mdl.fit,newdata = test.data$predictors,type = "prob")

score <- logLossEval(pred.probs[,1],test.data$response)
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
    file.name <- paste0("model_",mdl.fit$method,"_",modelPerf.df$date.time[last.idx],".RData")
    file.name <- gsub(" ","_",file.name)
    file.name <- gsub(":","_",file.name)
    
    save(LEVEL0.MODELS,PREPARE.MODEL.DATA,mdl.fit,file=paste0(WORK.DIR,"/",file.name))
    
    # estalish pointer to current model
    writeLines(file.name,paste0(WORK.DIR,"/this_model"))
} else {
    cat("no improvement!!!\n")
    flush.console()
}



