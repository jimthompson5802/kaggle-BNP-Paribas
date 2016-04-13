###
# Model training
###

library(data.table)
library(plyr)
library(caret)
# add any model specific package library commands
library(gbm)

# set working directory
WORK.DIR <- "./src/L0_gbm41"  # modify to specify directory to contain model artififacts

# Common Functions and Global variables
source("./src/CommonFunctions.R")

# import model configuration parameters
source(paste0(WORK.DIR,"/model_parameters.R"))


# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="none",
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

MODEL.COMMENT <- "prepL0FeatureSet2, 5-fold training"

# amount of data to train
FRACTION.TRAIN.DATA <- 1.0

# force recording model flag
FORCE_RECORDING_MODEL <- TRUE

# get training data
train.raw <- fread(paste0(DATA.DIR,"/train.csv"))
setkey(train.raw,ID)

# get data fold specification
load(paste0(DATA.DIR,"/fold_specification.RData"))



library(doMC)
registerDoMC(cores = 6)

# library(doSNOW)
# cl <- makeCluster(5,type="SOCK")
# registerDoSNOW(cl)
# clusterExport(cl,list("logLossEval"))


trainFolds <- function(this.fold) {
    # prepare data for training
    test.data <- PREPARE.MODEL.DATA(train.raw[this.fold,])
    test.data$ID <- train.raw[this.fold,ID]
    
    train.data <- PREPARE.MODEL.DATA(train.raw[-this.fold,])
    
    
    if (FRACTION.TRAIN.DATA != 1 ) {
        # extract subset for inital training
        set.seed(29)
        idx <- createDataPartition(train.data$response,p=FRACTION.TRAIN.DATA,list=FALSE)
        train.data$predictors <- train.data$predictors[idx,]
        train.data$response <- train.data$response[idx]
    }
    
    set.seed(825)
    time.data <- system.time(mdl.fit <- do.call(train,c(list(x=train.data$predictors,
                                                             y=train.data$response),
                                                        CARET.TRAIN.PARMS,
                                                        MODEL.SPECIFIC.PARMS,
                                                        CARET.TRAIN.OTHER.PARMS)))
    time.data
    
    
    pred.probs <- predict(mdl.fit,newdata = test.data$predictors,type = "prob")
    
    score <- logLossEval(pred.probs[,"Class_1"],test.data$response)
    score
    
    ans <- list(score=score,
                level1.features=data.frame(ID=test.data$ID,pred.probs,response=test.data$response))
    
    return(ans)
    
}
# train the model
Sys.time()

time.data <- system.time(ll <- llply(data.folds,trainFolds,.parallel = TRUE))

time.data
# stopCluster(cl)

fold.scores <- unlist(lapply(ll,function(x){x$score}))
level1.features <- do.call(rbind,lapply(ll,function(x){x$level1.features}))

mean(fold.scores)

# record Model performance
modelPerf.df <- read.delim(paste0(WORK.DIR,"/model_performance.tsv"),
                         stringsAsFactors=FALSE)
# determine if score improved
improved <- ifelse(mean(fold.scores) < min(modelPerf.df$score),"Yes","No")

recordModelPerf(paste0(WORK.DIR,"/model_performance.tsv"),
                              CARET.TRAIN.PARMS$method,
                              time.data,
                              data.frame(),
                              mean(fold.scores),
                              improved=improved,
                              bestTune=NA,
                              tune.grid=flattenDF(CARET.TUNE.GRID),
                              model.parms=paste(names(MODEL.SPECIFIC.PARMS),
                                                as.character(MODEL.SPECIFIC.PARMS),
                                                sep="=",collapse=","),
                              comment=MODEL.COMMENT)

modelPerf.df <- read.delim(paste0(WORK.DIR,"/model_performance.tsv"),
                         stringsAsFactors=FALSE)


#display model performance record for this run
tail(modelPerf.df[,1:10],1)

# if last score recorded is better than previous ones save model object
last.idx <- length(modelPerf.df$score)
if (last.idx == 1 || improved == "Yes"  || FORCE_RECORDING_MODEL) {
    cat("found improved model, saving...\n")
    flush.console()
    #yes we have improvement or first score, save generated model
    file.name <- paste0("level1_features_",CARET.TRAIN.PARMS$method,"_",modelPerf.df$date.time[last.idx],".RData")
    file.name <- gsub(" ","_",file.name)
    file.name <- gsub(":","_",file.name)
    
    save(level1.features,PREPARE.MODEL.DATA,file=paste0(WORK.DIR,"/",file.name))
    
    # estalish pointer to current model
    writeLines(file.name,paste0(WORK.DIR,"/this_level1_features"))
} else {
    cat("no improvement!!!\n")
    flush.console()
}



