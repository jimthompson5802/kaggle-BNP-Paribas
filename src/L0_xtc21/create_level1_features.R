###
# Model training
###

library(data.table)
library(plyr)
library(caret)
# add any model specific package library commands


# set working directory
WORK.DIR <- "./src/L0_xtc21"  # modify to specify directory to contain model artififacts

# Common Functions and Global variables
source("./src/CommonFunctions.R")

# import model configuration parameters
source(paste0(WORK.DIR,"/model_parameters.R"))



MODEL.COMMENT <- "prepL0FeatureSetAll, 5-fold training"

# amount of data to train
FRACTION.TRAIN.DATA <- 0.1

# force recording model flag
FORCE_RECORDING_MODEL <- FALSE

# get training data
train.raw <- fread(paste0(DATA.DIR,"/train.csv"))
setkey(train.raw,ID)

# get data fold specification
load(paste0(DATA.DIR,"/fold_specification.RData"))


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
    
    # save prepared training data for Python function
    # put response as first column in data set
    write.table(cbind(response=train.data$response,train.data$predictors),
                file=paste0(WORK.DIR,"/py_train.tsv"),row.names = FALSE,
                sep="\t")
    
    
    # invoke Python training model
    python.train.command <- paste(PYTHON_COMMAND,paste0(WORK.DIR,"/train_model.py"),WORK.DIR)
    
    Sys.time()
    
    
    time.data <- system.time(system(python.train.command))
    
    time.data
    # stopCluster(cl)
    
    # prepare data for training
    write.table(test.data$predictors,file=paste0(WORK.DIR,"/py_test.tsv"),row.names = FALSE,
                sep="\t")
    
    # execute Python prediction code
    python.test.command <- paste(PYTHON_COMMAND,paste0(WORK.DIR,"/make_prediction.py"),
                                 WORK.DIR,
                                 "possible_model",
                                 "py_test.tsv",
                                 "py_test_predictions.tsv")
    system(python.test.command)
    
    # get predictions from Python model
    pred.probs <- fread(paste0(WORK.DIR,"/py_test_predictions.tsv"), sep="\t")
    
    score <- logLossEval(pred.probs[,Class_1],test.data$response)
    score
    
    # clean up files no longer needed
    file.remove(c(paste0(WORK.DIR,"/py_train.tsv"),paste0(WORK.DIR,"/py_test.tsv"),
                  paste0(WORK.DIR,"/possible_model"),
                  paste0(WORK.DIR,"/py_test_predictions.tsv")))
    
    ans <- list(score=score,
                level1.features=data.frame(ID=test.data$ID,pred.probs,response=test.data$response))
    
    return(ans)
    
}
# train the model
Sys.time()

time.data <- system.time(ll <- llply(data.folds,trainFolds,.parallel = FALSE))

time.data

fold.scores <- unlist(lapply(ll,function(x){x$score}))
level1.features <- do.call(rbind,lapply(ll,function(x){x$level1.features}))

mean(fold.scores)

# record Model performance
modelPerf.df <- read.delim(paste0(WORK.DIR,"/model_performance.tsv"),
                         stringsAsFactors=FALSE)
# determine if score improved
improved <- ifelse(mean(fold.scores) < min(modelPerf.df$score),"Yes","No")

recordModelPerf(paste0(WORK.DIR,"/model_performance.tsv"),
                                MODEL.NAME,
                              time.data,
                              data.frame(),
                              mean(fold.scores),
                              improved=improved,
                              bestTune=NA,
                              tune.grid=NA,
                              model.parms=NA,
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
    file.name <- paste0("level1_features_",MODEL.NAME,"_",modelPerf.df$date.time[last.idx],".RData")
    file.name <- gsub(" ","_",file.name)
    file.name <- gsub(":","_",file.name)
    
    save(level1.features,PREPARE.MODEL.DATA,file=paste0(WORK.DIR,"/",file.name))
    
    # estalish pointer to current model
    writeLines(file.name,paste0(WORK.DIR,"/this_level1_features"))
} else {
    cat("no improvement!!!\n")
    flush.console()
}



