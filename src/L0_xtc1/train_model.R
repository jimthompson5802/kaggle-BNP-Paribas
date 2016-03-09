###
# training module to python scikit-learn algorithm
###

library(data.table)
library(caret)
# add any model specific package library commands

# set working directory
WORK.DIR <- "./src/L0_xtc1"  # modify to specify directory to contain model artififacts

# Common Functions and Global variables
source("./src/CommonFunctions.R")

# set caret training parameters
MODEL.NAME <- "ExtraTreeClassifier"

#PREPARE.MODEL.DATA <- function(data){return(data)}  #default data prep
PREPARE.MODEL.DATA <- prepL0FeatureSetAll

MODEL.COMMENT <- "All Features, Python model"


# amount of data to train
FRACTION.TRAIN.DATA <- 1
# get training data
load(paste0(DATA.DIR,"/train_calib_test.RData"))
train.df <- rbind(train0.raw,train1.raw,calib.raw)

# extract subset for inital training
set.seed(29)
idx <- createDataPartition(train.df$target,p=FRACTION.TRAIN.DATA,list=FALSE)
train.df <- train.df[idx,]

# prepare data for training
train.data <- PREPARE.MODEL.DATA(train.df)

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
test.data <- PREPARE.MODEL.DATA(test.raw)
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

# record Model performance
modelPerf.df <- read.delim(paste0(WORK.DIR,"/model_performance.tsv"),
                         stringsAsFactors=FALSE)
# determine if score improved
improved <- ifelse(score < min(modelPerf.df$score),"Yes","No")

recordModelPerf(paste0(WORK.DIR,"/model_performance.tsv"),
                              MODEL.NAME,
                              time.data,
                              train.data$predictors,
                              score,
                              improved=improved,
                              bestTune="NA",
                              tune.grid="NA",
                              model.parms="NA",
                              comment=paste0(MODEL.COMMENT))

modelPerf.df <- read.delim(paste0(WORK.DIR,"/model_performance.tsv"),
                         stringsAsFactors=FALSE)


#display model performance record for this run
tail(modelPerf.df[,1:10],1)

# if last score recorded is better than previous ones save model object
last.idx <- length(modelPerf.df$score)
if (last.idx == 1 || improved == "Yes") {
    cat("found improved model, saving...\n")
    flush.console()
    #yes we have improvement or first score, save R-based data
    file.name <- paste0("model_",MODEL.NAME,"_",modelPerf.df$date.time[last.idx],".RData")
    file.name <- gsub(" ","_",file.name)
    file.name <- gsub(":","_",file.name)
    save(PREPARE.MODEL.DATA,file=paste0(WORK.DIR,"/",file.name))
    
    # save Python model data
    py.file.name <- paste0("model_",MODEL.NAME,"_",modelPerf.df$date.time[last.idx],".PyData")
    py.file.name <- gsub(" ","_",py.file.name)
    py.file.name <- gsub(":","_",py.file.name)
    file.rename(paste0(WORK.DIR,"/possible_model"),paste0(WORK.DIR,"/",py.file.name))
    
    # estalish pointer to current model
    writeLines(c(file.name,py.file.name),paste0(WORK.DIR,"/this_model"))
} else {
    cat("no improvement!!!\n")
    flush.console()
}

# clean up files no longer needed
file.remove(c(paste0(WORK.DIR,"/py_train.tsv"),paste0(WORK.DIR,"/py_test.tsv"),
              paste0(WORK.DIR,"/py_test_predictions.tsv")))

