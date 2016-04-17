###
# training skeleton
###

library(data.table)
library(caret)
# add any model specific package library commands
library(xgboost)

# set working directory
WORK.DIR <- "./src/L0_xgb31"  # modify to specify directory to contain model artififacts

# Common Functions and Global variables
source("./src/CommonFunctions.R")

# import model configuration parameters
source(paste0(WORK.DIR,"/model_parameters.R"))

MODEL.COMMENT <- "Build Model"


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



# amount of data to train
FRACTION.TRAIN.DATA <- 1.0

# force recording model flag
FORCE_RECORDING_MODEL <- FALSE

# get training data
train.df <- fread(paste0(DATA.DIR,"/train.csv"))
setkey(train.raw,ID)

if (FRACTION.TRAIN.DATA != 1.0) {
    # extract subset for inital training
    set.seed(29)
    idx <- createDataPartition(train.df$target,p=FRACTION.TRAIN.DATA,list=FALSE)
    train.df <- train.df[idx,]
}

# prepare data for training
train.data <- PREPARE.MODEL.DATA(train.df)

library(doMC)
registerDoMC(cores = 6)

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

cat("saving...\n")
date.time <- as.character(Sys.time())
file.name <- paste0("model_",CARET.TRAIN.PARMS$method,"_",date.time[last.idx],".RData")
file.name <- gsub(" ","_",file.name)
file.name <- gsub(":","_",file.name)

save(mdl.fit,PREPARE.MODEL.DATA,file=paste0(WORK.DIR,"/",file.name))

# estalish pointer to current model
writeLines(file.name,paste0(WORK.DIR,"/this_model"))




