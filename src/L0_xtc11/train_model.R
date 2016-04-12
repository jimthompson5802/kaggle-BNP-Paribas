###
# training skeleton
###

library(data.table)
library(caret)
# add any model specific package library commands


# set working directory
WORK.DIR <- "./src/L0_xtc11"  # modify to specify directory to contain model artififacts

# Common Functions and Global variables
source("./src/CommonFunctions.R")

# import model configuration parameters
source(paste0(WORK.DIR,"/model_parameters.R"))

# import model configuration parameters
source(paste0(WORK.DIR,"/model_parameters.R"))

MODEL.COMMENT <- "K-Fold, Build Model"


# amount of data to train
FRACTION.TRAIN.DATA <- 0.1

# force recording model flag
FORCE_RECORDING_MODEL <- FALSE

# get training data
train.df <- fread(paste0(DATA.DIR,"/train.csv"))
setkey(train.df,ID)

if (FRACTION.TRAIN.DATA != 1.0) {
    # extract subset for inital training
    set.seed(29)
    idx <- createDataPartition(train.df$target,p=FRACTION.TRAIN.DATA,list=FALSE)
    train.df <- train.df[idx,]
}

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


cat("saving...\n")
date.time <- as.character(Sys.time())
file.name <- paste0("model_",MODEL.NAME,"_",date.time,".RData")
file.name <- gsub(" ","_",file.name)
file.name <- gsub(":","_",file.name)
save(PREPARE.MODEL.DATA,file=paste0(WORK.DIR,"/",file.name))

# save Python model data
py.file.name <- paste0("model_",MODEL.NAME,"_",date.time,".PyData")
py.file.name <- gsub(" ","_",py.file.name)
py.file.name <- gsub(":","_",py.file.name)
file.rename(paste0(WORK.DIR,"/possible_model.PyData"),paste0(WORK.DIR,"/",py.file.name))

# estalish pointer to current model
writeLines(c(file.name,py.file.name),paste0(WORK.DIR,"/this_model"))

# clean up files no longer needed
file.remove(paste0(WORK.DIR,"/py_train.tsv"))
              #paste0(WORK.DIR,"/py_test.tsv"),
              #paste0(WORK.DIR,"/py_test_predictions.tsv")
              #))


