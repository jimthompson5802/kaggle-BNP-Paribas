###
# For this Level 1 model, create calibration data for determining weights for Level 2
###

library(caret)
library(data.table)
# add model specific libraries
library(caTools)

# set working directory
WORK.DIR <- "./src/L1_xtc1"   # directory where model artifacts are stored

# Common Functions and Global variables
source("./src/CommonFunctions.R")

# retrive generated model-name created in training run
model.files <- readLines(paste0(WORK.DIR,"/this_model"))

# get R based data
model.file.name <- model.files[1]
load(paste0(WORK.DIR,"/",model.file.name))

# get Python based model data
py.model.file.name <- model.files[2]

# read kaggle submission data
load(paste0(DATA.DIR,"/train_calib_test.RData"))

# data used for determining optimal weights
new.df <- calib.raw
#save id vector
id <- new.df$ID

# prep the data for prediction model
calib.data <- PREPARE.MODEL.DATA(LEVEL0.MODELS,new.df,includeResponse=TRUE)

# predict class probabilities
write.table(calib.data$predictors,file=paste0(WORK.DIR,"/py_calib.tsv"),row.names = FALSE,
            sep="\t")
# execute Python prediction code
python.predict.command <- paste(PYTHON_COMMAND,paste0(WORK.DIR,"/make_prediction.py"),
                             WORK.DIR,
                             py.model.file.name,
                             "py_calib.tsv",
                             "py_calib_predictions.tsv")
system(python.predict.command)

# retrieve predictions from Python
pred.probs <- fread(paste0(WORK.DIR,"/py_calib_predictions.tsv"), sep="\t")

# augment with identifier and target variable
calib.pred.probs <- cbind(ID=id,pred.probs,target=calib.data$response)

# data used to test 
new.df <- test.raw
#save id vector
id <- new.df$ID

# prep the data for prediction model
test.data <- PREPARE.MODEL.DATA(LEVEL0.MODELS,new.df,includeResponse=TRUE)

# predict class probabilities
write.table(calib.data$predictors,file=paste0(WORK.DIR,"/py_test.tsv"),row.names = FALSE,
            sep="\t")

# execute Python prediction code
python.predict.command <- paste(PYTHON_COMMAND,paste0(WORK.DIR,"/make_prediction.py"),
                                WORK.DIR,
                                py.model.file.name,
                                "py_test.tsv",
                                "py_test_predictions.tsv")
system(python.predict.command)

# retrieve predictions from Python
pred.probs <- fread(paste0(WORK.DIR,"/py_test_predictions.tsv"), sep="\t")

# augment with identifier and target variable
test.pred.probs <- cbind(ID=id,pred.probs,target=test.data$response)


# save data for calibrating Level 2 model weights
save(calib.pred.probs,test.pred.probs,file=paste0(WORK.DIR,"/data_for_level2_optimization.RData"))




