###
# generate submission for  model
###

library(caret)
library(data.table)
# add model specific libraries
library(caTools)

# set working directory
WORK.DIR <- "./src/L0_xtc21"   # directory where model artifacts are stored

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
new.df <- fread(paste0(DATA.DIR,"/test.csv"))

#save id vector
id <- new.df$ID

# prep the data for submission
submission <- PREPARE.MODEL.DATA(new.df,includeResponse=FALSE)


# predict class probabilities
write.table(submission$predictors,file=paste0(WORK.DIR,"/py_test.tsv"),row.names = FALSE,
            sep="\t")

# execute Python prediction code
python.test.command <- paste(PYTHON_COMMAND,paste0(WORK.DIR,"/make_prediction.py"),
                             WORK.DIR,
                             py.model.file.name,
                             "py_test.tsv",
                             "py_test_predictions.tsv")
system(python.test.command)

# retrieve predictions from Python
pred.probs <- fread(paste0(WORK.DIR,"/py_test_predictions.tsv"), sep="\t")

#create kaggle submission file
write.csv(data.frame(ID=id,PredictedProb=pred.probs[,Class_1]),file=paste0(WORK.DIR,"/submission.csv"),
          row.names=FALSE)

file.remove(c(paste0(WORK.DIR,"/py_test.tsv"),
              paste0(WORK.DIR,"/py_test_predictions.tsv")))


