###
#  create ensemble model combining selected models
###


library(caret)
library(gbm)
library(randomForest)
library(kernlab)

# import global variabels and common functions
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/ensemble_model"

# load optimal weighting factors
load(paste0(WORK.DIR,"/opt_wts_2015-05-26_21_15_00.RData"))

# get near zero Vars to eliminate
load(paste0(DATA.DIR,"/near_zero_vars.RData"))

# read kaggle submission data
new.df <- read.csv(unz(paste0(DATA.DIR,"/test.csv.zip"),"test.csv"),stringsAsFactors=FALSE)

#save id vector
id <- new.df$id

#
# make gbm prediction
#

# # prep the data for submission
# source("./src/gbm_model/ModelCommonFunctions.R")
# new.df <- prepModelData(new.df,only.predictors=TRUE)
# 
# # retrive gbm model
# load("./src/gbm_model/gbmFit1_2015-04-07_21_12_42.RData")
# 
# # predict class probabilities
# gbm.probs <- predict(gbmFit1,newdata = new.df$predictors,type = "prob")
# 
# # combine with id
# gbm.probs <- data.frame(id,gbm.probs)

#
# make rf prediction
#

# # read kaggle submission data
# new.df <- read.csv(unz(paste0(DATA.DIR,"/test.csv.zip"),"test.csv"),stringsAsFactors=FALSE)
# 
# #save id vector
# id <- new.df$id
# 
# # prep the data for submission
# source("./src/rf_model/ModelCommonFunctions.R")
# new.df <- prepModelData(new.df,only.predictors=TRUE)
# 
# # retrive rf model
# load("./src/rf_model/rfFit1_2015-04-09_23_06_33.RData")
# 
# # predict class probabilities
# rf.probs <- predict(rfFit1,newdata = new.df$predictors,type = "prob")
# 
# # recombine with id
# rf.probs <- data.frame(id,rf.probs)

#
# make rf prediction with expanded feature set
#
source("./src/rf2_model/ModelCommonFunctions.R")
# read kaggle submission data
new.df <- read.csv(unz(paste0(DATA.DIR,"/test.csv.zip"),"test.csv"),stringsAsFactors=FALSE)
new.df <- prepModelData(new.df,only.predictors = TRUE)
#save id vector
id <- new.df$id

# retrive rf model with expanded features
load("./src/rf2_model/model_rf_all_data_ntree_4000.RData")

# predict class probabilities
system.time(rf2.probs <- predictInParallel(mdl.fit,new.df$predictors,5,only.predictors = TRUE))

#
# make one vs all using gbm predictions
#

predictForOneClass <- function(this.class,mdls,new.data) {
    pred.probs <- predict(mdls[[this.class]],newdata = new.data,type = "prob")
    return(pred.probs[,1])
}

# read kaggle submission data
new.df <- read.csv(unz(paste0(DATA.DIR,"/test.csv.zip"),"test.csv"),stringsAsFactors=FALSE)

#save id vector
id <- new.df$id

# prep the data for submission
source("./src/gbm2_model/ModelCommonFunctions.R")
new.df <- prepModelData(new.df,only.predictors=TRUE)

# retrive one versus all gbm model
load("./src/gbm2_model/model_gbm_one_vs_all_2015-05-08_22_59_43.RData")

# predict class probabilities
classes <- paste("Class_",1:9,sep="")  # generate list of classes to model
ll <- lapply(classes,predictForOneClass,gbm.mdls,new.df$predictors)
names(ll) <- classes

gbm2.probs <- do.call(cbind,ll)


#
# gbm one vs all model
# with class specific synthetic features
#

# import global variabels and common functions
source("./src/CommonFunctions.R")
source("./src/gbm4_model/ModelCommonFunctions.R")

# get class specific feature set
load("./eda/selected_features_for_each_class.RData")

# read kaggle submission data
new.df <- read.csv(unz(paste0(DATA.DIR,"/test.csv.zip"),"test.csv"),stringsAsFactors=FALSE)

d.new.df <- data.frame(calcPairwiseDiff(setdiff(names(new.df),c("id","target")),new.df))

#save id vector
id <- new.df$id

# prep the data for submission
submission <- prepModelData(new.df,d.new.df,only.predictors=TRUE)

# retrive one versus all gbm model
load("src/gbm4_model/model_gbm_one_vs_all_2015-05-14_12_00_38.RData")

# predict class probabilities
ll <- lapply(PRODUCT.CLASSES,predictForOneClass,gbm.mdls,submission$predictors,
             class.feature.list)
names(ll) <- PRODUCT.CLASSES

gbm4.probs <- do.call(cbind,ll)


# #
# # Average the individual probablities
# #
# 
# pred.probs <- ((1/3)*rf2.probs) + 
#                 ((1/3)*gbm2.probs) +
#                 ((1/3)*gbm4.probs)


# 
# use optimal weights calculated
#

# combine three models probabilities into a single matrix
probs.mat <- cbind(rf2.probs,gbm2.probs,gbm4.probs)

#set up weight matrix to combine the individual weights
wmat <- rbind(diag(opt.wts$par[1:9]),diag(opt.wts$par[10:18]),diag(opt.wts$par[19:27]))

# compute overall probablities using invidual model class weights
pred.probs <- probs.mat %*% wmat
colnames(pred.probs) <- paste0("Class_",1:9)
    

#create kaggle submission file
write.csv(data.frame(id,pred.probs),file=paste0(WORK.DIR,"/submission.csv"),
          row.names=FALSE)

