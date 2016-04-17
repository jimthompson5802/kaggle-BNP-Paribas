###
#  train ensemble model by calculating weight factors
###


library(caret)
library(alabama)
library(data.table)

# import global variabels and common functions
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/L2_blnd2"

MODEL.METHOD <- "blend"

MODEL.COMMENT <- "Determine optimal weights"

FORCE_RECORDING_MODEL <- FALSE

# get training data for calibration
# L1_nnet11
nnet11.env <- new.env()
load("./src/L1_nnet11/level2_features.RData",envir=nnet11.env)

# L1_xgb11
xgb11.env <- new.env()
load("./src/L1_xgb11/level2_features.RData",envir=xgb11.env)

# combine Level 1 Calibration data
train.data <- list()
train.data$predictors <- cbind(nnet11=nnet11.env$level2.data[,"Class_1"],
                               xgb11=xgb11.env$level2.data[,"Class_1"])

train.data$response = nnet11.env$level2.data$response


#
# determine optimal weighting factor for combining model estimates
#
makeBlendFunction <- function(target,probs.mat) {
    
    n.probs <- ncol(probs.mat)
    
    function(x) {
        
        w <- c(x,1-x)
        
        wt.mat <- matrix(rep(w,nrow(probs.mat)),
                         ncol=ncol(probs.mat),byrow=TRUE)
        
        pred.probs <- probs.mat * wt.mat
            
        pred.probs <- apply(pred.probs,1,sum)
        
        ans <- logLossEval(pred.probs,target)
        return(ans)
    }
}


ensFunc <- makeBlendFunction(train.data$response,as.matrix(train.data$predictors))

wt1 <- seq(0.01,0.99,0.01)

value <- sapply(wt1,ensFunc)


cat("optimal weights",opt.wts$par,", test score:",score,"\n")

#
# record Model performance
#

blending.weights <- opt.wts$par

# model.weights <- paste(names(blending.weights),blending.weights,sep="=",collapse=",")
# bestTune <- data.frame(model.weights, stringsAsFactors=FALSE)
# 
# # record Model performance
# modelPerf.df <- read.delim(paste0(WORK.DIR,"/model_performance.tsv"),
#                            stringsAsFactors=FALSE)
# # determine if score improved
# improved <- ifelse(score < min(modelPerf.df$score),"Yes","No")
# 
# recordModelPerf(paste0(WORK.DIR,"/model_performance.tsv"),
#                 MODEL.METHOD,
#                 time.data,
#                 train.data$predictors,
#                 score,
#                 improved=improved,
#                 bestTune=flattenDF(bestTune),
#                 tune.grid=NA,
#                 model.parms=NA,
#                 comment=MODEL.COMMENT)
# 
# modelPerf.df <- read.delim(paste0(WORK.DIR,"/model_performance.tsv"),
#                            stringsAsFactors=FALSE)
# 
# 
# #display model performance record for this run
# tail(modelPerf.df[,1:10],1)
# 
# # if last score recorded is better than previous ones save model object
# last.idx <- length(modelPerf.df$score)
# if (last.idx == 1 || improved == "Yes" || FORCE_RECORDING_MODEL) {
#     cat("found improved model, saving...\n")
#     flush.console()
#     #yes we have improvement or first score, save generated model
#     file.name <- paste0("model_",MODEL.METHOD,"_",modelPerf.df$date.time[last.idx],".RData")
#     file.name <- gsub(" ","_",file.name)
#     file.name <- gsub(":","_",file.name)
#     
#     save(blending.weights,file=paste0(WORK.DIR,"/",file.name))
#     
#     # estalish pointer to current model
#     writeLines(file.name,paste0(WORK.DIR,"/this_model"))
# } else {
#     cat("no improvement!!!\n")
#     flush.console()
# }

