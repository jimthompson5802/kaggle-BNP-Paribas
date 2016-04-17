###
#  average Level 1 Models
###


library(caret)
library(alabama)
library(data.table)

# import global variabels and common functions
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/L2_geom"

MODEL.METHOD <- "goemetric mean"

load(paste0(DATA.DIR,"/train_calib_test.RData"))


#
#  simple average
#
avg.probs <- apply(train.data$predictors,1,sum)/ncol(train.data$predictors)

cat("geometric avg",logLossEval(avg.probs,train.data$response),"\n")


# check score for test data
test.data <- prepL2FeatureSet(test.raw)

pred.probs <- test.data$predictors * matrix(rep(opt.wts$par,nrow(test.data$predictors)),
                                            ncol=ncol(test.data$predictors),byrow=TRUE)
pred.probs <- apply(pred.probs,1,sum)

test.score <- logLossEval(pred.probs,test.data$response)

cat("optimaal weights",opt.wts$par,", test score:",test.score,"\n")

#
# record Model performance
#

ensemble.weights <- c(opt.wts$par[1],opt.wts$par[2], opt.wts$par[3])
names(ensemble.weights) <- c("rf", "gbm_one_vs_all","gbm_one_vs_all_synth")

model.weights <- paste(c("rf", "gbm_one_vs_all","gbm_one_vs_all_syth"),ensemble.weights,sep="=",collapse=",")
bestTune <- data.frame(model.weights, stringsAsFactors=FALSE)

# set up dummy data structures to account for recrodModelPerf() function
time.data <- system.time(dummy.df <- data.frame())

# record performance
modPerf.df <- recordModelPerf(modPerf.df,MODEL.METHOD,time.data,
                              dummy.df,
                              score,bestTune)
save(modPerf.df,file=paste0(WORK.DIR,"/modPerf.RData"))

#display model performance record for this run
tail(modPerf.df[,1:(ncol(modPerf.df)-1)],1)

# if last score recorded is better than previous ones save model object
last.idx <- length(modPerf.df$score)
if (last.idx == 1 ||
        modPerf.df$score[last.idx] < min(modPerf.df$score[1:(last.idx-1)])) {
    cat("found improved model, saving...\n")
    flush.console()
    #yes we have improvement or first score, save generated model
    file.name <- paste0("/ensembleWeights_",Sys.time(),".RData")
    file.name <- gsub(" ","_",file.name)
    file.name <- gsub(":","_",file.name)
    
    save(ensemble.weights,file=paste0(WORK.DIR,file.name))
} else {
    cat("no improvement!!!\n")
    flush.console()
}

