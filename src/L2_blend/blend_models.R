###
#  train ensemble model by calculating weight factors
###


library(caret)
library(alabama)
library(data.table)

# import global variabels and common functions
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/L2_blend"

MODEL.METHOD <- "blend"

load(paste0(DATA.DIR,"/train_calib_test.RData"))

# training data for blending
train.data <- prepL2FeatureSet(calib.raw)



#
#  simple average
#
avg.probs <- apply(train.data$predictors,1,sum)/ncol(train.data$predictors)

cat("simple avg",logLossEval(avg.probs,train.data$response),"\n")




#
# determine optimal weighting factor for combining model estimates
#
makeBlendFunction <- function(target,probs.mat) {
    
    n.probs <- ncol(probs.mat)
    
    function(w) {
        
        pred.probs <- probs.mat * matrix(rep(w,nrow(probs.mat)),
                                             ncol=ncol(probs.mat),byrow=TRUE)
        pred.probs <- apply(pred.probs,1,sum)
        
        logLossEval(pred.probs,target)
    }
}


ensFunc <- makeBlendFunction(train.data$response,as.matrix(train.data$predictors))


# define equality constraints
heq <- function(w) {
    h <- rep(NA,1)
    
    h[1] <- sum(w) - 1

    return(h)
}

# heq.jac <- function(w){
#     j <- matrix(NA,1,length(w))
#     j[1,] <- rep(1,length(w))
# }


# define inequality constraints
hin <- function(w) {
    
    return(c(w,1-w))
    
}
# 
# hin.jac <- function(w) {
#     
#     return(rbind(diag(1,27),diag(-1,27)))
# }
# 
system.time(opt.wts <- constrOptim.nl(rep(1/ncol(train.data$predictors),ncol(train.data$predictors)),
                                      fn=ensFunc,  #gr=grFunc,
                                      hin=hin, #hin.jac=hin.jac,
                                      heq=heq, #heq.jac=heq.jac,
                                      #control.outer=list(itmax=10),
                                      control.optim=list(trace=2)))

score <- opt.wts$value

names(opt.wts$par) <- names(train.data$predictors)
cat("optimaal weights",opt.wts$par,", training score:",opt.wts$value,"\n")


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

