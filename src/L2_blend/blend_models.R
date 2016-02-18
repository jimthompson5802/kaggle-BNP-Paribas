###
#  train ensemble model by calculating weight factors
###


library(caret)
library(gbm)
library(randomForest)
library(alabama)

source("./src/CommonFunctions.R")

# import global variabels and common functions
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/ensemble_model"

MODEL.METHOD <- "ensemble"

load(paste0(DATA.DIR,"/train_calib_test.RData"))
load(paste0(DATA.DIR,"/diff_train_calib_test.RData"))
#
# make rf prediction with expanded feature set
#
source("./src/rf2_model/ModelCommonFunctions.R")
# read kaggle submission data
new.df <- prepModelData(calib.raw)

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
source("./src/gbm2_model/ModelCommonFunctions.R")
new.df <- prepModelData(calib.raw)

# retrive one versus all gbm model
load(paste0("./src/gbm2_model/model_gbm_one_vs_all_2015-05-08_22_59_43.RData"))

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
source("./src/gbm4_model/ModelCommonFunctions.R")

# get class specific feature set
load("./eda/selected_features_for_each_class.RData")

# read kaggle submission data
new.df <- calib.raw

d.new.df <- d.calib.raw

#save id vector
id <- new.df$id

# prep the data for submission
new.df <- prepModelData(new.df,d.new.df)

# retrive one versus all gbm model
load("src/gbm4_model/model_gbm_one_vs_all_2015-05-14_12_00_38.RData")

# predict class probabilities
ll <- lapply(PRODUCT.CLASSES,predictForOneClass,gbm.mdls,new.df$predictors,
             class.feature.list)
names(ll) <- PRODUCT.CLASSES

gbm4.probs <- do.call(cbind,ll)

#
# individual model Log Loss
#
cat("rf2",logLossEval(rf2.probs,calib.raw$target),"\n")
cat("gbm2",logLossEval(gbm2.probs,calib.raw$target),"\n")
cat("gbm4",logLossEval(gbm4.probs,calib.raw$target),"\n")


#
#  simple average
#
avg.probs <- (rf2.probs + gbm2.probs + gbm4.probs) / 3.0

cat("simple avg",logLossEval(avg.probs,calib.raw$target),"\n")




#
# determine optimal weighting factor for combining model estimates
#
makeEnsembleFunction <- function(target,rf2.probs, gbm2.probs,gbm4.probs) {
    
    probs.mat <- cbind(rf2.probs,gbm2.probs,gbm4.probs)
    
    function(w) {
        wmat <- rbind(diag(w[1:9]),diag(w[10:18]),diag(w[19:27]))
        
        pred.probs <- probs.mat %*% wmat
        
        colnames(pred.probs) <- paste0("Class_",1:9)
        logLossEval(pred.probs,target)
    }
}

ensFunc <- makeEnsembleFunction(calib.raw$target,rf2.probs,gbm2.probs,gbm4.probs)



# grFunc <- makeGradientFunction(calib.raw$target,rf2.probs,gbm2.probs,gbm4.probs)

# define equality constraints
heq <- function(w) {
    h <- rep(NA,9)
    
    h[1] <- sum(w[c(1,10,19)]) - 1
    h[2] <- sum(w[c(2,11,20)]) - 1
    h[3] <- sum(w[c(3,12,21)]) - 1
    h[4] <- sum(w[c(4,13,22)]) - 1
    h[5] <- sum(w[c(5,14,23)]) - 1
    h[6] <- sum(w[c(6,15,24)]) - 1
    h[7] <- sum(w[c(7,16,25)]) - 1
    h[8] <- sum(w[c(8,17,26)]) - 1
    h[9] <- sum(w[c(9,18,27)]) - 1
    return(h)
}

heq.jac <- function(w){
    
    return(cbind(diag(1,9),diag(1,9),diag(1,9)))
}

# define inequality constraints
hin <- function(w) {
    
    return(c(w,1-w))
    
}

hin.jac <- function(w) {
    
    return(rbind(diag(1,27),diag(-1,27)))
}

system.time(opt.wts <- constrOptim.nl(rep(1/3,27),fn=ensFunc,  #gr=grFunc,
                                      hin=hin, hin.jac=hin.jac,
                                      heq=heq, heq.jac=heq.jac,
                                      control.outer=list(itmax=1),
                                      control.optim=list(trace=2)))

score <- opt.wts$value


opt.probs <- opt.wts$par[1] * rf2.probs + opt.wts$par[2] * gbm2.probs + opt.wts$par[3] * gbm4.probs

cat("optimaal weights",logLossEval(opt.probs,calib.raw$target),"\n")



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

