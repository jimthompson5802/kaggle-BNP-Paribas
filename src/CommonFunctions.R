###
# Global Variables
###

DATA.DIR <- "./data"


###
# Model Evaluation function - Log Los
###
# Evaulation Function
logLossEval <- function(pred.probs, true.class) {
    # pred.probs: vector of predicted probabilities
    # true.class:  true class designation
    
    # adjust probabilities to avoid numerical issues with log() function
    new.probs <- pmax(pmin(pred.probs,1-1e-15),1e-15)
    
    resp <- ifelse(true.class == "Class_1",1,0)
    
    ans <- -sum(resp*log(new.probs) + (1-resp)*log((1-new.probs)))/length(new.probs)
    
    if (is.nan(ans) | is.na(ans)) {
        stop("Returning NAN or NA from LogLoss Function.")
    }
    
    return(ans) 
}

# caret custom model performance function for log-loss
caretLogLossSummary <- function(data,lev,model) {
    out <- logLossEval(data[,"Class_1"],data[,"obs"])
    if (is.na(out) | is.nan(out)) {
        stop("Error in LogLoss Function")
    }
    names(out) <- "LogLoss"
    out
}

# function to record model performance
recordModelPerf <- function(model.performance.file,
                            model=NULL,time.data=NULL,train.df=NULL,score=NA,
                            improved="No", bestTune="", tune.grid="",
                            model.parms="", comment="") {
    # time.data is a proc_time object from system.time() function call
    # bestTune is data frame for optimal model hyper-paramters
    # 
    
    new.row <- data.frame(date.time=as.character(Sys.time()),
                          model=model,
                          user.cpu.time=summary(time.data)["user"],
                          sys.cpu.time=summary(time.data)["system"],
                          elapsed.time=summary(time.data)["elapsed"],
                          num.observations=nrow(train.df),
                          num.features=ncol(train.df),
                          score=score,
                          improved=improved,
                          bestTune=bestTune,
                          tune.grid=tune.grid,
                          model.parms=model.parms,
                          comment=comment,
                          stringsAsFactors=FALSE) 
    
    # write new performance records
    write.table(new.row,file=model.performance.file,
                append=TRUE,sep="\t",row.names=FALSE,col.names=FALSE)
    
    return(NULL)
}

#function to flatten one or more rows of data.frame into a string representation
flattenDF <- function(df) {
    #convert data frame to list
    x <- as.list(df)
    
    # convert named list to a string
    return(paste(names(x),as.character(x),sep="=",collapse=","))
}



###
#  
#  Model Data Preparation Functions  
#                                    
####


# this function prepares data for training or use in predictions
prepL0SkltnModelData <- function(df,includeResponse=TRUE){
    # df: raw data
    # if only.predcitors is TRUE then return list(predictors)
    # if only.predictors is FALSE then return list(predictors,response)
    
    require(plyr)
    require(caret)
    
    # load in structures on what to model
    #     load(paste0(DATA.DIR,"/char_attributes.RData"))
    #     load(paste0(DATA.DIR,"/numeric_attr_to_model.RData"))
    #     load(paste0(DATA.DIR,"/pp_medianImpute.RData"))
    
    num.vars <- c("v10","v12","v14", "v17", "v32", "v48", "v50", "v51", "v64", "v73", 
                  "v76","v93","v101","v106","v62","v129")
    
    char.vars <- c("v31", "v47", "v66", "v110")
    
    if (includeResponse) {
        df2  <- na.omit(df[,c("target",num.vars,char.vars),with=FALSE])   
        
    } else {
        df2  <- na.omit(df[,c(num.vars,char.vars),with=FALSE])  
    }
    
    
    # eliminate unwanted variables
    predictors <- df2[,c(num.vars,char.vars),with=FALSE]
    for (x in char.vars) {
        predictors[[x]] <- factor(predictors[[x]])
    }
    
    
    if (includeResponse) {
        
        response <- factor(ifelse(df2$target == 1,"Class_1","Class_0"),
                           levels=c("Class_1","Class_0"))
        ans <- list(predictors=predictors,response=response)
        
    } else {
        
        ans <- list(predictors=predictors)
    }
    
    return(ans)
}

prepL1SkltnModelData <- function(df,includeResponse=TRUE){
    # df: raw data
    # if only.predcitors is TRUE then return list(predictors)
    # if only.predictors is FALSE then return list(predictors,response)
    
    require(plyr)
    require(caret)
    
    # load in structures on what to model
    #     load(paste0(DATA.DIR,"/char_attributes.RData"))
    #     load(paste0(DATA.DIR,"/numeric_attr_to_model.RData"))
    #     load(paste0(DATA.DIR,"/pp_medianImpute.RData"))
    
    pred.names <- setdiff(names(df),c("ID","target"))
    
    predictors <- df[,pred.names,with=FALSE]
    
    if (only.predictors) {
        
        ans <- list(predictors=predictors)
        
    } else {
        
        response <- factor(ifelse(df$target == 1,"Class_1","Class_0"),
                           levels=c("Class_1","Class_0"))
        ans <- list(predictors=predictors,response=response)
    }
    
    return(ans)
}
