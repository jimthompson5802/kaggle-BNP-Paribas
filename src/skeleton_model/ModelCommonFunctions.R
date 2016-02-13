###
#  Model specific common functions
###

# this function prepares data for training or use in predictions
prepModelData <- function(df,only.predictors=FALSE){
    # df: raw data
    # if only.predcitors is TRUE then return list(predictors)
    # if only.predictors is FALSE then return list(predictors,response)
    
    require(plyr)
    
    # load in structures on what to model
    load(paste0(DATA.DIR,"/char_attributes.RData"))
    load(paste0(DATA.DIR,"/numeric_attr_to_model.RData"))
    load(paste0(DATA.DIR,"/pp_medianImpute.RData"))
    
    # eliminate unwanted variables
    predictors <- df[,char.attr]
    
    # handle missing values for character attributes
    ll <- alply(predictors,2,function(one.col){
        ans <- one.col[,1]
        idx <- nchar(ans) == 0 | ans == "-1"
        ans[idx] <- "MISSING"
        ans <- make.names(ans)
        return(ans)}
    )
    
    names(ll) <- names(predictors)
    predictors <- data.frame(do.call(cbind,ll))
    
    num.predictors <- predict(pp.medianImpute,df[,num.attr])
    
    
    predictors <- cbind(predictors,num.predictors)
   
    if (only.predictors) {

        ans <- list(predictors=predictors)
        
    } else {
        
        response <- factor(ifelse(df$target == 1,"Yes","No"),levels=c("Yes","No"))
        
        ans <- list(predictors=predictors,response=response)
    }

    return(ans)
}

