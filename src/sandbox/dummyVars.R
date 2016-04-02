###
#  Dummy vars test
###

library(caret)

source("./src/CommonFunctions.R")

load(file=paste0(DATA.DIR,"/train_calib_test.RData"))

prepL0FeatureSet7 <- function(df,includeResponse=TRUE){
    # prepL0FeatureSet7
    # df: raw data
    # if only.predcitors is TRUE then return list(predictors)
    # if only.predictors is FALSE then return list(predictors,response)
    
    require(plyr)
    require(caret)
    require(Boruta)
    
    ans <- list(data.set.name="FeatureSet7")
    
    # use only attributes confirmed by Boruta feature analysis
    load(paste0(DATA.DIR,"/boruta_feature_analysis2.RData"))
    
    # get data types and change all strings to factors
    load(paste0(DATA.DIR,"/attr_data_types.RData"))
    load(paste0(DATA.DIR,"/factor_levels.RData"))
    
    # get Boruta revelant attributes
    relevant.vars <- setdiff(getSelectedAttributes(bor.results),
                              c("all.var.na.count","imp.var.na.count"))
    
    # pick out the numeric variables to scale [0,1]
    number.vars <- intersect(relevant.vars,union(attr.data.types$numeric,attr.data.types$integer))
    ll <- lapply(number.vars,function(x){
        xnew <- (df[[x]] - center.scale.parms[[x]]$min.value) / 
            (center.scale.parms[[x]]$max.value - center.scale.parms[[x]]$min.value)
        idx <- is.na(xnew)
        xnew[idx] <- -1
        return(xnew)        
    })
    
    xnew <- do.call(cbind,ll)
    names(xnew) <- number.vars
    
    
    # get categorical variables
    char.vars <- intersect(getSelectedAttributes(bor.results),attr.data.types$character)
    
    # create dummy variables 
    ll <- lapply(char.vars,function(x){
        y <- predict(dummy.vars[[x]],newdata=df[,x,with=FALSE])
        return(y)
    })
    
    y <- do.call(cbind,ll)
    
    predictors <- cbind(xnew,y)
    
    ans <- c(ans,list(predictors=predictors))
    
    if (includeResponse) {
        
        response <- factor(ifelse(df$target == 1,"Class_1","Class_0"),
                           levels=c("Class_1","Class_0"))
        ans <- c(ans,list(response=response))
        
    } 
    
    return(ans)
}


train.data <- prepL0FeatureSet7(train0.raw)
