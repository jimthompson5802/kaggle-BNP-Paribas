###
#  Dummy vars test
###

library(caret)

source("./src/CommonFunctions.R")

load(file=paste0(DATA.DIR,"/train_calib_test.RData"))

# Features selected from expanded Boruta analysis 
# number attributes set to raw values, NA set to -999
# character attributes set as dummy variables
prepL0FeatureSet99 <- function(df,includeResponse=TRUE){
    # prepL0FeatureSet5
    # df: raw data
    # if only.predcitors is TRUE then return list(predictors)
    # if only.predictors is FALSE then return list(predictors,response)
    
    require(plyr)
    require(caret)
    require(Boruta)
    require(data.table)
    
    ans <- list(data.set.name="FeatureSet99")
    
    # use only attributes confirmed by Boruta feature analysis
    load(paste0(DATA.DIR,"/boruta_feature_analysis2.RData"))
    
    # get data types and change all strings to factors
    load(paste0(DATA.DIR,"/attr_data_types.RData"))
    load(paste0(DATA.DIR,"/factor_levels.RData"))
    
    number.vars <- union(intersect(getSelectedAttributes(bor.results),attr.data.types$numeric),
          intersect(getSelectedAttributes(bor.results),attr.data.types$integer))
    
    # get categorical variables
    char.vars <- intersect(getSelectedAttributes(bor.results),attr.data.types$character)
    
    # create dummy variables 
    ll <- lapply(char.vars,function(x){
        y <- predict(dummy.vars[[x]],newdata=df[,x,with=FALSE])
        return(y)
    })
    
    y <- do.call(cbind,ll)
    
    predictors <- cbind(df[,number.vars,with=FALSE],y)
    
    # set NA to -999
    for (x in number.vars) {
        idx <- is.na(predictors[[x]])
        predictors[[x]][idx] <- -999
    }
    
    ans <- c(ans,list(predictors=predictors))
    
    if (includeResponse) {
        
        response <- factor(ifelse(df$target == 1,"Class_1","Class_0"),
                           levels=c("Class_1","Class_0"))
        ans <- c(ans,list(response=response))
        
    } 
    
    return(ans)
}


train.data <- prepL0FeatureSet99(train0.raw)
