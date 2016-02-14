###
#  Model specific common functions
###

# this function prepares data for training or use in predictions
prepGBMModelData <- function(df,only.predictors=FALSE){
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

    if (only.predictors) {
        df2  <- na.omit(df[,c(num.vars,char.vars),with=FALSE])  
    } else {
        df2  <- na.omit(df[,c("target",num.vars,char.vars),with=FALSE])   
    }

    
    # eliminate unwanted variables
    predictors <- df2[,c(num.vars,char.vars),with=FALSE]
    for (x in char.vars) {
        predictors[[x]] <- factor(predictors[[x]])
    }

   
    if (only.predictors) {

        ans <- list(predictors=predictors)
        
    } else {
        
        response <- factor(ifelse(df2$target == 1,"Class_1","Class_0"),
                           levels=c("Class_1","Class_0"))
        ans <- list(predictors=predictors,response=response)
    }

    return(ans)
}

