###
# Global Variables
###

DATA.DIR <- "./data"

PYTHON_COMMAND <- "/Users/jim/anaconda/bin/python"


#####
# Model Evaluation functions - LogLoss
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




#####
# functions to record model performance
recordModelPerf <- function(model.performance.file,
                            model=NULL,time.data=NULL,train.df=NULL,score=NA,
                            improved="No", bestTune="", tune.grid="",
                            model.parms="", comment="") {
    # time.data is a proc_time object from system.time() function call
    # bestTune is data frame for optimal model hyper-paramters
    # 
    parts.of.file.name <- unlist(strsplit(model.performance.file,split = "/"))
    model.level <- parts.of.file.name[length(parts.of.file.name)-1]
    
    new.row <- data.frame(date.time=as.character(Sys.time()),
                          model.level=model.level,
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
    
    # write new performance records to consolidate area
    write.table(cbind(WORK.DIR,new.row),
                file="./model_results/ConsolidatedModelPerformance.tsv",
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



#####
#  Model Data Preparation Functions  

# # generic function to create Level 2 features from Level 1 model predictions
# createLevel2Features <- function (model.dir,df,...) {
#     # model.dir: directory containing Level 0 model
#     # df: training data to generate Level 1 features
#     
#     force(df)
#     force(model.dir)
#     
#     # create environment to hold Level 0 Model data structures
#     l0.env <- new.env()
#     model.file.name <- readLines(paste0("./src/",model.dir,"/this_model"))
#     load(paste0("./src/",model.dir,"/",model.file.name),envir=l0.env)
#     
#     #prepare data for L0 model
#     train.data <- l0.env$PREPARE.MODEL.DATA(df,...)
#     
#     # create Level 1 features
#     pred.probs <- predict(l0.env$mdl.fit,newdata = train.data$predictors,type = "prob")
#     
#     # Attribute Level 0 model to the created predictions
#     new.names <- paste0(model.dir,".",names(pred.probs))
#     names(pred.probs) <- new.names
#     
#     return(pred.probs)
# }
# 
# # create Level 2 Features From Level 1 Models - Class_1 probabilities only
# prepL2FeatureSet <- function(level1.models,includeResponse=TRUE){
#     # df: raw data
#     # if only.predcitors is TRUE then return list(predictors)
#     # if only.predictors is FALSE then return list(predictors,response)
#     
#     require(plyr)
#     require(caret)
#     
#     force(level1.models)
#     
#     ll <- lapply(level1.models,createLevel1Features,df,includeResponse)
#     
#     predictors <- do.call(cbind,ll)
#     
#     #extract only Class_1 probabilities
#     class1.names <- grep("Class_1",names(predictors),value = TRUE)
#     predictors <- predictors[class1.names]
#     
#     if (includeResponse) {
#         
#         response <- factor(ifelse(df$target == 1,"Class_1","Class_0"),
#                            levels=c("Class_1","Class_0"))
#         ans <- list(predictors=predictors,response=response)
#         
#     } else {
#         
#         ans <- list(predictors=predictors)
#         
#     }
#     
#     return(ans)
# }

# generic function to create Level 1 features from Level 0 model predictions
createLevel1Features <- function (model.dir,df,...) {
    # work.dir: directory containing Level 0 model
    # df: training data to generate Level 1 features
    
    force(df)
    force(model.dir)
    
    # create environment to hold Level 0 Model data structures
    l0.env <- new.env()

    # determine if R or Python model
    model.file.name <- readLines(paste0("./src/",model.dir,"/this_model"))
    if (length(model.file.name) == 1) {
        # R Model
        load(paste0("./src/",model.dir,"/",model.file.name[1]),envir=l0.env)
        
        #prepare data for L0 model
        train.data <- l0.env$PREPARE.MODEL.DATA(df,...)
        
        # create Level 1 features
        pred.probs <- predict(l0.env$mdl.fit,newdata = train.data$predictors,type = "prob")
        
        # Attribute Level 0 model to the created predictions
        new.names <- paste0(model.dir,".",names(pred.probs))
        names(pred.probs) <- new.names

    } else {
        # Python model
        
        # get R based data
        load(paste0("./src/",model.dir,"/",model.file.name[1]),envir=l0.env)
        
        # get Python based model data
        py.model.file.name <- model.file.name[2]
        
        #prepare data for L0 model
        train.data <- l0.env$PREPARE.MODEL.DATA(df,...)
        
        # write out L0 data for predictions
        write.table(train.data$predictors,file=paste0("./src/",model.dir,"/py_test.tsv"),row.names = FALSE,
                    sep="\t")
        
        # execute Python prediction code
        python.test.command <- paste(PYTHON_COMMAND,paste0("./src/",model.dir,"/make_prediction.py"),
                                     paste0("./src/",model.dir),
                                     py.model.file.name,
                                     "py_test.tsv",
                                     "py_test_predictions.tsv")
        system(python.test.command)
        
        # retrieve predictions from Python
        pred.probs <- fread(paste0("./src/",model.dir,"/py_test_predictions.tsv"), sep="\t")
        
        # Attribute Level 0 model to the created predictions
        new.names <- paste0(model.dir,".",names(pred.probs))
        names(pred.probs) <- new.names
        
        # clean up files
        file.remove(c(paste0("./src/",model.dir,"/py_test.tsv"),
                      paste0("./src/",model.dir,"/py_test_predictions.tsv")))
        
    }
    
    return(pred.probs)
}

# using only Class_1 probabilities for features
prepL1FeatureSet1 <- function(level0.models,df,includeResponse=TRUE){
    # l0_models: character vector of Level 0 models to include the Level 1 Feature set
    # df: raw data
    # if only.predcitors is TRUE then return list(predictors)
    # if only.predictors is FALSE then return list(predictors,response)
    
    require(plyr)
    require(caret)
    
    ll <- lapply(level0.models,createLevel1Features,df,includeResponse)
    
    predictors <- do.call(cbind,ll)
    
    #extract only Class_1 probabilities
    class1.names <- grep("Class_1",names(predictors),value = TRUE)
    predictors <- predictors[class1.names]
    
    if (includeResponse) {
        
        response <- factor(ifelse(df$target == 1,"Class_1","Class_0"),
                           levels=c("Class_1","Class_0"))
        ans <- list(predictors=predictors,response=response)
        
    } else {
        
        ans <- list(predictors=predictors)
        
    }
    
    return(ans)
}

# uses both Class_1 and Class_0 probabilities as features
prepL1gbm1ModelData <- function(df,includeResponse=TRUE){
    # df: raw data
    # if only.predcitors is TRUE then return list(predictors)
    # if only.predictors is FALSE then return list(predictors,response)
    
    require(plyr)
    require(caret)
    
    level0.models <- c("./src/L0_gbm1",
                       "./src/L0_rngr1",
                       "./src/L0_xgb2",
                       "./src/L0_xgb1")
    
    ll <- lapply(level0.models,createLevel1Features,df,includeResponse)
    
    predictors <- do.call(cbind,ll)
    
    if (includeResponse) {
        
        response <- factor(ifelse(df$target == 1,"Class_1","Class_0"),
                           levels=c("Class_1","Class_0"))
        ans <- list(predictors=predictors,response=response)
        
    } else {
        
        ans <- list(predictors=predictors)
        
    }
    
    return(ans)
}

# using only Class_1 probabilities for features
prepL1gbm2ModelData <- function(df,includeResponse=TRUE){
    # df: raw data
    # if only.predcitors is TRUE then return list(predictors)
    # if only.predictors is FALSE then return list(predictors,response)
    
    require(plyr)
    require(caret)
    
    level0.models <- c("./src/L0_gbm1",
                       "./src/L0_gbm2",
                       "./src/L0_rngr1",
                       "./src/L0_xgb2",
                       "./src/L0_xgb1")
    
    ll <- lapply(level0.models,createLevel1Features,df,includeResponse)
    
    predictors <- do.call(cbind,ll)
    
    #extract only Class_1 probabilities
    class1.names <- grep("Class_1",names(predictors),value = TRUE)
    predictors <- predictors[class1.names]
    
    if (includeResponse) {
        
        response <- factor(ifelse(df$target == 1,"Class_1","Class_0"),
                           levels=c("Class_1","Class_0"))
        ans <- list(predictors=predictors,response=response)
        
    } else {
        
        ans <- list(predictors=predictors)
        
    }
    
    return(ans)
}

# Boruta selected attributes, characters set as factor levels
# original Boruta analysis
prepL0FeatureSet1 <- function(df,includeResponse=TRUE){
    # df: raw data
    # if only.predcitors is TRUE then return list(predictors)
    # if only.predictors is FALSE then return list(predictors,response)
    
    require(plyr)
    require(caret)
    require(Boruta)
    
   # use only attributes confirmed by Boruta feature analysis
    load(paste0(DATA.DIR,"/boruta_feature_analysis.RData"))
    
    # eliminate unwanted variables
    predictors <- df[,predictor.vars,with=FALSE]
    
    # get data types and change all strings to factors
    load(paste0(DATA.DIR,"/attr_data_types.RData"))
    load(paste0(DATA.DIR,"/factor_levels.RData"))
    
    # standardize factor levels for modeling
    char.attr <- intersect(predictor.vars,attr.data.types$character)
    
    for (x in char.attr) {
        predictors[[x]] <- factor(predictors[[x]],levels = factor.levels[[x]])
    }
    
    
    if (includeResponse) {
        
        response <- factor(ifelse(df$target == 1,"Class_1","Class_0"),
                           levels=c("Class_1","Class_0"))
        ans <- list(predictors=predictors,response=response)
        
    } else {
        
        ans <- list(predictors=predictors)
    }
    
    return(ans)
}

# Features selected from expanded Boruta analysis 
# character attributes set as factor level numbers
prepL0FeatureSet2 <- function(df,includeResponse=TRUE){
    # prepL0FeatureSet2
    # df: raw data
    # if only.predcitors is TRUE then return list(predictors)
    # if only.predictors is FALSE then return list(predictors,response)
    
    require(plyr)
    require(caret)
    require(Boruta)
    
    # use only attributes confirmed by Boruta feature analysis
    load(paste0(DATA.DIR,"/boruta_feature_analysis2.RData"))
    
    predictor.vars <- setdiff(getSelectedAttributes(bor.results),
                              c("all.var.na.count","imp.var.na.count"))
    
    # count missing values for all attributes and those deemed important by Boruta
    all.var.na.count <- apply(df,1,function(row){sum(is.na(row))})
    imp.var.na.count <- apply(df[,predictor.vars,with=FALSE],1,function(row){sum(is.na(row))})
    
    # eliminate unwanted variables
    predictors <- cbind(df[,predictor.vars,with=FALSE],all.var.na.count,imp.var.na.count)
    
    # get data types and change all strings to factors
    load(paste0(DATA.DIR,"/attr_data_types.RData"))
    load(paste0(DATA.DIR,"/factor_levels.RData"))
    
    # standardize factor levels for modeling
    char.attr <- intersect(predictor.vars,attr.data.types$character)
    
    for (x in char.attr) {
        predictors[[x]] <- factor(predictors[[x]],levels = factor.levels[[x]])
    }
    
    
    if (includeResponse) {
        
        response <- factor(ifelse(df$target == 1,"Class_1","Class_0"),
                           levels=c("Class_1","Class_0"))
        ans <- list(predictors=predictors,response=response)
        
    } else {
        
        ans <- list(predictors=predictors)
    }
    
    return(ans)
}

# Features selected from expanded Boruta analysis 
# character attributes set as factor level numbers
# set numeric NA to -999
prepL0FeatureSet2a <- function(df,includeResponse=TRUE){
    # prepL0FeatureSet2a
    # df: raw data
    # if only.predcitors is TRUE then return list(predictors)
    # if only.predictors is FALSE then return list(predictors,response)
    
    require(plyr)
    require(caret)
    require(Boruta)
    
    # use only attributes confirmed by Boruta feature analysis
    load(paste0(DATA.DIR,"/boruta_feature_analysis2.RData"))
    
    predictor.vars <- setdiff(getSelectedAttributes(bor.results),
                              c("all.var.na.count","imp.var.na.count"))
    
    # count missing values for all attributes and those deemed important by Boruta
    all.var.na.count <- apply(df,1,function(row){sum(is.na(row))})
    imp.var.na.count <- apply(df[,predictor.vars,with=FALSE],1,function(row){sum(is.na(row))})
    
    # eliminate unwanted variables
    predictors <- cbind(df[,predictor.vars,with=FALSE],all.var.na.count,imp.var.na.count)
    
    # get data types and change all strings to factors
    load(paste0(DATA.DIR,"/attr_data_types.RData"))
    load(paste0(DATA.DIR,"/factor_levels.RData"))
    
    # set NA to -999
    num.attr <- intersect(predictor.vars,c(attr.data.types$numeric,attr.data.types$integer))
    for (x in num.attr) {
        idx <- is.na(predictors[[x]])
        predictors[[x]][idx] <- -999
    }
    
    # standardize factor levels for modeling
    char.attr <- intersect(predictor.vars,attr.data.types$character)
    
    for (x in char.attr) {
        predictors[[x]] <- as.integer(factor(predictors[[x]],levels = factor.levels[[x]]))
    }
    
    
    if (includeResponse) {
        
        response <- factor(ifelse(df$target == 1,"Class_1","Class_0"),
                           levels=c("Class_1","Class_0"))
        ans <- list(predictors=predictors,response=response)
        
    } else {
        
        ans <- list(predictors=predictors)
    }
    
    return(ans)
}

# All Features in raw format including synthetic features
# Categorical represented as integers
# numeric NA set to -999
prepL0FeatureSetAll <- function(df,includeResponse=TRUE){
    # prepL0FeatureSetAll
    # df: raw data
    # if only.predcitors is TRUE then return list(predictors)
    # if only.predictors is FALSE then return list(predictors,response)
    
    require(plyr)
    require(caret)

    predictor.vars <- setdiff(names(df),c("ID","target"))
    
    # count missing values for all attributes and those deemed important by Boruta
    all.var.na.count <- apply(df,1,function(row){sum(is.na(row))})

    # eliminate unwanted variables
    predictors <- cbind(df[,predictor.vars,with=FALSE],all.var.na.count)
    
    # get data types and change all strings to factors
    load(paste0(DATA.DIR,"/attr_data_types.RData"))
    load(paste0(DATA.DIR,"/factor_levels.RData"))
    
    # set NA to -999
    num.attr <- intersect(predictor.vars,c(attr.data.types$numeric,attr.data.types$integer))
    for (x in num.attr) {
        idx <- is.na(predictors[[x]])
        predictors[[x]][idx] <- -999
    }
    
    # standardize factor levels for modeling
    char.attr <- intersect(predictor.vars,attr.data.types$character)
    
    for (x in char.attr) {
        predictors[[x]] <- as.integer(factor(predictors[[x]],levels = factor.levels[[x]]))
    }
    
    
    if (includeResponse) {
        
        response <- factor(ifelse(df$target == 1,"Class_1","Class_0"),
                           levels=c("Class_1","Class_0"))
        ans <- list(predictors=predictors,response=response)
        
    } else {
        
        ans <- list(predictors=predictors)
    }
    
    return(ans)
}

# Feature set based on Rscipt w/ one synthetic feature
# Categorical represented as integers
# numeric NA set to -999
prepL0FeatureSet3 <- function(df,includeResponse=TRUE){
    # prepL0FeatureSet3
    # df: raw data
    # if only.predcitors is TRUE then return list(predictors)
    # if only.predictors is FALSE then return list(predictors,response)
    
    require(plyr)
    require(caret)
    
    predictor.vars <- setdiff(names(df),c('ID','target','v8','v23','v25','v31','v36','v37','v46','v51',
                                          'v53','v54','v63','v73','v75','v79','v81','v82','v89','v92',
                                          'v95','v105','v107','v108','v109','v110','v116','v117','v118',
                                          'v119','v123','v124','v128'))
    
    # count missing values for all attributes and those deemed important by Boruta
    all.var.na.count <- apply(df,1,function(row){sum(is.na(row))})
    
    # eliminate unwanted variables
    predictors <- cbind(df[,predictor.vars,with=FALSE],all.var.na.count)
    
    # get data types and change all strings to factors
    load(paste0(DATA.DIR,"/attr_data_types.RData"))
    load(paste0(DATA.DIR,"/factor_levels.RData"))
    
    # set NA to -999
    num.attr <- intersect(predictor.vars,c(attr.data.types$numeric,attr.data.types$integer))
    for (x in num.attr) {
        idx <- is.na(predictors[[x]])
        predictors[[x]][idx] <- -999
    }
    
    # standardize factor levels for modeling
    char.attr <- intersect(predictor.vars,attr.data.types$character)
    
    for (x in char.attr) {
        predictors[[x]] <- as.integer(factor(predictors[[x]],levels = factor.levels[[x]]))
    }
    
    
    if (includeResponse) {
        
        response <- factor(ifelse(df$target == 1,"Class_1","Class_0"),
                           levels=c("Class_1","Class_0"))
        ans <- list(predictors=predictors,response=response)
        
    } else {
        
        ans <- list(predictors=predictors)
    }
    
    return(ans)
}

# Feature set based on Rscipt
# Categorical represented as integers
# numeric NA set to -999
prepL0FeatureSet4 <- function(df,includeResponse=TRUE){
    # prepL0FeatureSet4
    # df: raw data
    # if only.predcitors is TRUE then return list(predictors)
    # if only.predictors is FALSE then return list(predictors,response)
    
    require(plyr)
    require(caret)
    
    predictor.vars <- setdiff(names(df),c('ID','target','v8','v23','v25','v31','v36','v37','v46','v51',
                                          'v53','v54','v63','v73','v75','v79','v81','v82','v89','v92',
                                          'v95','v105','v107','v108','v109','v110','v116','v117','v118',
                                          'v119','v123','v124','v128'))
    
    # eliminate unwanted variables
    predictors <- df[,predictor.vars,with=FALSE]
    
    # get data types and change all strings to factors
    load(paste0(DATA.DIR,"/attr_data_types.RData"))
    load(paste0(DATA.DIR,"/factor_levels.RData"))
    
    # set NA to -999
    num.attr <- intersect(predictor.vars,c(attr.data.types$numeric,attr.data.types$integer))
    for (x in num.attr) {
        idx <- is.na(predictors[[x]])
        predictors[[x]][idx] <- -999
    }
    
    # standardize factor levels for modeling
    char.attr <- intersect(predictor.vars,attr.data.types$character)
    
    for (x in char.attr) {
        predictors[[x]] <- as.integer(factor(predictors[[x]],levels = factor.levels[[x]]))
    }
    
    
    if (includeResponse) {
        
        response <- factor(ifelse(df$target == 1,"Class_1","Class_0"),
                           levels=c("Class_1","Class_0"))
        ans <- list(predictors=predictors,response=response)
        
    } else {
        
        ans <- list(predictors=predictors)
    }
    
    return(ans)
}

# data prep Level 0 rngr1 model
prepL0rngr1ModelData <- function(df,includeResponse=TRUE){
    # df: raw data
    # if only.predcitors is TRUE then return list(predictors)
    # if only.predictors is FALSE then return list(predictors,response)
    
    require(plyr)
    require(caret)
    require(Boruta)
    
    # use only attributes confirmed by Boruta feature analysis
    load(paste0(DATA.DIR,"/boruta_feature_analysis2.RData"))
    
    predictor.vars <- getSelectedAttributes(bor.results)
    
    
    #     # eliminate unwanted variables
    predictors <- df[,predictor.vars,with=FALSE]
    
    # get data types and change all strings to factors
    load(paste0(DATA.DIR,"/attr_data_types.RData"))
    load(paste0(DATA.DIR,"/factor_levels.RData"))
    load(paste0(DATA.DIR,"/median_values.RData"))
    
    # standardize factor levels for modeling
    char.attr <- intersect(predictor.vars,attr.data.types$character)
    for (x in char.attr) {
        predictors[[x]] <- factor(predictors[[x]],levels = factor.levels[[x]])
    }
    
    # impute pre-defined median value for any missing values
    num.attr <- intersect(predictor.vars,c(attr.data.types$numeric,attr.data.types$integer))
    for (x in num.attr) {
        idx <- is.na(predictors[[x]])
        predictors[[x]][idx] <- median.values[[x]]
    }
    
    
    if (includeResponse) {
        
        response <- factor(ifelse(df$target == 1,"Class_1","Class_0"),
                           levels=c("Class_1","Class_0"))
        ans <- list(predictors=predictors,response=response)
        
    } else {
        
        ans <- list(predictors=predictors)
    }
    
    return(ans)
}

# data prep Level 0 xgb1 model
prepL0xgb1ModelData <- function(df,includeResponse=TRUE){
    # df: raw data
    # if only.predcitors is TRUE then return list(predictors)
    # if only.predictors is FALSE then return list(predictors,response)
    
    require(plyr)
    require(caret)
    require(Boruta)
    
    # use only attributes confirmed by Boruta feature analysis
    load(paste0(DATA.DIR,"/boruta_feature_analysis.RData"))
    
    # get data types and change all strings to factors
    load(paste0(DATA.DIR,"/attr_data_types.RData"))
    load(paste0(DATA.DIR,"/factor_levels.RData"))
    load(paste0(DATA.DIR,"/median_values.RData"))
    
    predictor.vars <- getSelectedAttributes(bor.results)
    
    
    #     # eliminate unwanted variables
    predictors <- df[,predictor.vars,with=FALSE]
    

    # convert factors to numeric reprsentation
    char.attr <- intersect(predictor.vars,attr.data.types$character)
    for (x in char.attr) {
        predictors[[x]] <- as.integer(factor(predictors[[x]],levels = factor.levels[[x]]))
    }
    
    # impute pre-defined median value for any missing values
    num.attr <- intersect(predictor.vars,c(attr.data.types$numeric,attr.data.types$integer))
    for (x in num.attr) {
        idx <- is.na(predictors[[x]])
        predictors[[x]][idx] <- median.values[[x]]
    }
    
    
    if (includeResponse) {
        
        response <- factor(ifelse(df$target == 1,"Class_1","Class_0"),
                           levels=c("Class_1","Class_0"))
        ans <- list(predictors=predictors,response=response)
        
    } else {
        
        ans <- list(predictors=predictors)
    }
    
    return(ans)
}

# template function for data preparaton of Level 0 Models
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
        df2  <- df[,c("target",num.vars,char.vars),with=FALSE]
        
    } else {
        df2  <- df[,c(num.vars,char.vars),with=FALSE] 
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

# template data prep function for L1 models
prepL1SkltnModelData <- function(df,includeResponse=TRUE){
    # df: raw data
    # if only.predcitors is TRUE then return list(predictors)
    # if only.predictors is FALSE then return list(predictors,response)
    
    require(plyr)
    require(caret)
    
    level0.models <- c("./src/L0_skeleton_model")
    
    ll <- lapply(level0.models,createLevel1Features,df,includeResponse)
    
    predictors <- do.call(cbind,ll)
    
    if (includeResponse) {
        
        response <- factor(ifelse(df$target == 1,"Class_1","Class_0"),
                           levels=c("Class_1","Class_0"))
        ans <- list(predictors=predictors,response=response)
        
    } else {
        
        ans <- list(predictors=predictors)
        
    }
    
    return(ans)
}

