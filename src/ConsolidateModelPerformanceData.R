###
# Consolidate model performance data
###

source("./src/CommonFunctions.R")


model.dirs <- list.dirs("./src",recursive=TRUE)
model.dirs <- setdiff(model.dirs,c("L0_skeleton_model",
                                   "L1_skeleton_model"))

#initialize consolidation data frame
consolidatedModelPerf.df <- data.frame()

for (dir in model.dirs){
    full.file.name <- paste(dir,"model_performance.tsv",sep="/")
    if (file.exists(full.file.name)) {
        df <- read.delim(full.file.name, stringsAsFactors=FALSE)
        df <- data.frame(dir=rep(dir,nrow(df)),df,stringsAsFactors=FALSE)
        consolidatedModelPerf.df <- rbind(consolidatedModelPerf.df,df)
    }
}

write.table(consolidatedModelPerf.df,
            file="./model_results/ConsolidatedModelPerformance.tsv",
            sep="\t",row.names=FALSE)
