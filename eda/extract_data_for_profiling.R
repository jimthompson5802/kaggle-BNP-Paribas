###
#  initial data profiling
###

library(plyr)
library(data.table)

DATA.DIR <- "./data"
TARGET.VAR <- "target"
ID.VAR <- "ID"

load(paste0(DATA.DIR,"/train_calib_test.RData"))
train.raw <- rbind(train1.raw,train2.raw)

# extract data for profling
set.seed(19)
sample.df <-  data.frame(train.raw[sample(nrow(train.raw),0.5*nrow(train.raw)),],
                         stringsAsFactors = FALSE)
comment(sample.df) <- "sample of training data for initial data profiling"

rm(train1.raw,train2.raw,test.raw,calib.raw,train.raw)

# deterimine data types
explanatory.attributes <- setdiff(names(sample.df),c(ID.VAR,TARGET.VAR))
data.classes <- sapply(explanatory.attributes,function(x){class(sample.df[,x])})

# categorize data types in the data set?
unique.classes <- unique(data.classes)

attr.by.data.types <- lapply(unique.classes,function(x){names(data.classes[data.classes==x])})
names(attr.by.data.types) <- unique.classes
comment(attr.by.data.types) <- "list that categorize training data types"
save(sample.df,attr.by.data.types,
     file=paste0(DATA.DIR,"/sample_data_for_eda.RData"))
