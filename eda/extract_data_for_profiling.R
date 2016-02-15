###
#  initial data profiling
###

library(plyr)
library(data.table)

DATA.DIR <- "./data"
TARGET.VAR <- "target"
ID.VAR <- "ID"

load(paste0(DATA.DIR,"/train_calib_test.RData"))
train.raw <- rbind(train0.raw,train1.raw)

# extract data for profling
set.seed(19)
sample.df <-  train.raw[sample(nrow(train.raw),0.5*nrow(train.raw))]
comment(sample.df) <- "sample of training data for initial data profiling"

rm(train0.raw,train1.raw,test.raw,calib.raw,train.raw)

# deterimine data types
explanatory.attributes <- setdiff(names(sample.df),c(ID.VAR,TARGET.VAR))
data.classes <- sapply(explanatory.attributes,function(x){class(sample.df[[x]])})

# categorize data types in the data set?
unique.classes <- unique(data.classes)

attr.data.types <- lapply(unique.classes,function(x){names(data.classes[data.classes==x])})
names(attr.data.types) <- unique.classes
comment(attr.data.types) <- "list that categorize training data types"

# find numeric attributes that contain missing values
num.missing <- sapply(c(attr.data.types$numeric,attr.data.types$integer),
                      function(x){sum(is.na(sample.df[[x]]))})
attr.data.types$contain.na <- names(num.missing)[num.missing>0]


save(sample.df,file=paste0(DATA.DIR,"/sample_data_for_eda.RData"))
save(attr.data.types,file=paste0(DATA.DIR,"/attr_data_types.RData"))
