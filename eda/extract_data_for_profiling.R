###
#  initial data profiling
###

library(plyr)
library(data.table)

DATA.DIR <- "./data"

load(paste0(DATA.DIR,"/train_calib_test.RData"))
train.raw <- rbind(train0.raw,train1.raw)

# extract data for profling
set.seed(19)
sample.df <-  train.raw[sample(nrow(train.raw),0.5*nrow(train.raw))]
comment(sample.df) <- "sample of training data for initial data profiling"

rm(train0.raw,train1.raw,test.raw,calib.raw,train.raw)

save(sample.df,file=paste0(DATA.DIR,"/sample_data_for_eda.RData"))

