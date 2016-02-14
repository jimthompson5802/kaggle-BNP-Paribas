###
# create training, calibration and test data sets
###

library(caret)
library(data.table)


data.dir <- "./data"


raw <- data.table(read.csv(paste0(data.dir,"/train.csv"),stringsAsFactors=FALSE))
setkey(raw,ID)

nrow(raw)
table(raw$target)/nrow(raw)

# create the partitions
set.seed(13)
idx <- createDataPartition(raw$target, p = 0.8, list=FALSE)
train.raw <- raw[as.vector(idx)]
raw <- raw[as.vector(-idx)]

set.seed(123)
idx <- createDataPartition(raw$target, p = 0.5, list=FALSE)

calib.raw <- raw[as.vector(idx)]
test.raw <- raw[as.vector(-idx)]

set.seed(17)
idx <- createDataPartition(train.raw$target, p=0.5, list=FALSE)
train0.raw <- train.raw[as.vector(idx)]
train1.raw <- train.raw[as.vector(-idx)]

nrow(do.call(rbind,list(train0.raw,train1.raw,calib.raw,test.raw)))

table(train0.raw$target)/nrow(train0.raw)
table(train1.raw$target)/nrow(train1.raw)

table(calib.raw$target)/nrow(calib.raw)
table(test.raw$target)/nrow(test.raw)
rm(raw,train.raw)

#save training, calibration and test data sets
save(train0.raw,train1.raw,calib.raw,test.raw,file=paste0(data.dir,"/train_calib_test.RData"))
