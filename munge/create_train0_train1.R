###
# create train0/train1
###

library(caret)
library(data.table)


data.dir <- "./data"


raw <- fread(paste0(data.dir,"/train.csv"))
setkey(raw,ID)

nrow(raw)
table(raw$target)/nrow(raw)

# create the partitions
set.seed(13)
idx <- createDataPartition(raw$target, p = 0.5, list=FALSE)
train0.raw <- raw[idx]
train1.raw <- raw[-idx]


table(train0.raw$target)/nrow(train0.raw)
table(train1.raw$target)/nrow(train1.raw)

#save training, calibration and test data sets
save(train0.raw,train1.raw,file=paste0(data.dir,"/train0_train1.RData"))

