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
data.folds <- createFolds(raw$target, k=2)

# check porportion of classes
dim(raw)

length(raw$target[folds[[1]]]) + length(raw$target[folds[[2]]])

table(raw$target[folds[[1]]])/length(raw$target[folds[[1]]])

table(raw$target[folds[[2]]])/length(raw$target[folds[[2]]])


#save folds
save(data.folds,file=paste0(data.dir,"/fold_specification.RData"))

