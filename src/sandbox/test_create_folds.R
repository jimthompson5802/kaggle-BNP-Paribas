###
#  Test creating Folds
###

library(caret)


library(caret)
library(data.table)


data.dir <- "./data"


raw <- fread(paste0(data.dir,"/train.csv"))
setkey(raw,ID)

nrow(raw)
table(raw$target)/nrow(raw)

set.seed(13)
folds <- createFolds(raw$target, k=2)

dim(raw)

length(raw$target[folds[[1]]]) + length(raw$target[folds[[2]]])

table(raw$target[folds[[1]]])/length(raw$target[folds[[1]]])

table(raw$target[folds[[2]]])/length(raw$target[folds[[2]]])

train0.raw <- raw[folds[[1]],]

train1.raw <- raw[folds[[2]],]
