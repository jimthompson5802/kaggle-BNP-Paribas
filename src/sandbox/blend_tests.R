###
#  Blending test2
###

source("./src/CommonFunctions.R")

# retrieve Level 2 features
nnet11.env <- new.env()
load("./src/L1_nnet11/level2_features.RData",envir=nnet11.env)

xgb11.env <- new.env()
load("./src/L1_xgb11/level2_features.RData",envir=xgb11.env)

# baseline log loss function
cat("nnet11:",logLossEval(nnet11.env$level2.data[,"Class_1"],nnet11.env$level2.data[,"response"]))
cat("xgb11:",logLossEval(xgb11.env$level2.data[,"Class_1"],xgb11.env$level2.data[,"response"]))

nnet11 <- nnet11.env$level2.data$Class_1
xgb11 <- xgb11.env$level2.data$Class_1
response <- nnet11.env$level2.data$response


cat("average:",logLossEval((nnet11+xgb11)/2,response))

cat("geometric mean:",logLossEval(sqrt(nnet11*xgb11),response))




