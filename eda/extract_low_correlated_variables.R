###
# this code determines coorelated variables to eliminate
###

library(data.table)
library(caret)

DATA.DIR <- "./data"


# get attribute data types
load(paste0(DATA.DIR,"/attr_data_types.RData"))

# get the data
load(paste0(DATA.DIR,"/train_calib_test.RData"))

#use only training data for imputing missing values
all.data <- rbind(train0.raw,train1.raw)

# get only the numeric/integer variables
all.data <- all.data[,c(attr.data.types$numeric,attr.data.types$integer),
                     with=FALSE]
all.data <- na.omit(all.data)

idx <- findCorrelation(cor(all.data))

correlated.vars <- names(all.data)[idx]

low.correlated.vars <- setdiff(names(all.data),correlated.vars)

comment(low.correlated.vars) <- "low correlation variables"

save(low.correlated.vars,file=paste0(DATA.DIR,"/low_correlated_vars.RData"))
