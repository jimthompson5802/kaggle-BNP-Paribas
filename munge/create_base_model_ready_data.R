###
#  create baseline model ready data
#  by imputing missing values using the median
#  and adding indicator for missing values
###

library(data.table)
library(plyr)

DATA.DIR <- "./data"

# load  data
load(paste0(DATA.DIR,"/train_calib_test.RData"))

# load data types
load(paste0(DATA.DIR,"/attr_data_types.RData"))

#consolidate training data
train.raw <- rbind(train0.raw,train1.raw)

num.attrs <- c(attr.data.types$numeric,attr.data.types$integer)

# find median for numeric attributes
medians <- sapply(num.attrs, function(x){median(train.raw[[x]],na.rm=TRUE)})
comment(medians) <- "medians for all numeric attributes based on train0 and train1 data sets"


# fill in missing values and create indciator variables
fillInMissing <- function(attr.name) {
    # find locations for any missing 
    idx <- is.na(train.raw[[attr.name]])
    if (sum(idx) > 0) {
        train.raw[[attr.name]][idx] <- medians[attr.name]
        ll <- list()
        ll[[attr.name]] <- train.raw[[attr.name]]
        train.raw[[paste0(attr.name,".na")]] <- 
        ans <- cbind()
    } 
}

