###
# this code extract the median values for all numeric attributes
# this is used for imputing missing values
###

library(data.table)

DATA.DIR <- "./data"


# get attribute data types
load(paste0(DATA.DIR,"/attr_data_types.RData"))

# get the data
load(paste0(DATA.DIR,"/train_calib_test.RData"))

#use only training data for imputing missing values
all.data <- rbind(train0.raw,train1.raw)

#determine levels for each factor attribute
median.values <- lapply(c(attr.data.types$numeric,attr.data.types$integer),function(x){
    median.value <- median(all.data[[x]],na.rm = TRUE)
    return(median.value)
})

names(median.values) <- c(attr.data.types$numeric,attr.data.types$integer)

save(median.values,file=paste0(DATA.DIR,"/median_values.RData"))
