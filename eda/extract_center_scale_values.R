###
# this code determines mean and sd for center/scaling and 
# median values post center/scaling this is used for imputing missing values
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
center.scale.parms <- lapply(c(attr.data.types$numeric,attr.data.types$integer),function(x){
    mean.value <- mean(all.data[[x]],na.rm=TRUE)
    sd.value <- sd(all.data[[x]],na.rm=TRUE)
    center.scale.x <- (all.data[[x]]-mean.value)/sd.value
    median.value <- median(center.scale.x,na.rm = TRUE)
    return(list(mean.value=mean.value,sd.value=sd.value,median.value=median.value))
})

names(center.scale.parms) <- c(attr.data.types$numeric,attr.data.types$integer)
comment(center.scale.parms) <- "parameters to center and scale numeric attributes, median.value post center and scale"

save(center.scale.parms,file=paste0(DATA.DIR,"/center_scale_parms.RData"))
