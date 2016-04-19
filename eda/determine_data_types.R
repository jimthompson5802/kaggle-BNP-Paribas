###
# determine data types
###

library(data.table)

ID.VAR <- "ID"
TARGET.VAR <- "target"

# get training data
train.raw <- fread(paste0(DATA.DIR,"/train.csv"))
setkey(train.raw,ID)


# extract only candidate feture names
candidate.features <- setdiff(names(train.raw),c(ID.VAR,TARGET.VAR))
data.type <- sapply(candidate.features,function(x){class(train.raw[[x]])})
table(data.type)

print(data.type)

# deterimine data types
explanatory.attributes <- setdiff(names(train.raw),c(ID.VAR,TARGET.VAR))
data.classes <- sapply(explanatory.attributes,function(x){class(train.raw[[x]])})

# categorize data types in the data set?
unique.classes <- unique(data.classes)

attr.data.types <- lapply(unique.classes,function(x){names(data.classes[data.classes==x])})
names(attr.data.types) <- unique.classes
comment(attr.data.types) <- "list that categorize training data types"

# find numeric attributes that contain missing values
num.missing <- sapply(c(attr.data.types$numeric,attr.data.types$integer),
                      function(x){sum(is.na(train.raw[[x]]))})
attr.data.types$contain.na <- names(num.missing)[num.missing>0]
save(attr.data.types,file=paste0(DATA.DIR,"/attr_data_types.RData"))
