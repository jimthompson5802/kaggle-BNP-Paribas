###
# prototype to pass model parameters from R to Python
###

WORK.DIR <- "./src/sandbox"


MODEL.PARMS <- list(n.tree=1000, max.depth=20, alpaha=0.5)

write.table(data.frame(MODEL.PARMS,stringsAsFactors = FALSE),file=paste0(WORK.DIR,"/py_parms.tsv"),
            sep="\t", row.names=FALSE)