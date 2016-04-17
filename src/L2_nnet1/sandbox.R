makeGradientFunction <- function(target,rf2.probs, gbm2.probs,gbm4.probs) {
    
    # adjust probabilities to avoid numerical issues with log() function
    rf2.probs <- apply(rf2.probs,c(1,2),function(elem){max(min(elem,(1-1e-15)),1e-15)})
    rf2.probs <- rf2.probs/apply(rf2.probs,1,sum)
    
    gbm2.probs <- apply(gbm2.probs,c(1,2),function(elem){max(min(elem,(1-1e-15)),1e-15)})
    gbm2.probs <- gbm2.probs/apply(rf2.probs,1,sum)
    
    gbm4.probs <- apply(gbm4.probs,c(1,2),function(elem){max(min(elem,(1-1e-15)),1e-15)})
    gbm4.probs <- gbm4.probs/apply(rf2.probs,1,sum)
    
    function(w) {
        gr <- rep(NA,3)
        
        # create indicator matrix
        ll <- lapply(target,function(class){as.integer(colnames(rf2.probs)==class)})
        y <- do.call(rbind,ll)
        colnames(y) <- paste0("Class_",1:9)
        
        denom <- w[1]*rf2.probs + w[2]*gbm2.probs + w[3]*gbm4.probs
        
        pred.probs <- rf2.probs/denom
        gr[1] <- -sum(pred.probs*y)/nrow(pred.probs)
        
        pred.probs <- gbm2.probs/denom
        gr[2] <- -sum(pred.probs*y)/nrow(pred.probs)
        
        pred.probs <- gbm4.probs/denom
        gr[3] <- -sum(pred.probs*y)/nrow(pred.probs)
        
        return(gr)
    }
}


#
# 2nd version for optimal weights determine weights for each class
#

makeEnsembleFunction2 <- function(target,rf2.probs, gbm2.probs,gbm4.probs) {
    function(w) {
        
        ll <- lapply(1:9, function(i){w[i]*rf2.probs[,i] +
                                          w[9+i]*gbm2.probs[,i] +
                                          w[18+i]*gbm4.probs[,i]})
        
        
        pred.probs <- do.call(cbind,ll)
        
        colnames(pred.probs) <- paste0("Class_",1:9)
        
        logLossEval(pred.probs,target)
    }
}

ensFunc2 <- makeEnsembleFunction2(calib.raw$target,rf2.probs,gbm2.probs,gbm4.probs)

# define equality constraints
heq2 <- function(w) {
    
    h <- t(sapply(1:9,function(i){ans<-rep(0,27);ans[i]<-1;ans[9+i]<-1;ans[18+i]<-1;return(ans)}))
    
    h <- w*h
    
    return(apply(h,1,sum)-1)
}

heq2.jac <- function(w){
    j <- matrix(NA,9,length(w))
    
    j[1,] <- rep(c(1,0,0,0,0,0,0,0,0),3)
    j[2,] <- rep(c(0,1,0,0,0,0,0,0,0),3)
    j[3,] <- rep(c(0,0,1,0,0,0,0,0,0),3)
    j[4,] <- rep(c(0,0,0,1,0,0,0,0,0),3)
    j[5,] <- rep(c(0,0,0,0,1,0,0,0,0),3)
    j[6,] <- rep(c(0,0,0,0,0,1,0,0,0),3)
    j[7,] <- rep(c(0,0,0,0,0,0,1,0,0),3)
    j[8,] <- rep(c(0,0,0,0,0,0,0,1,0),3)
    j[9,] <- rep(c(0,0,0,0,0,0,0,0,1),3)
    
    return(j)
}

# define inequality constraints
hin2 <- function(w) {
    h <- rep(NA,2*length(w))
    
    for (i in 1:length(w)) {
        h[2*(i-1)+1] <- w[i]
        h[2*(i-1)+2] <- 1 - w[i]
    }
    
    return(h)
    
}


hin2.jac <- function(w) {
    j <- matrix(0,2*length(w),length(w))
    
    
    for (i in 1:length(w)) {
        j[2*(i-1)+1,i] <- 1
        j[2*(i-1)+2,i] <- -1
    }
    
    return(j)
}

Sys.time()
system.time(opt2.wts <- constrOptim.nl(rep(1/3,27),fn=ensFunc2, 
                                       hin=hin2, hin.jac=hin2.jac,
                                       heq=heq2, heq.jac=heq2.jac,
                                       control.outer=list(trace=TRUE,itmax=10),
                                       control.optim=list(trace=2)))

opt2.wts