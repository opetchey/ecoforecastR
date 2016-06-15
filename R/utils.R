##' @name mat2mcmc.list
##' @title mat2mcmc.list
##' @export
##' @author Mike Dietze
##' @description convert a matrix to a CODA mcmc.list
##' @param w  matrix
mat2mcmc.list <- function(w){
  temp <- list()
  chain.col = which(colnames(w)=="CHAIN")
  for(i in unique(w[,"CHAIN"])){
    temp[[i]] <- as.mcmc(w[w[,"CHAIN"]==i,-chain.col])
  }
  return(as.mcmc.list(temp))
}

##' @name ciEnvelope
##' @title ciEnvelope
##' @export 
##' @author Mike Dietze
##' @description plot a confidence/credible interval
##' @param x x-axis data
##' @param ylo y-axis lower confidence bound
##' @param yhi y-axis upper confidence bound
##' @param ... optional graphical parameters
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

##' @name plot_ss
##' @title plot_ss
##' @export
##' @author Mike Dietze
##' @description plot time series output of a state-space model
##' @param time time axis values
##' @param fit fit_dlm output object
##' @param ... optional graphical parameters
plot_ss <- function(time,fit,...){
  ci <- apply(as.matrix(fit$predict),2,quantile,c(0.025,0.5,0.975))
  plot(time,ci[2,],type='n',ylim=range(ci,na.rm=TRUE),...)
  ciEnvelope(time,ci[1,],ci[3,],col="lightBlue")
  lines(time,ci[2,],col="blue")
  points(time,fit$data$OBS,pch="+",cex=0.5)
}


### NEED TO ADD FUNCTION TO DETECT AND TRIM BURN-IN