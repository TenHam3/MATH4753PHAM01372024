#' @title myboot2
#' @importFrom graphics segments
#' @importFrom stats quantile
#'
#' @param iter The number of iterations to resample the original sample
#' @param x The original sample from which to resample with replacement
#' @param fun The function used to create a statistic for each resample and estimate the population parameter
#' @param alpha Value used to determine the (1-alpha)100 percent confidence interval
#' @param cx Value to control size of plot elements
#' @param ... Additional parameters used in development of the histogram
#'
#' @return A list of values including the confidence interval ci, function used for parameter estimation fun, the original sample x, and the list of population parameter estimates from the resamples
#' @export
#'
#' @examples
#' \dontrun{myboot2(iter=10000,x=sam,alpha=0.05,fun="mean")}
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){  #Notice where the ... is repeated in the code
  n=length(x)   #sample size

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun) # xstat is a vector and will have iter values in it
  ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")# Vertical line
  segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x,xstat=xstat))# Some output to use if necessary
}
