#' @title mypvalue
#' @importFrom stats dt pt
#'
#' @param t0 T-calc value obtained by assuming the null hypothesis is true and plugging in the null value into the T-statistic equation
#' @param xmax X-value used to create the bounds for the curve plotted for the T-statistic distribution
#' @param n Sample size
#' @param alpha Value that determines the confidence level of the confidence interval and used to compare with the p-value to determine whether to reject the null hypothesis or not
#'
#' @return A list containing the quantile t_(alpha/2) as q and the area corresponding to the p-value as pvalue
#' @export
#'
#' @examples
#' mypvalue(t0=1.326252)
mypvalue=function(t0,xmax=4,n=20, alpha=0.05){
  #calculate alpha/2
  va=round(pt(-t0,df=n-1),4)
  pv=2*va
  x=NULL
  # plot the t dist
  curve(dt(x,df=n-1),xlim=c(-xmax,xmax),ylab="T Density",xlab=expression(t),
        main=substitute(paste("P-value=", pv, " alpha=", alpha)))


  # set up points on the polygon to the right
  xcurve=seq(t0,xmax,length=1000)
  ycurve=dt(xcurve,df=n-1)

  # set up points to the left
  xlcurve=seq(-t0,-xmax,length=1000)
  ylcurve=dt(xcurve,df=n-1)

  # Shade in the polygon defined by the line segments
  polygon(c(t0,xcurve,xmax),c(0,ycurve,0),col="green")
  polygon(c(-t0,xlcurve,-xmax),c(0,ylcurve,0),col="green")

  # make quantiles
  q=qt(1-alpha/2,n-1)
  abline( v=c(q,-q),lwd=2) # plot the cut off t value
  axis(3,c(q,-q),c(expression(abs(t[alpha/2])),expression(-abs(t[alpha/2]))))


  # Annotation
  text(0.5*(t0+xmax),max(ycurve),substitute(paste(area, "=",va)))
  text(-0.5*(t0+xmax),max(ycurve),expression(area))

  return(list(q=q,pvalue=pv))
}
