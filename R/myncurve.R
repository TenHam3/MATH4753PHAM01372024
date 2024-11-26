#' @title Normal Curve
#'
#' @importFrom graphics curve polygon text
#' @importFrom stats dnorm pnorm
#'
#' @param mu Mean of the distribution
#' @param sigma Standard deviation of the distribution
#' @param a Upper bound of lower tail probability
#'
#' @return A list of the parameters
#' @export
#'
#' @examples
#' myncurve(5,3,2)
myncurve = function(mu, sigma, a){
  x <- NULL
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu+3*sigma))

  xcurve <- seq(mu-3*sigma,a,length=1000)
  ycurve <- dnorm(xcurve,mu,sigma)
  polygon(c(mu-3*sigma,xcurve,a), c(0,ycurve,0),col="Red")

  prob <- pnorm(a,mu,sigma)
  prob <- round(prob,4)

  text(x=(a + (mu-3*sigma)) / 2,y=0.5*dnorm((a + (mu-3*sigma)) / 2,mu,sigma),paste0("Area = ", prob))

  list(mu = mu, sigma = sigma, area = prob)
}
