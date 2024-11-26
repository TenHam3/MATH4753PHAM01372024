#' @title myci
#' @importFrom stats qnorm qt sd
#'
#' @param x Vector containing sample data
#'
#' @return 95% confidence interval of the population mean of x
#' @export
#'
#' @examples
#' myci(c(1,2,3))
myci <- function(x) {
  alpha=0.05
  n=length(x)
  mp=c(-1,1)
  quant=qt(1-alpha/2,n-1)
  ci=mean(x) + mp*quant*(sd(x)/sqrt(n))
  return(ci)
}
