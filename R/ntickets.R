#' @title ntickets
#'
#' @importFrom graphics abline
#' @importFrom stats pbinom uniroot
#'
#' @param N Number of seats on the flight
#' @param gamma Probability of overbooking
#' @param p Probability of success (a person showing up)
#'
#' @return A list of the optimal number of tickets to sell for the discrete distribution and continuous approximation, the number of seats on the flight, the probability of overbooking, and the probability of a person showing up
#' @export
#'
#' @examples
#' ntickets(400,0.02,0.95)
ntickets = function(N=400,gamma=0.02,p=0.95) {
  # nd - n for discrete/optimal number of tickets to be sold for discrete
  disc.n <- seq(N, floor(N + N/10), by=1)
  tmp <- 1 - gamma - pbinom(N,disc.n,p)
  ind <- which.min(abs(tmp))
  nd <- disc.n[ind]
  plot(disc.n,tmp,xlab="n",ylab="Objective Function",
       main=paste("Objective function vs n to find optimal tickets sold (", nd, ")
                gamma=", gamma, " N=", N, "discrete"))
  abline(h=tmp[ind],v=disc.n[ind], col="Blue",lwd=1.5)

  # nc - n for continuous/optimal number of tickets to be sold for continuous
  f <- function(x) {
    return (1-gamma-pnorm(N+0.5,x*p,sqrt(x*p*(1-p))))
  }
  options(digits=10)
  nc <- uniroot(f,interval=c(N,floor(N+(N/10))))$root
  plot(f, from=N, to=floor(N+(N/10)), xlab="n",ylab="Objective Function",
       main=paste("Objective function vs n to find optimal tickets sold (", nc, ")
                gamma=", gamma, " N=", N, "continuous"))
  abline(h=f(nc),v=nc, col="Blue",lwd=1.5)

  list(nd=nd,nc=round(nc,4),N=N,p=p,gamma=gamma)
}
