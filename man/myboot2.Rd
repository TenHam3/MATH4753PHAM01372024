% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myboot2.R
\name{myboot2}
\alias{myboot2}
\title{myboot2}
\usage{
myboot2(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...)
}
\arguments{
\item{iter}{The number of iterations to resample the original sample}

\item{x}{The original sample from which to resample with replacement}

\item{fun}{The function used to create a statistic for each resample and estimate the population parameter}

\item{alpha}{Value used to determine the (1-alpha)100 percent confidence interval}

\item{cx}{Value to control size of plot elements}

\item{...}{Additional parameters used in development of the histogram}
}
\value{
A list of values including the confidence interval ci, function used for parameter estimation fun, the original sample x, and the list of population parameter estimates from the resamples
}
\description{
myboot2
}
\examples{
\dontrun{myboot2(iter=10000,x=sam,alpha=0.05,fun="mean")}
}
