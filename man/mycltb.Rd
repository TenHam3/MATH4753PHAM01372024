% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mycltb.R
\name{mycltb}
\alias{mycltb}
\title{mycltb}
\usage{
mycltb(n, iter, p = 0.5, ...)
}
\arguments{
\item{n}{sample size}

\item{iter}{number of samples to create}

\item{p}{probability of success on a single trial of the binomial}

\item{...}{other arguments for hist}
}
\value{
a histogram of the distribution of the mean of the samples
}
\description{
mycltb
}
\examples{
mycltb(n=20,iter=10000,p=0.3)
}
