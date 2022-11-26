% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boot.R
\name{print.bootstrap}
\alias{print.bootstrap}
\title{Bootstrap Print}
\usage{
\method{print}{bootstrap}(x)
}
\value{
stats runif
}
\description{
This bootstrap function is a modification of the the bootstrap.function with a different output and the estimated of the variance.
}
\examples{
set.seed(566)
boot2 <- Bootstrap(matrix(rnorm(200),nc=2), statistic = var)
boot2
}