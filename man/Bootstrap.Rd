% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boot.R
\name{Bootstrap}
\alias{Bootstrap}
\title{Bootstrap Function Modification}
\usage{
Bootstrap(x, B = 1000, statistic, sample_size = length(x))
}
\arguments{
\item{x}{represent the original sample}

\item{B}{number of bootstrap samples. By default B = 1000}

\item{statistic}{is an indicator of the statistic applied to the samples}

\item{sample_size}{is the length of every bootstrap sample. By default sample_size = "the original sample size"}
}
\value{
A \code{list} of type bootstrap.class composed of relevant values provided by the Bootstrap function:
\describe{
    \item{theta_hat}{The statistic applied to the original sample}
    \item{theta_star}{A \code{list} of length B composed of statistics applied to each bootstrapped samples}
    \item{varBoot}{The variance applied to theta_star}
    \item{Unbiased_estimator}{Unbieased variance of the estimator}
    \item{coerced_to_a_vector}{Returning a True or Flase regarding the initial sample if it was coerced to a vector}
    \item{stat}{Return the name of the statistic used thruough the Bootstrap function}
    \item{B}{Number of bootrsapped samples}
    \item{sample_size}{Return the length of every bootstrap sample}
}
}
\description{
This Bootstrap function will provide you always an object of class bootstrap.
}
\examples{
Bootstrap(rnorm(100), statistic = mean)
}
\author{
Group-E
}
