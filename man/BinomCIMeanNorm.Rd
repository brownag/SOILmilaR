% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binomial-ci.R
\name{BinomCIMeanNorm}
\alias{BinomCIMeanNorm}
\title{Calculate confidence interval for the mean of binomial proportions}
\usage{
BinomCIMeanNorm(x, n, alpha = 0.05)
}
\arguments{
\item{x}{matrix or numeric. First column of the matrix, or the numeric vector, should represent the estimated binomial proportion for each experiment.}

\item{n}{integer. Number of trials per experiment. If \code{length(n)} does not match the number of estimated binomial proportions it is repeated.}

\item{alpha}{numeric. Probability level for Z-value in normal distribution confidence interval calculation.}
}
\value{
matrix containing columns \code{"est"}, \code{"lwr.ci"}, \code{"upr.ci"}
}
\description{
This method does not account for estimated confidence intervals for each unique experiment, and it uses the normal approximation to build confidence intervals. This is suitable for cases where the Central Limit Theorem holds (i.e. 30+ \emph{experiments}).
}
\examples{

library(DescTools)

# example: 3 binomial experiments, 10 trials, confidence level 0.8
x <- DescTools::BinomCI(c(5,6,7), 10, 0.8)

# Inspect input
x

# Calculate mean binomial probability and CI using normal approximation
# NOTE: the estimated CI from BinomCI() are _not_ considered in this method
BinomCIMeanNorm(x, 10)

# compare to straight arithmetic average
colMeans(x)

}
