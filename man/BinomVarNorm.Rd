% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pooled-variance.R
\name{BinomVarNorm}
\alias{BinomVarNorm}
\title{Calculate pooled variance for related binomial experiments with confidence intervals}
\usage{
BinomVarNorm(n, p_values, alpha = 0.05)
}
\arguments{
\item{n}{integer. Number of trials per experiment.}

\item{p_values}{numeric. Probability of success for each experiment.}

\item{alpha}{numeric. Probability level for Z-value in normal distribution confidence interval calculation.}
}
\description{
This method calculates pooled variance for related binomial experiments with confidence intervals. It relies on the normal approximation / Central Limit Theorem to develop a confidence interval each binomial probability in the vector \code{p_values}. In order for this assumption to work well, \code{n} should be large. The method adjusts the calculated binomial variance to account for the normal approximation confidence interval, and then calculates pooled variance across all experiments.
}
\examples{

n <- c(10, 5, 30)
p_values <- c(0.2, 0.3, 0.4)
alpha <- 0.05

BinomVarNorm(n, p_values, alpha)
}
