#' Calculate confidence interval for the mean of binomial proportions
#'
#' This method does not account for estimated confidence intervals for each unique experiment, and it uses the normal approximation to build confidence intervals. This is suitable for cases where the Central Limit Theorem holds (i.e. 30+ _experiments_).
#'
#' @param x matrix or numeric. First column of the matrix, or the numeric vector, should represent the estimated binomial proportion for each experiment.
#' @param n integer. Number of trials per experiment. If `length(n)` does not match the number of estimated binomial proportions it is repeated.
#' @param alpha numeric. Probability level for Z-value in normal distribution confidence interval calculation.
#'
#' @return matrix containing columns `"est"`, `"lwr.ci"`, `"upr.ci"`
#' @export
#'
#' @examples
#'
#' library(DescTools)
#'
#' # example: 3 binomial experiments, 10 trials, confidence level 0.8
#' x <- DescTools::BinomCI(c(5,6,7), 10, 0.8)
#'
#' # Inspect input
#' x
#'
#' # Calculate mean binomial probability and CI using normal approximation
#' # NOTE: the estimated CI from BinomCI() are _not_ considered in this method
#' BinomCIMeanNorm(x, 10)
#'
#' # compare to straight arithmetic average
#' colMeans(x)
#'
BinomCIMeanNorm <- function(x, n, alpha = 0.05) {

  if (!is.matrix(x))
    x <- matrix(x, nrow = length(x))

  p <- x[, 1]

  if (length(n) != length(p)) {
    n <- rep(n, length(p))
  }

  if (length(p) != length(n)) {
    stop("Number of trials per experiment and number of binomial probabilies must match.")
  }

  if (length(alpha) != 1) {
    stop("Only one alpha level for the mean binomial probability confidence interval is allowed")
  }

  wp <- sum(p * n) / sum(n)
  sum_squared_deviations <- sum((p - wp)^2 * n)
  standard_error <- sqrt(sum_squared_deviations / sum(n))
  z_value <- qnorm(1 - alpha / 2)
  mean_lower <- wp - z_value * standard_error
  mean_upper <- wp + z_value * standard_error

  res <- matrix(c(wp, mean_lower, mean_upper), ncol = 3)
  colnames(res) <- c("est", "lwr.ci", "upr.ci")
  res
}

#' Calculate confidence interval for the mean of binomial proportions using bootstrapping
#'
#' This method accounts for unique confidence intervals from each experiment by sampling from the confidence intervals during bootstrapping. Upper and lower confidence interval boundaries are determined using quantiles (corresponding to the specified level of `alpha`) of the bootstrapped estimates.
#'
#' @param x matrix or numeric. First column of the matrix, or the numeric vector, should represent the estimated binomial proportion for each experiment.
#' @param n integer. Number of trials per experiment. If `length(n)` does not match the number of estimated binomial proportions it is repeated.
#' @param alpha numeric. Probability level for Z-value in normal distribution confidence interval calculation.
#' @param B integer. Number of bootstrap replicates to use. Default: `1000`
#'
#' @return matrix containing columns `"est"`, `"lwr.ci"`, `"upr.ci"`
#' @export
#'
#' @examples
#'
#' library(DescTools)
#'
#' # example: 3 binomial experiments, 10 trials, confidence level 0.8
#' x <- DescTools::BinomCI(c(5,6,7), 10, 0.8)
#'
#' # Inspect input
#' x
#'
#' # Calculate mean binomial probability and CI using bootstrapping
#' # NOTE: the estimated CI from BinomCI() are considered in this method
#' BinomCIMeanBootCustom(x, 10, B = 100)
#'
#' # Compare to normal approximation
#' BinomCIMeanNorm(x, 10)
#'
#' # Compare to straight arithmetic average of experiments
#' colMeans(x)
#'
BinomCIMeanBootCustom <- function(x, n, alpha = 0.05, B = 1000) {

  if (!is.matrix(x))
    stop("`x` should be a matrix with the first column containing binomial probabilities and lower and upper confidence interval bounds in second and third columns")

  p <- x[, 1]
  ci_intervals <- x[, 2:3]

  if (length(n) != length(p)) {
    n <- rep(n, length(p))[seq(p)]
  }

  if (length(p) != length(n)) {
    stop("Number of trials per experiment and number of binomial probabilies must match.")
  }

  # bootstrap function, including sampling from supplied CI interval
  .bootstrap_resample <- function(n, ci_interval) {
    successes <- sum(rbinom(1, n, runif(1, ci_interval[1], ci_interval[2])))
    return(successes / n)
  }

  # Perform bootstrap resampling B times
  bootstrap_means <- replicate(B, sapply(1:length(p), function(i) .bootstrap_resample(n[i], ci_intervals[i,])))

  # Calculate confidence intervals based on bootstrap samples
  lower_quantile <- alpha / 2
  upper_quantile <- 1 - alpha / 2
  mean_est <- apply(bootstrap_means, 2, mean)
  mean_lower <- apply(bootstrap_means, 2, quantile, lower_quantile)
  mean_upper <- apply(bootstrap_means, 2, quantile, upper_quantile)

  # calculate mean of estimated parameters/CI
  res <- matrix(colMeans(cbind(mean_est,
                               mean_lower,
                               mean_upper)), ncol = 3)
  colnames(res) <- c("est", "lwr.ci", "upr.ci")
  res
}

#' Calculate confidence interval for the mean of binomial probabilities using pooled variance
#'
#' This method estimates confidence intervals for each unique experiment using the normal approximation. This is suitable for when the number of trials in each experiment is large enough such that Central Limit Theorem holds (i.e. 30+ trials per experiment). It does not necessarily rely on many repetitions of each experiment because the final mean binomial probability is calculated using the t-distribution and number of degrees of freedom corresponding to the effective sample size. The effective sample size is used to calculate a standard error from the pooled variance across experiments and construct the final confidence interval.
#'
#' @param x matrix or numeric. First column of the matrix, or the numeric vector, should represent the estimated binomial proportion for each experiment.
#' @param n integer. Number of trials per experiment. If `length(n)` does not match the number of estimated binomial proportions it is repeated.
#' @param alpha numeric. Probability level for Z-value in normal distribution confidence interval calculation.
#'
#' @return matrix containing columns `"est"`, `"lwr.ci"`, `"upr.ci"`
#' @export
#'
#' @examples
#'
#' library(DescTools)
#'
#' # example: 3 binomial experiments, 10 trials, confidence level 0.8
#' x <- DescTools::BinomCI(c(5,6,7), 10, 0.8)
#'
#' # Inspect input
#' x
#'
#' BinomCIMeanPoolNorm(x, 10)
#'
#' # Compare to arithmetic average
#' colMeans(x)
#'
BinomCIMeanPoolNorm <- function(x, n, alpha = 0.05) {

  if (!is.matrix(x))
    x <- matrix(x, nrow = length(x))

  p <- x[, 1]

  if (length(n) != length(p)) {
    n <- rep(n, length(p))[seq(p)]
  }

  if (length(p) != length(n)) {
    stop("Number of experiments and probability values must match.")
  }

  wp <- sum(p * n) / sum(n)

  pv <- BinomVarNorm(n, p, alpha = alpha)
  se <- sqrt(pv) / sum(n)

  df <- sum(n) - 1
  tv <- qt(1 - alpha / 2, df)

  est_lwr <- wp - tv * se
  est_upr <- wp + tv * se

  res <- matrix(c(wp, est_lwr, est_upr), ncol = 3)
  colnames(res) <- c("est", "lwr.ci", "upr.ci")
  res
}


#' Calculate confidence interval for the mean of binomial probabilities using pooled variance
#'
#' This method accounts for unique confidence intervals from each experiment by calculating the pooled variance adjusted for the confidence interval around each binomial probability. It does not use bootstrapping or rely on a large number of trials per experiment (though, the number of trials should not be _very_ small; i.e `n` should be at least 5, ideally larger) It does not necessarily rely on many repetitions of each experiment because the final mean binomial probability is calculated using the t-distribution and number of degrees of freedom corresponding to the effective sample size. The effective sample size is used to calculate a standard error from the pooled variance across experiments and construct the final confidence interval.
#'
#' @param x matrix or numeric. First column of the matrix, or the numeric vector, should represent the estimated binomial proportion for each experiment.
#' @param n integer. Number of trials per experiment. If `length(n)` does not match the number of estimated binomial proportions it is repeated.
#' @param alpha numeric. Probability level for Z-value in normal distribution confidence interval calculation.
#'
#' @return matrix containing columns `"est"`, `"lwr.ci"`, `"upr.ci"`
#' @export
#'
#' @examples
#'
#' library(DescTools)
#'
#' # example: 3 binomial experiments, 10 trials, confidence level 0.8
#' x <- DescTools::BinomCI(c(5,6,7), 10, 0.8)
#'
#' # Inspect input
#' x
#'
#' # Calculate confidence interval for mean binomial probability
#' # NOTE: accounting for custom CI in pooled variance
#' BinomCIMeanPoolCustom(x, 10)
#'
#' # Compare to t-distribution
#' BinomCIMeanPoolNorm(x, 10)
#'
#' # Compare to arithmetic average
#' colMeans(x)
#'
BinomCIMeanPoolCustom <- function(x, n, alpha = 0.05) {

  if (!is.matrix(x))
    x <- matrix(x, nrow = length(x))

  p <- x[, 1]
  ci <- lapply(seq(nrow(x)), function(y) x[y, 2:3])

  if (length(n) != length(p)) {
    n <- rep(n, length(p))[seq(p)]
  }

  if (length(p) != length(n)) {
    stop("Number of experiments and probability values must match.")
  }

  ##  calculate custom binomial variance to determine standard error
  wp <- sum(p * n) / sum(n)
  pv <- BinomVarCustom(n, p, ci_intervals = ci)
  se <- sqrt(pv) / sum(n)

  ## then use effective sample size/t-distribution for CI
  df <- sum(n) - 1
  tv <- qt(1 - alpha / 2, df)
  est_lwr <- wp - tv * se
  est_upr <- wp + tv * se

  res <- matrix(c(wp, est_lwr, est_upr), ncol = 3)
  colnames(res) <- c("est", "lwr.ci", "upr.ci")
  res
}
