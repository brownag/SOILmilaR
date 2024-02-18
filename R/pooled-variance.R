#' Calculate pooled variance for related binomial experiments with confidence intervals
#'
#' This method calculates pooled variance for related binomial experiments with confidence intervals. It relies on the normal approximation / Central Limit Theorem to develop a confidence interval each binomial probability in the vector `p_values`. In order for this assumption to work well, `n` should be large. The method adjusts the calculated binomial variance to account for the normal approximation confidence interval, and then calculates pooled variance across all experiments.
#'
#'
#' @param n integer. Number of trials per experiment.
#' @param p_values numeric. Probability of success for each experiment.
#' @param alpha numeric. Probability level for Z-value in normal distribution confidence interval calculation.
#'
#' @export
#' @examples
#'
#' n <- c(10, 5, 30)
#' p_values <- c(0.2, 0.3, 0.4)
#' alpha <- 0.05
#'
#' BinomVarNorm(n, p_values, alpha)
BinomVarNorm <- function(n, p_values, alpha = 0.05) {
  k <- length(p_values)

  if (length(n) < length(p_values)) {
    n <- rep(n, length(p_values))
  }

  if (length(alpha) < length(p_values)) {
    alpha <- rep(alpha, length(p_values))
  }

  # Calculate variances
  variances <- n * p_values * (1 - p_values)

  # Calculate confidence intervals for each p
  z_value <- qnorm(1 - alpha / 2)  # Z-value for normal distribution
  ci <- z_value * sqrt(p_values * (1 - p_values) / n)

  # Adjust variances for confidence intervals
  adjusted_variances <- variances + ci^2

  # Initialize total variance
  total_variance <- sum(adjusted_variances)

  if (length(adjusted_variances) > 1) {
    # Calculate covariances and update total variance
    for (i in 1:(k - 1)) {
      for (j in (i + 1):k) {
        cov_ij <- 2 * n[i] * p_values[i] * (1 - p_values[i]) * p_values[j] * (1 - p_values[j])
        total_variance <- total_variance + cov_ij
      }
    }
  }
  return(total_variance)
}

#' Calculate pooled variance for related binomial experiments with given confidence intervals
#'
#' This method calculates pooled variance for related binomial experiments with confidence intervals. Instead of relying on the normal approximation, the user can calculate or estimate their own confidence intervals. The method adjusts the calculated binomial variance to account for an arbitrary given confidence interval, and then calculates pooled variance across all experiments.
#'
#' @param n integer. Number of trials per experiment
#' @param p_values numeric. Probability of success for each experiment.
#' @param ci_intervals list of numeric vectors. The list has length equal to length of `p_values`. Each element of the list contains a numeric vector of length 2 denoting the lower and upper confidence limits.
#' @export
#'
#' @examples
#'
#' n <- 10  # Number of trials for each experiment
#'
#' p_values <- c(0.2, 0.3, 0.4) # Probability of success for each experiment
#'
#' # Wilson score intervals for each p
#' wilson_intervals <- list(
#'   c(0.1, 0.3),
#'   c(0.2, 0.4),
#'   c(0.3, 0.5)
#' )
#'
#' # Calculate pooled variance with custom confidence intervals
#' BinomVarCustom(n, p_values, wilson_intervals)
BinomVarCustom <- function(n, p_values, ci_intervals) {

  # TODO: adjust this to use DescTools-like matrix input

  k <- length(p_values)

  if (length(ci_intervals) != k) {
    stop("Number of confidence intervals must match the number of p_values.")
  }

  # Adjust variances for custom confidence intervals
  adjusted_variances <- sapply(seq_along(p_values), function(i) {
    ci <- ci_intervals[[i]]
    ci_lower <- ci[1]
    ci_upper <- ci[2]
    p_hat <- p_values[i]
    (n[i] * p_hat * (1 - p_hat)) + (ci_upper - p_hat)^2 / 4 + (p_hat - ci_lower)^2 / 4
  })

  # Initialize total variance
  total_variance <- sum(adjusted_variances)

  if (length(adjusted_variances) > 1) {
    # Calculate covariances and update total variance
    for (i in 1:(k - 1)) {
      for (j in (i + 1):k) {
        cov_ij <- 2 * n[i] * p_values[i] * (1 - p_values[i]) * p_values[j] * (1 - p_values[j])
        total_variance <- total_variance + cov_ij
      }
    }
  }
  return(total_variance)
}
