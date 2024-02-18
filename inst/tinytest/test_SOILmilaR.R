set.seed(456)

x <- do.call('rbind', lapply(1:3, \(i) data.frame(id = paste0(LETTERS[1:10], i),
                                                  taxpartsize = c("fine-loamy", "loamy", "fine-loamy", "fine-loamy",
                                                                  "coarse-loamy", "coarse-loamy", "coarse-loamy", "loamy-skeletal",
                                                                  "loamy-skeletal", "loamy-skeletal"),
                                                  depth = runif(10, 35, 150),
                                                  pscs_clay = c(runif(4, 18, 35), runif(6, 14, 18)),
                                                  pscs_frags = c(runif(3, 0, 15), runif(4, 10, 34),
                                                                 runif(3, 35, 60) + c(0, 40, 0)))))

rate_taxpartsize <- function(x) {
  # TODO: this is just made up logic for this example and needs to be updated
  dplyr::case_match(x,
                    c("sandy-skeletal") ~ 1,
                    c("sandy") ~ 2,
                    c("loamy", "coarse-loamy", "coarse-silty") ~ 3,
                    c("fine-loamy", "fine-silty") ~ 4,
                    c("clayey", "fine") ~ 5,
                    c("very-fine") ~ 6,
                    c("loamy-skeletal", "clayey-skeletal") ~ 7)
}

rate_depthclass <- function(x,
                            breaks = c(
                              `very shallow` = 25,
                              `shallow` = 50,
                              `moderately deep` = 100,
                              `deep` = 150,
                              `very deep` = 1e4
                            ),
                            ...) {
  res <- cut(x, c(0, breaks))
  factor(res, levels = levels(res), labels = names(breaks), ordered = TRUE)
}

rate_pscs_clay <- function(x,
                           breaks = c(18, 27, 40, 60, 100)) {
  res <- cut(x, c(0, breaks))
  factor(res, levels = levels(res), ordered = TRUE)
}

m <- list(taxpartsize = rate_taxpartsize,
          depth = rate_depthclass,
          pscs_clay = rate_pscs_clay)

s <- similar_soils(x, m)

# Placeholder with simple test
expect_equal(nrow(s), nrow(x))

## testing pooled variance functions

# for very large n, single experiment, the normal approximation and custom approach are equivalent
x1 <- BinomVarNorm(1e6, 0.5, alpha = 0.05)
x2 <- BinomVarCustom(1e6, 0.5, list(c(0.4, 0.6)))
expect_equal(x1, x2, tolerance = 1e-5)

# for multiple experiments, constant n, some variation in p, alpha = 0.05
expect_equal(BinomVarNorm(10, c(0.33, 0.5, 0.55), alpha = 0.05), 10.89949, tolerance = 1e-5)

if (!inherits(try(requireNamespace("DescTools", quietly = TRUE), silent = TRUE), 'try-error')) {

  library(DescTools)

  set.seed(123)

  # example: 3 binomial experiments, 10 trials, confidence level 0.8
  x <- DescTools::BinomCI(c(5,6,7), 10, 0.8)

  # basic arithmetic mean of parameters across experiments
  avg <- matrix(colMeans(x), ncol = 3)
  colnames(avg) <- colnames(x)

  res <- list(
    avg_norm      = BinomCIMeanNorm(x, 10),
    avg_pool_norm = BinomCIMeanPoolNorm(x, 10),
    avg_pool_cust = BinomCIMeanPoolCustom(x, 10),
    avg_boot_cust = BinomCIMeanBootCustom(x, 10, B = 2000)
  )

  # data struct/format
  expect_true(all(sapply(res, class)[1,] == "matrix"))
  expect_true(all(sapply(res, ncol) == 3))

  # bootstrap mean probability should be ~close or equal to to arithmetic average
  expect_equal(res$avg_norm[1,1], avg[1, 1])
  expect_equal(res$avg_pool_norm[1,1], avg[1, 1])
  expect_equal(res$avg_pool_cust[1,1], avg[1, 1])
  expect_true(abs(res$avg_boot_cust[1, 1] - avg[1, 1]) < 0.02)

  # relative CI widths: normal < pool_cust ~= boot_cust < pool_norm
  expect_equal(names(sort(sapply(res, function(y) as.numeric(y[, 3] - y[, 2])))),
    c("avg_norm", "avg_boot_cust", "avg_pool_cust", "avg_pool_norm"))
}
