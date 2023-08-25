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

