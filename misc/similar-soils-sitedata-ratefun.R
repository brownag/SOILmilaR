set.seed(123)

# generate 5 synthetic transects
#  - assuming a similar soil forming function for transects across n delineations
#  - taxonomic particle size is generally fine-loamy, but coarser textured in higher fragment material, more dynamic hillslopes
#  - depth varies from shallow to very deep, uniform but centered around moderately deep
#  - pscs_* quantities provided for example
x <- do.call('rbind', lapply(1:3, \(i) data.frame(id = paste0(LETTERS[1:10], i),
           taxpartsize = c("fine-loamy","fine-loamy","fine-loamy","fine-loamy",
                           "coarse-loamy", "coarse-loamy", "coarse-loamy", "loamy-skeletal",
                           "loamy-skeletal", "loamy-skeletal"),
           depth = runif(10, 35, 150),
           pscs_clay = c(runif(4, 18, 35), runif(6, 14, 18)),
           pscs_frags = c(runif(3, 0, 15), runif(4, 10, 34),
                          runif(3, 35, 60) + c(0, 40, 0)))))

rate_taxpartsize <- function(x) {
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
  factor(res, levels = levels(res), labels = names(breaks))
}

m <- list(taxpartsize = rate_taxpartsize,
            depth =  rate_depthclass)

# iterative filtering and application of similar soils
res0 <- similar_soils(x, m)
res0

y <- subset(x, !res0$similar,
            select = c("id", "taxpartsize", "depth"))

res1 <- similar_soils(y, m)
res1

z <- subset(x, !x$id %in% c(res0$id[res0$similar], res1$id[res1$similar]),
            select = c("id", "taxpartsize", "depth"))
res2 <- similar_soils(z, m)
res2

fin <- do.call('rbind', list(
  data.frame(component = "alpha", subset(res0, similar)),
  data.frame(component = "beta", subset(res1, similar)),
  data.frame(component = "gamma", subset(res2, similar))
))

fin <- rbind(fin, data.frame(component = "unassigned", subset(res0, !res0$id %in% fin$id)))
fin <- fin[order(fin$id), ]
fin$similar_dist <- res0$similar_dist
fin$similar <- res0$similar # show similarity to primary component
fin

prop.table(table(fin$component))

# Alpha-Beta-Gamma complex?
#  - Alpha (55%): fine-loamy, moderately deep to deep
#  - Beta (30%): loamy-skeletal, moderately deep (dissimilar and limiting)
#  - Gamma (15%): coarse-loamy, shallow (very contrasting and limiting)
#  - Minor components (5%)
