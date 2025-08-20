library(SOILmilaR)
data("loamy", package = "SOILmilaR")

rate_taxpartsize <- function(x) {
  dplyr::case_match(x,
                    c("sandy-skeletal") ~ 1,
                    c("sandy") ~ 3,
                    c("loamy", "coarse-loamy", "coarse-silty") ~ 5,
                    c("fine-loamy", "fine-silty") ~ 7,
                    c("clayey", "fine") ~ 9,
                    c("very-fine") ~ 11,
                    c("loamy-skeletal", "clayey-skeletal") ~ 13,
                    "fragmental" ~ 15)
}

rate_depthclass <- function(x, breaks = c(`very shallow` = 25, `shallow` = 50, `moderately deep` = 100, `deep` = 150, `very deep` = 1e4), ...) {
  res <- cut(x, c(0, breaks))
  factor(res, levels = levels(res), labels = names(breaks))
}

m <- list(taxpartsize = rate_taxpartsize, depth = rate_depthclass)

mapunit_composition <- design_mapunit(loamy, m)

mapunit_composition[order(mapunit_composition$component), ]

sort(prop.table(table(mapunit_composition$component)), decreasing = TRUE)
