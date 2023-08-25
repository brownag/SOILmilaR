library(aqp)
library(soilDB)
data("loafercreek")

set.seed(0)
spc <- loafercreek[sample(1:length(loafercreek), 20)]

# spc$profile_id <-
#   do.call('c', aggregate(spc$pedon_id, by = list(spc$pedon_id), \(x) paste0(x, c("", "_2")[1:length(x)]))$x)

rate_taxpartsize <- function(x, taxpartsize = "taxpartsize", ...) {
  dplyr::case_match(x[["taxpartsize"]],
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
                            pattern = "R|Cr|Cd|kk|m",
                            hzdesgn = aqp::guessHzDesgnName(x, required = TRUE),
                            ...) {
  hzd <- aqp::horizonDepths(x)
  x$.dc <- minDepthOf(x, pattern = pattern, hzdesgn = hzdesgn, ...)[[hzd[1]]]
  res <- cut(x$.dc, c(0, breaks))
  factor(res, levels = levels(res), labels = names(breaks))
}

site(spc) <- estimatePSCS(spc)

# calculate soil depth
spc$dp <- minDepthOf(spc, pattern = "R|Cr|Cd|kk|m")[[aqp::horizonDepths(spc)[1]]]

# calculate some categorical ratings
spc$rd <- rate_depthclass(spc)
spc$rt <- rate_taxpartsize(spc)

# calculate some continuous quantities for PSCS
site(spc) <- site(mutate_profile(trunc(spc, spc$pscs_top, spc$pscs_bottom),
  pc = weighted.mean(clay, hzdepb - hzdept, na.rm=T),
  pf = weighted.mean(total_frags_pct, hzdepb - hzdept, na.rm=T),
  mc = suppressWarnings(max(clay, na.rm=T)),
  mf = suppressWarnings(max(total_frags_pct, na.rm=T)))
)[c("peiid", "pc", "pf", "mc", "mf")]

spc <- spc[which(complete.cases(subset(site(spc), select=c("rt", "rd", "pc", "pf", "dp", "mc", "mf")))),]

# set up data frames for various clustering models
x0 <- subset(site(spc), select = c("taxpartsize", "rd"))
x1 <- subset(site(spc), select = c("rt", "rd"))
x2 <- subset(site(spc), select = c("pc", "pf", "rd"))
x3 <- subset(site(spc), select = c("pc", "pf", "dp"))
x4 <- subset(site(spc), select = c("rt", "rd", "pc", "pf", "dp"))
x5 <- subset(site(spc), select = c("rt", "rd", "pc", "pf", "dp", "mc", "mf"))

.clusterfun_agnes <- \(y, spc) {
  rownames(y) <- profile_id(spc)
  y[] <- lapply(y, \(z) if (is.character(z) || (!is.numeric(z))) factor(z) else z)
  cluster::agnes(y[complete.cases(y),], method = "gaverage")
}

.clusterfun_diana <- \(y, spc) {
  # NB: code ordinal factors beforehand if needed
  y[] <- lapply(y, \(z) if (is.character(z) || (!is.numeric(z))) factor(z) else z)
  rownames(y) <- profile_id(spc)
  cluster::agnes(cluster::daisy(y, metric = "gower"), method = "gaverage")
}

.CLUSTERFUN <- .clusterfun_diana #.clusterfun_agnes

# the simplest model uses the particle size family and the depth class
m0 <- .CLUSTERFUN(x0, spc)
# plot(as.dendrogram(m0), ylim = c(0, 10))
sharpshootR::plotProfileDendrogram(spc, m0)

# if we transform the particle size family, we have the option to combine
# similar classes, and also increase the taxonomic distance between other classes.
# in this case, we code "fine-loamy" as a 4 and "loamy-skeletal" as a 7.
# the clustering is identical to the simplest model, but the taxonomic distance
# between the two main groups of pedons is now greater (3 versus 1)
m1 <- .CLUSTERFUN(x1, spc)
plot(as.dendrogram(m1), ylim = c(0, 10))
sharpshootR::plotProfileDendrogram(spc, m1)

# in the next most complex model we replace the taxonomic particle size class rating
# with the PSCS weighted average percent clay and the percent fragments.
#
# we find the same outgroup of three pedons we separated earlier, the magnitude of taxonomic
# distances (y axis) further increases, and we get a lot more subtle variation within
# that reflects more continuous variation in PSCS clay and fragments.
m2 <- .CLUSTERFUN(x2, spc)
plot(as.dendrogram(m2))
sharpshootR::plotProfileDendrogram(spc, m2)

# since most of the soils in the loafercreek dataset are moderately deep we do not
# get much information from the depth class grouping. instead, in the next model
# we replace the depth class with the actual depth to root-limiting layer
#
# we find again that the loamy-skeletal soils are separated, and some distances
# of groups within the fine-loamy soils shift, but ultimately there is little effect
# as the variation is still constrained within moderately deep depth class.
m3 <- .CLUSTERFUN(x3, spc)
plot(as.dendrogram(m3))
sharpshootR::plotProfileDendrogram(spc, m3)

# if we add the taxpartsize and depth class ratings back in with the continuous data
# we see that we get the exact same clustering as with the prior model (which included
# no categorical predictors). this indicates the categories are adequately covered by
# the numeric quantities we replaced them with.
m4 <- .CLUSTERFUN(x4, spc)
plot(as.dendrogram(m4))
sharpshootR::plotProfileDendrogram(spc, m4, print.id=FALSE, name=NULL, plot.depth.axis=FALSE, width = 0.5, color = "fragvoltot")

m5 <- .CLUSTERFUN(x5, spc)
plot(as.dendrogram(m5, ))
sharpshootR::plotProfileDendrogram(spc, m5, print.id=FALSE, name=NULL, plot.depth.axis=FALSE, width = 0.5, color = "fragvoltot")


site(spc)[, c("pedon_id", "taxpartsize", "rt", "dp", "rd", "pc", "pf", "mc", "mf")]

site(spc)[, c("pedon_id", "taxpartsize", "rt", "dp", "rd", "pc", "pf")]

