% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/similar_soils.R
\name{similar_soils}
\alias{similar_soils}
\title{Similar Soil Contrasts}
\usage{
similar_soils(
  x,
  mapping,
  condition = NULL,
  idname = "id",
  thresh = 1L,
  absolute = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{x}{A \emph{data.frame} or a \emph{SoilProfileCollection}}

\item{mapping}{A named \emph{list} of functions. List element names refer to site-level data columns of \code{x}. Each specified function provides a conversion of the data element in \code{x} to a value used in the "similar soils" calculation.}

\item{condition}{integer or character. Default: \code{NULL} the value is calculated internally based on the dominant condition of intersection of mapping results in \code{x}. You may specify an integer row ID in \code{x} for specific similar soil contrasts, or you can specify a character dominant condition equivalent to the name assigned by \code{interaction()} e.g. \code{"4.3"} for a two rating mapping result where the first rating has value \code{4} and the second has value \code{3}}

\item{idname}{ID column name, default \code{"id"}}

\item{thresh}{Sum of differences relative to dominant condition in \code{x}. Default \code{1}. See details for discussion on the default calculation for similarity.}

\item{absolute}{logical. Report absolute difference? Default: \code{TRUE}. Absolute difference is always used for comparison against thresh.}

\item{verbose}{Default: \code{TRUE} message about selected \code{condition}}
}
\value{
A \emph{data.frame} containing inputs and two new columns: \code{similar_dist} (cumulative sum of differences relative to dominant condition), \code{similar} (logical)
}
\description{
A method for applying standardized, customizable "similar soils" rules to site-level data derived from various sources.
}
\details{
The sum of differences across conditions (specified by the intersection of output of the functions in \code{mapping}) is used as the "distance" of a soil relative to a dominant (or otherwise specified) condition. A threshold value is used to decide which are "similar" and which are not. The functions in mapping can be customized to use alternate thresholds.
}
\examples{
\dontshow{if (!inherits(try(requireNamespace("aqp", quietly=TRUE) && requireNamespace("dplyr", quietly=TRUE) && requireNamespace("cluster", quietly=TRUE), silent=TRUE), 'try-error')) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
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
                            pattern = "R|Cr|Cd|kk|m",
                            hzdesgn = aqp::guessHzDesgnName(x, required = TRUE),
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
head(s)

# inspect distances using agglomerative clustering+dendrogram
d <- cluster::agnes(s[, 5, drop = FALSE], method="gaverage")
d$height <- d$height + 0.2 # fudge factor for 0-distance
plot(stats::as.dendrogram(d), center=TRUE, type="triangle")

# allow relative contrast ratings to be negative
# (i.e. ordinal factors, concept of "limiting")
# absolute value is still used for "similar" threshold
s2 <- similar_soils(x, m, absolute=FALSE)

# inspect distances unsing agglomerative clustering+dendrogram
d2 <- cluster::agnes(s2[, 5, drop = FALSE], method="gaverage")
d2$height <- d2$height + 0.2 # fudge factor for 0-distance
plot(stats::as.dendrogram(d2), center=TRUE, type="triangle")
\dontshow{\}) # examplesIf}
}
\references{
Norfleet, M.L. and Eppinette, R.T. (1993), A Mathematical Model for Determining Similar and Contrasting Inclusions for Map Unit Descriptons. Soil Survey Horizons, 34: 4-5. \url{https://doi.org/10.2136/sh1993.1.0004}
}
