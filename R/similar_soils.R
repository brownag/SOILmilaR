#' Similar Soil Contrasts
#'
#' A method for applying standardized, customizable "similar soils" rules to site-level data derived from various sources.
#'
#' @param x A _data.frame_ or a _SoilProfileCollection_
#' @param mapping A named _list_ of functions. List element names refer to site-level data columns of `x`. Each specified function provides a conversion of the data element in `x` to a value used in the "similar soils" calculation.
#' @param condition integer or character. Default: `NULL` the value is calculated internally based on the dominant condition of intersection of mapping results in `x`. You may specify an integer row ID in `x` for specific similar soil contrasts, or you can specify a character dominant condition equivalent to the name assigned by `interaction()` e.g. `"4.3"` for a two rating mapping result where the first rating has value `4` and the second has value `3`
#' @param idname ID column name, default `"id"`
#' @param thresh Sum of differences relative to dominant condition in `x`. Default `1`. See details for discussion on the default calculation for similarity.
#' @param absolute logical. Report absolute difference? Default: `TRUE`. Absolute difference is always used for comparison against thresh.
#' @param verbose Default: `TRUE` message about selected `condition`
#' @details
#'
#' The sum of differences across conditions (specified by the intersection of output of the functions in `mapping`) is used as the "distance" of a soil relative to a dominant (or otherwise specified) condition. A threshold value is used to decide which are "similar" and which are not. The functions in mapping can be customized to use alternate thresholds.
#'
#' @return A _data.frame_ containing inputs and two new columns: `similar_dist` (cumulative sum of differences relative to dominant condition), `similar` (logical)
#' @export
#'
#' @examples
#' set.seed(456)
#'
#' x <- do.call('rbind', lapply(1:3, \(i) data.frame(id = paste0(LETTERS[1:10], i),
#'   taxpartsize = c("fine-loamy", "fine-loamy", "fine-loamy", "fine-loamy",
#'                   "coarse-loamy", "coarse-loamy", "coarse-loamy", "loamy-skeletal",
#'                   "loamy-skeletal", "loamy-skeletal"),
#'   depth = runif(10, 35, 150),
#'   pscs_clay = c(runif(4, 18, 35), runif(6, 14, 18)),
#'   pscs_frags = c(runif(3, 0, 15), runif(4, 10, 34),
#'                  runif(3, 35, 60) + c(0, 40, 0)))))
#'
#' rate_taxpartsize <- function(x) {
#'   # TODO: this is just made up logic for this example and needs to be updated
#'   dplyr::case_match(x,
#'                     c("sandy-skeletal") ~ 1,
#'                     c("sandy") ~ 2,
#'                     c("loamy", "coarse-loamy", "coarse-silty") ~ 3,
#'                     c("fine-loamy", "fine-silty") ~ 4,
#'                     c("clayey", "fine") ~ 5,
#'                     c("very-fine") ~ 6,
#'                     c("loamy-skeletal", "clayey-skeletal") ~ 7)
#' }
#'
#' rate_depthclass <- function(x,
#'                             breaks = c(
#'                               `very shallow` = 25,
#'                               `shallow` = 50,
#'                               `moderately deep` = 100,
#'                               `deep` = 150,
#'                               `very deep` = 1e4
#'                             ),
#'                             pattern = "R|Cr|Cd|kk|m",
#'                             hzdesgn = aqp::guessHzDesgnName(x, required = TRUE),
#'                             ...) {
#'   res <- cut(x, c(0, breaks))
#'   factor(res, levels = levels(res), labels = names(breaks), ordered = TRUE)
#' }
#'
#' rate_pscs_clay <- function(x,
#'                            breaks = c(18, 27, 40, 60, 100)) {
#'   res <- cut(x, c(0, breaks))
#'   factor(res, levels = levels(res), ordered = TRUE)
#' }
#'
#' m <- list(taxpartsize = rate_taxpartsize,
#'           depth = rate_depthclass,
#'           pscs_clay = rate_pscs_clay)
#'
#' s <- similar_soils(x, m)
#' head(s)
#'
#' # inspect distances using agglomerative clustering+dendrogram
#' d <- cluster::agnes(s[, 5, drop = FALSE], method="gaverage")
#' d$height <- d$height + 0.2 # fudge factor for 0-distance
#' plot(stats::as.dendrogram(d), center=TRUE, type="triangle")
#'
#' # allow relative contrast ratings to be negative
#' # (i.e. ordinal factors, concept of "limiting")
#' # absolute value is still used for "similar" threshold
#' s2 <- similar_soils(x, m, absolute=FALSE)
#'
#' # inspect distances unsing agglomerative clustering+dendrogram
#' d2 <- cluster::agnes(s2[, 5, drop = FALSE], method="gaverage")
#' d2$height <- d2$height + 0.2 # fudge factor for 0-distance
#' plot(stats::as.dendrogram(d2), center=TRUE, type="triangle")
#'
#' @importFrom aqp site
#' @importClassesFrom aqp SoilProfileCollection
similar_soils <- function(
   x,
   mapping,
   condition = NULL,
   idname = "id",
   thresh = 1L,
   absolute = TRUE,
   verbose = TRUE
 ) {

  # currently only site-level attributes of a SoilProfileCollection are considered
  if (inherits(x, 'SoilProfileCollection'))
    x <- aqp::site(x)

  # apply mapping to determine conditions from properties
  r <- .rate_fun(x, mapping)

  # calculate interaction of resulting conditions
  ir <- interaction(r)

  # use the dominant condition unless otherwise specified
  if (is.null(condition)) {
    dc <- names(which.max(table(ir)))
  } else if (is.character(condition)) {
    dc <- condition
  } else {
    dc <- as.character(ir[condition])
  }

  # calculate the non-dominant conditions and indices of the dominant condition
  lex <- (ir != dc)
  ien <- which(!lex)

  # message about what we are comparing against
  if (verbose) {
    message("comparing to dominant reference condition (`",
            dc,
            "` on ",
            length(ien),
            " rows)")#, paste0(ien, collapse = ", "), ")")
  }

  # subset the rated input data into the "in" and "out" group
  #  in group is the dominant reference condition
  #  out group is everything else
  ex <- r[which(lex), ]
  en <- r[ien, ]

  # iterate over each rating, calculate the absolute* difference
  #  (*) for "limiting" determination may not take the absolute value
  .absfun <- abs
  if (!absolute)
    .absfun <- function(x) x

  ex[] <- lapply(seq_along(names(ex)), function(y) {
    .absfun(ex[[y]] - en[[y]][1])
  })

  r$similar_dist <- 0
  r$similar_dist[lex] <- rowSums(ex)
  r$similar <- abs(r$similar_dist) <= thresh
  r <- data.frame(x[idname], r)

  r
}

.rate_fun <- function(x, mapping) {
  as.data.frame(sapply(names(mapping), \(y) {
    mapping[[y]](x[[y]])
  }))
}

