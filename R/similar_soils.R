#' Similar Soil Contrasts
#'
#' A method for applying standardized, customizable "similar soils" rules to
#' site-level data derived from various sources.
#'
#' @param x A _data.frame_ or a _SoilProfileCollection_
#' @param mapping A named _list_ of functions. List element names refer to
#'   site-level data columns of `x`. Each specified function provides a
#'   conversion of the data element in `x` to a value used in the "similar
#'   soils" calculation.
#' @param condition integer or character. Default: `NULL` the value is
#'   calculated internally based on the dominant condition of intersection of
#'   mapping results in `x`. You may specify an integer row ID in `x` for
#' specific similar soil contrasts, or you can specify a character dominant
#' condition equivalent to the name assigned by `interaction()` e.g. `"4.3"` for
#' a two rating mapping result where the first rating has value `4` and the
#' second has value `3`
#' @param idname ID column name, default `"id"`
#' @param thresh Deprecated. If used passed to `thresh_all`.
#' @param thresh_single Sum of differences relative to dominant condition in
#'   `x`. Default `1`. See details for discussion on the default calculation for
#'   similarity.
#' @param thresh_all Sum of differences relative to dominant condition in `x`.
#'   Default `1`. See details for discussion on the default calculation for
#'   similarity.
#' @param absolute logical. Report absolute difference? Default: `TRUE`.
#'   Absolute difference is always used for comparison against thresh.
#' @param verbose Default: `TRUE` message about selected `condition`
#' @details
#'
#' The sum of differences across conditions (specified by the intersection of
#' output of the functions in `mapping`) is used as the "distance" of a soil
#' relative to a dominant (or otherwise specified) condition. A threshold value
#' is used to decide which are "similar" and which are not. The functions in
#' mapping can be customized to use alternate thresholds.
#'
#' @return A _data.frame_ containing inputs and three new columns:
#'   `similar_single` (maximum difference in any one property, relative to
#'   `condition`), `similar_dist` (cumulative sum of differences relative to
#'   `condition`), `similar` (logical; soil is similar to `condition`)
#' @export
#'
#' @references Norfleet, M.L. and Eppinette, R.T. (1993), A Mathematical Model
#'   for Determining Similar and Contrasting Inclusions for Map Unit
#'   Descriptons. Soil Survey Horizons, 34: 4-5.
#'   <https://doi.org/10.2136/sh1993.1.0004>
#'
#' @examplesIf requireNamespace("dplyr", quietly=TRUE) && requireNamespace("cluster", quietly=TRUE)
#'
#' data(loamy, package = "SOILmilaR")
#'
#' rate_taxpartsize <- function(x) {
#'   dplyr::case_match(x,
#'                     c("sandy-skeletal") ~ 1,
#'                     c("sandy") ~ 3,
#'                     c("loamy", "coarse-loamy", "coarse-silty") ~ 5,
#'                     c("fine-loamy", "fine-silty") ~ 7,
#'                     c("clayey", "fine") ~ 9,
#'                     c("very-fine") ~ 11,
#'                     c("loamy-skeletal", "clayey-skeletal") ~ 13,
#'                     "fragmental" ~ 15)
#' }
#'
#'
#' rate_depthclass <- function(x, breaks = c( `very shallow` = 25, `shallow` =
#'   50, `moderately deep` = 100, `deep` = 150, `very deep` = 1e4 ), pattern =
#'   "R|Cr|Cd|kk|m", hzdesgn = aqp::guessHzDesgnName(x, required = TRUE),
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
#' m <- list(taxpartsize = rate_taxpartsize, depth = rate_depthclass,
#' pscs_clay = rate_pscs_clay)
#'
#' s <- similar_soils(loamy, m)
#' head(s)
#'
#' # inspect distances using agglomerative clustering+dendrogram
#' d <- cluster::agnes(s[, 5, drop = FALSE], method="gaverage")
#' d$height <- d$height + 0.2 # fudge factor for 0-distance
#' plot(stats::as.dendrogram(d), center=TRUE, type="triangle")
#'
#' # allow relative contrast ratings to be negative # (i.e. ordinal factors, concept of "limiting")
#' # absolute value is still used for "similar" threshold
#' s2 <- similar_soils(loamy, m, absolute=FALSE)
#'
#' # inspect distances using agglomerative clustering+dendrogram
#' d2 <- cluster::agnes(s2[, 5, drop = FALSE], method="gaverage")
#' d2$height <- d2$height + 0.2 # fudge factor for 0-distance
#' plot(stats::as.dendrogram(d2), center=TRUE, type="triangle")
similar_soils <- function(
   x,
   mapping,
   condition = NULL,
   idname = "id",
   thresh = NULL,
   thresh_single = 2,
   thresh_all = 3,
   absolute = TRUE,
   verbose = TRUE
 ) {

  if (!inherits(x, c("data.frame", "SoilProfileCollection"))) {
    stop("`x` must be a data.frame or SoilProfileCollection", call. = FALSE)
  }

  if (!missing(thresh)) {
    .Deprecated(msg = "`thresh` argument has been split into `thresh_single` and `thresh_all`, please use one or both instead. Passing `thresh_all=thresh` and `thresh_single=thresh` for backward compatibility.")
    thresh_all <- thresh
    thresh_single <- thresh
  }

  # currently only site-level attributes of a SoilProfileCollection are considered
  if (inherits(x, 'SoilProfileCollection') &&
      requireNamespace("aqp", quietly = TRUE)) {
    idname <- aqp::idname(x)
    x <- aqp::site(x)
  }

  # apply mapping to determine conditions from properties
  r <- .rate_fun(x, mapping)

  # calculate interaction of resulting conditions
  ir <- interaction(r)

  nr <- nrow(x)
  if (nr %in% c(0, 1)) {
    result <- r
    result <- cbind(result, data.frame(group = ir,
                                       similar_single = 0L,
                                       similar_all = 0L,
                                       similar = TRUE)[nr,])
    return(result)
  }

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
    .absfun(as.integer(ex[[y]]) - as.integer(en[[y]][1]))
  })

  r$similar_dist <- 0
  r$similar_single <- 0

  r$group <- ir
  r$similar_dist[which(lex)] <- rowSums(ex)
  r$similar_single[which(lex)] <- apply(ex, MARGIN = 1, max)
  r$similar <- ((abs(r$similar_dist) < thresh_all) & (abs(r$similar_single) < thresh_single))
  r <- data.frame(x[idname], r)

  r
}

.rate_fun <- function(x, mapping) {
  res <- lapply(names(mapping), \(y) {
    mapping[[y]](x[[y]])
  })
  names(res) <- names(mapping)
  as.data.frame(res)
}

