#' Calculate Particle Size Control Section Weighted Average
#'
#' Calculates the particle size control section weighted average for one or more variables (`vars`), using upper and lower boundary specified in `pscsbounds`. Profiles are truncated to the interval set by these bounds, and weighted averages are calculated for each column. The result has the same number of rows as the number of sites in the input SoilProfileCollection `x` and is suitable for joining back into the site table
#'
#' @param x A _SoilProfileCollection_
#' @param vars _character_. Column names of numeric variables in horizon table of `x` to summarize.
#' @param na.rm _logical_. Remove `NA`? Default: `FALSE`
#' @param pscsbounds _character_ of length 2. Column names of numeric variables in site table representing the profile specific upper and lower boundary. Default: `"psctopdepth"` and `"pscbotdepth"`
#' @param prefix _character_. Prefix to append before column name, to create unique name that does not conflict with existing horizon data. Default: `"pscs_"`
#'
#' @return A data.frame-like object corresponding to `aqp_df_class(x)`
#' @export
#'
#' @examplesIf !inherits(try(requireNamespace("aqp", quietly = TRUE) && requireNamespace("soilDB", quietly = TRUE), silent = TRUE), 'try-error')
#'
#' # load aqp, and data from soilDB
#' library(aqp)
#' data(loafercreek, package = "soilDB")
#'
#' # calculate PSCS weighted clay % and total fragment volume
#' pscs_cf <- pscs_weighted(loafercreek, c("clay", "fragvoltot"))
#'
#' # inspect
#' pscs_cf
#'
#' # left join to site table of loafercreek
#' site(loafercreek) <- pscs_cf
pscs_weighted <- function(x, vars, na.rm = FALSE,
                          pscsbounds = c("psctopdepth", "pscbotdepth"),
                          prefix = "pscs_") {
  if (!requireNamespace("aqp"))
    stop("package 'aqp' is required")
  x <- trunc(aqp::reduceSPC(x, vars), x[[pscsbounds[1]]], x[[pscsbounds[2]]])
  h <- data.table::data.table(horizons(x))
  idn <- idname(x)
  l <- list(h[[idn]])
  names(l) <- idn
  .summaryFUN <- function(y) {
    data.frame(lapply(which(sapply(y, is.numeric))[-c(1:2)], function(v) {
      weighted.mean(y[[v]], y[[1]] - y[[2]], na.rm = na.rm)
    }))
  }
  res <- h[, .summaryFUN(.SD), by = l, .SDcols = c(aqp::horizonDepths(x), vars)]
  vi <- which(colnames(res) %in% vars)
  colnames(res)[vi] <- paste0(prefix, colnames(res)[vi])
  aqp:::.as.data.frame.aqp(res, aqp::aqp_df_class(x))
}
