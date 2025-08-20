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
#'
#' loafercreek$pscs_clay
pscs_weighted <- function(x, vars, na.rm = FALSE,
                          pscsbounds = c("psctopdepth", "pscbotdepth"),
                          prefix = "pscs_") {
  if (!requireNamespace("aqp"))
    stop("package 'aqp' is required")
  x <- trunc(aqp::reduceSPC(x, vars), x[[pscsbounds[1]]], x[[pscsbounds[2]]])
  h <- data.table::data.table(aqp::horizons(x))
  idn <- aqp::idname(x)
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
  .spcdf(res, aqp::aqp_df_class(x))
}

.spcdf <- function (x, as.class = "data.frame", ...)
{
  if (class(x)[1] == "NULL")
    stop(sprintf("input object is NULL, expected '%s'", as.class))
  if (!inherits(x, "data.frame")) {
    stop(sprintf("input data class %s does not inherit from `data.frame`",
                 class(x)[1]), call. = TRUE)
  }
  cond <- class(x)[1] == as.class
  test <- all(length(cond) > 0 & cond)
  if (is.null(test) | is.na(test)) {
    as.class <- "data.frame"
    message("missing metadata for aqp_df_class -- run aqp::rebuildSPC(object) to fix slots and metadata")
  }
  else if (test) {
    rownames(x) <- NULL
    return(x)
  }
  switch(as.class, data.table = {
    if (requireNamespace("data.table", quietly = TRUE)) return(data.table::as.data.table(x,
                                                                                         ...))
    message("using data.table class in SoilProfileCollection slots requires the `data.table` package")
  },
  ## NB: tibble support removed, see aqp:::.as.data.frame.aqp
  {
    if (as.class != "data.frame") {
      message(sprintf("failed to use %s as data.frame class",
                      as.class))
      aqp::metadata(x)$aqp_df_class <- "data.frame"
      warning("data.table and tbl_df in SoilProfileCollection data.frame slots are EXPERIMENTAL, defaulting to data.frame",
              call. = FALSE)
    }
    res <- as.data.frame(x, ...)
    rownames(res) <- NULL
    return(res)
  })
}
