#' Design a Map Unit
#'
#' This function calls `similar_soils()` iteratively to estimate the proportions
#' of observations corresponding to distinct groups of similar soils. Use it
#' exhaustively group all observations (for example, within a map unit or other
#' conceptual unit of interest)
#'
#' @param x A data.frame or a SoilProfileCollection.
#' @param mapping A named list of rating functions.
#' @param idname The name of the ID column in `x`. Default: `"id"`.
#' @param component_labels A character vector of labels to assign to components.
#'   Default: `greekletters[[1]]`
#' @param ... Additional arguments passed on to `similar_soils()`, such as
#'   `threshold_single` or `threshold_all`.
#'
#' @return A data.frame containing the original data along with a new 'compname'
#'   column that assigns a map unit component label to each group of similar
#'   soils.
#'
#' @details This function automates the iterative process of calling
#'   `similar_soils()` to build out an estimate of map unit composition.
#'
#'   As a special case, if `threshold_single` and `threshold_all` are set to
#'   `0`, the function will use a more efficient method. It will directly group
#'   all soils that share the exact same combination of rated properties, as
#'   this is the logical outcome of a zero-distance threshold. The component
#'   labels are assigned based on the size of the groups, with the largest group
#'   being assigned the first label.
#'
#' @export
#' @examplesIf requireNamespace("dplyr")
#'
#' data("loamy", package = "SOILmilaR")
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
#' rate_depthclass <- function(x,
#'                             breaks = c(
#'                               `very shallow` = 25,
#'                               `shallow` = 50,
#'                               `moderately deep` = 100,
#'                               `deep` = 150,
#'                               `very deep` = 1e4
#'                             ),
#'                             ...) {
#'   res <- cut(x, c(0, breaks))
#'   factor(res, levels = levels(res), labels = names(breaks))
#' }
#'
#' m <- list(taxpartsize = rate_taxpartsize, depth = rate_depthclass)
#'
#' mapunit_composition <- design_mapunit(loamy, m)
#'
#' mapunit_composition[order(mapunit_composition$component), ]
#'
#' sort(prop.table(table(mapunit_composition$component)), decreasing = TRUE)
#'
design_mapunit <- function(x,
                           mapping,
                           idname = "id",
                           component_labels = SOILmilaR::greekletters[[1]],
                           ...) {

  if (inherits(x, 'SoilProfileCollection') &&
      requireNamespace("aqp")) {
    if (missing(idname)) {
      idname <- aqp::idname(x)
    }
    x <- aqp::site(x)[c(idname, names(mapping))]
  } else {
    x <- x[c(idname, names(mapping))]
  }

  args <- list(...)

  if (!is.null(args$threshold_single) &&
      !is.null(args$threshold_all) &&
      args$threshold_single == 0 &&
      args$threshold_all == 0) {

    rated_cols_list <- lapply(names(mapping), function(name) {
      rating_fun <- mapping[[name]]
      rating_fun(x[[name]])
    })
    names(rated_cols_list) <- names(mapping)

    grouping_factor <- interaction(rated_cols_list, drop = TRUE)
    sorted_group_names <- names(sort(table(grouping_factor), decreasing = TRUE))

    num_groups <- length(sorted_group_names)
    if (num_groups > length(component_labels)) {
      warning(paste(
        "Found", num_groups, "unique groups, but only",
        length(component_labels), "component_labels were provided. Only ",
        num_groups, " components will be identified!"
      ))
      # Truncate if necessary to avoid errors
      sorted_group_names <- sorted_group_names[seq_along(component_labels)]
    }

    component_lookup <- component_labels[seq_len(num_groups)]
    names(component_lookup) <- sorted_group_names

    result <- x
    result$component <- component_lookup[as.character(grouping_factor)]

    return(result)
  }

  remaining_data <- x
  component_list <- list()
  i <- 1

  while (nrow(remaining_data) > 0 && i <= length(component_labels)) {
    res <- similar_soils(remaining_data, mapping, idname = idname, verbose = FALSE, ...)

    similar_ids <- res[[idname]][res$similar]
    component_data <- subset(x, x[[idname]] %in% similar_ids)

    if (nrow(component_data) == 0) {
      component_data <- x
    }
    component_data$component <- component_labels[i]

    component_list[[i]] <- component_data

    remaining_ids <- res[[idname]][!res$similar]
    remaining_data <- subset(x, x[[idname]] %in% remaining_ids)
    i <- i + 1
  }

  final_result <- do.call("rbind", component_list)
  final_result <- final_result[match(x[[idname]], final_result[[idname]]), ]

  if (nrow(final_result) != nrow(x)) {
    warning("Could not classify all soils; consider providing more component_labels.")
  }

  return(final_result)
}
