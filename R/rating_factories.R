#' Create a Rating Function to "Match" Categorical Data
#'
#' This is a "function factory" that creates a new rating function based on a
#' mapping of input categories to desired output factor levels. I employs
#' similar logic to `dplyr::case_match()` within a rating function, but ensures
#' an ordered factor with informatie labels is returned.
#'
#' @param mapping A named list. The names of the list elements will become the
#'   levels of the output factor. Each element of the list should be a character
#'   vector containing the input values to be mapped to that name.
#' @param values integer. A vector of values to represent each element of
#'   mapping. The default `seq_along(mapping)` results in a distance of `1`
#'   between each rating level.
#' @param unmatched_label The label to use for values in the input data that do
#'   not match any of the categories in the `mapping` list. Default is `NA`.
#'
#' @return A new function that takes a single argument (a vector of data) and
#'   returns a factor with levels ordered according to the `mapping` list.
#'
#' @export
#'
#' @examples
#' # Define a mapping for soil texture classes
#' texture_map <- list(
#'   "Coarse" = c("sand", "loamy sand", "sandy loam"),
#'   "Medium" = c("loam", "silt loam", "silt"),
#'   "Fine" = c("clay loam", "sandy clay loam", "silty clay loam", "clay")
#' )
#'
#' # Create the rating function
#' rate_texture <- rating_factory_match(texture_map, values = c(2, 4, 10))
#'
#' # Use the new function
#' soil_textures <- c("loam", "sand", "clay", "silt", "unknown")
#' res <- rate_texture(soil_textures)
#'
#' res
#' str(res)
rating_factory_match <- function(mapping, values = seq_along(mapping), unmatched_label = NA_character_) {
  rate_function <- function(x) {

    output <- rep(NA_integer_, length(x))

    for (i in seq_along(mapping)) {
      categories_to_match <- mapping[[i]]
      matches <- x %in% categories_to_match
      output[matches] <- values[i]
    }

    res <- factor(output, ordered = TRUE)
    names(res) <- names(mapping)
    return(res)
  }

  return(rate_function)
}

#' Create a Rating Function to "Cut" Continuous Data
#'
#' This is a "function factory" that creates a new rating function that bins
#' a continuous variable into an ordered factor. It serves as a user-friendly
#' wrapper around `base::cut()`.
#'
#' @param breaks A numeric vector of two or more unique cut points.
#' @param labels A character vector of labels for the intervals. The number of
#'   labels must be one less than the number of breaks.
#' @param ... Additional arguments passed on to `base::cut()`, such as
#'   `right = FALSE` or `include.lowest = TRUE`.
#'
#' @return A new function that takes a single argument (a numeric vector) and
#'   returns an ordered factor.
#'
#' @export
#'
#' @examples
#' # Define breaks and labels for soil depth
#' depth_breaks <- c(0, 25, 50, 100, 150)
#' depth_labels <- c("Very Shallow", "Shallow", "Moderately Deep", "Deep")
#'
#' # Create the rating function built around base::cut()
#' rate_depth <- rating_factory_cut(depth_breaks, depth_labels)
#'
#' # Use the new function
#' soil_depths <- c(10, 45, 90, 120, 200)
#' rate_depth(soil_depths)
rating_factory_cut <- function(breaks, labels, ...) {

  # Check that the number of labels is correct
  if (length(labels) != length(breaks) - 1) {
    stop("The number of 'labels' must be one less than the number of 'breaks'.")
  }

  rate_function <- function(x) {
    cut(x, breaks = breaks, labels = labels, ordered_result = TRUE, ...)
  }

  return(rate_function)
}
