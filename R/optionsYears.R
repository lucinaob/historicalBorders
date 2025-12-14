#' Available Years
#'
#' @description
#' Returns all years for which historical maps are available within the historicalBorders package.
#'
#' @details
#' If a user tries to visualize a map outside of these options, historicalBorders functions will round
#' to the closest year given by this function.
#'
#' @returns List of years for which maps are available.
#'
#' @examples
#' optionsYears()
#'
#' @export
optionsYears <- function(){
  data <- historicalBorders::world

  options <- unique(data$year) |>
    sort()

  message("You can map borders of the world from the following years:")
  print(options)
}
