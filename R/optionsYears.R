#' Available Years
#'
#' Returns all years for which historical maps are available within the `historicalBorders` package.
#' Calling a year from outside these options in a `historicalBorders` function will lead to rounding:
#' if a user calls
#'
#' @returns
#'
#' @examples
#' @export
optionsYears <- function(){
  data <- historicalBorders::world

  options <- unique(data$year) |>
    sort()

  message("You can map borders of the world from the following years:")
  print(options)
}
