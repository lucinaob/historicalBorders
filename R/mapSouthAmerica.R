#' South America Map
#'
#' @description
#' Generates a map of South America at a given year since 1800.
#'
#' @details
#' This function will generate a map of the African continent for any year since 1800 that the user
#' inputs. If a given year is not in the database, the function will show a map for the closest year via
#' rounding. A user can also provide additional data to be mapped over these borders, in which case
#' they must also provide an aesthetic specification for which variables(s) will be mapped and how.
#' The function will return a ggplot2 object, from which a user can further customize their output.
#'
#' @param year The year the user wants to see reflected by borders.
#' @param dataset Additional data to map over historical borders.
#' @param aesthetics The aesthetic features (ex. fill, color) to map onto the borders
#'
#' @return This function will return a map of South America at a given time period.
#' @export
#'
#' @examples
#' mapSouthAmerica(1827)
#' mapSouthAmerica(1900, dataset = population1900, aesthetics = aes(fill = POP))
mapSouthAmerica <- function(year, dataset = NULL, aesthetics = NULL){

  if(!is.numeric(year)){
    stop("Year must be numeric (ex. 1800, 1925)")
  }

  if(length(year) > 1){
    stop("Only one year can be mapped at a time.")
  }

  if(!is.null(aesthetics) && is.null(dataset)){
    stop("You must specify an additional dataset to specify aesthetics")
  }

  if(!is.null(dataset) && is.null(aesthetics)){
    stop("To visualize additional data, you must specify an aesthetic mapping")
  }

  data <- historicalBorders::world

  options <- unique(data$year)

  if(!year %in% options){
    message(paste0("Map dated to ", year, " not found. Using closest year in dataset."))
    differences <- abs(options - year)
    closest_year <- which.min(differences)
    year <- options[closest_year]
    message("Using map dated to ", year)
  }

  data <- data[data$year == year, ]

  if(!is.null(dataset)){
    data <- merge(data, dataset, key = "NAME", all.x = TRUE)
    data |>
      ggplot(aesthetics) +
      geom_sf() +
      ylim(-55, 10) +
      xlim(-83, -35) +
      theme_void()
  } else{
    data |>
      ggplot() +
      geom_sf() +
      theme_void() +
      ylim(-55, 10) +
      xlim(-83, -35)
  }

}
