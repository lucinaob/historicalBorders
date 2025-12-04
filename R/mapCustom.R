#' Custom Map
#'
#' @description
#' Generates a map of a custom area at a given year since 1800. Area to be mapped can be specified
#' using coordinate pairs to form a rectangle around a certain area of the globe.
#'
#'
#' @param year The year the user wants to see reflected by borders.
#' @param lat The latitude of the area the user wants to map.
#' @param long The longitude of the area the user wants to map.
#' @param dataset The dataset of historical borders.
#'
#' @return A custom map of the latitude and longitudes specified by R.
#' @export
#'
#' @examples
#' mapCustom(1800, c(43.29, 57.10), c(74.21, 100.7))
#' mapCustom(1820, c(48.85, 55.5), c(2.35, 10.8))
#'
mapCustom <- function(year, lat, long, dataset = NULL){

  data <- historicalBorders::world

  if(length(lat) != 2 || length(long) != 2){
    stop("Please enter 2 pairs of latitude and longitude values!")
  }

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
    print("We will map geographic data here!")
  } else{
    data |>
      ggplot() +
      geom_sf() +
      theme_void() +
      ylim(long) +
      xlim(lat)
  }

}
