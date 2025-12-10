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
#' @param dataset Additional data to map over historical borders.
#' @param aesthetics The aesthetic features (ex. fill, color) to map onto the borders
#'
#' @return A custom map of the latitude and longitudes specified by R.
#' @export
#'
#' @examples
#' mapCustom(1800, c(19.5, 45.4), c(25.3, 41.2))
#' mapCustom(1900, c(62.7, 130.6), c(-3.3, 44.2), population1900, aes(fill = POP))
#'
mapCustom <- function(year, lat, long, dataset = NULL, aesthetics = NULL){

  if(length(lat) != 2 || length(long) != 2){
    stop("Please enter 2 pairs of latitude and longitude values!")
  }

  if(lat[2] < lat[1]){
    lat <- rev(lat)
  }

  if(long[2] < long[1]){
    long <- rev(long)
  }

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
      ylim(long) +
      xlim(lat) +
      theme_void()
  } else{
    data |>
      ggplot() +
      geom_sf() +
      theme_void() +
      ylim(long) +
      xlim(lat)
  }

}
