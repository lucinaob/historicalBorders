#' North America Map
#'
#' @description
#' Generates a map of North America at a given year since 1800.
#'
#' @param year The year the user wants to see reflected by borders.
#' @param dataset Additional data to map over historical borders.
#' @param aesthetics The aesthetic features (ex. fill, color) to map onto the borders
#'
#' @returns This function will return a map of North American at a given time period.
#'
#' @examples
#' mapNorthAmerica(1800)
#' mapNorthAmerica(1900, dataset = population1900, aesthetics = aes(fill = POP))
#'
#' @export
mapNorthAmerica <- function(year, dataset = NULL, aesthetics = NULL){

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
      ylim(7, 90) +
      xlim(-170, -60) +
      theme_void()
  } else{
    data |>
      ggplot() +
      geom_sf() +
      theme_void() +
      ylim(7, 90) +
      xlim(-170, -60)
  }

}
