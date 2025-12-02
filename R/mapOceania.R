#' Title
#'
#' @param year The year the user wants to see reflected by borders.
#' @param dataset The dataset of historical borders.
#'
#' @return This function will return a map of Oceania at a given time period.
#' @export
#'
#' @examples
#' mapOceania(1825)
#' mapOceania(1875)
mapOceania <- function(year, dataset = NULL){

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
    print("We will map geographic data here!")
  } else{
    data |>
      ggplot() +
      geom_sf() +
      theme_void() +
      ylim(-45, -4) +
      xlim(110, 180)
  }

}
