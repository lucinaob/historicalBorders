#' World Map
#'
#' @description
#' Map the world with historic borders.
#'
#' @details
#' This function will generate a historic map of the world at a given point in time.
#' Users can input additional data sets that they wish to map onto the historic map.
#' If users try to map a year that is not in the existing data, the world map closest to
#' that year will be mapped instead.
#'
#'
#' @param year The year the user wants to see reflected by borders.
#' @param dataset The optional additional dataset to graph onto a map
#'
#' @returns A world map ggplot object, with borders dated to a given year
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 theme_void
#' @importFrom sf st_read
#'
#' @examples
#' mapworld(1800)
#' mapworld(1875)
#'
#' @export
mapworld <- function(year, dataset = NULL){

  if(!is.numeric(year)){
    stop("You must supply a numeric year")
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
    print("We will map geographic data here!")
  } else{
    data |>
      ggplot() + geom_sf() + theme_void()
  }
}
