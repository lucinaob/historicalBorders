#' Africa Map
#'
#' @description
#' Generates a map of Africa at a given year since 1800.
#'
#' @param year The year the user wants to see reflected by borders
#' @param dataset The dataset of historical borders.
#'
#' @return This function will generate a historic map ggplot object of Africa at a given time period.
#' @export
#'
#' @importFrom ggplot2 ggplot geom_sf theme_void ylim xlim
#'
#' @examples
#' mapAfrica(1800)
#' mapAfrica(1850)
mapAfrica <- function(year, dataset = NULL, aesthetics = NULL){

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
      ylim(-35, 35) +
      xlim(-20, 52) +
      theme_void()
  } else{
    data |>
      ggplot() +
      geom_sf() +
      theme_void() +
      ylim(-35, 35) +
      xlim(-20, 52)
  }

}
