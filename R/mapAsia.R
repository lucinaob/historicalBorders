#' Asia Map
#'
#' @description
#' Generates a map of Asia at a given year since 1800.
#'
#'
#' @param year The year the user wants to see reflected by borders.
#' @param dataset Additional data to map over historical borders.
#' @param aesthetics The aesthetic features (ex. fill, color) to map onto the borders
#'
#' @return This function will return a map of Asia at a given time period.
#'
#'
#' @examples
#' mapAsia(1820)
#' mapAsia(1900, dataset = population1900, aesthetics = aes(fill = POP))
#'
#' @export
mapAsia <- function(year, dataset = NULL, aesthetics = NULL){

  if(!is.numeric(year)){
    stop("Year must be numeric (ex. 1800, 1925)")
  }

  if(length(year) > 1){
    stop("Only one year can be mapped at a time.")
  }

  if(!is.null(aesthetics)){
    if(is.null(dataset)){
      stop("You must specify an additional dataset to specify aesthetics")
    }
    if(!startsWith(aesthetics, "aes")){
      stop("Aesthetics must be wrapped in aes() \n
           ex. aesthetics = aes(fill = POP)")
    }
  }

  if(!is.null(dataset)){
    if(is.null(aesthetics)){
      stop("To visualize additional data, you must specify an aesthetic mapping")
    }
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
      ylim(-12, 80) +
      xlim(30, 180) +
      theme_void()
  } else{
    data |>
      ggplot() +
      geom_sf() +
      theme_void() +
      ylim(-12, 80) +
      xlim(30, 180)
  }

}
