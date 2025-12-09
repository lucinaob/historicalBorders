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
#' @param dataset Additional data to map over historical borders.
#' @param aesthetics The aesthetic features (ex. fill, color) to map onto the borders
#'
#' @returns A world map ggplot object, with borders dated to a given year
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 theme_void
#' @importFrom sf st_read
#'
#' @examples
#' mapWorld(1800)
#' mapWorld(1900, dataset = population1900, aesthetics = aes(fill = POP))
#'
#' @export
mapWorld <- function(year, dataset = NULL, aesthetics = NULL){

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
    data <- merge(data, dataset, key = "NAME", all.x = TRUE)
    data |>
      ggplot(aesthetics) +
      geom_sf() +
      theme_void()
  } else{
    data |>
      ggplot() + geom_sf() + theme_void()
  }
}
