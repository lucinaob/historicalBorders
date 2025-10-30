#' Historic Map
#'
#'@description
#'A function to map historic states and borders.
#'
#'@details
#'This function will map a historic country (or historic borders of a modern country) included
#'in the historicalBorders package. Country names must match the names in the dataset,
#'and if a year is specified that is not in the data set, the function will map the borders
#'at the closest year on record. Additional data can be passed to visualize over this map, such as population or development data.
#'
#'
#' @param country  The country that the user wants to visualize
#' @param year  The year the user wants to see reflected by borders
#' @param data  The dataset of historical borders
#' @param ...  Included for flexibility
#'
#' @returns A map of a historic territory's borders at a given year.
#'
#' @examples
#' map("USA", 1945)
#'
#'
#' @export
map <- function(country, year, data = historicData, ...){

  country_name <- country #tolower(country)

  if(!(country_name %in% data$name)){
    stop(paste0("No country named ", country, " found in the data. Did you spell everything correctly?"))
  }

  mapOptions <- data[data$name == country_name, ]

  if(!(year %in% mapOptions$year)){
    message(paste0("Map dated to ", year, " not found. Using closest year in dataset."))
    differences <- abs(mapOptions$year - year)
    closest_index <- which.min(differences)
    year <- mapOptions$year[closest_index]
    message("Using map dated to ", year)
  }

  to_map <- mapOptions[mapOptions$year == year, ]

  #Below this is all filler - we will make an actual map
  #of the given country in a given year,
  #but currently don't have the data to do this.
  #Instead, function will return data frame
  map <- to_map

  return(map)
}

