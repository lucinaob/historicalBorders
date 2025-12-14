#' Available Regions
#'
#' @description
#' Prints regions that can be mapped using mapRegion(), along with the modern countries within these regions.
#'
#' @details
#' Regions are defined using the United Nations' geoscheme designations. If a user wishes to map a region
#' that is not available here, they can use the mapCustom function.
#'
#'
#' @param continent Optional, filters available regions to a single continent
#'
#' @returns A data frame of available regions
#'
#' @examples
#' optionsRegions()
#' optionsRegions("Africa")
#'
#' @export
optionsRegions <- function(continent = NULL){
  regions = historicalBorders::regions

  options = c("Africa", "Americas", "Asia", "Europe", "Oceania")

  if(!is.null(continent)){
    if(!(continent %in% options)){
      stop(paste0("Please input one of the following continents:\nAfrica\nAmericas\nAsia\nEurope\nOceania"))
    } else{
      regions <- regions[regions$CONTINENT == continent, ]
    }

  }

  print(regions)
}
