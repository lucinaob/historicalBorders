#' Available Regions
#'
#' @param continent Optional, filters available regions to a single continent
#'
#' @returns A data frame of available regions
#'
#' @examples
#'
#' options_regions()
#' options_regions("Africa)
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

  View(regions)
}
