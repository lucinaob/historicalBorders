#' Map Countries
#'
#' @description
#' Map a certain country's historic borders.
#'
#' @details
#' This function will generate a map of any country for any year since 1800 that the user
#' inputs. If a given year is not in the database, the function will show a map for the closest year via
#' rounding. A user can also provide additional data to be mapped over these borders, in which case
#' they must also provide an aesthetic specification for which variable(s) will be mapped and how.
#' The function will return a ggplot2 object, from which a user can further customize their output.
#'
#' Note that country's names have changed over time, and referring to an area as it's modern
#' name will not always return the desired outcome For example, calling mapCountry(1800, "Germany")
#' will return a map of Germany from 1880, as the modern country of Germany did not exist at the
#' turn of the 19th century.
#'
#' @param year The year the user wants to see reflected by borders.
#' @param country The country a user wishes to map
#' @param dataset Additional data to map over historical borders.
#' @param aesthetics The aesthetic features (ex. fill, color) to map onto the borders
#'
#' @return A map of a country at a given time.
#' @export
#'
#' @examples
#' mapCountry(1880, "Germany")
#' mapCountry(1875, "Zambia")
#'

mapCountry <- function(year, country, dataset = NULL, aesthetics = NULL) {

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

  if(length(country) > 1){
    stop("You can only map one country at a time. You can map across regions using mapCustom()")
  }

  data <- historicalBorders::world

  if (!country %in% data$NAME) {
    stop(paste0("Country '", country, "' not found in dataset."))
  }

  country_years <- unique(data$year[data$NAME == country])

  if (length(country_years) == 0) {
    stop("This country has no geometry in the dataset at any year.")
  }

  target_year <- if (year %in% country_years) {
    year
  } else {
    closest <- country_years[which.min(abs(country_years - year))]
    message("No data for ", country, " in ", year,
            ". Using closest available year: ", closest)
    closest
  }

  data_sub <- data %>% dplyr::filter(data$NAME == country, year == target_year)
  data_sub <- data_sub[!st_is_empty(data_sub$geometry), ]

  if (nrow(data_sub) == 0) {
    stop("Geometry for ", country, " in ", target_year, " is empty or invalid.")
  }


  if(!is.null(dataset)){
    data_sub <- merge(data_sub, dataset, key = "NAME", all.x = TRUE)
    data_sub |>
      ggplot(aesthetics) +
      geom_sf() +
      coord_sf(lims_method = "geometry_bbox") +
      theme_void() +
      ggtitle(paste0(country, " in ", target_year))
  } else{
    data_sub |>
      ggplot() +
      geom_sf() +
      theme_void() +
      coord_sf(lims_method = "geometry_bbox")  +
      ggtitle(paste0(country, " in ", target_year))
  }

}
