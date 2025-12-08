#' Map Countries
#'
#' @description
#' Map a certain country's historic borders.
#'
#' @details
#' Note that country's names have changed over time, and referring to an area as it's modern
#' name will not always return the desired outcome For example, calling mapCountry(1800, "Germany")
#' will return a map of Germany from 1880, as the modern country of Germany did not exist at the
#' turn of the 19th century.
#'
#' @param year The year the user wants to see reflected by borders.
#' @param country The country a user wishes to map
#'
#' @return A map of a country at a given time.
#' @export
#'
#' @examples
#' mapCountry(1880, "Germany")
#' mapCountry(1875, "Zambia")
#'

mapCountry <- function(year, country, dataset = NULL, aesthetics = NULL) {

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
