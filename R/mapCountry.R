#' mapCountry
#'
#' @param year The year the user wants to see reflected by borders.
#' @param dataset The dataset of historical borders.
#'
#' @return This function will return a map of a country at a given time period.
#' @export
#'
#' @examples
#' mapCountry(1825, "Pakistan")
#' mapCountry(1875, "Zambia")
#'

mapCountry <- function(year, country, rda_path = "data/world.rda") {
  library(sf)
  library(dplyr)
  library(ggplot2)

  e <- new.env()
  load(rda_path, envir = e)
  data <- e[[ls(e)[1]]]

  data <- st_zm(data)
  data <- st_make_valid(data)

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

  data_sub <- data %>% filter(NAME == country, year == target_year)
  data_sub <- data_sub[!st_is_empty(data_sub$geometry), ]

  if (nrow(data_sub) == 0) {
    stop("Geometry for ", country, " in ", target_year, " is empty or invalid.")
  }

  ggplot(data_sub) +
    geom_sf() +
    coord_sf(lims_method = "geometry_bbox") +
    ggtitle(paste0(country, " in ", target_year))
}
