#' mapRegion
#'
#' @param year The year the user wants to see reflected by borders.
#' @param dataset The dataset of historical borders.
#'
#' @return This function will return a map of a region at a given time period.
#' @export
#'
#' @examples
#' mapRegion(1825, "Eastern Europe")
#' mapRegion(1875, "Western Africa")

mapRegion <- function(year, region, dataset = NULL) {

  region_bounds <- list(
    # AFRICA
    "Eastern Africa"  = c(xmin = 20,  xmax = 55,  ymin = -12, ymax = 20),
    "Middle Africa"   = c(xmin = 10,  xmax = 30,  ymin = -10, ymax = 10),
    "Northern Africa" = c(xmin = -20, xmax = 35,  ymin = 15,  ymax = 38),
    "Southern Africa" = c(xmin = 10,  xmax = 35,  ymin = -35, ymax = -10),
    "Western Africa"  = c(xmin = -20, xmax = 20,  ymin = 0,   ymax = 25),

    # AMERICAS
    "Caribbean"       = c(xmin = -90, xmax = -55, ymin = 10,  ymax = 27),
    "Central America" = c(xmin = -95, xmax = -75, ymin = 5,   ymax = 20),
    "Northern America"= c(xmin = -170,xmax = -50, ymin = 15,  ymax = 80),
    "South America"   = c(xmin = -85, xmax = -30, ymin = -60, ymax = 15),

    # ASIA
    "Central Asia"        = c(xmin = 45,  xmax = 90,  ymin = 30,  ymax = 55),
    "Eastern Asia"        = c(xmin = 100, xmax = 150, ymin = 20,  ymax = 55),
    "South-eastern Asia"  = c(xmin = 92,  xmax = 150, ymin = -12, ymax = 25),
    "Western Asia"        = c(xmin = 25,  xmax = 60,  ymin = 12,  ymax = 45),

    # EUROPE
    "Eastern Europe"  = c(xmin = 20,  xmax = 60,  ymin = 42,  ymax = 70),
    "Northern Europe" = c(xmin = -10, xmax = 30,  ymin = 50,  ymax = 72),
    "Southern Europe" = c(xmin = -10, xmax = 30,  ymin = 35,  ymax = 47),
    "Western Europe"  = c(xmin = -10, xmax = 15,  ymin = 42,  ymax = 55),

    # OCEANIA
    "Australia and New Zealand" = c(xmin = 110, xmax = 180, ymin = -50, ymax = -10),
    "Melanesia"    = c(xmin = 140, xmax = 170, ymin = -25, ymax = 0),
    "Micronesia"   = c(xmin = 130, xmax = 165, ymin = 0,   ymax = 15),
    "Polynesia"    = c(xmin = -160, xmax = -110, ymin = -30, ymax = 10)
  )

  if (!region %in% names(region_bounds)) {
    stop(
      "Region not recognized: '", region,
      "'. Use one of:\n",
      paste(names(region_bounds), collapse = ", ")
    )
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

  bounds <- region_bounds[[region]]

  xmin <- bounds["xmin"]
  xmax <- bounds["xmax"]
  ymin <- bounds["ymin"]
  ymax <- bounds["ymax"]

  print(
    ggplot(data) +
      geom_sf +
      coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
      ggtitle(paste0(region, " in ", year)) +
      theme_void()
  )
}
