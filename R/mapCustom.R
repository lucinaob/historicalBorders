mapCustom <- function(year, lat, long, dataset = NULL){
  data <- read_sf('data/world_years.geojson')

  if(length(lat) != 2 || length(long) != 2){
    stop("Please enter 2 pairs of latitude and longitude values!")
  }

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
    print("We will map geographic data here!")
  } else{
    data |>
      ggplot() +
      geom_sf() +
      theme_void() +
      ylim(long) +
      xlim(lat)
  }

}
