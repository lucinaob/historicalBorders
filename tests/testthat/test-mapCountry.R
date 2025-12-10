test_that("mapCountry errors for country not in dataset", {
  expect_error(
    mapCountry(1800, "Atlantis"),
    regexp = "not found in dataset"
  )
})

test_that("mapCountry returns ggplot for valid country and year", {
  year <- unique(historicalBorders::world$year)[1]
  country <- unique(historicalBorders::world$NAME)[1]

  plot <- mapCountry(year, country)
  expect_s3_class(plot, "ggplot")
})
