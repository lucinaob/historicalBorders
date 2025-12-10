test_that("input 'year' is numeric",{
  expect_error(mapWorld("Eighteen-hundred and five"))
})

test_that("mapWorld returns a ggplot object for valid input", {
  world_year <- unique(historicalBorders::world$year)[1]
  plot <- mapWorld(world_year)
  expect_s3_class(plot, "ggplot")
})

test_that("mapWorld does not modify the world dataset", {
  original <- historicalBorders::world
  year <- unique(original$year)[1]

  mapWorld(year)

  expect_identical(original, historicalBorders::world)
})
