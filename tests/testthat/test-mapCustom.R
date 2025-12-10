test_that("mapCustom errors when lat/long are not length 2", {
  expect_error(mapCustom(1800, c(1), c(2, 3)))
  expect_error(mapCustom(1800, c(1, 2), c(3)))
  expect_error(mapCustom(1800, c(1, 2, 3), c(3, 4)))
})

test_that("mapCustom returns a ggplot object", {
  plot <- mapCustom(1800, c(40, 50), c(10, 20))
  expect_s3_class(plot, "ggplot")
})
