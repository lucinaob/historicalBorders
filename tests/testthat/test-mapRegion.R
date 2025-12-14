test_that("non-numeric year triggers year-not-found error", {
  expect_error(
    mapRegion("Nineteen-hundred and five", "Eastern Europe")
  )
})

test_that("invalid region throws error", {
  expect_error(
    mapRegion(1900, "Narnia"),
    regexp = "Region not recognized"
  )
})
