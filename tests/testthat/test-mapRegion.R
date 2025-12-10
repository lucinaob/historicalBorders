test_that("non-numeric year triggers year-not-found message but no error", {
  expect_message(
    mapRegion("Nineteen-hundred and five", "Eastern Europe"),
    regexp = "closest year"
  )
})

test_that("invalid region throws error", {
  expect_error(
    mapRegion(1900, "Narnia"),
    regexp = "Region not recognized"
  )
})
