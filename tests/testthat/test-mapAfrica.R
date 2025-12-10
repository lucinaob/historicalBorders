test_that("input 'year' is numeric", {
  expect_error(
    mapAfrica("Nineteen-hundred and five"),
    regexp = "numeric"
  )
})

test_that("input year triggers message about closest borders", {
  nonexistent_year <- 9999
  expect_message(
    mapAfrica(nonexistent_year),
    regexp = "mapped to borders"
  )
})
