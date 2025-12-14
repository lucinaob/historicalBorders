test_that("input 'year' is numeric", {
  expect_error(
    mapAfrica("Nineteen-hundred and five")
  )
})
