test_that("input 'year' is numeric",{
  expect_error(optionsYears("Eighteen-hundred and five"))
})

test_that("optionsYears returns years in sorted order", {
  years <- optionsYears()
  expect_equal(years, sort(years))
})
