test_that("optionsRegions validates continent input", {
  expect_error(optionsRegions("Canada"))   # invalid
  expect_no_error(optionsRegions("Asia"))  # valid
})

test_that("optionsRegions returns a data frame", {
  res <- optionsRegions()
  expect_s3_class(res, "data.frame")
})

test_that("optionsRegions with no argument returns all continents included", {
  res <- optionsRegions()

  expected <- c("Africa", "Americas", "Asia", "Europe", "Oceania")
  expect_true(all(expected %in% unique(res$CONTINENT)))
})
