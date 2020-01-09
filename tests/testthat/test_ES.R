library(testthat)

test_credential <-
  es.credential(
    "entreprise",
    "admin123",
    "search",
    "finance",
    "realties"
  )

test_that("connection create a single url", {
  expect_equal(objected = length(test_credential),
               expected = 1)

})
