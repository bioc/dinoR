library(tibble)

# check that it works with randomly generated data
NomeData <- createExampleData()
NomeData <- footprintCalc(NomeData)
NomeData <- footprintQuant(NomeData)

test_that("diNOMeTest works", {
  expect_no_error(diNOMeTest(NomeData))
})
