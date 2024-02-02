#NomeData <- createExampleData()
#NomeData <- footprintCalc(NomeData)
#NomeData <- footprintQuant(NomeData)
#res <- diNOMeTest(NomeData)
#fp <- footprintPerc(NomeData)

res <- read.table(system.file("extdata", "res.txt", package = "dinoR"),header=TRUE)
fp <- read.table(system.file("extdata", "fp.txt", package = "dinoR"),header=TRUE)


test_that("compareFootprints works", {
  expect_no_error(compareFootprints(fp,res,plotcols="black",plot=FALSE))
})

