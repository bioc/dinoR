library(tibble)

#test that footprintPerc works with random example data
NomeData <- createExampleData()
NomeData <- footprintCalc(NomeData)
NomeData <- footprintQuant(NomeData)

test_that("footprintPerc works", {
  expect_no_error(footprintPerc(NomeData))
})

#test that footprintPerc returns the correct output with default example data
NomeData <- createExampleData(samples=c("WT_1"),group=c("WT"),nROI=5,randomMeth=FALSE)
NomeData <- footprintCalc(NomeData)
NomeData <- footprintQuant(NomeData)
footPerc <- footprintPerc(NomeData)
footPerc$open_WT_1 <- round(footPerc$open_WT_1)

output <- tibble(ROI=c("ROI5","ROI4","ROI3","ROI1","ROI2"),
                 ROIgroup=rep("motif1",5),
                 tf_WT_1=rep(25,5),
                 open_WT_1=rep(55,5),
                 upNuc_WT_1=rep(0,5),
                 Nuc_WT_1=rep(20,5),
                 downNuc_WT_1=rep(0,5)
)

test_that("footprintPerc returns the correct output", {
    expect_equal(footPerc,output)
   })
