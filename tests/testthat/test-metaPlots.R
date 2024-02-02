library(tibble)

#test that the output is as expected when using default example data
NomeData <- createExampleData(samples=c("WT_1"),group=c("WT"),nROI=3,randomMeth=FALSE)
plotData <- metaPlots(NomeData,span=0.2)
plotData$protection <- round(plotData$protection)
plotData$loess <- round(plotData$loess)

output <- tibble(position = -50:50,type=rep("motif1",101),
                 protection=rep(46,101),loess=rep(46,101),
                  sample=rep("WT_1",101))

test_that("metaPlots returns the correct output", {
   expect_equal(plotData,output)
 })

