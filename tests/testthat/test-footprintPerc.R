library(tibble)

# test that footprintPerc returns the correct output with default example data
NomeData <- createExampleData(samples = c("WT_1"),
    group = c("WT"), nROI = 3, randomMeth = FALSE)
NomeData <- footprintCalc(NomeData)
NomeData <- footprintQuant(NomeData)
footPerc <- footprintPerc(NomeData)
footPerc$open_WT_1 <- round(footPerc$open_WT_1)

output <- tibble(
    ROI = c("ROI3", "ROI1", "ROI2"),
    ROIgroup = rep("motif1", 3),
    tf_WT_1 = rep(25, 3),
    open_WT_1 = rep(55, 3),
    upNuc_WT_1 = rep(0, 3),
    Nuc_WT_1 = rep(20, 3),
    downNuc_WT_1 = rep(0, 3)
)

test_that("footprintPerc returns the correct output", {
    expect_equal(footPerc, output)
})
