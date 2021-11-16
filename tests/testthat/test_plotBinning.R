data("stackepi")
rse <- stackepi
rse <- addBins(rse, nbins = 3)
plot_bin1 <- function() plotBinning(rse)

gr2 <- data.frame(bin = rep(c(1,2,3,4), each = 5))
plot_bin2 <- function() plotBinning(gr2, palette = colorRampPalette(c("blue4", "forestgreen", "coral3", "goldenrod")))

data("stackepi_gr")
gr <- stackepi_gr
gr <- addBins(gr, nbins = 3)
plot_bin3 <- function() plotBinning(gr)

test_that("default palette", {
    vdiffr::expect_doppelganger("plotBin default", plot_bin1)
})

test_that("custom palette", {
    vdiffr::expect_doppelganger("plotBin custom", plot_bin2)
})

test_that("backward compatibility", {
    vdiffr::expect_doppelganger("plotBin GRanges", plot_bin3)
})
