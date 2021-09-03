data("stackepi")
gr <- stackepi
gr <- addBins(gr, nbins = 3)
plot_bin1 <- function() plotBinning(gr)

gr2 <- data.frame(bin = rep(c(1,2,3,4), each = 5))
plot_bin2 <- function() plotBinning(gr2, palette = colorRampPalette(c("blue4", "forestgreen", "coral3", "goldenrod")))

test_that("default palette", {
    vdiffr::expect_doppelganger("plotBin default", plot_bin1)
})

test_that("custom palette", {
    vdiffr::expect_doppelganger("plotBin custom", plot_bin2)
})
