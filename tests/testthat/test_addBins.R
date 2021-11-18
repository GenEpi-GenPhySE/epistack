data("stackepi")
data("stackepi_gr")

test_that("Return rse", {
   expect_s4_class(addBins(stackepi), "RangedSummarizedExperiment")
})

test_that("Return GRanges", {
    expect_s4_class(addBins(stackepi_gr), "GRanges")
})

test_that("Return proper bins", {
    expect_equal(
        SummarizedExperiment::rowRanges(addBins(stackepi, nbins = 5))$bin,
        c(rep(1, 139), rep(2, 138), rep(3, 139), rep(4, 138), rep(5, 139))
    )
})

