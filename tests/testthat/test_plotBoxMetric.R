data("stackepi")
test1 <- function() plotBoxMetric(
    stackepi,
    metric = "exp"
)

test2 <- function() plotBoxMetric(
    addBins(stackepi, nbins = 5),
    metric = "exp"
)

data("stackepi_gr")
test3 <- function() plotBoxMetric(
    stackepi_gr,
    metric = "exp"
)

test_that("default parameters", {
    vdiffr::expect_doppelganger("plotBoxMetric default", test1)
})
test_that("with bins", {
    vdiffr::expect_doppelganger("plotBoxMetric bins", test2)
})
test_that("Backward compatibility with GRanges", {
    vdiffr::expect_doppelganger("plotBoxMetric GRanges", test3)
})
