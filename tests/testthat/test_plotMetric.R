data("stackepi")
test1 <- function() plotMetric(SummarizedExperiment::rowRanges(stackepi)$exp)

test_that(
    "default parameters", {
    vdiffr::expect_doppelganger("plotMetric default", test1)
})
