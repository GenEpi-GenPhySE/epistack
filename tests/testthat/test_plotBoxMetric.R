data("gRanges_test")
test1 <- function() plotBoxMetric(
    gRanges_test,
    metric = "exp"
)

test_that("default parameters", {
    vdiffr::expect_doppelganger("plotBoxMetric default", test1)
})
