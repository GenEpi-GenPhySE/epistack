data("gRanges_test")
test1 <- function() plotBoxMetric(
    gRanges_test,
    metric = "exp"
)

test_that("default parameters", {
    vdiffr::expect_doppelganger("Stack profile default", test1)
})
