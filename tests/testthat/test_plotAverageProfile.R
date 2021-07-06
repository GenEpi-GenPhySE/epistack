data("gRanges_test")
test1 <- function() plotAverageProfile(gRanges_test)

test_that("default parameters", {
    vdiffr::expect_doppelganger("plotAverageProfile defauls", test1)
})
