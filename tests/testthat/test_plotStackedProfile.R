test1 <- function() plotStackedProfile(gRanges_test, zlim = c(0, 1))

test_that("default parameters", {
    vdiffr::expect_doppelganger("Stack profile default", test1)
})
