data("stackepi")
test1 <- function() plotAverageProfile(stackepi)

test_that("default parameters", {
    vdiffr::expect_doppelganger("plotAverageProfile defauls", test1)
})
