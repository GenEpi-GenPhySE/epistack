data("stackepi")
test1 <- function() plotAverageProfile(stackepi)
data("stackepi_gr")
test2 <- function() plotAverageProfile(stackepi_gr, pattern = "window")

test_that("default parameters", {
    vdiffr::expect_doppelganger("plotAverageProfile defauls", test1)
})

test_that("backward compatibility", {
    vdiffr::expect_doppelganger("plotAverageProfile GRanges", test2)
})
