data("stackepi")
test1 <- function() plotAverageProfile(stackepi)
data("stackepi_gr")
test2 <- function() plotAverageProfile(stackepi_gr, pattern = "window")
test3 <- function() plotAverageProfile(stackepi, y_title = "Y-Title")

test_that("default parameters", {
    vdiffr::expect_doppelganger("plotAverageProfile defauls", test1)
})

test_that("backward compatibility", {
    vdiffr::expect_doppelganger("plotAverageProfile GRanges", test2)
})

test_that("Y-axis title", {
    vdiffr::expect_doppelganger("plotAverageProfile y-title", test3)
})
