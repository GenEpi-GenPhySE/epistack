data("stackepi")
test1 <- function() plotStackProfile(stackepi, zlim = c(0, 1))

data("stackepi_gr")
test2 <- function() plotStackProfile(stackepi_gr, zlim = c(0, 1),
                                     pattern = "window")


test_that("default parameters", {
    vdiffr::expect_doppelganger("Stack profile default", test1)
})

test_that("Backward compatibility with GRanges", {
    vdiffr::expect_doppelganger("Stack profile GRanges", test2)
})
