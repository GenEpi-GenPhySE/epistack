data("stackepi")
test1 <- function() plotBoxMetric(
    stackepi,
    metric = "exp"
)

test_that("default parameters", {
    vdiffr::expect_doppelganger("plotBoxMetric default", test1)
})
