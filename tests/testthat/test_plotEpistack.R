data("gRanges_test")
plot_epistack <- function() plotEpistack(
    gRanges_test,
    metric_col = "exp",
    ylim = c(0, 1),
    metric_transfunc = function(x) log10(x+1)
)

test_that("epistack log10 trans", {
    vdiffr::expect_doppelganger("plotEpistack log10 tran", plot_epistack)
})
