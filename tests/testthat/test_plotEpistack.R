data("stackepi")
plot_epistack <- function() plotEpistack(
    stackepi,
    metric_col = "exp",
    ylim = c(0, 1),
    metric_transfunc = function(x) log10(x+1)
)

data("stackepi_gr")
plot_epistack_gr <- function() plotEpistack(
    stackepi_gr,
    metric_col = "exp",
    ylim = c(0, 1),
    metric_transfunc = function(x) log10(x+1),
    patterns = "window"
)


test_that("epistack log10 trans", {
    vdiffr::expect_doppelganger("plotEpistack log10 tran", plot_epistack)
})

test_that("epistack backward comp GRanges", {
    vdiffr::expect_doppelganger("plotEpistack GRanges", plot_epistack_gr)
})
