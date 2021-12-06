data("stackepi")
plot_epistack <- function() plotEpistack(
    stackepi,
    metric_col = "exp",
    ylim = c(0, 1),
    metric_transfunc = function(x) log10(x+1)
)

plot_epistack_title <- function() plotEpistack(
    stackepi,
    metric_col = "exp",
    ylim = c(0, 1),
    metric_transfunc = function(x) log10(x+1),
    main = "DNA methylation and gene expression"
)

plot_epistack_bin <- function() plotEpistack(
    addBins(stackepi),
    metric_col = "exp",
    ylim = c(0, 1),
    metric_transfunc = function(x) log10(x+1),
    main = "with bins"
)

data("stackepi_gr")
plot_epistack_gr <- function() plotEpistack(
    stackepi_gr,
    metric_col = "exp",
    ylim = c(0, 1),
    metric_transfunc = function(x) log10(x+1),
    patterns = "window"
)

plot_epistack_tiny <- function() plotEpistack(
    stackepi[1:200, ]
)


test_that("epistack log10 trans", {
    vdiffr::expect_doppelganger("plotEpistack log10 tran", plot_epistack)
})

test_that("epistack main title", {
    vdiffr::expect_doppelganger("plotEpistack main title", plot_epistack_title)
})

test_that("epistack bins", {
    vdiffr::expect_doppelganger("plotEpistack with bins", plot_epistack_bin)
})

test_that("epistack backward comp GRanges", {
    vdiffr::expect_doppelganger("plotEpistack GRanges", plot_epistack_gr)
})

test_that("epistack tiny matrix", {
    vdiffr::expect_doppelganger("epistack tiny matrix", plot_epistack_tiny)
})
