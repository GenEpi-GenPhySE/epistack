#' plotBoxMetric
#'
#' @param gr a GRanges input
#' @param trans_func a function to transform value of x.
#' Useful to apply log10 transformation (i.e. with `trans_func = function(x) log10(x+1)`).
#' @param colorPalette a function of parameter `n` that returns a palette of `n` colors.
#' @return a boxplot
#' @export
#'
#' @examples
#' boxplot_profileExp(gRanges_test$exp)
plotBoxMetric <- function(
    gr,
    metric = "expr", title = "Metric",
    trans_func = function(x) x,
    ylim = NULL, ylab = "metric",
    palette = colorRampPalette(c("magenta", "black", "green"))
){
    if (!is.null(gr$bin)) {
        boxplot(
            lapply(unique(gr$bin), function(i) trans_func(mcols(gr)[gr$bin == i, metric])),
            ylab = ylab, pch = 19, ylim = ylim,
            col = palette(length(unique(gr$bin))), axes = FALSE
        )
    } else {
        boxplot(
            trans_func(mcols(gr)[[metric]]),
            ylab = ylab, pch = 19, ylim = ylim,
            col = palette(1), axes = FALSE
        )
    }
    axis(1)
    axis(2, at = ylim)
    mtext(side = 3, title, line = 0.5,  cex = 0.8)
}
